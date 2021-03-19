library(tidyverse)
library(bertens)
library(pROC)
library(gbm)
library(rmda)
eclipse.raw <- read_csv("eclipse.csv") %>% filter (VISIT == "2 Years") %>% select (id = SUBJECT_ID, Age = AGE, sex = SEX, 
                                               fev1 = FEV1PSPC) %>%
                                       mutate(id = str_remove(id, "xyl"))


# better cardiovascular data also available.

packyear <- read_table2("packyear.txt") %>% select (id = SUBJECT_ID, packyears = SUPKYR) %>%
                                            mutate(id = str_remove(id, "ecl"))

cv_cond <- read_csv("cv_cond.csv") %>% select (id= SUBJECT_ID, 
                                               strokeHx = ATS8E,
                                               heartAttackHx = ATS8D) %>%
                                       mutate(id = str_remove(id, "xyl"))


exacerbation <- read_csv("exacerbation.csv") %>% select(id= SUBJECT_ID, 
                                                        year = YRCAT,
                                                        moderateSevereExacCount = EXACNUM1) %>%
                                                        #followup = OBSTIME) %>%
               pivot_wider(names_from = year, values_from = moderateSevereExacCount) %>%
               mutate(year1 = `Year 1`,
                      year2to3 = `Years 1-3` - year1,
                      id = str_remove(id, "xyl")) %>%
               select (id, year1, year2to3)


# the paper defines A history of vascular disease was defined as stroke, transient ischemic attack, or peripheral arterial disease.
# we used strokeHx | heartAttackHx as a proxy for it in ECLIPSE.
eclipse <- eclipse.raw %>% left_join(cv_cond, by = "id") %>% left_join(packyear, by = "id") %>%
                           mutate(strokeHx  = recode (strokeHx,
                                                      "Y" = "TRUE",
                                                      "N" = "FALSE",
                                                      "U" = "NA")) %>%
                           mutate(heartAttackHx  = recode (heartAttackHx,
                                                      "Y" = "TRUE",
                                                      "N" = "FALSE")) %>%
                           mutate(heartAttackHx = as.logical(heartAttackHx),
                                  strokeHx = as.logical(strokeHx),
                                  vascularDx = (strokeHx | heartAttackHx)) %>%
                          select(-strokeHx, -heartAttackHx) %>%
                          left_join(exacerbation, by = "id") %>%
                        #  filter(packyears > 0) %>%
                          mutate(Observed_Exac_in2to3 = ifelse(year2to3>0, 1, 0))  %>%
                          mutate(predictedBertens = 
                                   bertens(exacerbationHx = (year1>0), 
                                           fev1=fev1, 
                                           packYears = packyears,
                                           vascularDx = vascularDx)) 

eclipseComplete <- eclipse %>% drop_na()  %>% filter (predictedBertens != 0)
#second filteration is temporary till we figure out the logistics issue.


roc(predictor=eclipseComplete$predictedBertens, response = eclipseComplete$Observed_Exac_in2to3,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          
  


calibrate.plot(y = eclipseComplete$Observed_Exac_in2to3, p = eclipseComplete$predictedBertens)

dc_bertens <- decision_curve(Observed_Exac_in2to3 ~ predictedBertens, data = eclipseComplete)
dc_history <- decision_curve(Observed_Exac_in2to3 ~ (year1>0), data = eclipseComplete)
plot_decision_curve(list(dc_bertens, dc_history),
                    curve.names = c("Bertens Model", "Exacerbation History"),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE) #remove cost benefit axis)

write.csv(eclipse, "eclipse_bertens.csv")