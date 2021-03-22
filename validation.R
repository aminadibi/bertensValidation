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
                      year2 = `Year 2`,
                      year2to3 = `Years 1-3` - year1,
                      id = str_remove(id, "xyl")) %>%
               select (id, year1, year2, year2to3)


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
                          mutate(Observed_Exac_in2 = ifelse(year2>0, 1, 0))  %>%
                          mutate(year1ExacHx = ifelse(year1>0, 1, 0),
                                 predictedBertens = 
                                   bertens(exacerbationHx = year1ExacHx, 
                                           fev1=fev1, 
                                           packYears = packyears,
                                           vascularDx = vascularDx),
                                 predictedBertens1Yr = 1-sqrt(1-predictedBertens)) 

eclipseComplete <- eclipse %>% drop_na()

eclipseCompleteSmokers <- eclipseComplete %>% filter (packyears>=1)

# for two year prediction
roc(predictor=eclipseComplete$predictedBertens, response = eclipseComplete$Observed_Exac_in2to3,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          
 
#history alone for two years
roc(predictor=eclipseComplete$year1ExacHx, response = eclipseComplete$Observed_Exac_in2to3,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          

# for 1 year prediction
roc(predictor=eclipseComplete$predictedBertens1Yr, response = eclipseComplete$Observed_Exac_in2,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          

# for 1 year smokers only prediction 
roc(predictor=eclipseCompleteSmokers$predictedBertens1Yr, response = eclipseCompleteSmokers$Observed_Exac_in2,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          


#history alone for 1 year
roc(predictor=eclipseComplete$year1ExacHx, response = eclipseComplete$Observed_Exac_in2,
    plot = T, ci=T, print.auc=TRUE,  boot.n=1000, ci.alpha=0.95, stratified=FALSE, show.thres=TRUE, grid=TRUE)                          


# for two year outcome
calibrate.plot(y = eclipseComplete$Observed_Exac_in2to3, p = eclipseComplete$predictedBertens)

# for one year outcome
calibrate.plot(y = eclipseComplete$Observed_Exac_in2, p = eclipseComplete$predictedBertens1Yr)


dc_bertens_2yrs <- decision_curve(Observed_Exac_in2to3 ~ predictedBertens, data = eclipseComplete)
dc_history_2yrs <- decision_curve(Observed_Exac_in2to3 ~ year1ExacHx, data = eclipseComplete)
plot_decision_curve(list(dc_bertens_2yrs, dc_history_2yrs),
                    curve.names = c("Bertens Model", "Exacerbation History"),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE) #remove cost benefit axis)

dc_bertens_1yr <- decision_curve(Observed_Exac_in2 ~ predictedBertens1Yr, data = eclipseComplete)
dc_history_1yr <- decision_curve(Observed_Exac_in2 ~ year1ExacHx, data = eclipseComplete)
plot_decision_curve(list(dc_bertens_1yr, dc_history_1yr),
                    curve.names = c("Bertens 1Yr", "Exacerbation History"),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE) #remove cost benefit axis)

# one year Smokers
dc_bertens_1yrSmokers <- decision_curve(Observed_Exac_in2 ~ predictedBertens1Yr, data = eclipseCompleteSmokers)
dc_history_1yrSmokers <- decision_curve(Observed_Exac_in2 ~ year1ExacHx, data = eclipseCompleteSmokers)
plot_decision_curve(list(dc_bertens_1yrSmokers, dc_history_1yrSmokers),
                    curve.names = c("Bertens 1Yr Smokers", "Exacerbation History Smokers"),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE) #remove cost benefit axis)


write.csv(eclipse, "eclipse_bertens.csv")


## ACCEPT

externalResults$hadSevereExac <-ifelse(externalResults$observedSevereExacCount>0, 1, 0)

dc_accept <- decision_curve(hadSevereExac ~ predicted_severe_exac_rate, data = externalResults)
dc_history_accept <- decision_curve(hadSevereExac ~ obsExac_yr2, data = externalResults)
plot_decision_curve(list(dc_accept, dc_history_accept),
                    curve.names = c("ACCEPT Severe", "Exacerbation History - Severe"),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE) #remove cost benefit axis)
