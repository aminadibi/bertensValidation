library(dplyr)
library(tidyr)
library(bertens)
library(stringr)

eclipse.raw <- read_csv("eclipse.csv") %>% filter (VISIT == "2 Years") %>% select (id = SUBJECT_ID, Age = AGE, sex = SEX, 
                                               fev1 = FEV1PSPC) %>%
                                       mutate(id = str_remove(id, "xyl"))


# better cardiovasuclar data also available.

packyear <- read_table2("packyear.txt") %>% select (id = SUBJECT_ID, packyears = SUPKYR) %>%
                                            mutate(id = str_remove(id, "ecl"))

cv_cond <- read_csv("cv_cond.csv") %>% select (id= SUBJECT_ID, 
                                               strokeHx = ATS8E,
                                               heartAttackHx = ATS8D) %>%
                                       mutate(id = str_remove(id, "xyl"))


exacerbation <- read_csv("exacerbation.csv") %>% select(id= SUBJECT_ID, 
                                                        year = YRCAT,
                                                        exacCount = EXACNUM1) %>%
                                                        #followup = OBSTIME) %>%
               pivot_wider(names_from = year, values_from = exacCount) %>%
               mutate(year1 = `Year 1`,
                      year2to3 = `Years 1-3` - year1,
                      id = str_remove(id, "xyl")) %>%
               select (id, year1, year2to3)



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
                          mutate(predictedBertens = 
                                   bertens(exacerbationHx = year1, 
                                           fev1=fev1, 
                                           packYears = packyears,
                                           vascularDx = vascularDx))
                           
  