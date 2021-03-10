library(dplyr)
library(bertens)

eclipse <- read_csv("eclipse.csv") %>% select (id = SUBJECT_ID, visit = VISIT, Age = AGE, sex = SEX, 
                                               vascularDx = CMSTATIN, fev1 = FEV1PSPC)
# better cardiovasuclar data also available.