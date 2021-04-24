temp <- dc_bertens_1yr$derived.data
temp%>% filter(thresholds == 0.4 & model == "All")

Calc <- eclipseComplete %>%
  mutate(BertensWillHaveExac = ifelse(predictedBertens1Yr>=0.40, 1, 0),
         truePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==1)),
         falsePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==0)),
         trueNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==0)),
         falseNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==1)))

# All <- eclipseComplete %>%
#   mutate(BertensWillHaveExac = ifelse(predictedBertens1Yr>=0.40, 1, 0),
#          truePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==1)),
#          falsePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==0)),
#          trueNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==0)),
#          falseNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==1)))

Calc %>% select(truePos, falsePos, trueNeg, falseNeg) %>% summarise(count = sum(truePos),
                                                                    count2 = sum(falsePos),
                                                                    count3 = sum(trueNeg),
                                                                    count4 = sum(falseNeg))



Calc %>% summarise(count = sum(truePos)/n())
Calc %>% summarise(count = sum(falsePos)/n())
0.289 - 0.0927*(0.4/0.6)


CalcACCEPT <- eclipseACCEPT %>%
  mutate(ACCEPTWillHaveExac = ifelse(predicted_exac_probability>=0.40, 1, 0),
         truePos = ((ACCEPTWillHaveExac == 1) & (hadExac==1)),
         falsePos = ((ACCEPTWillHaveExac == 1) & (hadExac==0)),
         trueNeg = ((ACCEPTWillHaveExac == 0) & (hadExac==0)),
         falseNeg = ((ACCEPTWillHaveExac == 0) & (hadExac==1)))

# All <- eclipseComplete %>%
#   mutate(BertensWillHaveExac = ifelse(predictedBertens1Yr>=0.40, 1, 0),
#          truePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==1)),
#          falsePos = ((BertensWillHaveExac == 1) & (Observed_Exac_in2==0)),
#          trueNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==0)),
#          falseNeg = ((BertensWillHaveExac == 0) & (Observed_Exac_in2==1)))

CalcACCEPT %>% select(truePos, falsePos, trueNeg, falseNeg) %>% summarise(count = sum(truePos),
                                                                    count2 = sum(falsePos),
                                                                    count3 = sum(trueNeg),
                                                                    count4 = sum(falseNeg))



CalcACCEPT %>% summarise(count = sum(truePos)/n())
CalcACCEPT %>% summarise(count = sum(falsePos)/n())

0.539 - (0.360*40/60)



