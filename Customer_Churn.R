library(readxl)
library(tidyverse)
library(skimr)

customer_churn = read_excel('Cellphone.xlsx', sheet='Data')
customer_churn %>% skim_without_charts()

customer_churn$ContractRenewal=factor(customer_churn$ContractRenewal)
customer_churn$DataPlan=factor(customer_churn$DataPlan)
customer_churn$CustServCalls=factor(customer_churn$CustServCalls)

customer_churn$Churn=factor(customer_churn$Churn)
customer_churn %>% group_by(Churn) %>% count()


library(psych)
pairs.panels(customer_churn[,c(1:11)])

churn_model=glm(formula = Churn~AccountWeeks+ContractRenewal+DataPlan+
                  DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+
                  OverageFee+RoamMins, data = customer_churn)

summary(churn_model)
vif(churn_model)

trainid = sample(1:nrow(customer_churn), floor(0.8*nrow(customer_churn)))
train_set   = customer_churn[trainid,]
test_set    = customer_churn[-trainid,]

train_set

test_set


model_0=glm(formula = Churn~AccountWeeks+ContractRenewal+DataPlan+
              DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+
              OverageFee+RoamMins,family = binomial(link = 'logit'),
            data = customer_churn)

summary(model_0)

model_1=glm(formula = Churn ~ ContractRenewal+CustServCalls+
              MonthlyCharge+RoamMins, 
            family  = binomial(link = logit), 
            data    = customer_churn)
summary(model_1)


model_2 = glm(formula = Churn ~ AccountWeeks+ContractRenewal+CustServCalls+DayMins+
                MonthlyCharge+DayCalls+OverageFee+RoamMins, 
              family  = binomial(link = logit), 
              data    = customer_churn)
summary(model_2)

anova(model_0,model_1, test = 'Chisq')
anova(model_0,model_2, test = 'Chisq')
anova(model_1,model_2, test = 'Chisq')

prediction0 = predict(model_0, newdata = test_set, type = "response")
pred_data0=ifelse(prediction0 >= 0.5, 1, 0)

prediction1 = predict(model_1, newdata = test_set, type = "response")
pred_data1=ifelse(prediction1 >= 0.5, 1, 0)

prediction2 = predict(model_2, newdata = test_set, type = "response")
pred_data2=ifelse(prediction2 >= 0.5, 1, 0)


library(caret)
confusionMatrix(as.factor(pred_data0), 
                as.factor(test_set$Churn), 
                positive = "1", 
                mode = "everything")

confusionMatrix(as.factor(pred_data1), 
                as.factor(test_set$Churn), 
                positive = "1", 
                mode = "everything")

confusionMatrix(as.factor(pred_data2), 
                as.factor(test_set$Churn), 
                positive = "1", 
                mode = "everything")

library(pROC)

plotROC(test_set$Churn, pred_data1)
