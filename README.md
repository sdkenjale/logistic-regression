# logistic-regression
churn_input = as.data.frame( read.csv("C:/Users/Vikrant/Downloads/churn.csv") )
head(churn_input)
sum(churn_input$Churned)
Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic1)
Churn_logistic2 <- glm (Churned~Age + Married + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic2)
Churn_logistic3 <- glm (Churned~Age + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic3)
summary(Churn_logistic2)
pchisq(.9 , 1, lower=FALSE)
install.packages("ROCR")
library(ROCR)
pred = predict(Churn_logistic3, type="response")
predObj = prediction(pred, churn_input$Churned )
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 
