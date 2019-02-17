########### Running Models on real test set ################
#Filtering the test set with significant features
test$ExterCond.new <- as.numeric(factor(test$ExterCond, 
                                        levels = c("Ex", "Fa","Gd", "TA","Po"),
                                        labels = c(5,2,4,3,1) ,ordered = TRUE))
test$HeatingQC.new <- as.numeric(factor(test$HeatingQC, 
                                        levels = c("Ex", "Fa","Gd", "TA","Po"),
                                        labels = c(5,2,4,3,1) ,ordered = TRUE))
test$CentralAir.new <- as.numeric(factor(test$CentralAir, 
                                         levels = c("N", "Y"),
                                         labels = c(0,1) ,ordered = TRUE))

var.test <- c('OverallQual','OverallCond','YearBuilt','ExterCond.new',
              'TotalBsmtSF','HeatingQC.new', 
              'CentralAir.new','GrLivArea','BedroomAbvGr','KitchenAbvGr',
              'TotRmsAbvGrd','Fireplaces',
              'GarageArea','OpenPorchSF','PoolArea',
              'YrSold')
final.test <- test[,var.test]

#linear Regression
pred.kag <- predict(model.lm, newdata = final.test)
submit.lm <- exp(pred.kag)
lm_sub <- data.frame(test$Id, submit.lm)
colnames(lm_sub)<- c("Id","SalePrice")
write.csv(lm_sub, "lm_submission.csv", row.names = FALSE)
#CART
pred.kag2 <- predict(model.cart, newdata = final.test)
submit.cart <- exp(pred.kag2)
cart_sub <- data.frame(test$Id, submit.cart)
colnames(cart_sub)<- c("Id","SalePrice")
write.csv(cart_sub, "cart_submission.csv",row.names = FALSE)
#Random Forest
pred.kag3 <- predict(model.rf, newdata = final.test)
submit.rf <- exp(pred.kag3)
rf_sub <- data.frame(test$Id, submit.rf)
colnames(rf_sub)<- c("Id","SalePrice")
write.csv(rf_sub, "rf_submission.csv", row.names = FALSE)



