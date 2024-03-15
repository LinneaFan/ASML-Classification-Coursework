## ----warning = FALSE----------------------------------------------------------------------------------------------
if(!require("ggplot2")) install.packages("ggplot2",update = F,ask = F)
if(!require("stringr")) install.packages("stringr",update = F,ask = F)
if(!require("scales")) install.packages("scales",update = F,ask = F)
if(!require("grid")) install.packages("grid",update = F,ask = F)
if(!require("GGally")) install.packages("GGally",update = F,ask = F)
if(!require("rpart")) install.packages("rpart",update = F,ask = F)
if(!require("rpart.plot")) install.packages("rpart.plot",update = F,ask = F)
if(!require("mice")) install.packages("mice",update = F,ask = F)
if(!require("caret")) install.packages("caret",update = F,ask = F)
if(!require("Hmisc")) install.packages("Hmisc",update = F,ask = F)
if(!require("plyr")) install.packages("plyr",update = F,ask = F)
if(!require("pROC")) install.packages("pROC",update = F,ask = F)
if(!require("randomForest")) install.packages("randomForest",update = F,ask = F)
if(!require("e1071")) install.packages("e1071",update = F,ask = F)
if(!require("MASS")) install.packages("MASS",update = F,ask = F)
if(!require("car")) install.packages("car",update = F,ask = F)
if(!require("ResourceSelection")) install.packages("ResourceSelection",update = F,ask = F)
if(!require("rms")) install.packages("rms",update = F,ask = F)

library(ggplot2)
library(stringr)
library(scales)
library(grid)
library(GGally)
library(rpart)
library(rpart.plot)
library(mice)
library(caret)
library(Hmisc)
library(plyr)
library(pROC)
library(randomForest)
library("e1071")
library(MASS)
library(car)
library(ResourceSelection)
library(rms)


## ----warning=FALSE------------------------------------------------------------------------------------------------
data <- read.csv("telecom.csv", header=T)
attach(data)
class(data)


## -----------------------------------------------------------------------------------------------------------------
dim(data)
# summary(data)
# str(data)


## ----eval=FALSE---------------------------------------------------------------------------------------------------
## colSums(is.na(data))


## -----------------------------------------------------------------------------------------------------------------
hist(data$TotalCharges, breaks = 50, prob = TRUE, 
     main = "Histogram Of TotalCharges")


## -----------------------------------------------------------------------------------------------------------------
data$TotalCharges <- as.numeric(Hmisc::impute(data$TotalCharges, median))


## -----------------------------------------------------------------------------------------------------------------
str(data)


## -----------------------------------------------------------------------------------------------------------------
data$SeniorCitizen[data$SeniorCitizen == 0] <- "No"
data$SeniorCitizen[data$SeniorCitizen == 1] <- "Yes"


## -----------------------------------------------------------------------------------------------------------------
for(i in c(1:4,6:17,20)) {
  data[i] <- as.factor(unlist(data[i]))
}
str(data)


## -----------------------------------------------------------------------------------------------------------------
for(i in 9:14){
  print(xtabs(~ Churn + get(names(data)[i]), data = data))
}
levels(data$OnlineSecurity)[2] <- "No"
levels(data$OnlineBackup)[2] <- "No"
levels(data$DeviceProtection)[2] <- "No"
levels(data$TechSupport)[2] <- "No"
levels(data$StreamingTV)[2] <- "No"
levels(data$StreamingMovies)[2] <- "No"


## ----warning=FALSE------------------------------------------------------------------------------------------------
# describe(data[c("MonthlyCharges","TotalCharges")])
# data[c("MonthlyCharges", "TotalCharges")]

c_u_t <- function(x, n = 1) {
  result <- quantile(x, probs = seq(0, 1, 1/n))
 result[1] <- result[1]-0.001
  return(result)
}
data <- transform(data,
                  MonthlyCharges_c = cut(data$MonthlyCharges,
                  breaks = c_u_t(data$MonthlyCharges, n=4),
                  labels = c(1,2,3,4)),
                  TotalCharges_c = cut(data$TotalCharges,
                  breaks = c_u_t(data$TotalCharges, n=4),
                  labels = c(1,2,3,4)))
data <- within(data, {
  MonthlyCharges_c <- relevel(MonthlyCharges_c, ref = 1)
  TotalCharges_c <- relevel(TotalCharges_c, ref = 1)
})



## ----warning = FALSE----------------------------------------------------------------------------------------------
options(digits=4)

ggplot(data, aes(x = "" ,fill = Churn))+
   geom_bar(stat = "count", width = 0.4, position = 'stack')+
   coord_polar(theta = "y", start=0)+
   geom_text(stat = "count", 
             aes(label = scales::percent(..count../nrow(data), 0.01)), 
             size = 4, position=position_stack(vjust = 0.5)) +
   theme(
     panel.background = element_blank(),
     axis.title = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank()
   )

ggplot(data , aes(x =Churn , y =..count.. ,fill=Churn)) +
geom_bar(stat = "count", width = 0.4, position = 'identity')

table(Churn)


## ----warning=FALSE------------------------------------------------------------------------------------------------
cdata <- ddply(data, "Churn", summarise, tenure.median = median(tenure))
cdata
ggplot(data, aes(x = tenure, fill = Churn)) + geom_density(alpha = .3) + 
geom_vline(data = cdata, aes(xintercept = tenure.median,  colour = Churn),
               linetype = "dashed", size=1)
Percentage <- matrix(rep(1, nrow(data)),nrow = (nrow(data)),ncol = 1)
plot1 <- ggplot(data, aes(x = gender, y = Percentage, fill = Churn)) + geom_col(position = "fill")
plot2 <- ggplot(data, aes(x = SeniorCitizen, y = Percentage, fill = Churn)) + geom_col(position = "fill")
plot3 <- ggplot(data, aes(x = Partner, y = Percentage, fill = Churn)) + geom_col(position = "fill")
plot4 <- ggplot(data, aes(x = Dependents, y = Percentage, fill = Churn)) + geom_col(position = "fill")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y) 
  viewport(layout.pos.row = x, layout.pos.col = y)
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))
print(plot3, vp = vplayout(2, 1))
print(plot4, vp = vplayout(2, 2))


## -----------------------------------------------------------------------------------------------------------------
plot4 <- ggplot(data, aes(x=PhoneService, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot5 <- ggplot(data, aes(x=MultipleLines, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot6 <- ggplot(data, aes(x=InternetService, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot7 <- ggplot(data, aes(x=OnlineSecurity, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot8 <- ggplot(data, aes(x=OnlineBackup, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot9 <- ggplot(data, aes(x=DeviceProtection, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot10 <- ggplot(data, aes(x=TechSupport, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot11 <- ggplot(data, aes(x=StreamingTV, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))
plot12 <- ggplot(data, aes(x=StreamingMovies, y=Percentage, fill=Churn))+geom_col(position="fill")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,3)))
vplayout <- function(x,y) 
  viewport(layout.pos.row = x, layout.pos.col = y)

print(plot4, vp = vplayout(1, 1))
print(plot5, vp = vplayout(1, 2))
print(plot6, vp = vplayout(1, 3))
print(plot7, vp = vplayout(2, 1))
print(plot8, vp = vplayout(2, 2))
print(plot9, vp = vplayout(2, 3))
print(plot10, vp = vplayout(3, 1))
print(plot11, vp = vplayout(3, 2))
print(plot12, vp = vplayout(3, 3))


## ----warning=FALSE------------------------------------------------------------------------------------------------
cdata1 <- ddply(data, "Churn", summarise, MonthlyCharges.median=median(MonthlyCharges))
cdata2 <- ddply(data, "Churn", summarise, TotalCharges.median=median(TotalCharges))

plot13 <- ggplot(data, aes(x=MonthlyCharges, fill=Churn)) + geom_density(alpha=.3)+
	geom_vline(data=cdata1, aes(xintercept=MonthlyCharges.median,  colour=Churn),
               linetype="dashed", size=1)
plot14 <- ggplot(data, aes(x=TotalCharges, fill=Churn)) + geom_density(alpha=.3)+
	geom_vline(data=cdata2, aes(xintercept=TotalCharges.median,  colour=Churn),
               linetype="dashed", size=1)
plot15 <- ggplot(data, aes(x=Contract, y=Percentage, fill=Churn))+geom_col(position="fill")
plot16 <- ggplot(data, aes(x=PaperlessBilling, y=Percentage, fill=Churn))+geom_col(position="fill")
plot17 <- ggplot(data, aes(x=PaymentMethod, y=Percentage, fill=Churn))+geom_col(position="fill")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x,y) 
  viewport(layout.pos.row = x, layout.pos.col = y)
print(plot13, vp = vplayout(1, 1))
print(plot14, vp = vplayout(1, 2))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x,y) 
  viewport(layout.pos.row = x, layout.pos.col = y)
print(plot15, vp = vplayout(1, 1))
print(plot16, vp = vplayout(2, 1))
print(plot17, vp = vplayout(3, 1))


## -----------------------------------------------------------------------------------------------------------------
myFUN <-  function(x){chisq.test(data$Churn,x ,correct = TRUE)}

xx  = c("customerID","tenure","MonthlyCharges","TotalCharges")
dataset = data[,!names(data) %in% xx]

result <-  apply (dataset,2, myFUN)
p <-  function(x){x$p.value}
p2 <- as.data.frame(sapply(result,p))
result=as.data.frame(round(p2$`sapply(result, p)`,3))
A <- names(dataset)
rownames(result)=A
result


## -----------------------------------------------------------------------------------------------------------------
ggpairs(data,columns=c(5,18:19),ggplot2::aes(color=Churn))


## -----------------------------------------------------------------------------------------------------------------
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
data[num_columns] <- sapply(data[num_columns], as.numeric)

telco_int <- data[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))
telco_cat <- data[,-c(5,18,19)]


## -----------------------------------------------------------------------------------------------------------------
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
data_final <- cbind(telco_int,dummy)


## -----------------------------------------------------------------------------------------------------------------
set.seed(123)
data_final$Churn <- as.factor(data_final$Churn)
train <- sample(nrow(data_final),0.7*nrow(data_final)) 
tdata <- data_final[train,]
vdata <- data_final[-train,]


## -----------------------------------------------------------------------------------------------------------------
telco_prediction <- function(algorithm, prob, test = vdata){
  pred <- predict(algorithm, newdata=vdata[,-25], type = "class")
  table(pred, vdata$Churn)
  cmx <- confusionMatrix(vdata$Churn, pred)
  print(cmx)
  
  modelroc <- roc(vdata$Churn, prob[,2], levels = c("1", "0"), direction='>') 
  plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE,
       grid=c(0.1, 0.2), 
       grid.col=c("green", "red"), max.auc.polygon=TRUE,
       auc.polygon.col="skyblue", print.thres=TRUE)
  auc <- auc(modelroc)
}


## -----------------------------------------------------------------------------------------------------------------
set.seed(123)
dtree <- rpart(Churn~., data = tdata, method = "class", parms = list(split="information"))
dtree$cptable


## -----------------------------------------------------------------------------------------------------------------
dtree.pruned <- prune(dtree, cp=0.01)

rpart.plot(dtree, branch=1, type=2, fallen.leaves=T, sub="Decision Tree")

dtree.prob <- predict(dtree.pruned, vdata, type="prob")
telco_prediction(dtree.pruned, dtree.prob, vdata)


## ----warning=FALSE------------------------------------------------------------------------------------------------
#set.seed(123)

#err <- as.numeric()
#for(i in 1:(length(names(tdata)))-1){
# mtry_test <- randomForest(Churn~., data=tdata, mtry=i)
# err <- append(err, mean(mtry_test$err.rate))
#}
#print(err)


## -----------------------------------------------------------------------------------------------------------------
#mtry <- which.min(err)
#mtry


## -----------------------------------------------------------------------------------------------------------------
#ntree_fit <- randomForest(Churn~., data = tdata, mtry=4, ntree=1000)
#plot(ntree_fit)


## -----------------------------------------------------------------------------------------------------------------
set.seed(123)
fit.rf <- randomForest(Churn ~ ., data=tdata, proximity=FALSE,importance = FALSE,
                        ntree=500,mtry=4, do.trace=FALSE)
rf.prob <- predict(fit.rf, newdata=vdata[,-25], type="prob")
telco_prediction(fit.rf, rf.prob, vdata)


## -----------------------------------------------------------------------------------------------------------------
set.seed(123)
fit.svm <- svm(Churn~., data =tdata, probability=TRUE)
pred.svm <- predict(fit.svm, vdata, probability=TRUE)
svm.prob <- attr(pred.svm, "probabilities")
telco_prediction(fit.svm, svm.prob, vdata)


## -----------------------------------------------------------------------------------------------------------------
model_1 = glm(Churn ~ ., data = tdata, family = "binomial")
# summary(model_1)


## -----------------------------------------------------------------------------------------------------------------
model_2<- stepAIC(model_1, direction="both")
# summary(model_2)
vif(model_2)


## -----------------------------------------------------------------------------------------------------------------
model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
    Partner + InternetService.xFiber.optic + InternetService.xNo + 
    OnlineSecurity + OnlineBackup + TechSupport + 
    StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
    PaymentMethod.xElectronic.check, family = "binomial", data = tdata)
# summary(model_3)
vif(model_3)


## -----------------------------------------------------------------------------------------------------------------
model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
    Partner + InternetService.xFiber.optic + InternetService.xNo + 
    OnlineSecurity + OnlineBackup + TechSupport +  
    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
    PaymentMethod.xElectronic.check, family = "binomial", data = tdata)

# summary(model_4)
vif(model_4)


## -----------------------------------------------------------------------------------------------------------------
final_model <- model_3


## -----------------------------------------------------------------------------------------------------------------
pred <- predict(final_model, type = "response", newdata = vdata[,-25])
summary(pred)
vdata$prob <- pred

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(vdata$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")


## -----------------------------------------------------------------------------------------------------------------
perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


## -----------------------------------------------------------------------------------------------------------------
options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.277, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


## -----------------------------------------------------------------------------------------------------------------
cutoff_churn <- factor(ifelse(pred >=0.277, "Yes", "No"))
confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")

lpredict <- predict(final_model,type='response',newdata=vdata)
as.numeric(auc(as.numeric(vdata[,25]),lpredict))


## -----------------------------------------------------------------------------------------------------------------
set.seed(123)
folds <- createMultiFolds(y=data_final$Churn, k=10, times=100)
auc_value<-as.numeric()
for(i in 1:1000){
  fold_test <- data_final[folds[[i]],]
  fold_train <- data_final[-folds[[i]],]
  fold_predict <- predict(final_model,type='response',newdata=fold_test)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(fold_test[,25]),fold_predict)))
}
# auc_value
summary(auc_value)
mean(auc_value)


## -----------------------------------------------------------------------------------------------------------------
dd <- datadist(tdata)
options(datadist="dd")

fit1 <- lrm(Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
            Partner + InternetService.xFiber.optic + InternetService.xNo + 
            OnlineSecurity + OnlineBackup + TechSupport + 
            StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
            PaymentMethod.xElectronic.check,
            data = tdata,x=T,y=T)

cal1 <- calibrate(fit1, method='boot', B=1000)

plot(cal1,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability"
     )

