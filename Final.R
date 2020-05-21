# Library--------------------------------------------------

library(readr)
library(corrplot)
library(leaps)

# Data Loading and preparing ------------------------------

Admission_Predict_Ver1_1 <- read_csv("Admission_Predict_Ver1.1.csv")

data <- Admission_Predict_Ver1_1
originaldata <- data

rm(Admission_Predict_Ver1_1)


colnames(data)[colnames(data)=="GRE Score"] <- "GRE"
colnames(data)[colnames(data)=="TOEFL Score"] <- "TOEFL"
colnames(data)[colnames(data)=="University Rating"] <- "UniRating"
colnames(data)[colnames(data)=="Chance of Admit"] <- "AdmitChance"

data$`Serial No.` <- NULL
data
# Data Loading and preparing ------------------------------

# CORRELATION PLOT ----------------------------------------

#form the model matrix and correlation variable
varMat <- model.matrix(~.-Research,data=data)[,-1]  #Removing the first column as it has
#1's and 0's.

varMat
varCor <- cor(varMat)

#plot the correlation
corrplot(varCor,method = "circle",
         tl.col = "black", mar = c(0,0,2,0),
         title = "Graduate Admission numerical Variables Correlation")

corrplot(varCor,add = TRUE,                            # add the above plot
         type = "lower", method = "number",number.font = 2,
         number.cex = .75,col = "black",
         diag = FALSE,tl.pos = "n", cl.pos = "n")

rm(varMat, varCor)



# LINEAR REGRESSION ----------------------------------------

str(data)

#Check if there are any rows with missing values
anyNA(data)

regdata <- data

regdata$GRE <- as.integer(regdata$GRE)
regdata$TOEFL <- as.integer(regdata$TOEFL)
regdata$UniRating <- as.factor(regdata$UniRating)
regdata$Research <- as.logical(regdata$Research)

str(regdata)

contrasts(regdata$UniRating)

#Fit a model using all predictors
linear.fit=lm(AdmitChance ~.,data=regdata)
summary(linear.fit)

# SOP is not a reliable indicator with a p value of 0.73
#Fit a model using all predictors but SOP
linear.fit=lm(AdmitChance ~ .-SOP,data=regdata)
summary(linear.fit)

exp(coef(linear.fit))
# All predictors are significant now

#Use subsets and run the model
regfit.full = regsubsets(AdmitChance ~., regdata)
reg.summary <- summary(regfit.full)
reg.summary


names(reg.summary)

reg.summary$adjr2 #best with all indicators in place except SOP, 5 var => 0.819, 6 var => 0.820 marginal diff
reg.summary$cp #lowest with all predictors in place except SOP
reg.summary$rss #lowest with all predictors except SOP in place
reg.summary$bic #lowest with 5 predictors, SOP and UniRating not imporving BIC

windows()
par(mfrow = c(2,2))

xlab = "Number of Variables"
# 1st row 1st  column
plot(reg.summary$rss,xlab = xlab, ylab = "RSS",type = "l")
loc <- which.min(reg.summary$rss)
loc
points(loc,reg.summary$rss[loc], col = "red",cex = 2,pch = 20)

# 1st row 2nd  column
plot(reg.summary$adjr2,xlab = xlab, ylab = "Adjusted RSq",type = "l")
loc <- which.max(reg.summary$adjr2)
loc
points(loc,reg.summary$adjr2[loc], col = "red",cex = 2,pch = 20)

# 2nd row 1st column
plot(reg.summary$cp,xlab = xlab, ylab = "Cp",type = 'l')
loc <- which.min(reg.summary$cp)
loc
points(loc,reg.summary$cp[loc], col = "red",cex = 2,pch = 20)

# 2nd row 2nd column
plot(reg.summary$bic,xlab = xlab, ylab = "BIC",type = 'l')
loc <-  which.min(reg.summary$bic)
loc
points(loc,reg.summary$bic[loc], col = "red",cex = 2,pch = 20)

dev.off()

## What is the mean square error (base case)?
mse = round(reg.summary$rss[5]/nrow(regdata), 4)
mse #0.0035 for 5, 6, 7 variable model, lets see if train/test technique improves the result substantially

################ Attempt to Linear Regression with Train and Test subsets over 100 iterations ########
set.seed(123)
train = sample(c(TRUE,FALSE), nrow(regdata), replace = TRUE)

test = !train

# Find the best training set models
regfit.best = regsubsets(AdmitChance~., data = regdata[train,],nvmax = 7)

# Obtain the test set design matrix
test.mat = model.matrix(AdmitChance~., data = regdata[test,])

# Vector for errors
val.errors = rep(NA,7)

# Run for each number of variables in the model
for (i in 1:7) {
  # obtain the training set coefficients
  coefi = coef(regfit.best,id = i)
  
  # predict test set values
  pred = test.mat[,names(coefi)] %*% coefi
  
  # Obtain the MSE
  val.errors[i] = mean((regdata$AdmitChance[test] - pred)^2)
}

val.errors # 5 to 7 several variable scenarios are stable at 0.0035

predict.regsubsets =
  function(object,newdata,id,...){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form,newdata)
    coefi = coef(object,id = id)
    xvars = names(coefi)
    mat[,xvars] %*% coefi
  }

k <- 100
set.seed(123)
folds <- sample(1:k,nrow(regdata),  replace = TRUE)

cv.errors = matrix(NA,nrow = k,ncol = 7, dimnames = list(NULL, paste(1:7)))

# The fold and number of variables loops
for (j in 1:k) { # fold loop
  
  # The 19 best models with jth fold omitted
  bestfit.fold = regsubsets(AdmitChance ~., data = regdata[folds != j,],nvmax = 7)
  
  # The MSE for the fold prediction error
  for (i in 1:7) {# number of variable loop
    pred = predict.regsubsets(bestfit.fold, regdata[folds == j,],id = i)
    cv.errors[j,i] = mean((regdata$AdmitChance[folds == j] - pred)^2)
  }
}

# Find the mean across the fold MSE for each model
mean.cv.errors = apply(cv.errors,2,mean)
round(mean.cv.errors, 4) # best case is 0.0035 for 5 var model, not to different from base case 0.0036

par(mfrow = c(1,1))
plot(mean.cv.errors,type = 'b')
which.min(mean.cv.errors)

#############################################################


# Train and Test -----------------------------------------

set.seed(123)
num <- sample(1:500, nrow(data)*0.75, replace = FALSE)

train <- data[num,]
test <- data[-num,]

rm(num)

# Train and Test -----------------------------------------

temp <- test$AdmitChance
temp <- as.data.frame(temp)

# Random Forest -------------------------------------------

library(randomForest)
library(ggplot2)


rf <- randomForest(AdmitChance ~., data = train) # with all the variables

varImpPlot(rf, main = 'Model Importance Plot')

impplot <- rf$importance


impplot <- as.data.frame(impplot)

impplot$Attribute <- rownames(impplot)


p <- ggplot(data = impplot, aes(reorder(Attribute, IncNodePurity), IncNodePurity)) + geom_col(mapping = NULL, data = NULL, position = "stack",
                                                                 width = NULL, na.rm = FALSE, show.legend = NA,
                                                                 inherit.aes = TRUE,fill = 'steelblue4')
p + coord_flip() + xlab("Attributes") + labs(title = "Variable Importance Plot")

rm(p,impplot)

plot(rf, main = "Error with Number of Trees")

pretest <- predict(rf,test[-8])

temp$rf <- pretest


rf2 <- randomForest(AdmitChance ~ CGPA+UniRating+SOP+LOR+Research, data = train) # with few variables

pretest <- predict(rf2,test[c(-1,-2,-8)])

temp$rf2 <- pretest


rf3 <- randomForest(AdmitChance ~GRE+TOEFL+UniRating+SOP+LOR+CGPA, data = train) # without research based imp plot

pretest <- predict(rf3,test[c(-7,-8)])

temp$rf3 <- pretest

rm(pretest)

# Random Forest -------------------------------------------

# RMSE ----------------------------------------------------

library(ModelMetrics)

rmse(temp$temp,temp$rf) # with all variables = 0.03700746


rmse(temp$temp,temp$rf2)# with all variables = 0.05727096


rmse(temp$temp,temp$rf3) # without Research variable  = 0.03746171

# Based on the RMSE value model with all the predictors and without Research 
#is performed best

# RMSE ----------------------------------------------------


# RANDOM FOREST WITH CV ----------------------------------------------


k = 10

fold = 1:10

datacv <- data

datacv$kfold <- sample(1:k, nrow(datacv), replace = TRUE)

length(which(datacv$kfold == 9))

prediction <- data.frame()
test_sets <- data.frame()

train_sets <- data.frame()
train_pred <- data.frame()

for(n in 1:k){
  ###Grab all the rows with the id 'n', aggregate them into a test set
  test_set = datacv[which(datacv$kfold %in% n), -ncol(datacv)]
  
  ###All the other rows (the other 9 parts) go in the training set 
  train_set = datacv[which(datacv$kfold %in% fold[-c(n)]), -ncol(datacv)]
  
  forest = randomForest(AdmitChance ~., data = train_set, importance = TRUE,   ntree = 500)
  
  ###Run the model on the test set, save the prediction in a dataframe, the test set in another. Then you can compare them to get your performance measure.	
  n_predict = data.frame(predict(forest, test_set))
  prediction = rbind(prediction, n_predict)
  test_sets = rbind(test_sets, as.data.frame(test_set))
  
  train_sets = rbind(train_sets, train_set)
  train_pred = rbind(train_pred, as.data.frame(forest$predicted))
} 

test_sets$

library(ModelMetrics)

  
a <- rmse(prediction$predict.forest..test_set., test_sets$AdmitChance) # = 0.062

varImpPlot(forest)

b <- rmse(train_pred$`forest$predicted`, train_sets$AdmitChance) # = 0.062


rm(a,b)




# RANDOM FOREST WITH CV ----------------------------------------------
