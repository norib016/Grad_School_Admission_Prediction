# Library--------------------------------------------------

library(readr)
library(corrplot)
library(ggplot2)
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

# Data Loading and preparing ------------------------------

# CORRELATION PLOT ----------------------------------------

#form the model matrix and correlation variable
varMat <- model.matrix(~.-Research,data=data)[,-1]  #Removing the first column as it has all 1's.
varCor <- cor(varMat)

#plot the correlation
corrplot(varCor,method = "circle",
         tl.col = "black", mar = c(0,0,2,0),
         title = "Graduate Admission numerical Variables Correlation")


#create quick matrix accross the different outputs:

######################  START  ############################################

cor(varCor)
round(cor(varCor),1) # Round

cor(varCor, method = "kendall")
cor(varCor, method = "spearman")


## correlation between the 2
cot.test(varCor$GRE,varCor$AdmitChance) # wont work


corrplot(varCor)

corrplot(varCor, method = "pie", mar = c(0,0,2,0),
title = "Graduate Admission pie Variables Correlation")


corrplot(varCor, method = "color", mar = c(0,0,2,0),
title = "Graduate Admission color Variables Correlation")

corrplot(varCor, method = "number",  mar = c(0,0,2,0),
title = "Graduate Admission numerical Variables Correlation")

corrplot(varCor, type = "lower",  mar = c(0,0,2,0),
title = "Graduate Admission lower Variables Correlation")

corrplot(varCor, type = "upper", mar = c(0,0,2,0),
title = "Graduate Admission upper Variables Correlation")



# Correlogram: gives visual representation of the relationship of one variable vector and all the data in the dataset 

?hclust
colors() # gives me a list of different colors


# First: compute the matrix of p-value

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(varCor)
head(p.mat[, 1:5])





M <- cor(varCor) # method

p.mat <- cor.mtest(M)

title <- "Admission p-value significance"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         title=title, 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         mar = c(0,0,2,0)
)

# https://www.youtube.com/watch?v=2jeOeYSvozQ
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#https://cran.r-project.org/web/packages/corrgram/vignettes/corrgram_examples.html
#Explanation: https://www.youtube.com/watch?v=DFVjUFWGjHw


######################  END  ############################################

plot(varCor)

corrplot(varCor,add = TRUE,                            
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

reg.summary$adjr2 #best with all indicators except for SOP,
#5 var => 0.819, 6 var => 0.820 marginal diff
reg.summary$cp #lowest with all predictors except for SOP.
reg.summary$rss #lowest with all predictors except for SOP.
reg.summary$bic #lowest with 5 predictors, SOP and UniRating are not imporving BIC.

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
mse #0.0036 for 5, 6, 7 variable model, lets see if train/test technique improves the result substantially

################ Attempt Linear Regression with Train and Test subsets over 100 iterations ########
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

val.errors 

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
round(mean.cv.errors, 4) 

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


rf2 <- randomForest(AdmitChance ~ CGPA+UniRating+SOP+LOR+Research, data = train) 

pretest <- predict(rf2,test[c(-1,-2,-8)])

temp$rf2 <- pretest


rf3 <- randomForest(AdmitChance ~GRE+TOEFL+UniRating+SOP+LOR+CGPA, data = train) 

pretest <- predict(rf3,test[c(-7,-8)])

temp$rf3 <- pretest

rm(pretest)

# Random Forest -------------------------------------------

# RMSE ----------------------------------------------------

library(ModelMetrics)

rmse(temp$temp,temp$rf) # with all variables


rmse(temp$temp,temp$rf2)# with all variables


rmse(temp$temp,temp$rf3) # without Research variable



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
 
  test_set = datacv[which(datacv$kfold %in% n), -ncol(datacv)]
 
  train_set = datacv[which(datacv$kfold %in% fold[-c(n)]), -ncol(datacv)]
  
  forest = randomForest(AdmitChance ~., data = train_set, importance = TRUE,   ntree = 500)
  
  
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


#PCA - Principal component Analysis

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

Admission_chances <- read_csv("Admission_Predict_Ver1.1.csv")
Admission_chances

#Excluding the categorical variables,Because PCA works best with numerical data
Admissions.pca <- prcomp(Admission_chances[,c(2,3,5:7)],center = TRUE, scale = TRUE)
summary(Admissions.pca)
str(Admissions.pca) #  look at a PCA object.

#Visualizing the PCA 
ggbiplot(Admissions.pca)
#biplot(Admissions.pca, labels= rownames(Admissions.pca))

# Plotting PC3 and PC4 components
ggbiplot(Admissions.pca,choices=c(3,4)) 

# Changing some graphic parameters with ggbiplot
#1.drawingg a circle
ggbiplot(Admissions.pca,circle=TRUE)

#2.Scaling 
ggbiplot(Admissions.pca,obs.scale = 1, var.scale = 1)


#Customize the plot
ggbiplot(Admissions.pca,obs.scale = 1, var.scale = 1) +
  ggtitle("PCA of Admissions dataset")+
  theme_minimal()

#compute standard deviation of each principal component
std_dev <- Admissions.pca$sdev

#compute proportion of variance
pr_var <- std_dev^2
pve <- pr_var/sum(pr_var)

#Scree plot
x = 1:length(pve)
qplot(x,pve, xlab="Principal Component",
      ylab="Proportion of Variance Explained") +
  geom_line()+geom_point(shape=21,fill="red",cex=3)

#Cumulative Scree Plot
qplot(x,cumsum(pve), xlab="Principal Component",
      ylab="Cumulative Proportion of Variance Explained",
      main="  ",ylim=c(0,1))+
  geom_line()+geom_point(shape=21,fill="red",cex=3)




#Clustering ---------------------------------
#use data as the dataset
library(ISLR)
library("tidyr")
library(tidyverse)

library("cluster")
library("factoextra")
library("dplyr") 

#variable summaries:
apply(data, 2, mean)
apply(data, 2, var)

#calcualte principal components
pr.out=prcomp(data, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)

#Clustering:
res.dist <- get_dist(data, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(data, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = data)

#2 cluster for GRE & AdmitChance
data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(GRE, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

#2 Cluster for TOEFL & AdmitChance
data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(TOEFL, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)
k6 <- kmeans(data, centers = 10, nstart = 25)

data %>%
  as_tibble() %>%
  mutate(cluster = k3$cluster) %>%
  ggplot(aes(GRE, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(GRE, CGPA, color = factor(cluster), label = AdmitChance)) +
  geom_text()

data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(CGPA, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(SOP, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
  ggplot(aes(LOR, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()

data %>%
  as_tibble() %>%
  mutate(cluster = k4$cluster) %>%
  ggplot(aes(LOR, AdmitChance, color = factor(cluster), label = AdmitChance)) +
  geom_text()


p1 <- fviz_cluster(k2, geom = "point", data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

p5 <- fviz_cluster(k6, geom = "point",  data) + ggtitle("k = 10")
p5

#Withitn SS for different values of k
set.seed(150)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 20
k.values <- 1:20

# extract wss for 2-20 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
