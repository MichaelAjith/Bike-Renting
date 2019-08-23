#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("D:/MBA REF/edwisor/project/bikerental")

#Current working directory
getwd()

# importing all required library
Packages <- c("ggplot2", "corrgram", "corrplot", "randomForest",
                      "caret", "class", "e1071", "rpart", "mlr","grid",
                      "DMwR","usdm","dplyr","caTools","LiblineaR")

lapply(Packages, library, character.only = TRUE)

# Reading/Loading the csv file
bike = read.csv("day.csv")


# *****************************  Exploratory Data Analysis ****************************        


# ******************** Understanding the data  ********************************** 

# checking datatypes of all columns
class(bike)
dim(bike)
head(bike)
names(bike)
str(bike)
summary(bike)

#numeric variables:
num_columns <- c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt')

#categorical varibles:
cat_columns <- c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')

# ******************  Checking numerical variables **************************

# Checking numerical statistics of numerical columns:
summary(bike[,num_columns])


# ******************  Checking categorical variables **************************
#unique values in each category:
lapply(bike[,cat_columns], function(feat) length(unique(feat)))

#counting each unique values in categorical columns:
lapply(bike[,cat_columns], function(feature) table(feature))


# ***********************  Missing value analysis ***********************  

#checking missing value for each column in the bike dataset and storing the counting in dataframe with column name
missing_val <- data.frame(lapply(bike, function(feat) sum(is.na(feat))))
View(missing_val)

# ********************** Outlier Analysis *************************        

#box_plot function to find outliers in  numerical columns
box_plot <- function(column, dataset){
  dataset$x = 1
  ggplot(aes_string(x= 'x', y = column), data = dataset)+
    stat_boxplot(geom = 'errorbar', width = 0.5)+
    geom_boxplot(outlier.size = 2, outlier.shape = 18)+
    labs(y = "", x = column)+
    ggtitle(paste("BOX:",column))
}

#ploting histogram of numerical variable
hist_plot <- function(column, dataset){
  ggplot(aes_string(column), data = dataset)+
    geom_histogram(aes(y=..density..), fill = 'skyblue2')+
    geom_density()+
    labs(x = gsub('\\.', ' ', column))+
    ggtitle(paste("HIST:",gsub('\\.', ' ', column)))
}

#calling box_plot function and storing all plots in a list
all_box_plots <- lapply(c('temp', 'atemp', 'hum', 'windspeed'),box_plot, dataset = bike)

#calling hist_plot function and storing all plots in a list
all_hist_plots <- lapply(c('temp', 'atemp', 'hum', 'windspeed'),hist_plot, dataset = bike)

# printing all plots in one 
gridExtra::grid.arrange(all_box_plots[[1]],all_box_plots[[2]],all_box_plots[[3]],all_box_plots[[4]],
                        all_hist_plots[[1]],all_hist_plots[[2]],all_hist_plots[[3]],all_hist_plots[[4]],ncol=4,nrow=2)


# **************************  Feature Engineering  ****************************      

#plot barplot of a columns with respect to other column
plot_bar <- function(cat, y, fun){
  gp = aggregate(x = bike[, y], by=list(cat=bike[, cat]), FUN=fun)
  ggplot(gp, aes_string(x = 'cat', y = 'x'))+
    geom_bar(stat = 'identity')+
    labs(y = y, x = cat)+
    ggtitle(paste("Bar plot for",y,"wrt to",cat))
}

# plotting cnt with respect to month
plot_bar('mnth', 'cnt', 'sum')

# plotting cnt with respect to yr
plot_bar('yr', 'cnt', 'sum')

# plotting cnt with respect to yr
plot_bar('weekday', 'cnt', 'sum')

#creating bins of mnth and weekday
#changing values of month 5th to 10th as 1 and others 0
bike = transform(bike, mnth = case_when(
  mnth <= 4 ~ 0, 
  mnth >= 11 ~ 0,
  TRUE   ~ 1 
))
colnames(bike)[5] <- 'month_feat'
colnames(bike)
# changing values of weekday for day 0 and 1 the value will be 0
#and 1 for rest
bike = transform(bike, weekday = case_when(
  weekday < 2 ~ 0, 
  TRUE   ~ 1 
))
colnames(bike)[7] <- 'week_feat'

#                               Feature Selection        
#correlation plot for numerical feature:
corrgram(bike[,num_columns], order = FALSE,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlation Plot for bike dataset")

# heatmap plot for numerical features
corrplot(cor(bike[,num_columns]), method = 'color', type = 'lower')

cat_columns <- c('season', 'yr', 'month_feat', 'holiday', 'week_feat', 'workingday', 'weathersit')

#making every combination from cat_columns
combined_cat <- combn(cat_columns, 2, simplify = F)

#doing chi-square test for every combination
for(i in combined_cat){
  print(i)
  print(chisq.test(table(bike[,i[1]], bike[,i[2]])))
}

# finding important features
important_feat <- randomForest(cnt ~ ., data = bike[,-c(1,2,14,15)],
                               ntree = 200, keep.forest = FALSE, importance = TRUE)
importance_feat_df <- data.frame(importance(important_feat, type = 1))

# checking vif of numerical column withhout dropping multicollinear column
vif(bike[,c(10,11,12,13)])

# Checking VIF values of numeric columns after dropping multicollinear column i.e. atemp
vif(bike[,c(10,12,13)])

# Making factor datatype to each category
bike[,cat_columns] <- lapply(bike[,cat_columns], as.factor)

# releasing memory of R, removing all variables except dataset
rm(list = setdiff(ls(),"bike"))

# ************************  Data after Exploratory Data Analysis  *******************           

# creating another dataset with dropping outliers i.e. bikeo
bikeo <- bike

# removing outliers from hum and windspeed columns
for (i in c('hum', 'windspeed')){
  out_value = bikeo[,i] [bikeo[,i] %in% boxplot.stats(bikeo[,i])$out]
  bikeo = bikeo[which(!bikeo[,i] %in% out_value),]
}

# checking dimension of both dataset
dim(bike)
dim(bikeo)

# dropping unwanted columns
drop_col <- c('instant', 'dteday', 'holiday', 'atemp', 'casual', 'registered')
bike[,drop_col]<- NULL
bikeo[,drop_col] <- NULL

# *****************************  Building models  ********************************                

set.seed(1)
split = sample.split(bike$cnt, SplitRatio = 0.80)
train_set = subset(bike, split == TRUE)
test_set = subset(bike, split == FALSE)

split = sample.split(bikeo$cnt, SplitRatio = 0.80)
train_set_wo = subset(bikeo, split == TRUE)
test_set_wo = subset(bikeo, split == FALSE)

# making a function which will train model on training data and would show 
# K-fold R2 score , R2 score for test dataset and train dataset
prediction <- function(method, train_data, test_data){
  reg_fit <- caret::train(cnt~., data = train_data, method = method)
  
  y_pred <- predict(reg_fit, test_data[,-10])
  print("R2 on test dataset")
  print(caret::R2(y_pred, test_data[,10])) 
  
  y_pred <- predict(reg_fit, train_data[,-10])
  print("R2 on train dataset")
  print(caret::R2(y_pred, train_data[,10]))
  
  # creating 10 folds of data
  ten_folds = createFolds(train_data$cnt, k = 10)
  ten_cv = lapply(ten_folds, function(fold) {
    training_fold = train_data[-fold, ]
    test_fold = train_data[fold, ]
    reg_fit <- caret::train(cnt~., data = training_fold, method = method)
    
    y_pred <- predict(reg_fit, test_fold[,-10])
    return(as.numeric(caret::R2(y_pred, test_fold[,10]))) 
  })
  sum = 0
  for(i in ten_cv){
    sum = sum + as.numeric(i)
  }
  print("K-fold (K =10) explained variance")
  print(sum/10)
}


# ***************************  Linear Regression  ****************************

# building model for dataset bike
prediction('lm', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('lm', train_set_wo, test_set_wo)

# *******************************  KNN  ***************************************

# building model for dataset bike
prediction('knn', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('knn', train_set_wo, test_set_wo)

# *********************************  SVM  *************************************            

# building model for dataset bike
prediction('svmLinear3', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('svmLinear3', train_set_wo, test_set_wo)

# ****************************** Decision Tree Regression  ********************

# building model for dataset bike
prediction('rpart2', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('rpart2', train_set_wo, test_set_wo)

# **********************************  Random Forest  ************************* 

# building model for dataset bike
prediction('rf', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('rf', train_set_wo, test_set_wo)

# ***********************************  XGBRegressor  *************************      

# building model for dataset bike
prediction('xgbTree', train_set, test_set)

# building model for dataset bikeo i.e. without  outliers
prediction('xgbTree', train_set_wo, test_set_wo)

#************************ Hyperparameter tuning **************************************             

#************ Tuning Random Forest for bike dataset  ****************************

control <- trainControl(method="repeatedcv", number=10, repeats=3)
reg_fit <- caret::train(cnt~., data = train_set, method = "rf",trControl = control)
reg_fit$bestTune
y_pred <- predict(reg_fit, test_set[,-10])
print(caret::R2(y_pred, test_set[,10]))

# *******************  Tuning XGB for bike dataset  *****************************     

control <- trainControl(method="repeatedcv", number=10, repeats=3)
reg_fit <- caret::train(cnt~., data = train_set, method = "xgbTree",trControl = control)
reg_fit$bestTune
y_pred <- predict(reg_fit, test_set[,-10])
print(caret::R2(y_pred, test_set[,10]))
