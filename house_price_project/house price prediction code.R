## project6323. 
## xiaoyan Zhang
## purpose
#######################################################
## The project is to build the model to forcast the ###
## house price, useing house data from redfin and    ##
## Dallas HPI, linear regression, random forest, and ##
## gradient boosting tree are used and compared for  ##
## this project.                                     ##
#######################################################
library(ggplot2)
library(dplyr)
library(lubridate)
################ input data  #############
#read house data
house_data<- read.csv("C:\\Users\\xiaoy\\Desktop\\UTD S23\\6323\\project\\house_data.csv")

# read HPI data
hpi<- read.csv("C:\\Users\\xiaoy\\Desktop\\UTD S23\\6323\\project\\Dallas_HPI.csv")

# transformation
# convert house data sold_month to date
#library(lubridate)

house_data$SOLD_MONTH<-mdy(house_data$SOLD_MONTH)
house_data$base_month<- floor_date(house_data$SOLD_MONTH, unit = "month") - months(1)


house_data<- house_data[house_data$SOLD_MONTH< as.Date("2023-01-01"),]

house_data2<-house_data[house_data$BEDS ==1, ]
#drop beds outlier
house_data<- house_data[-2098,]

# convert HPI date string  to date
hpi$DATE<-mdy(hpi$DATE)

#create lag hpi
#library(dplyr)
hpi<- hpi %>%
  mutate(hpi_lag1 = lag(HPI)) %>%
  mutate(hpi_lag3 = lag(HPI, n=3)) %>%
  mutate(hpi_lag6= lag(HPI, n=6))

#create the hpi percentage change
hpi$hpi_1m_pct <- (hpi$HPI-hpi$hpi_lag1)/hpi$hpi_lag1
hpi$hpi_3m_pct <- (hpi$HPI-hpi$hpi_lag3)/hpi$hpi_lag3
hpi$hpi_6m_pct <- (hpi$HPI-hpi$hpi_lag6)/hpi$hpi_lag6

#join house table and hpi table by date
house_df<- merge(house_data, hpi,by.x="base_month", by.y="DATE", all.x= TRUE, all.y=FALSE)

# do histogram of price, squarefeet, lotsize
hist(house_df$PRICE)
hist(house_df$SQUAREFEET)
hist(house_df$LOTSIZE)
hist(house_df$BEDS)

## create new variables of house data
# extract sold year from house data
house_df$sold_year <- year(as.Date(house_df$SOLD_MONTH))
print(house_df$sold_year)

#  house age 
house_df$houseage <- house_df$sold_year- house_df$YEARBUILT
house_df$houseage

#total beds and bath
house_df$total_beds_baths <- house_df$BEDS + house_df$BATHS

#baths and beds ratio
house_df$bath_bed_ratio <- ifelse(house_df$BEDS==0,NA, house_df$BATHS/house_df$BEDS)

#living squarefeet and beds ratio
house_df$sqrt_beds <- ifelse(house_df$BEDS==0, NA, house_df$SQUAREFEET/house_df$BEDS)

############################ data clean #################
# check price
quantile(na.omit((house_df$PRICE)),c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))
# check beds
quantile(na.omit((house_df$BEDS)),c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))
#check baths
quantile(na.omit((house_df$BATHS)),c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))
#check lotsize 
quantile(na.omit((house_df$LOTSIZE)),c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))
#check living squarefeet
quantile(na.omit((house_df$SQUAREFEET)),c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))

# impute the lotsize outlier to median
med_lot<- median(house_df$LOTSIZE, na.rm = TRUE )
house_df$LOTSIZE_imputed <- ifelse(is.na(house_df$LOTSIZE)|house_df$LOTSIZE > 30000|house_df$LOTSIZE< 2000, med_lot,
                                   house_df$LOTSIZE)
house_df$LOTSIZE_imputed_ind<-ifelse(is.na(house_df$LOTSIZE)|house_df$LOTSIZE > 30000|house_df$LOTSIZE< 2000, 1,0)
  

#check  missing value 
sum(is.na(house_df$PRICE))
sum(is.na(house_df$LOTSIZE))
sum(is.na(house_df$BEDS))
sum(is.na(house_df$BATHS))
sum(is.na(house_df$SQUAREFEET))

#rows missing
#rows_missing <- setdiff(1:nrow(house_df["PRICE"]),which(complete.cases((house_df$PRICE))))

# drop rows of missing price
house_df <- house_df[complete.cases(house_df$PRICE), ]

## variable transformation 
#log transformation for house price, lotsize, squarefeet

hist(house_df$PRICE)
hist(log(house_df$PRICE))
house_df$log_price <- log(house_df$PRICE)

hist(house_df$SQUAREFEET)
hist(log(house_df$SQUAREFEET))
# log sqrt is skewed to the left, original is better

hist(house_df$LOTSIZE_imputed)
hist(log(house_df$LOTSIZE_imputed))
house_df$log_lotsize <- log(house_df$LOTSIZE_imputed)
hist(house_df$BEDS)
hist(house_df$BATHS)

############################# data analysis ####################
## cross table
# frequency of zip code
zip_freq<- table(house_df$ZIP)
zip_freq
#frequency of house age
houseage_freq <- table(house_df$houseage)
houseage_freq

# frequency of number of beds
beds_freq<- table(house_df$BEDS)
beds_freq
# frequency of number of baths
baths_freq<- table(house_df$BATHS)
baths_freq
#cross table by zip *number of beds
ZIP_BEDS<- xtabs(~ ZIP + BEDS, data= house_df)
ZIP_BEDS
#cross table by zip *number of baths
ZIP_BATHS<- xtabs(~ ZIP + BATHS, data = house_df)
ZIP_BATHS
#cross table by number of beds * number of baths
beds_baths<- xtabs(~ BEDS + BATHS, data = house_df )
beds_baths

#mean price by month
price_aver_month<-aggregate(PRICE~ SOLD_MONTH, data= house_df, mean)
price_aver_month
plot(price_aver_month$SOLD_MONTH,price_aver_month$PRICE, xlab="SOLD_MONTH", ylab ="PRICE",
     main="Mean Price by Month", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_aver_month$SOLD_MONTH, labels = price_aver_month$SOLD_MONTH) 

#mean price by number of beds
price_aver_beds<-aggregate(PRICE ~ BEDS , data= house_df, mean)
price_aver_beds
plot(price_aver_beds$BEDS,price_aver_beds$PRICE, xlab="BEDS", ylab ="PRICE",
     main="Mean Price by Beds", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_aver_beds$BEDS, labels = price_aver_beds$BEDS) 

#mean price by number of baths
price_aver_BATHS<-aggregate(PRICE ~ BATHS , data= house_df, mean)
price_aver_BATHS
plot(price_aver_BATHS$BATHS,price_aver_BATHS$PRICE, xlab="BATHS", ylab ="PRICE",
     main="Mean Price by Baths", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_aver_BATHS$BATHS, labels = price_aver_BATHS$BATHS)
#mean price by houseage
price_aver_houseage<-aggregate(PRICE ~ houseage , data= house_df, mean)
price_aver_houseage
plot(price_aver_houseage$houseage,price_aver_houseage$PRICE, xlab="houseage", ylab ="PRICE",
     main="Mean Price by houseage", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_aver_houseage$houseage, labels = price_aver_houseage$houseage)

# scatter plot of price by living square feet

p <- ggplot(house_df,aes(x=SQUAREFEET, y=PRICE ))+geom_point()+ geom_smooth(method="lm", se = FALSE)
print(p)

#median price by month
price_med_month <- aggregate(PRICE ~ SOLD_MONTH, data=house_df, median)
plot(price_med_month$SOLD_MONTH, price_med_month$PRICE, xlab="SOLD_MONTH", ylab ="PRICE",
     main="Median Price by Month", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_med_month$SOLD_MONTH, labels = price_med_month$SOLD_MONTH) 

#median price by number of beds
price_med_beds <- aggregate(PRICE ~ BEDS, data=house_df, median)
plot(price_med_beds$BEDS, price_med_beds$PRICE, xlab="BEDS", ylab ="PRICE",
     main="Median Price by Beds", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_med_beds$BEDS, labels = price_med_beds$BEDS) 

#median price by baths
price_med_baths <- aggregate(PRICE ~ BATHS, data=house_df, median)
plot(price_med_baths$BATHS, price_med_baths$PRICE, xlab="BATHS", ylab ="PRICE",
     main="Median Price by Baths", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_med_baths$BATHS, labels = price_med_baths$BATHS) 

#median price by houseage
price_med_houseage <- aggregate(PRICE ~ houseage, data=house_df, median)
plot(price_med_houseage$houseage, price_med_houseage$PRICE, xlab="houseage", ylab ="PRICE",
     main="Median Price by houseage", type= "l",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_med_houseage$houseage, labels = price_med_houseage$houseage) 

# create new variable age group 

#library(dplyr)
house_df <- house_df %>%
mutate(age_group = cut(houseage, breaks = c(-0.5,5,10,15,20,25, Inf), labels = c("0-5", "6-10", "11-15","16-20","21-25","25+"))) 
house_df
#mean price by age group
price_aver_age_group<-aggregate(PRICE ~ age_group , data= house_df, mean)
price_aver_age_group
plot(price_aver_age_group$age_group,price_aver_age_group$PRICE, xlab="age_group", ylab ="PRICE",
     main="Mean Price by age_group ", type= "p",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_aver_age_group$age_group, labels = price_aver_age_group$age_group)

#median price by age group
price_med_age_group<-aggregate(PRICE ~ age_group , data= house_df, median)
price_med_age_group
plot(price_med_age_group$age_group,price_med_age_group$PRICE, xlab="age_group", ylab ="PRICE",
     main="Median Price by age_group ", type= "p",xaxt = "n")  # xaxt = "n" removes default x-axis labels
axis(side = 1, at =  price_med_age_group$age_group, labels = price_med_age_group$age_group)


# create new variable month from sold month 
house_df$month_ind<- as.factor(month(house_df$SOLD_MONTH))

# keep list
keep_list<-  c("BEDS","BATHS","SQUAREFEET","HPI","hpi_lag1","hpi_lag3","hpi_lag6","hpi_1m_pct", 
               "hpi_3m_pct","hpi_6m_pct","houseage","total_beds_baths","bath_bed_ratio","sqrt_beds","LOTSIZE_imputed_ind",
               "log_price","log_lotsize", "month_ind")
house_df<-house_df[,keep_list]


################## split the data ##################################
#split the data to training data and testing data

library(rsample)
set.seed(123)
initial_split_df<- initial_split(house_df, prop = 0.7)
train_data<- training(initial_split_df)
test_data<- testing(initial_split_df)

########################### build 3 models ##################################
## build linear regression model


reg_model <- lm(log_price  ~ 
                  #BEDS + 
                  BATHS+
                  SQUAREFEET+
                  #HPI+
                 # hpi_lag1+
                  hpi_lag3+
                  #hpi_lag6+
                 # hpi_1m_pct+
                  hpi_3m_pct+
                 # hpi_6m_pct+
                  houseage+
                  #total_beds_baths,
                  #bath_bed_ratio+
                  #sqrt_beds+
                  LOTSIZE_imputed_ind+
                  log_lotsize,
                  #month_ind, 
                  data = train_data )
summary(reg_model)
# predict linear regression model  on testing data
prediction <- predict(reg_model, newdata = test_data)
plot(prediction, test_data$log_price)
mse_pred<- mean((test_data$log_price - prediction)^2)
sqrt(mse_pred)

# do prediction with lasso
library(glmnet)
grid <- 10^seq(10, -5, length = 1000)  # lambda search grid

x_train<- select(train_data, -log_price)
month_df <- data.frame(model.matrix(~ month_ind - 1, data= x_train))
# add column name to minth_ind
colnames(month_df)<- paste0("month_", levels(x_train$month_ind))
x_train2<- cbind(x_train,month_df )
x_train2<- select(x_train2, - month_ind)
# change testing data as training data
x_test<- select(test_data, -log_price)
month_df_test <- data.frame(model.matrix(~ month_ind - 1, data= x_test))
# add column name to minth_ind
colnames(month_df)<- paste0("month_", levels(x_test$month_ind))
x_test2<- cbind(x_test,month_df_test )
x_test2<- select(x_test2, - month_ind)


lasso_model <- glmnet(x_train2, train_data[ ,"log_price"] , alpha = 1, lambda= grid )
# to find the best lambda
set.seed(1)
cv.out <- cv.glmnet(as.matrix(x_train2), train_data[ ,"log_price"], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso_model, s = bestlam, newx = as.matrix(x_test2))
mean((lasso.pred - test_data$log_price)^2)     # lowest MSE


## build random forest model

#check missing value of train_data
train_data_rf<- na.omit(train_data)
colSums(is.na(train_data_rf))

# tune random forest 
rf_tuned<- tuneRF(select(train_data_rf, -log_price), train_data_rf$log_price, 
                  ntreeTry=500, stepFactor= 1.5,
                  improve = 0, trace= TRUE, plot=TRUE)


library(randomForest)
set.seed(1)
# Bagging of ensemble of 500 trees with all mtry = 7 variables at each split
bag_train_data <- randomForest(log_price ~ ., data=train_data, ntree=500,
                            mtry = 7, na.action= na.omit, importance = TRUE)
bag_train_data


## Prediction
test_pred <- predict(bag_train_data, newdata = test_data)
plot(test_pred, test_data$log_price)
abline(0, 1)
mean((test_pred - test_data$log_price)^2)

## build gradient boosting tree model 
library(gbm)
set.seed(1)
n_trees <- 200
learning_rate <- 0.05
max_depth <- 10
predictions <- matrix(0, nrow = nrow(test_data), ncol = n_trees)
#use for loop to make predict
for (i in 1:n_trees) {
  model <- gbm(log_price ~ ., data = train_data, n.trees = i, 
               interaction.depth = max_depth, shrinkage = learning_rate,
               distribution = "gaussian")
  predictions[, i] <- predict(model, newdata = test_data, n.trees = i)
}
#combine the prediction
error_list <- apply((predictions-test_data$log_price)^2, 2, mean
  )

plot(1:n_trees, error_list)


## best tune
gbmgrid<- expand.grid(interaction.depth = c(1,3,5,7,9), n.trees= c(50,100,150,200), 
                      shrinkage =c(0.01, 0.05, 0.1), n.minobsinnode= 5)
set.seed(123)
gbm_model<- train(log_price~ ., data=test_data, method ="gbm",
                  trControl = trainControl(method ="cv", number= 10),
                  tuneGrid= gbmgrid)



#best model
best_model <- gbm(log_price ~ ., data = train_data, n.trees = 100, 
             interaction.depth = 9, shrinkage = 0.1,n.minobsinnode = 5,
             distribution = "gaussian")
best_pred <- predict(best_model, newdata = test_data)
mse<-mean((test_data$log_price - best_pred)^2)


################################# out of time testing ####################
# read house data oot
house_data_oot<- read.csv("C:\\Users\\xiaoy\\Desktop\\UTD S23\\6323\\project\\house_data_oot.csv")

house_data_oot$SOLD_MONTH<- as.Date("2023-01-01")
house_data_oot$base_month<- floor_date(house_data_oot$SOLD_MONTH, unit = "month") - months(1)



# create required variables


# merge hpi table with house table

house_df_oot<- merge(house_data_oot, hpi,by.x="base_month", by.y="DATE", all.x= TRUE, all.y=FALSE)
house_df_oot$sold_year <- year(as.Date(house_df_oot$SOLD_MONTH))


#  house age 
house_df_oot$houseage <- house_df_oot$sold_year- house_df_oot$YEARBUILT

#total beds and bath
house_df_oot$total_beds_baths <- house_df_oot$BEDS + house_df_oot$BATHS

#baths and beds ratio
house_df_oot$bath_bed_ratio <- ifelse(house_df_oot$BEDS==0,NA, house_df_oot$BATHS/house_df_oot$BEDS)

#living squarefeet and beds ratio
house_df_oot$sqrt_beds <- ifelse(house_df_oot$BEDS==0, NA, house_df_oot$SQUAREFEET/house_df_oot$BEDS)
# impute the lotsize outlier to median
med_lot<- median(house_df_oot$LOTSIZE, na.rm = TRUE )# we should training median as imputation
house_df_oot$LOTSIZE_imputed <- ifelse(is.na(house_df_oot$LOTSIZE)|house_df_oot$LOTSIZE > 30000|house_df_oot$LOTSIZE< 2000, med_lot,
                                       house_df_oot$LOTSIZE)
house_df_oot$LOTSIZE_imputed_ind<-ifelse(is.na(house_df_oot$LOTSIZE)|house_df_oot$LOTSIZE > 30000|house_df_oot$LOTSIZE< 2000, 1,0)

#create log lotsize
house_df_oot$log_lotsize <- log(house_df_oot$LOTSIZE_imputed)



# create new variable age group 

#library(dplyr)
house_df_oot <- house_df_oot %>%
  mutate(age_group = cut(houseage, breaks = c(-0.5,5,10,15,20,25, Inf), labels = c("0-5", "6-10", "11-15","16-20","21-25","25+"))) 
house_df_oot

# create new variable month from sold month 
house_df_oot$month_ind<- as.factor(month(house_df_oot$SOLD_MONTH))

#predict oot data on best model
house_df_oot$pred_log_price <- predict(best_model, newdata = house_df_oot)
house_df_oot$pred_price <- exp(house_df_oot$pred_log_price)



pred_error_oot<- ((house_df_oot$pred_price- house_df_oot$PRICE)/ house_df_oot$PRICE)



