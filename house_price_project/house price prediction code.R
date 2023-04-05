## project6323. 
## author: xiaoyan Zhang
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
house_data<- read.csv("C:\\Users\\xiaoy\\Desktop\\UTD S23\\6323\\project\\6323data.csv")

# read HPI data
hpi<- read.csv("C:\\Users\\xiaoy\\Desktop\\UTD S23\\6323\\project\\Dallas_HPI.csv")

# transformation
# convert house data sold_month to date
#library(lubridate)

house_data$SOLD_MONTH<-mdy(house_data$SOLD_MONTH)
house_data$base_month<- floor_date(house_data$SOLD_MONTH, unit = "month") - months(1)
 
  
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
house_df$month_ind<- month(house_df$SOLD_MONTH)

################## split the data ##################################
#split the data to training data and testing data

library(rsample)
set.seed(123)
initial_split_df<- initial_split(house_df, prop = 0.7, strata = "PRICE")
train_data<- training(initial_split_df)
test_data<- testing(initial_split_df)

########################### build 3 models ##################################
## build linear regression model



## build random forest model




## build gradient boosting tree model 

