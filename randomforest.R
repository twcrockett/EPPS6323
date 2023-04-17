library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(datasets)
library(caret)
library(mlbench)

redfin  <- read_xlsx("redfindata.xlsx", 
                      col_types = c("date", # SELLDATE
                                    "skip", # SELLMONTH
                                    "skip", # SELLYEAR
                                    "skip", # ADDRESS
                                    "skip", # CITY
                                    "skip", # STATE
                                    "text", # ZIP
                                    "numeric", # PRICE
                                    "numeric", # BEDS
                                    "numeric", # BATHS
                                    "skip", # LOCATION
                                    "numeric", # SQFT
                                    "numeric", # LOTSIZE
                                    "numeric", #YARDSPACE
                                    "skip", # YEARBUILT
                                    "numeric", #SELLAGE
                                    "numeric", # PRICE_SQFT
                                    "numeric", # HOA_MONTH
                                    "numeric", # MLS#
                                    "skip", # LATITUDE
                                    "skip" # LONGITUDE
                            )) %>% 
  na.omit %>%
  remove_rownames %>% 
  column_to_rownames(var="MLS#") %>%
  filter(SELLDATE <= "2022-12-31") %>%
  filter(YARDSPACE >= 0)

###XIAOYAN VARIABLES###
  #total beds and bath
  redfin$BEDS_BATHS_TOTAL <- redfin$BEDS + redfin$BATHS
  
  #baths and beds ratio
  redfin$BATH_BED_RATIO <- ifelse(redfin$BEDS==0,NA, redfin$BATHS/redfin$BEDS)
  
  #living squarefeet and beds ratio
  redfin$SQFT_BEDS_RATIO <- ifelse(redfin$BEDS==0, NA, redfin$SQFT/redfin$BEDS)

# sort by date, recreate sell month (.csv hampers it)
redfin <- redfin[order(as.Date(redfin$SELLDATE, format="%d/%m/%Y")),]
redfin$SELLMONTH <- format(as.Date(redfin$SELLDATE, format="%yyyy-%m-%d"),"%m-%y")
redfin$REALMONTH <- format(floor_date(redfin$SELLDATE, unit = "month") - months(1), "%m-%y")

### merge in HPI data
hpi <- read.csv("DallasHPI.csv")
hpi$DATE <- format(mdy(hpi$DATE),"%m-%y")

# account for lag (xiaoyan's)
hpi<- hpi %>%
  mutate(HPI_LAG1 = lag(HPI)) %>%
  mutate(HPI_LAG3 = lag(HPI, n=3)) %>%
  mutate(HPI_LAG6 = lag(HPI, n=6))

# create the hpi percentage change (xiaoyan's)
hpi$HPI_1M_PCT <- (hpi$HPI-hpi$HPI_LAG1)/hpi$HPI_LAG1
hpi$HPI_3M_PCT <- (hpi$HPI-hpi$HPI_LAG3)/hpi$HPI_LAG3
hpi$HPI_6M_PCT <- (hpi$HPI-hpi$HPI_LAG6)/hpi$HPI_LAG6

df <- merge(redfin, hpi, by.x="REALMONTH", by.y="DATE")

# factorize some variables, drop the ones we don't need
drop <- c("REALMONTH","SELLDATE", "SELLMONTH")
df <- df[ , !(names(df) %in% drop)]

df$ZIP <- as.factor(df$ZIP)

# summary(redfin)
# hist(redfin$PRICE)
# scatterplot3d::scatterplot3d(redfin$`SOLD DATE`,redfin$PRICE,redfin$`PRICE/SQFT`)
# boxplot(redfin$PRICE ~ redfin$SELLMONTH)

## standardize numeric data
# df <- df %>% mutate_if(is.numeric,scale)


# divide test and train data
set.seed(48)
ind <- sample(2, nrow(redfin), replace=TRUE, prob=c(0.7,0.3))
train <- df[ind==1,]
test <- df[ind==2,]

mtry <- sqrt(ncol(df))

rf <- randomForest(PRICE ~ ., data=train, importance=TRUE, proximity=TRUE, ntree=1000)
rf.plot <- plot(rf, log="y")


train.pred <- train
train.pred$PRED <- predict(rf, train)
train.pred$DIFF <- scale(train.pred$PRED) - scale(train.pred$PRICE)
train.avgdiff <- mean(train.pred$DIFF)

test.pred <- test
test.pred$PRED <- predict(rf, test)
test.pred$DIFF <- scale(test.pred$PRED) - scale(test.pred$PRICE)
test.avgdiff <- mean(test.pred$DIFF)

# see what variable causes the most error
cor(as.matrix(test.pred$DIFF,as.matrix(sapply(test.pred, is.numeric))))
