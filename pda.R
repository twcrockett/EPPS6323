library(readxl)
library(dplyr)
library(ggplot2)

redfin <- read_excel("6323rawdata.xlsx", 
                      col_types = c("skip", # SALE TYPE
                                    "date", # SOLD DATE
                                    "skip", # PROPERTY TYPE
                                    "skip", # ADDRESS
                                    "skip", # CITY
                                    "skip", # STATE OR PROVINCE
                                    "text", # ZIP OR POSTAL CODE
                                    "numeric", # PRICE
                                    "numeric", # BEDS
                                    "numeric", # BATHS
                                    "skip", # LOCATION
                                    "numeric", # SQUARE FEET
                                    "numeric", # LOT SIZE
                                    "numeric", # YEAR BUILT
                                    "skip", # DAYS ON MARKET
                                    "skip", # $/SQUARE FEET
                                    "skip", # HOA/MONTH
                                    "skip", # STATUS
                                    "skip", # NEXT OPEN HOUSE START TIME
                                    "skip", # NEXT OPEN HOUSE END TIME
                                    "skip", # URL
                                    "skip", # SOURCE
                                    "skip", # MLS#
                                    "skip", # FAVORITE
                                    "skip", # INTERESTED
                                    "skip", # LATITUDE
                                    "skip" # LONGITUDE
                            )) %>% na.omit
redfin$`$/SQ FOOT` <- redfin$PRICE / redfin$`SQUARE FEET`
redfin$`MONTH SOLD` <- format(as.Date(redfin$`SOLD DATE`, format="%d/%m/%Y"),"%m-%y")

# sort by date
redfin <- redfin[order(as.Date(redfin$`SOLD DATE`, format="%d/%m/%Y")),]

summary(redfin)
hist(redfin$PRICE)
scatterplot3d::scatterplot3d(redfin$`SOLD DATE`,redfin$PRICE,redfin$`$/SQ FOOT`)
boxplot(redfin$PRICE ~ redfin$`MONTH SOLD`)

ggplot(redfin, aes(x = `SOLD DATE`, y = PRICE)) + 
        geom_point() +
        stat_smooth(method = "lm", col = "red") + 
        scale_y_continuous(trans='log10')

# --- BEDS ---

beds <- redfin %>% group_by(`MONTH SOLD`, BEDS)%>% 
  summarize(Mean=mean(PRICE), Max=max(PRICE), Min=min(PRICE), 
            Med=median(PRICE), Std=sd(PRICE))

ggplot(beds, aes(x = `BEDS`, y = Med)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_y_continuous(trans='log10')


# --- BATHS ---

baths <- redfin %>% group_by(`MONTH SOLD`, BATHS)%>% 
  summarize(Mean=mean(PRICE), Max=max(PRICE), Min=min(PRICE), 
            Med=median(PRICE), Std=sd(PRICE))

ggplot(baths, aes(x = `BATHS`, y = Med)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_y_continuous(trans='log10')


# --- SQ FT ---

sqft <- redfin %>% group_by(`MONTH SOLD`, `SQUARE FEET`)%>% 
  summarize(Mean=mean(PRICE), Max=max(PRICE), Min=min(PRICE), 
            Med=median(PRICE), Std=sd(PRICE))

ggplot(sqft, aes(x = `SQUARE FEET`, y = Med)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_y_continuous(trans='log10')

ggplot(redfin, aes(x = `SQUARE FEET`, y = PRICE)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_y_continuous(trans='log10')


# --- LOT SIZE ---

lotsize <- redfin %>% group_by(`MONTH SOLD`, `LOT SIZE`)%>% 
  summarize(Mean=mean(PRICE), Max=max(PRICE), Min=min(PRICE), 
            Med=median(PRICE), Std=sd(PRICE))

ggplot(lotsize, aes(x = `LOT SIZE`, y = Med)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_y_continuous(trans='log10')

ggplot(redfin, aes(x = `LOT SIZE`, y = PRICE)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log2')


# --- ZIP CODE ---

zipcode <- redfin %>% group_by(`MONTH SOLD`, `ZIP OR POSTAL CODE`)%>% 
  summarize(Mean=mean(PRICE), Max=max(PRICE), Min=min(PRICE), 
            Med=median(PRICE), Std=sd(PRICE))

boxplot(zipcode$Med ~ zipcode$`ZIP OR POSTAL CODE`)
