library(fpp2)
library(tidyverse)
library(readxl)
library(forecast)

library(ggplot2)
library(dplyr)
library(reshape2)

library(janitor)
library(tibble)
#read data


getwd()
setwd('C:/Users/kesha/Desktop/MSBA Keshav/Case Competitions/MSBA Case Competition')

#elec = read.table("dtwnLAozone.txt", skip=2, header = TRUE)
elec = read_excel('UPC Utility Data 2018-2021.xlsx', sheet="Electricity", range = "A6:AY232")

class(elec)
elec
elec_ts_clean = data.frame(t(elec[c(-1,-224,-225),-1:-2]))
elec_ts_clean
head(elec_ts_clean)



dim(elec_ts_clean)
#elec_ts_clean[1,223] = "TOT"

elec_ts_TOT = elec_ts_clean[-1,223]
elec_ts_TOT = as.numeric(elec_ts_TOT)
class(elec_ts_TOT)
length(elec_ts_TOT)

# Create a time series object for data
elec_ts_TOT.ts = ts(elec_ts_TOT,start=c(2018,1),frequency=12)



autoplot(elec_ts_TOT.ts)

length(elec_ts_TOT.ts)

# Divide data into training and testing sets and call y.train and y.test
y.train = window(elec_ts_TOT.ts, end=c(2020,3))
y.train

y.test = window(elec_ts_TOT.ts, start=c(2020,4))
y.test
length(y.test)

# Naive models: non-seasonal naive model and seasonal naive model

# Naive model prediction for time t+1 = Actual data at time t


# Build a model on training set and predict on the testing set

?naive

M1 = naive(y.train, h = length(y.test))
M1

# Fitted values: predicted values on the training set
M1$fitted

# Forecast on the testing set
M1$mean

# Plot all data (train + test) and overlay fitted values and forecasts

autoplot(elec_ts_TOT.ts) + autolayer(M1$fitted) + autolayer(M1$mean,size=2)+
  theme_bw()

#Accuracy metrics on both training and testing sets:

accuracy(M1,y.test)

# Seasonal pattern was not modeled.  Instead of naive we should use snaive.
# snaive = seasonal naive model:

M2 = snaive(y.train, h = length(y.test), level=95)

autoplot(elec_ts_TOT.ts) + autolayer(M2$fitted) + autolayer(M2$mean,size=2)+
  theme_bw()
#autoplot(y) + autoplot(

tsdisplay(elec_ts_TOT.ts)
ndiffs(elec_ts_TOT.ts)
nsdiffs(elec_ts_TOT.ts)
?ARIMA

?ma
  
elec.trend = ma(elec_ts_TOT.ts, order = 12)
autoplot(elec_ts_TOT.ts) + autolayer(elec.trend)

elec_ts_TOT.ts

elec_ts_clean = janitor::row_to_names(elec_ts_clean, row_number=1) %>% clean_names()
elec_ts_clean

length(unique(colnames(elec_ts_clean)))

length(colnames(elec_ts_clean))

colnames(elec_ts_clean)

df = tibble(x=colnames(elec_ts_clean))

dplyr::count(df, x,sort=TRUE)
row.names(elec_ts_clean)
colnames(elec_ts_clean)
#elec_ts_clean[elec_ts_clean$]
#colnames(elec_ts_clean) = elec_ts_clean[1, ]

elec_ts_clean$adm <- as.numeric(elec_ts_clean$adm)

#steps: get the header put in... done with clean names?
elec_ts_clean = elec_ts_clean %>% mutate(Ramp=c(rep(0,27),1:21))
elec_ts_clean = elec_ts_clean %>% mutate(Bump=c(rep(0,27),rep(1,21)))
elec_ts_clean$Month = 1:(dim(elec_ts_clean)[1])
elec_ts_clean = elec_ts_clean %>% mutate(MonthShort=c('Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec'))



elec_ts_clean$Bump
elec_ts_clean$Ramp
elec_ts_clean$Month
elec_ts_clean$MonthShort

#create lag1
elec_ts_clean$adm_Lag1 <- elec_ts_clean$adm %>% lag(1)


#M11 creation - example to make loop

elec_ts_clean$M11ADM
M11=lm(adm ~ Bump + Ramp + Month + MonthShort + adm_Lag1,data=elec_ts_clean)


elec_ts_clean$M11ADM[2:48] = M11$fitted.values
elec_ts_clean$M11ADM

elec_ts_clean %>% ggplot(mapping=aes(x=Month,y=adm)) + geom_point() + theme_bw()+
  geom_line(aes(x=Month,y=adm)) +
  geom_line(mapping=aes(x=Month,y=M11ADM),col="red",lwd=1)
#M11 test creation end - commented out section
summary(M11)[9] #R^2 reference


M11$coefficients[3]

plot(M11$residuals)
#answer -50.41905



dim(elec_ts_clean)
colnames(elec_ts_clean)[1:223]

#create data frame to store results, row names are the bldg codes and coefficients are the post-covid trend coefficient (Ramp)
elec_results = data.frame(colnames(elec_ts_clean[1:223]),coefficient_value=0,row.names = colnames(elec_ts_clean[1:223]))
elec_results = elec_results[-1]
#colnames(elec_results)
elec_results

adm_Lag1 <- elec_ts_clean['adm'] %>% lag(1)


for (i in colnames(elec_ts_clean)[1:223]) {
        elec_ts_clean[i] <- as.numeric(unlist(elec_ts_clean[i]))
        lag_name <- paste("i", "_Lag1", sep = "")
        elec_ts_clean[lag_name] = elec_ts_clean[i] %>% lag(1)
        M_i = lm(unlist(elec_ts_clean[i]) ~ Ramp + Bump + Month + MonthShort + i_Lag1,data=elec_ts_clean)
        elec_results[i,"coefficient_value"] = M_i$coefficients[3]
        elec_results[i,"Model R Squared"] = summary(M_i)[9]
        elec_results[i,"mean"] = mean(unlist(elec_ts_clean[i]))
}
elec_results

write.csv(elec_results,"electricity_mean_trend.csv")

#mean(unlist(elec_ts_clean["adm"]))

####debug negative R2
elec_ts_clean$dmv <- as.numeric(elec_ts_clean$dmv)

elec_ts_clean$dmv_Lag1 <- elec_ts_clean$dmv %>% lag(1)


M11=lm(dmv ~ Bump + Ramp + Month + MonthShort + dmv_Lag1,data=elec_ts_clean)

elec_ts_clean %>% ggplot(mapping=aes(x=Month,y=dmv)) + geom_point() + theme_bw()+
  geom_line(aes(x=Month,y=dmv)) +
  geom_line(mapping=aes(x=Month,y=M11ADM),col="red",lwd=1)

summary(M11)



########end debug negative R2


#colnames(elec_ts_clean)
#for (i in colnames(elec_ts_clean)[1:223]){
    #print(dim(elec_ts_clean[i])[1])
    #print(count(elec_ts_clean[i]))
#        print(i)
#}

#length(elec_ts_clean$adm)

#M_adm = lm(adm ~ Bump + Month,data=elec_ts_clean)
#elec_results["adm","coefficient_value"] = M_adm$coefficients[3]


#test to see if i can find address of cell using row and col name
#elec_results["adm","coefficient_value"]
#elec_results

#elec_results <- elec_results %>%
#  column_to_rownames('colnames.elec_ts_clean.')

#for (i in colnames(elec_ts_clean)){
  #print(dim(elec_ts_clean[i])[1])
 # print(count(elec_ts_clean[i]))
  #print(i)
#}

#remove outliers
#elec_results_clean = elec_results[elec_results$mean <= 10000000,]

#plot(elec_results_clean$coefficient_value, elec_results_clean$mean, main="Scatterplot",
#     xlab="coefficient", ylab="mean")


########
# Electricity section attempt
########

elec = read_excel('UPC Utility Data 2018-2021.xlsx', sheet="Electricity", range = "A6:AY232")
elec_clean = data.frame(t(elec[c(-1,-224,-225),-1:-2]))


elec_clean = janitor::row_to_names(elec_clean, row_number=1) %>% clean_names()
elec_clean


#create data frame to store results, row names are the bldg codes and coefficients are the post-covid trend coefficient (Ramp)
elec_results = data.frame(colnames(elec_clean[1:223]),coefficient_value=0,row.names = colnames(elec_clean[1:223]))
elec_results = elec_results[-1]
#colnames(elec_results)
elec_results

elec_clean = elec_clean %>% mutate(Ramp=c(rep(0,27),1:21))
elec_clean = elec_clean %>% mutate(Bump=c(rep(0,27),rep(1,21)))
elec_clean$Month = 1:(dim(elec_clean)[1])
elec_clean = elec_clean %>% mutate(MonthShort=c('Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec'))

#elec_clean$adm_Lag1 <- elec_clean$adm %>% lag(1)

for (i in colnames(elec_clean)[1:223]) {
  elec_clean[i] <- as.numeric(unlist(elec_clean[i]))
  M_i = lm(unlist(elec_clean[i]) ~ Ramp + Bump + Month + MonthShort,data=elec_clean)
  elec_results[i,"coefficient_value"] = M_i$coefficients[3]
  elec_results[i,"mean"] = mean(unlist(elec_clean[i]))
}
elec_results

write.csv(elec_results,"electricity_mean_trend.csv")

################
# nat_gas section start
################
nat_gas = read_excel('UPC Utility Data 2018-2021.xlsx', sheet="Natural Gas", range = "A3:AY181")
nat_gas_clean = data.frame(t(nat_gas[c(-179,-180),-1:-2]))
head(nat_gas_clean)
dim(nat_gas_clean)
nat_gas_clean[1:175]
#janitor to get rid of auto generated column names and put proper column names on
nat_gas_clean = janitor::row_to_names(nat_gas_clean, row_number=1) %>% clean_names()
dim(nat_gas_clean)
nat_gas_clean = nat_gas_clean[c(-176,-177,-178)]
nat_gas_clean

nat_gas_clean = nat_gas_clean %>% mutate(Ramp=c(rep(0,27),1:21))
nat_gas_clean = nat_gas_clean %>% mutate(Bump=c(rep(0,27),rep(1,21)))
nat_gas_clean$Month = 1:(dim(nat_gas_clean)[1])
nat_gas_clean = nat_gas_clean %>% mutate(MonthShort=c('Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec'))




#create data frame to store results, row names are the bldg codes and coefficients are the post-covid trend coefficient (Ramp)
nat_gas_results = data.frame(colnames(nat_gas_clean[1:175]),coefficient_value=0,row.names = colnames(nat_gas_clean[1:175]))
nat_gas_results = nat_gas_results[-1]
#colnames(elec_results)
nat_gas_results

for (i in colnames(nat_gas_clean)[1:175]) {
  nat_gas_clean[i] <- as.numeric(unlist(nat_gas_clean[i]))
  M_i = lm(unlist(nat_gas_clean[i]) ~ Ramp + Bump + Month + MonthShort,data=nat_gas_clean)
  nat_gas_results[i,"coefficient_value"] = M_i$coefficients[3]
  nat_gas_results[i,"mean"] = mean(unlist(nat_gas_clean[i]))
}
nat_gas_results

write.csv(nat_gas_results,"nat_gas_mean_trend.csv")
###############################
#  Nat Gas Section End
###############################



################
# water section start
################

water = read_excel('UPC Utility Data 2018-2021.xlsx', sheet="Water", range = "A5:AY193") #this read cuts off the totals row which we don't happen to use and two houses without acronyms
water_clean = data.frame(t(water[c(-1),c(-1,-3)]))

head(water_clean)
dim(water_clean)
water_clean[1:187]
#janitor to get rid of auto generated column names and put proper column names on
water_clean = janitor::row_to_names(water_clean, row_number=1) %>% clean_names()
dim(water_clean)
#nat_gas_clean = nat_gas_clean[c(-176,-177,-178)]
#nat_gas_clean

water_clean = water_clean %>% mutate(Ramp=c(rep(0,27),1:21))
water_clean = water_clean %>% mutate(Bump=c(rep(0,27),rep(1,21)))
water_clean$Month = 1:(dim(water_clean)[1])
water_clean = water_clean %>% mutate(MonthShort=c('Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec',
                                                      'Jan','Feb','March','April','May','June','July','Aug','Sep','Oct','Nov','Dec'))




#create data frame to store results, row names are the bldg codes and coefficients are the post-covid trend coefficient (Ramp)
water_results = data.frame(colnames(water_clean[1:187]),coefficient_value=0,row.names = colnames(water_clean[1:187])) #187 columns to avoid selecting factor columns
water_results = water_results[-1]
#colnames(elec_results)
water_results

for (i in colnames(water_clean)[1:187]) {
  water_clean[i] <- as.numeric(unlist(water_clean[i]))
  M_i = lm(unlist(water_clean[i]) ~ Ramp + Bump + Month + MonthShort,data=water_clean)
  water_results[i,"coefficient_value"] = M_i$coefficients[3]
  water_results[i,"mean"] = mean(unlist(water_clean[i]))
}
water_results

write.csv(water_results,"water_mean_trend.csv")
###############################
#  Water Section End
###############################


###############################
# Chilled Water Section Start
###############################


write.csv(water_results,"chill_water_mean_trend.csv")

###############################
# Chilled Water Section End
###############################
