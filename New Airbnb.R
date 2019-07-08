#### Airbnb New User Bookings ##########
#### Where will a new guest book thier first travel experience?

setwd("~/Airbnb_Kaggle")

# library packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(caTools)

########################################## 1. train #######################################

train <- read.csv("train_users.csv")
options(scipen=200)
dim(train)
# [1] 213451     16
names(train)
head(train)

# check missing values and bad values
summary(train)
# date first booking has 124543 blank values
124543/213451 # date first booking has 58% blank values
# gender has 95688 "unknown" (bad values)
# age has 87990 missing values and some bad values
# first affiliate tracked has 6065 blank values
# first browser has 27266 "unknown" (bad values)

# check missing value
colSums(is.na(train))
# age has 87990 NA values
87990/213451
# age has 41% missing values.

# check variables propotion using for loop
for (i in names(train[,])) {
  print(names(train[i]))
  print(prop.table(table(train[,i])))
}
# gender has 45% unknown
# age has bad values
# first affiliate tracked has 3% blank values
# first browser has unknown 

# check variable types
str(train)
# date_account_created: factor
# timestamp_first_active: num 
# date_first_booking: factor

# change correct variable type
# 1. date account created
head(train)
train$date_account_created <- as.Date(train$date_account_created)

# 观察用户增长情况
# 计算离首次注册时间相差的天数
train$dac_train_day = train$date_account_created - min(train$date_account_created)
dac_train_day = train %>% count(date_account_created, dac_train_day)
plot(dac_train_day$dac_train_day,dac_train_day$n, main = "Accounts Created vs Days", ylab = "Accounts Created",xlab = 
       "Days",col="pink")
# n: 每一天新建用户的数量
# dac train day: 离首次注册时间相差的天数

# 2. timestamp first active
train$timestamp_first_active <- ymd_hms(as.character(train$timestamp_first_active))
train$date_first_active <- format(train$timestamp_first_active,"%Y-%m-%d")
train$date_first_active <- as.Date(train$date_first_active)
train$tfa_train_day = train$date_first_active - min(train$date_first_active)
tfa_train_day = train %>% count(date_first_active,tfa_train_day)
plot(tfa_train_day$tfa_train_day,tfa_train_day$n, main = "User First Active vs Days", ylab = "User First Active",xlab = 
       "Days",col="pink")

# 提取用户从首次活动到最终注册所花费的时间作为新的特征
train$dt_span = train$date_account_created - train$date_first_active

# Check the age column for group bucket
span_class <- function(dt_span){
  if (dt_span == 0){
    return('One day')
  }else if (dt_span <= 7) {
    return('One week')
  }else if (dt_span <= 30) {
    return('One month')
  }else if (dt_span <= 365) {
    return('One year')
  }else if (dt_span > 365) {
    return('Over one year')
  }
}

train$span_class <- sapply(train$dt_span, span_class)
class(train$span_class)  # 重组之后先返回的character格式
train$span_class <- as.factor(train$span_class)  # 然后再定义为factor格式
levels(train$span_class)
plot(table(train$span_class), main = "Span Class Distribution", ylab = "Counts", xlab = 
       "Time",col="pink")
# insights: 首次活动到最终注册大部分所花费的时间为一天

# 3. date first booking
# 因为有58%的missing values，所以把剩下的42%的值简单看看增长情况怎么样
date_first_booking = train[train$date_first_booking!='',]
date_first_booking$date_first_booking <- ymd(as.character(date_first_booking$date_first_booking))
date_first_booking$dfb_train_day = date_first_booking$date_first_booking - min(date_first_booking$date_first_booking)
dfb_train_day = date_first_booking %>% count(date_first_booking, dfb_train_day)
plot(dfb_train_day$dfb_train_day,dfb_train_day$n, main = "User First Booking vs Days", ylab = "User First Booking", xlab = 
       "Days",col="pink")

# 4. age
summary(train$age)
plot(train$age)
# the problem of age is not only on NA values but the range is unrealistic
train %>% select(age) %>% filter(age >= 1000 & age <= 2014) %>% arrange(age) %>% head
# for age in the range between 1924 and 2014, we predict these are customers' birth year
train$age = ifelse((train$age >= 1924) & (train$age <= 2014), 2015-train$age, train$age)
# select the range of age from 10 to 100
train$age = ifelse((train$age >= 10) & (train$age <=100), train$age, mean(train$age))
# convert all NA values into mean value for age
train$age[is.na(train$age)] <- round(mean(train$age, na.rm = T))
summary(train$age)
hist(train$age,xlab = 'age',col = 'salmon',border = 'gray',main = 'Age Distrubution')

# Check the age column for group bucket
group_age <- function(age){
  if (age >= 15 & age <= 19){
    return('10s')
  }else if (age > 19 & age <= 29) {
    return('20s')
  }else if (age > 29 & age <= 39) {
    return('30s')
  }else if (age > 39 & age <= 49) {
    return('40s')
  }else if (age > 49 & age <= 59) {
    return('50s')
  }else if (age > 59 & age <= 69) {
    return('60s')
  }else if (age > 69 & age <= 79) {
    return('70s')
  }else if (age > 79 & age <= 89) {
    return('80s')
  }else if (age > 89) {
    return('90s+')
  }
}

train$age_group <- sapply(train$age, group_age)
class(train$age_group)  # 重组之后先返回的character格式
train$age_group <- as.factor(train$age_group)  # 然后再定义为factor格式
levels(train$age_group)

# 5. gender
summary(train$gender)
# unknown has 95688, choose to fill them
# 把gender里的unknown变成NA值，以便之后填充别的值
train$gender = factor(ifelse(train$gender == "-unknown-","", as.character(train$gender)))

# 因为target variable是country destination，所以是按照国家的比例来填充gender
prop.table(table(train$country_destination))
# US占29.22%, NDF占58.35%，其余都归类为outside US
train$country = train$country_destination
train$country = factor(ifelse(train$country == "US", "US",
                              ifelse(train$country == "NDF", "NDF", "Outside US")))

# 1) country = US
train %>% count(country,gender) %>% filter(gender != '' & country == 'US')%>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
df_us = train[train$country == 'US' & train$gender == '',]
set.seed(123)
df_us_f =train %>% filter(gender == '' & country == 'US') %>% sample_n(round(20109*0.54),0)
female = df_us_f$id
# setdiff(差)：求向量x与向量y中不同的元素(只取x中不同的元素)
male = setdiff(df_us$id,df_us_f$id)
train$gender = ifelse(train$id %in% female, 'FEMALE',
                      ifelse(train$id %in% male, 'MALE', as.character(train$gender)))
train %>% filter(country == 'US') %>% count(gender)

# 2) country = NDF
train %>% count(country,gender) %>% filter(gender != '' & country == 'NDF') %>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
df_ndf = train[train$gender == '' & train$country == 'NDF',]
set.seed(123)
df_ndf_f = train %>% filter(gender == '' & country == 'NDF') %>% sample_n(round(66670*0.54),0) 
female = df_ndf_f$id
male = setdiff(df_ndf$id,df_ndf_f$id)
train$gender = ifelse(train$id %in% female, 'FEMALE',
                      ifelse(train$id %in% male, 'MALE',as.character(train$gender)))
train %>% filter(country == 'NDF') %>% count(gender)

# 3) country = Outside US
train %>% count(country,gender) %>% filter(gender != '' & country == 'Outside US') %>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
df_outside = train[train$country == 'Outside US' & train$gender=='',]
set.seed(123)
df_outside_f = train %>% filter(country == 'Outside US' & gender == '') %>% sample_n(round(8909*0.53),0)
female = df_outside_f$id
male = setdiff(df_outside$id,df_outside_f$id)
train$gender = ifelse(train$id %in% female, 'FEMALE',
                      ifelse(train$id %in% male, 'MALE', as.character(train$gender)))
train %>% filter(country == 'Outside US') %>% count(gender)

train$gender <- as.factor(train$gender)

barplot(table(train$gender),main = "Gender Distribution",xlab = "Gender",ylab = "Counts",col = "salmon")

# 6. signup method
barplot(table(train$signup_method),main = "Signup Method Distribution",xlab = "Signup Methods",ylab = "Counts",col = "salmon")

# 7. signup flow
barplot(table(train$signup_flow),main = "Signup Flow Distribution",xlab = "Signup Flows",ylab = "Counts",col = "salmon")

# 8. language
barplot(table(train$language),main = "Language Distribution",xlab = "Languages",ylab = "Counts",col = "salmon")

# 9. affiliate channel
barplot(table(train$affiliate_channel),main = "Affiliate Channel Distribution",xlab = "Affiliate Channels",ylab = "Counts",col = "salmon")

# 10. affiliate provider
barplot(table(train$affiliate_provider),main = "Affiliate Provider Distribution",xlab = "Affiliate Providers",ylab = "Counts",col = "salmon")

# 11. first affiliate tracked
barplot(table(train$first_affiliate_tracked),main = "First Affiliate Tracked Distribution",xlab = "First Affiliate Tracked",ylab = "Counts",col = "salmon")

# 12. signup app
barplot(table(train$signup_app),main = "Signup App Distribution",xlab = "Signup App",ylab = "Counts",col = "salmon")

# 13. First device type
barplot(table(train$first_device_type), width = 0.3, main = "First Device Type Distribution",xlab = "First Device Type",ylab = "Counts",col = "salmon")

# 14. First Browser
barplot(table(train$first_browser),  main = "First Browser Distribution",xlab = "First Browser Type",ylab = "Counts",col = "salmon")

# 15. Country Destination
barplot(table(train$country_destination), main = "Country Destination Distribution",xlab = "Country Destination",ylab = "Counts",col = "salmon")

################################### 2. Session ###################################
sessions <- read.csv("sessions.csv")
summary(sessions)
# user_id has 34496 blank values
# action_type has 1126204 blank values and 1031170 unknown.
# action_detail has 1126204 blank values and 1031141 unknown.
# secs_elapsed has 136031 NA's.

length(unique(sessions$user_id))
# 135484
# 发现有135483个不同的id，这说明10567737行数据由135484个人提供，代表每个人平均提供78条数据左右

# rename: 将user_id改名为id，为了后面的数据合并
sessions = rename(sessions, id = user_id)

# 将空值统一起来，赋值为NAN
sessions$action = factor(ifelse(sessions$action == "","NAN", as.character(sessions$action)))
sessions$action_type = factor(ifelse(sessions$action_type == "","NAN", as.character(sessions$action_type)))
sessions$action_detail = factor(ifelse(sessions$action_detail == "","NAN", as.character(sessions$action_detail)))
sessions$device_type = factor(ifelse(sessions$device_type == "","NAN", as.character(sessions$device_type)))

# 1.action (用户行为)
# action有很多分类，如果分的太细会减小模型的普适性，所以对用户发生次数较少的行为列为OTHER一类
# 将特征action次数低于100的列为other
# 一个向量中各个元素出现的次数
# as.data.frame(table(sessions$action))
sort(table(sessions$action),decreasing = TRUE)
# number of unique actions
action = sessions %>% group_by(action) %>% count(action) %>% arrange(desc(n))
action_less_than_100 = action %>% filter(n<100) 
low_action = action_less_than_100$action
sessions$action = ifelse(sessions$action %in% low_action, 'Other', as.character(sessions$action))
as.data.frame(table(sessions$action))

# number of total actions per id
ta_per_id = sessions %>% filter(id!='') %>% group_by(id) %>% count() %>% arrange(desc(n))
ta_per_id = rename(ta_per_id, ta_per_id_count = n)

# number of per action per id
per_action_per_id = sessions %>% filter(id!='') %>% group_by(id,action) %>% count() %>% ungroup %>% group_by(id) %>% mutate(total_n = sum(n))
per_action_per_id = rename(per_action_per_id, action_n = n)

summary(action$n)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.0      30.0     472.5   29354.8    4948.8 2768278.0
# 每一个action平均有473条（因为有outlier）所以取中位数比较合理
sd(action$n)
# 169887.9 <- 代表大部分数值和平均数之间的差异较大
# 标准差是一组数据平均值分散程度的一种度量。 
# 一个较大的标准差，代表大部分数值和其平均值之间差异较大；一个较小的标准差，代表这些数值较接近平均值。

summary(ta_per_id$ta_per_id_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   16.00   41.00   77.75   95.00 2722.00 
# 平均一个用户有41条actions(因为有outlier) 所以取中位数比较合理

# secs_elapsed 特征中的缺失值用0填充再获取具体的停留时长值
sessions$secs_elapsed = ifelse(is.na(sessions$secs_elapsed),"0", as.numeric(sessions$secs_elapsed))
sessions$secs_elapsed <- as.numeric(sessions$secs_elapsed)

# 2 action_type(用户行为类型，click等)
# number of unique actions_type
as.data.frame(table(sessions$action_type))
action_type = sessions %>% group_by(action_type) %>% count(action_type) 
plot(action_type$n)
summary(action_type$n)
# Min. 1st Qu.  Median   Mean   3rd Qu.    Max. 
# 4      18952  623357  960703  1561194  3560902 
# 每一个action_type平均有623357条（因为有outlier）所以取中位数比较合理

per_action_type_per_id = sessions %>% filter(id!='') %>% group_by(id,action_type) %>% count() %>% ungroup %>% group_by(id) %>% mutate(total_action_type_n = sum(n))
per_action_type_per_id = rename(per_action_type_per_id, action_type_n = n)

# 求每个行为类型总的停留时长
action_type_per_totaltime = sessions %>% group_by(action_type) %>% summarise(total_time =sum(secs_elapsed))
action_type_detail = inner_join(action_type, action_type_per_totaltime,by = "action_type")
action_type_detail$average_time = action_type_detail$total_time / action_type_detail$n
plot(action_type_detail$average_time)
summary(action_type_detail$average_time)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7205   17823   28465   32278   36035   96621
# 每个行为类型的平均停留时长28465

per_action_type_time_per_id = sessions %>% filter(id!='') %>% group_by(id,action_type) %>% summarise(time = sum(secs_elapsed)) %>% ungroup %>% group_by(id) %>% mutate(total_action_type_time = sum(time))


# 3. action_detail(用户行为具体)
# number of unique action_detail
as.data.frame(table(sessions$action_detail))
action_detail = sessions %>% group_by(action_detail) %>% count(action_detail) %>% arrange(desc(n))
plot(action_detail$n)
summary(action_detail$n)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.0     319.8    2733.0   67741.9   26635.8 1776885.0 
# 每一个action_detail平均有2733条（因为有outlier）所以取中位数比较合理

per_action_detail_per_id = sessions %>% filter(id!='') %>% group_by(id,action_detail) %>% count() %>% ungroup %>% group_by(id) %>% mutate(total_action_detail_n = sum(n))
per_action_detail_per_id = rename(per_action_detail_per_id, action_detail_n = n)

# 4. device_type
# number of unique device_type
as.data.frame(table(sessions$device_type))
device_type = sessions %>% group_by(device_type) %>% count(device_type) %>% arrange(desc(n))
plot(device_type$n)
summary(device_type$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 68   11736  175582  754838  800581 3594286 
# 每一个device_type平均有175582条（因为有outlier）所以取中位数比较合理

per_device_type_per_id = sessions %>% filter(id!='') %>% group_by(id,device_type) %>% count() %>% ungroup %>% group_by(id) %>% mutate(total_device_type_n = sum(n))
per_device_type_per_id = rename(per_device_type_per_id, device_type_n = n)

# 5. secs_elapsed
summary(sessions$secs_elapsed)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#  0     208    1108      19156    8193   1799977


######################## 3. train & test ##################################
test <- read.csv("test_users.csv")
options(scipen=200)
summary(test)
# date_frist_booking all missing
# gender "unknown" has 62096 values and need to fill them
# age has missing and bad value
# dosen't have country destination


# test drop date_first_booking
test$date_first_booking = NULL

# test: date account created
test$date_account_created <- as.Date(test$date_account_created)
# test: timestamp first active
test$timestamp_first_active <- ymd_hms(as.character(test$timestamp_first_active))


# train drop date_first_booking and country_destination
train$date_first_booking = NULL
train_2 = subset(train,select = id:first_browser)

# 合并train_2和test文件：便于进行相同的特征提取操作
test_data = rbind(train_2,test)

# 1. timestamp first active
# 1.1 break out timestamp_first_active by year, month and day
library(stringr)
tfa = as.data.frame(str_split_fixed(test_data$timestamp_first_active,'-',3))
tfa
test_data['tfa_year'] = tfa[,1]
test_data['tfa_month'] = tfa[,2]
test_data['tfa_day_time'] = tfa[,3]
tfa_day_time = as.data.frame(str_split_fixed(test_data$tfa_day_time,' ',2))
tfa_day_time
test_data['tfa_day']=tfa_day_time[,1]
test_data['tfa_time']=tfa_day_time[,2]
test_data$tfa_day_time = NULL
head(test_data)

# 1.2 提取特征：weekday
test_data$tfa_wd = wday(test_data$timestamp_first_active, label = TRUE)
head(test_data)

# 1.3 提取特征：season
test_data$tfa_ymd <- format(test_data$timestamp_first_active,"%Y-%m-%d")
test_data$tfa_ymd <- as.Date(test_data$tfa_ymd)
test_data$tfa_ymd = update(test_data$tfa_ymd, year = 2000)


# season def
tfa_season <- function(tfa_ymd){
  if (tfa_ymd >= '2000-01-01' & tfa_ymd <= '2000-03-20'){
    return('winter')
  }else if (tfa_ymd >= '2000-03-21' & tfa_ymd <= '2000-06-20'){
    return('spring')
  }else if (tfa_ymd >= '2000-06-21' & tfa_ymd <= '2000-09-22') {
    return('summer')
  }else if (tfa_ymd >= '2000-09-23' & tfa_ymd <= '2000-12-20') {
    return('autumn')
  }else if (tfa_ymd >= '2000-12-21' & tfa_ymd <= '2000-12-31') {
    return('winter')
  }
}

test_data$tfa_season <- sapply(test_data$tfa_ymd, tfa_season)
class(test_data$tfa_season)  # 重组之后先返回的character格式
test_data$tfa_season <- as.factor(test_data$tfa_season)  # 然后再定义为factor格式
levels(test_data$tfa_season)

summary(test_data$tfa_season)
# autumn spring summer winter 
# 45021  72967 105988  51571 

test_data$tfa_ymd = NULL

# 2. date account created
# 2.1 break out date_account_created by year, month and day
dac = as.data.frame(str_split_fixed(test_data$date_account_create, '-',3))
dac
test_data['dac_year'] = dac[,1]
test_data['dac_month'] = dac[,2]
test_data['dac_day'] = dac[,3]

# 2.2 提取特征：weekday
test_data$dac_wd = wday(test_data$date_account_created, label = TRUE)
head(test_data)

# 2.3 提取特征：season
test_data$dac_ymd = update(test_data$date_account_created, year = 2000)

# season def
dac_season <- function(dac_ymd){
  if (dac_ymd >= '2000-01-01' & dac_ymd <= '2000-03-20'){
    return('winter')
  }else if (dac_ymd >= '2000-03-21' & dac_ymd <= '2000-06-20'){
    return('spring')
  }else if (dac_ymd >= '2000-06-21' & dac_ymd <= '2000-09-22') {
    return('summer')
  }else if (dac_ymd >= '2000-09-23' & dac_ymd <= '2000-12-20') {
    return('autumn')
  }else if (dac_ymd >= '2000-12-21' & dac_ymd <= '2000-12-31') {
    return('winter')
  }
}

test_data$dac_season <- sapply(test_data$dac_ymd, dac_season)
class(test_data$dac_season)  # 重组之后先返回的character格式
test_data$dac_season <- as.factor(test_data$dac_season)  # 然后再定义为factor格式
levels(test_data$dac_season)

summary(test_data$dac_season)
# autumn spring summer winter 
# 45027  72964 105979  51577

test_data$dac_ymd = NULL

# 3. date_account_created和timestamp_first_active之间的差值 
#    即用户在airbnb平台活跃到正式注册所花的时间
test_data$date_first_active <- format(test_data$timestamp_first_active,"%Y-%m-%d")
test_data$date_first_active <- as.Date(test_data$date_first_active)
test_data$time_span = test_data$date_account_created - test_data$date_first_active

test_data_span_class <- function(time_span){
  if (time_span == 0){
    return('One day')
  }else if (time_span <= 7) {
    return('One week')
  }else if (time_span <= 30) {
    return('One month')
  }else if (time_span <= 365) {
    return('One year')
  }else if (time_span > 365) {
    return('Over one year')
  }
}

test_data$test_data_span_class <- sapply(test_data$time_span, test_data_span_class)
class(test_data$test_data_span_class)  # 重组之后先返回的character格式
test_data$test_data_span_class <- as.factor(test_data$test_data_span_class)  # 然后再定义为factor格式
levels(test_data$test_data_span_class)
plot(table(test_data$test_data_span_class), main = "Span Class Distribution", ylab = "Counts", xlab = 
       "Time",col="pink")
# insights: 首次活动到最终注册大部分所花费的时间为一天

# 4. age
summary(test_data$age)
plot(test_data$age)
# the problem of age is not only on NA values but the range is unrealistic
test_data %>% select(age) %>% filter(age >= 500 & age <= 2002) %>% arrange(age) %>% head
# for age in the range between 1920 and 2002, we predict these are customers' birth year
test_data$age = ifelse((test_data$age >= 1920) & (test_data$age <= 2002), 2015-test_data$age, test_data$age)
# select the range of age from 10 to 100
test_data$age = ifelse((test_data$age >= 10) & (test_data$age <=100), test_data$age, mean(test_data$age))
# convert all NA values into mean value for age
test_data$age[is.na(test_data$age)] <- round(mean(test_data$age, na.rm = T))
summary(test_data$age)
hist(test_data$age,xlab = 'age',col = 'salmon',border = 'gray',main = 'Age Distrubution')

td_age_group <- function(age){
  if (age >= 13 & age <= 19){
    return('10s')
  }else if (age > 19 & age <= 29) {
    return('20s')
  }else if (age > 29 & age <= 39) {
    return('30s')
  }else if (age > 39 & age <= 49) {
    return('40s')
  }else if (age > 49 & age <= 59) {
    return('50s')
  }else if (age > 59 & age <= 69) {
    return('60s')
  }else if (age > 69 & age <= 79) {
    return('70s')
  }else if (age > 79 & age <= 89) {
    return('80s')
  }else if (age > 89) {
    return('90s+')
  }
}

test_data$age_group <- sapply(test_data$age, td_age_group)
class(test_data$age_group)  # 重组之后先返回的character格式
test_data$age_group <- as.factor(test_data$age_group)  # 然后再定义为factor格式
levels(test_data$age_group)
plot(table(test_data$age_group), main = "Age Distribution", ylab = "Counts", xlab = 
       "Age",col="pink")
# insight: 30s比较多，其次是20s，再到40s

# 5. gender
summary(test_data$gender)
# unknown has 33792, choose to fill them
# 把gender里的unknown变成NA值，以便之后填充别的值
test_data$gender = factor(ifelse(test_data$gender == "-unknown-","", as.character(test_data$gender)))
gender_missing = test_data %>% filter(gender == '')
td_gender_missing = gender_missing$id
test_data %>% filter(gender == '') %>% count()
# 33792
gender_pct = test_data %>% filter(gender != '') %>% count(gender) %>% mutate(total_n = sum(n)) %>% mutate(pct=n/total_n)
gender_pct
# female: 0.534
# male: 0.465
# other: 0.00138
td_f = test_data %>% filter(gender == '') %>% sample_n(round(33792*0.534),0)
count(td_f) # 18045
td_female = td_f$id
td_m = test_data %>% filter(gender == '') %>% sample_n(round(33792*0.465),0)
count(td_m) # 15713
td_male = td_m$id
male_other = setdiff(gender_missing$id,td_f$id)
td_other = setdiff(male_other,td_m$id)

test_data$gender = factor(ifelse(test_data$id %in% td_female, 'FEMALE',
                      ifelse(test_data$id %in% td_male, 'MALE', 
                             ifelse(test_data$id %in% td_other, 'OTHER', as.character(test_data$gender)))))
table(test_data$gender)

# 6. signup method
barplot(table(test_data$signup_method),main = "Signup Method Distribution",xlab = "Signup Methods",ylab = "Counts",col = "salmon")

# 7. signup flow
barplot(table(test_data$signup_flow),main = "Signup Flow Distribution",xlab = "Signup Flows",ylab = "Counts",col = "salmon")

# 8. language
barplot(table(test_data$language),main = "Language Distribution",xlab = "Languages",ylab = "Counts",col = "salmon")

# 9. affiliate channel
barplot(table(test_data$affiliate_channel),main = "Affiliate Channel Distribution",xlab = "Affiliate Channels",ylab = "Counts",col = "salmon")

# 10. affiliate provider
barplot(table(test_data$affiliate_provider),main = "Affiliate Provider Distribution",xlab = "Affiliate Providers",ylab = "Counts",col = "salmon")

# 11. first affiliate tracked
barplot(table(test_data$first_affiliate_tracked),main = "First Affiliate Tracked Distribution",xlab = "First Affiliate Tracked",ylab = "Counts",col = "salmon")

# 12. signup app
barplot(table(test_data$signup_app),main = "Signup App Distribution",xlab = "Signup App",ylab = "Counts",col = "salmon")

# 13. First device type
barplot(table(test_data$first_device_type), width = 0.3, main = "First Device Type Distribution",xlab = "First Device Type",ylab = "Counts",col = "salmon")

# 14. First Browser
barplot(table(test_data$first_browser),  main = "First Browser Distribution",xlab = "First Browser Type",ylab = "Counts",col = "salmon")
# 因为之后random forest条件的限制，level不能超过55个，要把first browser的level减少(把次数少于10的browser变成other)
test_data$first_browser_1 = test_data$first_browser
first_browser_1 = test_data %>% group_by(first_browser_1) %>% count(first_browser_1) %>% arrange(desc(n))
first_browser_less_than_10 = first_browser_1 %>% filter(n<10) 
low_first_browser = first_browser_less_than_10$first_browser_1
test_data$first_browser_1 = factor(ifelse(test_data$first_browser_1 %in% low_first_browser, 'Other', as.character(test_data$first_browser_1)))
as.data.frame(table(test_data$first_browser_1))
barplot(table(test_data$first_browser),  main = "First Browser Distribution",xlab = "First Browser Type",ylab = "Counts",col = "salmon")

# 15. Country Destination
barplot(table(test_data$country_destination), main = "Country Destination Distribution",xlab = "Country Destination",ylab = "Counts",col = "salmon")


####################### 模型构建 #######################

# 1. 将train和test 数据进行分离操作
train_t = test_data[1:213451,]
test_t = test_data[213452:275547,]

train_t$country_destination = train$country_destination
# write.csv(train_t,"~/BA/HW/HW/Week 6-7/Airbnb Kaggle/train_t.csv")
# write.csv(test_t,"~/BA/HW/HW/Week 6-7/Airbnb Kaggle/test_t.csv")


# 2. Machine learning Model[Random Forest Model]
# 提取10%的数据进行模型训练：减少训练模型花费的时间
set.seed(123)
train_samp = sample(1:nrow(train_t),0.1*nrow(train_t))
train_3 <- train_t[train_samp,]

# Deleting duplicate and unnecessary columns
del_col = c("id","first_browser","timestamp_first_active","tfa_year","tfa_month","tfa_day","tfa_time","tfa_wd",
            "date_account_created","dac_year","dac_month","dac_day","dac_wd",
            "date_first_active","time_span","age_group")
train_3 = train_3[,-which(names(train_3) %in% del_col)]
names(train_3)


# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
# split represents how do I want my training data to split, split ratio is 75%
split = sample.split(train_3$country_destination, SplitRatio = 0.75)
# Training set get 75% data of total, other 25% gives to test set
training_set = subset(train_3, split == TRUE)
test_set = subset(train_3, split == FALSE)

# Fitting Random Forest model 
library(randomForest)
set.seed(123)
model <- randomForest(country_destination~.,
                      data = training_set,
                      ntree = 60,
                      importance = TRUE)
model
# oob error rate is 38.28% (misclassification rate)
plot(model)
# 0.4这条实线代表average的情况
# 每一个颜色代表不同的country destination，并且它们的波动不是特别大

importance(model)
# Variance Importance plot
varImpPlot(model, sort = TRUE, n.var = 10, main = "Top 10 Feature Importance")
# 看accuracy
# age, signup method, signup flow, first affiliate tracked


# Prediciting the test_set results
y_pred = predict(model, newdata = test_set, type = "response")
y_pred

# Confusion matrix
library(caret)
# install.packages('e1071',dependencies = TRUE)
caret::confusionMatrix(y_pred, test_set$country_destination)
table = table(Predicted = y_pred, Actual = test_set$country_destination)
print(paste('Random Forest Accuracy', sum(diag(table)/sum(table))))
# 62.18%

# 3. xgboost
# 需要建模的变量

# 3.1 构建稀疏矩阵
train_t$signup_flow <- as.numeric(train_t$signup_flow)

# 由于xgboost模型要求所有的变量都为数值型，因此存在分类变量则需要将分类变量转化为0，1格式的稀疏矩阵

# 构建稀疏矩阵
mymatrix <- function(train_t){
  matrix_num <- train_t[,c("age","signup_flow","test_data_span_class")]
  matrix_num <- cbind(matrix_num,
                      model.matrix(~dac_year-1,train_t),
                      model.matrix(~dac_season-1,train_t),
                      model.matrix(~gender-1,train_t),
                      model.matrix(~signup_method-1,train_t),
                      model.matrix(~language-1,train_t),
                      model.matrix(~affiliate_channel-1,train_t),
                      model.matrix(~affiliate_provider-1,train_t),
                      model.matrix(~first_affiliate_tracked-1,train_t),
                      model.matrix(~signup_app-1,train_t),
                      model.matrix(~first_device_type-1,train_t),
                      model.matrix(~first_browser-1,train_t),
                      model.matrix(~tfa_year-1,train_t),
                      model.matrix(~tfa_season-1,train_t)
  )
  return(data.matrix(matrix_num))
}

# 设置种子，创建训练数据的训练子集和测试子集
set.seed(1234)
library(caret)
ind <- createDataPartition(train_t$country_destination, p=.7,list = FALSE)
train_val <- train_t[ind,]
test_val <- train_t[-ind,]

# 获取每个数据集的稀疏矩阵
xgb.train_val <- mymatrix(train_val)
xgb.test_val <- mymatrix(test_val)
xgb.test <- mymatrix(test_t)
# 生成用于xgboots模型的DMatrix

library(xgboost)







# 注意，预测变量集和响应变量是要分开的
dtrain_val <- xgb.DMatrix(data = xgb.train_val,label=train_val$country_destination)
dtest_val <- xgb.DMatrix(data = xgb.test_val,label=test_val$country_destination)
dtest_sub <- xgb.DMatrix(data = xgb.test)

xgb <- xgboost(data = dtrain_val, label = train_val$country_destination,
               eta = 0.1, max_depth = 15, nround = 25, subsample = 0.5,
               colsample_bytree = 0.5, seed = 1, 
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3)


model_xgboost <- xgboost(data = dtrain_val, nrounds = 5)
# [1]	train-rmse:6.350208 
# [2]	train-rmse:4.693036 
# [3]	train-rmse:3.613974 
# [4]	train-rmse:2.942385 
# [5]	train-rmse:2.548774 

summary(model_xgboost)
#                Length Class              Mode       
# handle             1  xgb.Booster.handle externalptr
# raw            19154  -none-             raw        
# niter              1  -none-             numeric    
# evaluation_log     2  data.table         list       
# call              13  -none-             call       
# params             1  -none-             list       
# callbacks          2  -none-             list       
# feature_names    157  -none-             character  
# nfeatures          1  -none-             numeric    

pred_xgboost <- predict(model_xgboost, dtest_val)
xgb.importance(colnames(xgb.train_val),model_xgboost)
#                                 Feature          Gain          Cover   Frequency
# 1:                                   age 0.58077560328 0.350724864603 0.129032258
# 2:                 signup_methodfacebook 0.22195856550 0.094598269882 0.040322581
# 3:                           signup_flow 0.02933970968 0.049089688079 0.088709677
# 4:              affiliate_channelcontent 0.01968060835 0.061305769955 0.080645161
# 5:                          dac_year2011 0.01777862078 0.040689576214 0.020161290
# 6:               first_affiliate_tracked 0.01204690878 0.035240420818 0.044354839
# 7:                          dac_year2014 0.01185083113 0.030200806230 0.028225806
# 8:                         signup_appWeb 0.01116987377 0.029358192988 0.016129032
# 9:                first_browser-unknown- 0.01019469925 0.021524876549 0.028225806
# 10:                          tfa_year2013 0.00912232550 0.018829147721 0.016129032
# 11:            first_affiliate_trackedomg 0.00762056611 0.025174315049 0.028225806
# 12:        affiliate_channelsem-non-brand 0.00682684954 0.040831897301 0.032258065
# 13:                       signup_appMoweb 0.00580371401 0.020264350672 0.024193548
# 14:                   first_browserChrome 0.00526906468 0.029617267146 0.020161290
# 15:          first_device_typeMac Desktop 0.00387721836 0.022334681273 0.016129032
# 16:                          dac_year2012 0.00320514553 0.009624208975 0.016129032
# 17:                            languageen 0.00308739159 0.014025754460 0.020161290
# 18:                   first_browserSafari 0.00306001899 0.011876230885 0.024193548
# 19:      first_affiliate_trackeduntracked 0.00275693414 0.013634993701 0.020161290
# 20:        first_device_typeOther/Unknown 0.00237191220 0.004713339536 0.012096774
# 21:                          tfa_year2011 0.00183613575 0.008573205080 0.008064516
# 22:                          dac_year2010 0.00173213702 0.000345960163 0.012096774
# 23:        first_affiliate_trackedproduct 0.00155010938 0.001921221545 0.016129032
# 24:                affiliate_channelother 0.00148187716 0.005166097081 0.012096774
# 25:                           genderOTHER 0.00136278303 0.000278533002 0.012096774
# 26:               first_device_typeiPhone 0.00134946274 0.002300895288 0.012096774
# 27:                      dac_seasonspring 0.00131488941 0.001422305810 0.008064516
# 28:                      tfa_seasonsummer 0.00129942819 0.002578749493 0.004032258
# 29:                  first_browserFirefox 0.00128481538 0.010299611909 0.016129032
# 30:                          genderFEMALE 0.00125507214 0.001778674192 0.008064516
# 31:                affiliate_providerbing 0.00111467156 0.000629471009 0.008064516
# 32: affiliate_providerfacebook-open-graph 0.00108842303 0.000075346458 0.004032258
# 33:                      tfa_seasonwinter 0.00104175760 0.004536626072 0.004032258
# 34:          affiliate_channelremarketing 0.00100164665 0.005699405193 0.008064516
# 35:                 first_browserCoolNovo 0.00090895286 0.001285188832 0.008064516
# 36:                    signup_methodbasic 0.00083348449 0.001207127187 0.004032258
# 37:                            languagefr 0.00072970322 0.000494842954 0.012096774
# 38:               affiliate_channeldirect 0.00072526411 0.000273555158 0.008064516
# 39:                      tfa_seasonautumn 0.00071773364 0.005555273981 0.004032258
# 40:            affiliate_providerfacebook 0.00068953676 0.001843838696 0.012096774
# 41:                  test_data_span_class 0.00066238200 0.000733779469 0.008064516
# 42:                            languageit 0.00064259606 0.000177392261 0.004032258
# 43:        first_device_typeAndroid Phone 0.00060163364 0.000577656178 0.008064516
# 44:      first_device_typeWindows Desktop 0.00058647258 0.002892127404 0.008064516
# 45:  first_affiliate_trackedtracked-other 0.00057206000 0.000042764206 0.008064516
# 46:                     first_browserSilk 0.00050231799 0.000451399951 0.004032258
# 47:                            languagezh 0.00046128206 0.000325822521 0.004032258
# 48:                  affiliate_channelseo 0.00045097561 0.000028735736 0.004032258
# 49:                affiliate_providervast 0.00044465084 0.000294145331 0.004032258
# 50:                  affiliate_channelapi 0.00042878380 0.000019911376 0.004032258
# 51:          first_browserAndroid Browser 0.00041576213 0.000177165996 0.004032258
# 52:                      dac_seasonwinter 0.00038028220 0.004260808257 0.004032258
# 53:                          tfa_year2014 0.00038028220 0.001044668457 0.004032258
# 54:      first_device_typeDesktop (Other) 0.00037592607 0.000020137642 0.004032258
# 55:                       first_browserIE 0.00033157079 0.002263335192 0.004032258
# 56:                            languagenl 0.00030429927 0.000089601193 0.004032258
# 57:      first_affiliate_trackedlocal ops 0.00024698741 0.004311491760 0.004032258
# 58:             first_browserAOL Explorer 0.00024442687 0.000172640683 0.004032258
# 59:         first_affiliate_trackedlinked 0.00022980334 0.000106118585 0.008064516
# 60:               affiliate_providerother 0.00021310335 0.000007466766 0.004032258
# 61:                            languageja 0.00020900972 0.000043895534 0.004032258
# 62:                            languagepl 0.00016291858 0.000281021924 0.004032258
# 63:                            languagede 0.00003802822 0.001753332440 0.004032258


pred.test <- predict(model_xgboost,dtest_sub)

###########################################################

# 1. 加载的所有数据库
# install.packages("xgboost")
library(xgboost)
library(readr)
library(stringr)
library(caret)
# install.packages("car")
library(car)

# 将目标变量进行labels encoding
table(train$country_destination)
train$country_destination_code = train$country_destination
a <- sub("AU","0", train$country_destination_code)
b <- sub("CA", "1", a)
c <- sub("DE","2", b)
d <- sub("ES", "3", c)
e <- sub("FR","4", d)
f <- sub("GB","5", e)
g <- sub("IT","6", f)
h <- sub("NDF","7", g)
i <- sub("NL","8", h)
j <- sub("other","9",i)
k <- sub("PT","10", j)
l <- sub("US","11",k)
train$country_destination_code <- l
train$country_destination_code <- as.numeric(train$country_destination_code)

test_data_x = test_data

# test drop unnecessary columns
test_data_x$date_account_created = NULL
test_data_x$timestamp_first_active = NULL
test_data_x$tfa_month = NULL
test_data_x$tfa_day = NULL
test_data_x$tfa_time = NULL
test_data_x$tfa_wd = NULL
test_data_x$dac_month = NULL
test_data_x$dac_day = NULL
test_data_x$dac_wd = NULL
test_data_x$date_first_active = NULL
test_data_x$time_span = NULL
test_data_x$age = NULL
test_data_x$first_browser_1 = NULL


# 独热编码分类特征
ohe_feats = c('gender','age_group','signup_method','signup_flow','language','affiliate_channel',
              'affiliate_provider','first_affiliate_tracked','signup_app','first_device_type',
              'first_browser','tfa_year','tfa_season','dac_year','dac_season','test_data_span_class')

dummies <- dummyVars(~ gender + age_group + signup_method + signup_flow + language + affiliate_channel +
              affiliate_provider + first_affiliate_tracked + signup_app + first_device_type +
                     first_browser + tfa_year +tfa_season + dac_year + dac_season + test_data_span_class, data = test_data_x)

df_all_ohe <- as.data.frame(predict(dummies, newdata = test_data_x))
df_all_combined <- cbind(test_data_x[,-c(which(colnames(test_data_x) %in% ohe_feats))], df_all_ohe)
colnames(df_all_combined)[1] <- "id"

# split train and test
X_train = df_all_combined[df_all_combined$id %in% train$id,]
y = as.numeric(train$country_destination_code)
X_test = df_all_combined[df_all_combined$id %in% test$id,]


# Splitting the dataset into the Training set and Test set
set.seed(123)
# 抽取三分之二的数据作为train，三分之一的作为test
num = length(X_train[,1]) 
# 213451
X_training = sample(1:num,(num/3)*2)
X_training_data = X_train[X_training,]
X_testing_data = X[-X_training,]

y_training = train[train$id %in% X_training_data$id,]
y_training_label = as.numeric(y_training$country_destination_code)

xgb <- xgboost(data = data.matrix(X_training_data[,-1]), 
               label = y_training_label, 
               eta = 0.1,
               max.depth = 15,
               nround = 25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3)

# [1]	train-merror:0.418271 
# [2]	train-merror:0.418250 
# [3]	train-merror:0.418243 
# [4]	train-merror:0.418222 
# [5]	train-merror:0.418215 
# [6]	train-merror:0.418222 
# [7]	train-merror:0.418236 
# [8]	train-merror:0.418194 
# [9]	train-merror:0.418194 
# [10]	train-merror:0.418159 
# [11]	train-merror:0.418117 
# [12]	train-merror:0.418117 
# [13]	train-merror:0.418103 
# [14]	train-merror:0.418039 
# [15]	train-merror:0.418046 
# [16]	train-merror:0.417983 
# [17]	train-merror:0.417899 
# [18]	train-merror:0.417906 
# [19]	train-merror:0.417793 
# [20]	train-merror:0.417681 
# [21]	train-merror:0.417604 
# [22]	train-merror:0.417583 
# [23]	train-merror:0.417442 
# [24]	train-merror:0.417294 
# [25]	train-merror:0.417210 

# 在测试集预测的值
y_pred_xgboost <- predict(xgb, data.matrix(X_testing_data[,-1]))
y_pred_xgboost

# Confusion matrix
library(caret)
# install.packages('e1071',dependencies = TRUE)

y_testing = train[train$id %in% X_testing_data$id,]
y_testing_label = as.factor(y_testing$country_destination_code)

y_pred_xgboost <- as.factor(y_pred_xgboost)

caret::confusionMatrix(y_pred_xgboost, y_testing_label)
# 58.26%

# 进一步尝试找出模型中重要的变量并且缩小变量列表
# 寻找实际的树是什么样子的
model_xgboost_1 <- xgb.dump(xgb,with_stats = T)
model_xgboost_1[1:10] # This statement prints top 10 nodes of the model

# [1] "booster[0]"                                                          
# [2] "0:[f38<0.5] yes=1,no=2,missing=1,gain=0.0229492188,cover=10848.293"  
# [3] "1:leaf=-0.0529542156,cover=10842.4873"                               
# [4] "2:[f16<1.5] yes=3,no=4,missing=3,gain=0.479607046,cover=5.8055563"   
# [5] "3:leaf=-0.0447761193,cover=4.58333397"                               
# [6] "4:leaf=0.0149999997,cover=1.22222233"                                
# [7] "booster[1]"                                                          
# [8] "0:[f40<0.5] yes=1,no=2,missing=1,gain=1.32324219,cover=10896.1123"   
# [9] "1:[f37<0.5] yes=3,no=4,missing=3,gain=0.0620117188,cover=10893.5146" 
# [10] "3:[f52<0.5] yes=7,no=8,missing=7,gain=0.00756835938,cover=10873.3486"

# 获得特征的真实名称
names <- dimnames(data.matrix(X_training_data[,-1]))[[2]]
names

# 计算特征重要性矩阵
importance_matrix <- xgb.importance(names,model = xgb)

# 制图
xgb.plot.importance(importance_matrix[1:10,])
# signup flow, gender.female, signup method.basic, age group.30s, first affiliate tracked.linked, 
# first affiliate tracked.untracked, first browser.Chrome, signup method.facebook, first device type.windows desktop, tfa year.2013

#################################################################
# Predicting the test_t results
y_pred_test_t = predict(model,newdata = test_t, type = "response")
y_pred_test_t
y_pred_test_t <- data.frame(y_pred_test_t)
test_result = cbind(test_t,y_pred_test_t)
