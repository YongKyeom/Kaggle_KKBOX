#################
#### Setting ####
#################
options(stringsAsFactors = FALSE, 
        scipen = 100,        # 100자리까지 표시
        max.print = 999999)   
rm(list = ls(all.names = TRUE))
gc(reset = T)
load("~/KKBOX_KimYongKyeom.RData")

## 시스템 사양 확인
NCmisc::top()$RAM
parallel::detectCores()

rm(Validation_DT)

#########################
#### install library ####
#########################
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")), 
                 dependencies = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
install.packages(c("RCurl","jsonlite"), 
                 dependencies = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))


# install.packages(c("DescTools", "caret", "ggplot2", "gridExtra", "dplyr", "corrplot",
#                    "data.table", "readr", "tibble", "stringr", "tidyr", "DBI", "rJava", "RJDBC", "ROCR", "pROC"), 
#                  dependencies = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
# install.packages(c('lubridate', 'recipes'), dependencies = TRUE)
# devtools::install_version("caret")



######################
#### load Library ####
######################
library <- c("DescTools", "caret", "ggplot2", "gridExtra", "dplyr", "corrplot",
             "data.table", "readr", "tibble", "stringr", "tidyr")
sapply(library, require, character.only = T)

print("==============train==========")
glimpse(train)
print("==============test==========")
glimpse(test)
print("==============members==========")
glimpse(members)
print("==============trans============")
glimpse(trans)
print("==============logs============")
glimpse(logs_v2)


intersect(train$msno, train_v2$msno) %>% NROW()
train    %>% filter(msno =='Yz0LDev6Avq+OdJgnTIfPtm1+5M29vLioX2dFDphecs=')
train_v2 %>% filter(msno =='Yz0LDev6Avq+OdJgnTIfPtm1+5M29vLioX2dFDphecs=')

# is_churn = 0 -> renewal
train %>% left_join(train_v2,
                    by = 'msno') %>% 
  filter(is_churn.x == 1 & is_churn.y == 0)

train %>% left_join(train_v2,
                    by = 'msno') %>% 
  filter(is_churn.x == 0 & is_churn.y == 1)


#############################
#### 1. Data explanation ####
#############################

#### .. 1.1 train ####
train %>% head()
train %>% glimpse()

## chrun = 서비스 제공자를 바꾸는 고객
train$is_churn %>% table() %>% prop.table()

## check NA
apply(train, 2, anyNA)



train_v2 %>% head()
train_v2 %>% glimpse()

## check NA
apply(train_v2, 2, anyNA)

train_v2$is_churn %>% table() %>% prop.table()

train$msno    %>% unique() %>% NROW()   # 992,931
train_v2$msno %>% unique() %>% NROW()   # 970,960

setdiff(train$msno, train_v2$msno) %>% NROW()  # 111,230
setdiff(train_v2$msno, train$msno) %>% NROW()  # 89,259


#### .. 1.2 test ####
test %>%  head()
test %>% glimpse()
## check NA
apply(test, 2, anyNA)

test_v2 %>% head()
test_v2 %>% glimpse()
## check NA
apply(test_v2, 2, anyNA)


#### .. 1.3 members ####
# msno
# city
# bd: age. Note: ranging from -7000 to 2015
# gender
# registered_via: registration method
# registration_init_time: format %Y%m%d

members %>% head()
members %>% glimpse()
# members %>% Desc()
## check NA
apply(members, 2, anyNA)

members %>% NROW()   # 6,769,473
members$msno %>% unique() %>% NROW()  == members %>% NROW()   # TRUE

members$city %>% table()
members$bd %>% summary()
members$bd %>% hist()

members %>% filter(bd < 0) %>% NROW() # 274
members %>% filter(bd == 0) %>% NROW() # 4,540,489
members %>% filter(bd < 1) %>% head(100)
members %>% filter(bd < 0 | bd > 100) %>% NROW() # 5,651

members$gender %>% table(useNA = 'always')
members$gender %>% table() %>% prop.table()


members %>% glimpse()

members$registered_via %>% summary()
members$registered_via %>% hist()
members %>% group_by(registered_via) %>% summarise(CNT = n())

members$registration_init_time %>% summary()



#### .. 1.4 trans ####
# msno: user id
# payment_method_id: payment method
# payment_plan_days: length of membership plan in days
# plan_list_price: in New Taiwan Dollar (NTD)
# actual_amount_paid: in New Taiwan Dollar (NTD)
# is_auto_renew
# transaction_date: format %Y%m%d
# membership_expire_date: format %Y%m%d
# is_cancel: whether or not the user canceled the membership in this transaction.

trans %>% head()
trans %>% glimpse()
View(trans %>% head(100))
trans %>% filter(membership_expire_date >= 20170301 &
                   membership_expire_date <= 20170331) %>% head(100) %>% View()
trans$membership_expire_date %>% summary()

## check NA
apply(trans, 2, anyNA)

trans %>% NROW()                       # 21,547,746
trans$msno %>% unique() %>% NROW()     # 2,363,626
trans$msno %>% unique() %>% NROW() == trans %>% NROW()  # FALSE

trans %>% 
  filter(msno == trans$msno %>% unique() %>% head(1)) %>% 
  View()

trans$payment_method_id %>% hist()
trans$payment_method_id %>% summary()

trans$payment_plan_days %>% hist()
trans$payment_plan_days %>% summary()
trans$payment_plan_days %>% quantile(probs = c(0.5, 0.9, 0.95, 0.99))

trans$is_auto_renew %>% table() %>% prop.table()
trans$is_cancel %>% table() %>% prop.table()

trans %>% 
  filter(is_cancel == 1) %>% head() %>% glimpse()



trans %>% glimpse()
trans_v2 %>% glimpse()

trans$transaction_date %>% summary()

## 2017/02의 data
trans_v2$transaction_date %>% summary()
trans_v2$msno %>% unique() %>% NROW()


#### .. 1.5 logs ####
# msno: user id
# date: format %Y%m%d
# num_25:    # of songs played less than 25% of the song length
#   num_50:  # of songs played between 25% to 50% of the song length
#   num_75:  # of songs played between 50% to 75% of of the song length
#   num_985: # of songs played between 75% to 98.5% of the song length
#   num_100: # of songs played over 98.5% of the song length
#   num_unq: # of unique songs played
#   total_secs: total seconds played

logs_v2 %>% head()
logs_v2 %>% glimpse()
logs_v2$msno %>% unique() %>% NROW() == logs_v2 %>% NROW()   # FALSE
logs_v2$msno %>% unique() %>% NROW()   # 1,103,894 
logs_v2 %>% NROW()                     # 18,396,362

logs_v2$date %>% summary()   # 2017/03/01 ~ 2017/03/31

setdiff(train_v2$msno, logs_v2$msno %>% unique())
train_v2 %>% 
  filter(msno %in% c('k+GvbFdzazOZ19zpN1PrMtYXfSm78TwZpiJiFMOP/oo=',
                     'n0VdABOJR/bdQBZqIKWmQRFTMPiMIEIdBmrsLseemxw=',
                     'SgaZ++LqIBL6zigY92AkoRSmY/sOr9PH51KuemEdfqg='))

train_v2 %>% 
  filter(msno %in% setdiff(train_v2$msno, logs_v2$msno %>% unique())) %>% 
  group_by(is_churn) %>% 
  summarise(CNT = n())

logs_v2 %>% 
  filter(msno == 'u9E91QDTvHLq6NXjEaWv8u4QIqhrHk72kE+w31Gnhdg=') %>% 
  View()



###########################
#### 2. Visualization  ####
###########################

Data_MST %>% glimpse()

Data_MST <- Train_MST
Data_MST$table <- NULL

Data_MST$Age_YN <- as.factor(Data_MST$Age_YN)
Data_MST$Gender_YN <- as.factor(Data_MST$Gender_YN)
Data_MST$City1_YN <- as.factor(Data_MST$City1_YN)
Data_MST$Reg_Via <- as.factor(Data_MST$Reg_Via)
Data_MST$is_churn <- as.factor(Data_MST$is_churn)
Data_MST$is_churn <- as.character(Data_MST$is_churn)
Data_MST <- Data_MST %>% mutate(is_churn = ifelse(is_churn == 0, 'Renew', 'Churn'))
Data_MST$is_churn <- as.factor(Data_MST$is_churn)
Data_MST$Auto_Renew <- as.factor(Data_MST$Auto_Renew)
Data_MST$Expire_Next_Month <- as.factor(Data_MST$Expire_Next_Month)

Data_MST$Cancel <- as.factor(Data_MST$Cancel)
Data_MST$gender <- as.factor(Data_MST$gender)



######################
#### .. 2.1 city  ####
######################


Data_MST %>% 
  filter(!is.na(city)) %>% 
  ggplot(aes(x = city, fill = is_churn)) +
  # geom_bar() +
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.8, 1.0)) +
  scale_fill_grey()

Data_MST$city %>% summary()
Data_MST %>% filter(is.na(city)) %>% NROW()

Data_MST %>% 
  filter(!is.na(city)) %>% 
  mutate(City1_YN = ifelse(city == 1, 'Y', 'N')) %>% 
  ggplot(aes(x = City1_YN, fill = is_churn)) + 
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.8, 1.0)) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  filter(!is.na(city)) %>% 
  mutate(City1_YN = ifelse(city == 1, 'Y', 'N')) %>% 
  group_by(City1_YN, is_churn) %>% 
  summarise(cnt = n())
# A tibble: 4 x 3
# Groups:   City1_YN [?]
# City1_YN is_churn    cnt
# <chr>    <fct>     <int>
# 1 N        Churn     53097
# 2 N        Renew    365272
# 3 Y        Churn     28351
# 4 Y        Renew    414247

# 비율 검정(ratio test)
matrix(c(365272, 53097, 414247, 28351), nrow = 2) %>% chisq.test()  
# p-value < 0.00000000000000022 


Data_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'X',
                           ifelse(city == 1, 'Y', 'N'))) %>% 
  group_by(City1_YN, is_churn) %>% 
  summarise(cnt = n())

Data_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'X',
                           ifelse(city == 1, 'Y', 'N'))) %>% 
  ggplot(aes(x = City1_YN, fill = is_churn)) + 
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.85, 1.0))

Data_MST %>% 
  filter(is.na(city)) %>% 
  NROW() / Data_MST %>% NROW() # 11%

########################
#### .. 2.2 gender  ####
########################


Data_MST %>%
  mutate(gender = as.character(gender)) %>% 
  mutate(gender = ifelse(is.na(gender), 'X', gender)) %>%
  group_by(gender, is_churn) %>% 
  summarise(cnt = n())


Data_MST %>% 
  mutate(Gender = as.character(gender)) %>% 
  # mutate(Gender = ifelse(gender == '', 'X', gender)) %>%
  ggplot(aes(x = Gender, fill = is_churn)) + 
  geom_bar(position = "fill") +
  # geom_bar() +
  coord_cartesian(ylim = c(.8, 1.0)) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))


Data_MST %>% 
  mutate(gender = as.character(gender)) %>% 
  mutate(gender = ifelse(gender == '', 'X', gender)) %>% 
  ggplot(aes(x = gender, fill = is_churn)) + 
  # geom_bar(stat = "count")
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.85, 1.0))   +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))


Data_MST %>% 
  mutate(gender = as.character(gender)) %>% 
  mutate(Gender_YN = ifelse(gender == '', 'N', 'Y')) %>% 
  ggplot(aes(x = Gender_YN, fill = is_churn)) + 
  # geom_bar()
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.85, 1.0)) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))




#########################
#### .. 2.3 age(bd)  ####
#########################

Data_MST$bd %>% hist()
Data_MST$bd %>% summary()

Data_MST %>% 
  filter(!is.na(bd)) %>% 
  ggplot(aes(x = bd, fill = is_churn)) + 
  geom_histogram(position = 'fill')

Data_MST %>% 
  filter(!is.na(bd)) %>% 
  filter(between(bd, 0, 100)) %>% 
  ggplot(aes(x = bd, fill = is_churn)) + 
  geom_histogram() +
  # geom_histogram(position = 'fill') +
  scale_fill_grey() +
  # coord_cartesian(ylim = c(.5, 1.0)) +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))


Data_MST %>% 
  filter(!is.na(bd)) %>% 
  filter(between(bd, 10, 100)) %>% 
  ggplot(aes(x = bd, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  filter(!is.na(bd)) %>% 
  .$bd %>% 
  quantile(probs = c(0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99))

Data_MST %>% 
  filter(is.na(bd)) %>% NROW() / Data_MST %>% NROW()


Data_MST %>% 
  filter(bd == 0) %>% 
  NROW() / Data_MST %>% NROW()

Data_MST %>% 
  filter(bd < 0) %>% 
  NROW() / Data_MST %>% NROW()




Data_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'X',
                         ifelse(bd <= 0, 'X', 'Y'))) %>% 
  group_by(Age_YN, is_churn) %>% 
  summarise(cnt = n())

Data_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'X',
                         ifelse(bd <= 0, 'X', 'Y'))) %>% 
  ggplot(aes(x = Age_YN, fill = is_churn)) + 
  geom_bar(position = "fill") +
  coord_cartesian(ylim = c(.8, 1.0)) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

###########################
#### .. 2.4 regis_via  ####
###########################

Data_MST$registered_via %>% summary()    
Data_MST$registered_via %>% hist()

is.na(Data_MST$registered_via) %>% sum() / Data_MST %>% NROW()

Data_MST %>% 
  mutate(Reg_Via_YN = ifelse(is.na(registered_via), 'N', 'Y')) %>% 
  ggplot(aes(x = Reg_Via_YN, fill = is_churn)) + 
  geom_bar(position = "fill")

Data_MST %>% 
  filter(!is.na(registered_via)) %>%
  ggplot(aes(x = registered_via, fill = is_churn)) + 
  geom_bar(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(.5, 1.0))

Data_MST$registered_via %>% unique()

Data_MST %>% 
  filter(!is.na(registered_via)) %>%
  ggplot(aes(x = registered_via, fill = is_churn)) + 
  geom_bar(stat = "count") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 'Reg_7_9_13'))) %>% 
  ggplot(aes(x = Reg_Via, fill = is_churn)) + 
  geom_bar(stat = "count")

Data_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13')))) %>% 
  ggplot(aes(x = Reg_Via, fill = is_churn)) + 
  geom_bar(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(.7, 1.0))

Data_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13')))) %>% 
  ggplot(aes(x = Reg_Via, fill = is_churn)) + 
  geom_bar(stat = "count")



##############################
#### .. 2.5 reg_to_today  ####
##############################


Data_MST %>% 
  filter(!is.na(reg_to_today)) %>% 
  ggplot(aes(x = reg_to_today, fill = is_churn)) + 
  geom_histogram(position = 'fill')

Data_MST %>% 
  mutate(Reg_YN = ifelse(is.na(reg_to_today), 'N', 'Y')) %>% 
  ggplot(aes(x = Reg_YN, fill = is_churn)) + 
  geom_bar(position = "fill")

Data_MST %>% 
  mutate(Reg_YN = ifelse(is.na(reg_to_today), 'N', 'Y')) %>% 
  group_by(Reg_YN, is_churn) %>% 
  summarise(cnt = n())

# A tibble: 4 x 3
# Groups:   Reg_YN [?]
# Reg_YN is_churn    cnt
# <chr>  <fct>     <int>
# 1 N      Churn      5882
# 2 N      Renew    104111
# 3 Y      Churn     81448
# 4 Y      Renew    779519  

104111 / (104111 + 5882)   # 0.9465239
779519 / (779519 + 81448)  # 0.9053994

# 비율 검정(ratio test)
matrix(c(104111, 5882, 779519, 81448), nrow = 2) %>% chisq.test()  

# p-value < 0.00000000000000022 

Data_MST %>% 
  filter(is.na(reg_to_today)) %>% 
  filter(!is.na(registered_via)) %>% NROW()
# 0





############################
#### .. 2.6 Pay_Method  ####
############################
Data_MST %>% 
  filter(is.na(Pay_Method)) %>% NROW() / Data_MST %>% NROW()
# 결측값 2.5%

Data_MST$Pay_Method %>% summary()
Data_MST %>% 
  filter(!is.na(Pay_Method)) %>% 
  ggplot(aes(x = Pay_Method, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  # geom_histogram() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(.7, 1.0))



##############################
#### .. 2.7 Pay_Plan_Day  ####
##############################

Data_MST %>%
  filter(is.na(Pay_Plan_Day)) %>% NROW() / Data_MST %>% NROW()
# 0.2599489%

Data_MST$Pay_Plan_Day %>% summary()
Data_MST$Pay_Plan_Day %>% hist()
Data_MST %>% 
  filter(!is.na(Pay_Plan_Day)) %>% 
  ggplot(aes(x = Pay_Plan_Day, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 200.0))

Data_MST %>% 
  filter(!is.na(Pay_Plan_Day)) %>% 
  ggplot(aes(x = Pay_Plan_Day, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 100.0))


cor(Data_MST$Pay_Method, Data_MST$Pay_Plan_Day, use = 'complete.obs')

############################
#### .. 2.8 Plan_Price  ####
############################

Data_MST %>%
  filter(is.na(Plan_Price)) %>% NROW() / Data_MST %>% NROW()
# 2.5%

Data_MST$Plan_Price %>% hist()
Data_MST %>% 
  filter(!is.na(Plan_Price)) %>% 
  ggplot(aes(x = Plan_Price, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 300.0))

Data_MST %>% 
  filter(!is.na(Plan_Price)) %>% 
  ggplot(aes(x = Plan_Price, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))




cor(Data_MST$Plan_Price, Data_MST$Pay_Plan_Day, use = 'complete.obs')
cor(Data_MST$Plan_Price, Data_MST$Pay_Method, use = 'complete.obs')

############################
#### .. 2.9 Diff_Price  ####
############################

Data_MST %>% filter(is.na(Diff_Price)) %>% NROW() / Data_MST %>% NROW()

Data_MST$Diff_Price %>% summary()
Data_MST %>% 
  filter(!is.na(Diff_Price)) %>% 
  ggplot(aes(x = Diff_Price, fill = is_churn)) + 
  geom_histogram(binwidth = 10, position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  filter(!is.na(Diff_Price)) %>% 
  ggplot(aes(x = Diff_Price, fill = is_churn)) + 
  geom_histogram(binwidth = 1) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 10.0))

cor(Data_MST$Diff_Price, Data_MST$Pay_Plan_Day, use = 'complete.obs')
cor(Data_MST$Diff_Price, Data_MST$Pay_Method, use = 'complete.obs')
cor(Data_MST$Plan_Price, Data_MST$Diff_Price, use = 'complete.obs')


#############################
#### .. 2.10 Auto_Renew  ####
#############################
Data_MST %>% filter(is.na(Auto_Renew)) %>% NROW() / Data_MST %>% NROW()

Data_MST %>% 
  filter(!is.na(Auto_Renew)) %>% 
  ggplot(aes(x = Auto_Renew, fill = is_churn)) + 
  geom_bar() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  filter(!is.na(Auto_Renew)) %>% 
  ggplot(aes(x = Auto_Renew, fill = is_churn)) + 
  geom_bar(position = 'fill') +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  # filter(!is.na(Auto_Renew)) %>% 
  mutate(Auto_Renew = ifelse(is.na(Auto_Renew), 'X', Auto_Renew)) %>%
  ggplot(aes(x = Auto_Renew, fill = is_churn)) + 
  geom_bar(position = 'fill')


#########################
#### .. 2.11 Cancel  ####
#########################
Data_MST %>% filter(is.na(Cancel)) %>% NROW() / Data_MST %>% NROW()

Data_MST %>% glimpse()

Data_MST %>% 
  # filter(!is.na(Cancel)) %>% 
  mutate(Cancel = ifelse(is.na(Cancel), 'X', Cancel)) %>% 
  ggplot(aes(x = Cancel, fill = is_churn)) + 
  geom_bar(position = 'fill')



####################################
#### .. 2.12 Expire_Next_Month  ####
####################################
Data_MST %>% filter(is.na(Expire_Next_Month)) %>% NROW() / Data_MST %>% NROW()

Data_MST %>% glimpse()

Data_MST %>% 
  mutate(Expire_Next_Month = as.character(Expire_Next_Month)) %>% 
  mutate(Expire_Next_Month = ifelse(Expire_Next_Month == '', 'X', Expire_Next_Month)) %>% 
  ggplot(aes(x = Expire_Next_Month, fill = is_churn)) + 
  geom_bar() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

Data_MST %>% 
  mutate(Expire_Next_Month = as.character(Expire_Next_Month)) %>% 
  mutate(Expire_Next_Month = ifelse(Expire_Next_Month == '', 'X', Expire_Next_Month)) %>% 
  ggplot(aes(x = Expire_Next_Month, fill = is_churn)) + 
  geom_bar(position = 'fill') +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(0.25, 1.0))

Data_MST %>% 
  mutate(Expire_Next_Month = ifelse(is.na(Expire_Next_Month), 'X', Expire_Next_Month)) %>% 
  ggplot(aes(x = Expire_Next_Month, fill = is_churn)) + 
  geom_bar()


############################
#### .. 2.13 Trans_CNT  ####
############################

Data_MST %>% glimpse()
Data_MST$Trans_CNT %>% summary()
Data_MST$Trans_CNT %>% hist()

Data_MST %>% 
  filter(!is.na(Trans_CNT)) %>% 
  ggplot(aes(x = Trans_CNT, fill = is_churn)) + 
  geom_histogram(position = "fill")

Data_MST %>% 
  filter(!is.na(Trans_CNT)) %>% 
  ggplot(aes(x = Trans_CNT, fill = is_churn)) + 
  geom_histogram()



################################
#### .. 2.14 Trans_PlanDay  ####
################################
Data_MST$Trans_PlanDay %>% summary()
Data_MST$Trans_PlanDay %>% hist()
Data_MST %>% 
  filter(!is.na(Trans_PlanDay))%>% 
  .$Trans_PlanDay %>% 
  quantile(probs = c(.1, .9, .95, .99))


Data_MST %>% 
  filter(!is.na(Trans_PlanDay)) %>% 
  ggplot(aes(x = Trans_PlanDay, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 200.0))

Data_MST %>% 
  filter(!is.na(Trans_PlanDay)) %>% 
  ggplot(aes(x = Trans_PlanDay, fill = is_churn)) + 
  geom_histogram(binwidth = 10) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 100.0))


cor(Data_MST$Diff_Price, Data_MST$Trans_PlanDay, use = 'complete.obs')
cor(Data_MST$Trans_PlanDay, Data_MST$Pay_Plan_Day, use = 'complete.obs')
cor(Data_MST$Trans_PlanDay, Data_MST$Pay_Method, use = 'complete.obs')
cor(Data_MST$Plan_Price, Data_MST$Trans_PlanDay, use = 'complete.obs')


Data_MST %>% 
  filter(Trans_PlanDay >= 500) %>% NROW()




######################
#### .. 2.15 CNT  ####
######################

Data_MST$CNT %>% summary()
Data_MST$CNT %>% hist()
Data_MST %>% 
  filter(!is.na(CNT))%>% 
  .$CNT %>% 
  quantile(probs = c(.1, .9, .95, .99))


Data_MST %>% 
  filter(!is.na(CNT)) %>% 
  ggplot(aes(x = CNT, fill = is_churn)) + 
  geom_histogram(position = "fill")

Data_MST %>% 
  filter(!is.na(Trans_PlanDay)) %>% 
  ggplot(aes(x = Trans_PlanDay, fill = is_churn)) + 
  geom_histogram()


#################################
#### .. 2.16 Trans_CNT_Mean  ####
#################################      

Data_MST$Trans_CNT_Mean %>% summary()
Data_MST$Trans_CNT_Mean %>% hist()
Data_MST %>% 
  filter(!is.na(Trans_CNT_Mean))%>% 
  .$Trans_CNT_Mean %>% 
  quantile(probs = c(.1, .9, .95, .99))


Data_MST %>% 
  filter(!is.na(Trans_CNT_Mean)) %>% 
  ggplot(aes(x = Trans_CNT_Mean, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 10.0))


Data_MST %>% 
  filter(!is.na(Trans_CNT_Mean)) %>% 
  ggplot(aes(x = Trans_CNT_Mean, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 10.0))




Data_MST %>% glimpse()



###############################
#### .. 2.17 avg_ratio_25  ####
###############################     


Data_MST %>% 
  filter(!is.na(avg_ratio_25)) %>% 
  ggplot(aes(x = avg_ratio_25, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))


Data_MST %>% 
  filter(!is.na(avg_ratio_25)) %>% 
  ggplot(aes(x = avg_ratio_25, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(0.8, 1.0))


Data_MST %>% 
  filter(!is.na(avg_ratio_25)) %>% 
  ggplot(aes(x = avg_ratio_25, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))





################################
#### .. 2.18 ratio_25_diff  ####
################################     

Data_MST %>% 
  filter(!is.na(ratio_25_diff)) %>% 
  ggplot(aes(x = ratio_25_diff, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(-0.5, 0.5))


Data_MST %>% 
  filter(!is.na(ratio_25_diff)) %>% 
  ggplot(aes(x = ratio_25_diff, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(0.8, 1.0))


Data_MST %>% 
  filter(!is.na(ratio_25_diff)) %>% 
  ggplot(aes(x = ratio_25_diff, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))





##################################
#### .. 2.19 avg_ratio_75_up  ####
##################################     
Data_MST %>% glimpse()

Data_MST %>% 
  filter(!is.na(avg_ratio_75_up)) %>% 
  ggplot(aes(x = avg_ratio_75_up, fill = is_churn)) + 
  geom_histogram() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))


Data_MST %>% 
  filter(!is.na(avg_ratio_75_up)) %>% 
  ggplot(aes(x = avg_ratio_75_up, fill = is_churn)) + 
  geom_histogram(position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(0.85, 1))


Data_MST %>% 
  filter(!is.na(avg_ratio_75_up)) %>% 
  ggplot(aes(x = avg_ratio_75_up, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))




##########################
#### .. 2.20 avg_unq  ####
##########################     
Data_MST %>% glimpse()

Data_MST %>% 
  filter(!is.na(avg_unq)) %>% 
  ggplot(aes(x = avg_unq, fill = is_churn)) + 
  geom_histogram(binwidth = 5) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 200))


Data_MST %>% 
  filter(!is.na(avg_unq)) %>% 
  ggplot(aes(x = avg_unq, fill = is_churn)) + 
  geom_histogram(binwidth = 10, position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 300), ylim = c(0.75, 1))


Data_MST %>% 
  filter(!is.na(avg_ratio_75_up)) %>% 
  ggplot(aes(x = avg_ratio_75_up, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))





############################
#### .. 2.21 avg_secs   ####
############################     
Data_MST %>% glimpse()

Data_MST %>% 
  filter(!is.na(avg_secs)) %>% 
  ggplot(aes(x = avg_secs, fill = is_churn)) + 
  geom_histogram(binwidth = 1000) +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 30000))


Data_MST %>% 
  filter(!is.na(avg_secs)) %>% 
  ggplot(aes(x = avg_secs, fill = is_churn)) + 
  geom_histogram(binwidth = 1000, position = "fill") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0.8, 1))


Data_MST %>% 
  filter(!is.na(avg_secs)) %>% 
  ggplot(aes(x = avg_secs, fill = is_churn)) + 
  geom_density() + 
  scale_fill_grey() +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 20))

