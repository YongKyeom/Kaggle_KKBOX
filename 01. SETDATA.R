

#####################
#### Data upload ####
#####################
train    <- as.tibble(fread('~/train.csv'))
train_v2 <- as.tibble(fread('~/train_v2.csv'))

test    <- as.tibble(fread('~/sample_submission_zero.csv'))
test_v2 <- as.tibble(fread('~/sample_submission_v2.csv'))

members <- as.tibble(fread('~/members_v3.csv'))

trans    <- as.tibble(fread('~/transactions.csv'))
trans_v2 <- as.tibble(fread('~/transactions_v2.csv'))

Trans_MST <- rbind(trans, trans_v2)
Trans_MST <- Trans_MST %>% filter(transaction_date > 20161001)

Train_MST <- as.tibble(fread('~/Train_MST.csv'))
Train_v2_MST <- as.tibble(fread('~/Train_v2_MST.csv'))

Train_Total_MST <- as.tibble(fread('~/Train_Total_MST.csv'))

Test_MST <- as.tibble(fread('~/Test_MST.csv'))
Test_v2_MST <- as.tibble(fread('~/Test_v2_MST.csv'))


Trans_1701_MST <- as.tibble(fread('~/Trans_1701_MST.csv'))
Trans_1702_MST <- as.tibble(fread('~/Trans_1702_MST.csv'))
Trans_1703_MST <- as.tibble(fread('~/Trans_1703_MST.csv'))


# logs    <- as.tibble(fread('~/user_logs.csv'))
logs_v2 <- as.tibble(fread('~/user_logs_v2.csv'))


# fwrite(Train_MST, "Train_MST.csv")
# fwrite(Train_v2_MST, "Train_v2_MST.csv")
# fwrite(Test_MST, "Test_MST.csv")
# fwrite(Test_v2_MST, "Test_v2_MST.csv")
# fwrite(Trans_1701_MST, "Trans_1701_MST.csv")
# fwrite(Trans_1702_MST, "Trans_1702_MST.csv")
# fwrite(Trans_1703_MST, "Trans_1703_MST.csv")
# 
# 
# fwrite(Train_Total_MST, "Train_Total_MST.csv")
# 
# fwrite(Train_BackUp, "Train_BackUp.csv")


# rm(Train_MST); rm(Train_v2_MST); 
# rm(Trans_1701_MST); rm(Trans_1702_MST); rm(Trans_1703_MST);
# rm(Train_Total_MST)
# rm(Train_BackUp)
# 
# rm(Trans_MST)
# rm(Fit_Adaboost)
# 
# rm(Train_Down)
# rm(train_v2)
# rm(test_v2)
# rm(trans); rm(trans_v2)
###########################
#### 2. data preprocss ####
###########################

##############################
#### .. 2.1 train / test  ####
##############################
train <- train %>% 
  mutate(is_churn = factor(train$is_churn,
                           levels = c('1', '0'),
                           labels = c('Churn', 'Renew')))

train_v2 <- train_v2 %>% 
  mutate(is_churn = factor(train_v2$is_churn,
                           levels = c('1', '0'),
                           labels = c('Churn', 'Renew')))

test$is_churn <- NA
test_v2$is_churn <- NA


########################
#### .. 2.2 member  ####
########################
members <- members %>% 
  mutate(gender = factor(members$gender,
                         levels = c("female", "male"),
                         labels = c("Female", "Male")),
         registration_init_time = 
           as.Date(paste(substr(members$registration_init_time, 1, 4),
                         substr(members$registration_init_time, 5, 6),
                         substr(members$registration_init_time, 7, 8),
                         sep = "-"),
                   format = '%Y-%m-%d'))

members <- members %>% 
  mutate(reg_init_year = (as.POSIXlt(members$registration_init_time))$year + 1900,
         reg_init_mon = (as.POSIXlt(members$registration_init_time))$mon,
         reg_init_mday = (as.POSIXlt(members$registration_init_time))$mday)

step_count <- c()
step_count <- c(step_count, '1')

##############################
#### .. 2.3 transaction   ####
##############################
trans <- 
  trans %>% 
  mutate(membership_expire_date = 
           as.Date(paste(substr(trans$membership_expire_date, 1, 4),
                         substr(trans$membership_expire_date, 5, 6),
                         substr(trans$membership_expire_date, 7, 8),
                         sep = "-"),
                   format = '%Y-%m-%d'),
         transaction_date = 
           as.Date(paste(substr(trans$transaction_date, 1, 4),
                         substr(trans$transaction_date, 5, 6),
                         substr(trans$transaction_date, 7, 8),
                         sep = "-"),
                   format = '%Y-%m-%d'),
         is_auto_renew = factor(trans$is_auto_renew,
                                levels = c('0', '1'),
                                labels = c('N', 'Y')),
         is_cancel = factor(trans$is_cancel,
                            levels = c('0', '1'),
                            labels = c('N', 'Y')))

trans_v2 <- 
  trans_v2 %>% 
  mutate(membership_expire_date = 
           as.Date(paste(substr(trans_v2$membership_expire_date, 1, 4),
                         substr(trans_v2$membership_expire_date, 5, 6),
                         substr(trans_v2$membership_expire_date, 7, 8),
                         sep = "-"),
                   format = '%Y-%m-%d'),
         transaction_date = 
           as.Date(paste(substr(trans_v2$transaction_date, 1, 4),
                         substr(trans_v2$transaction_date, 5, 6),
                         substr(trans_v2$transaction_date, 7, 8),
                         sep = "-"),
                   format = '%Y-%m-%d'),
         is_auto_renew = factor(trans_v2$is_auto_renew,
                                levels = c('0', '1'),
                                labels = c('N', 'Y')),
         is_cancel = factor(trans_v2$is_cancel,
                            levels = c('0', '1'),
                            labels = c('N', 'Y')))



step_count <- c(step_count, '2')


#######################
#### .. 2.4 logs   ####
#######################










#######################
#### 2. data merge ####
#######################


# train
Train_MST <- 
  train %>% left_join(members,
                      by = 'msno')
rm(train)

Train_MST <- 
  Train_MST %>% 
  mutate(reg_to_today = as.Date('2017-01-31', format = '%Y-%m-%d') - Train_MST$registration_init_time)


# test
Test_MST <- 
  test %>% left_join(members,
                     by = 'msno')
rm(test)

Test_MST <- 
  Test_MST %>% 
  mutate(reg_to_today = as.Date('2017-02-28', format = '%Y-%m-%d') - Test_MST$registration_init_time)


# train_v2
Train_v2_MST <- 
  train_v2 %>% left_join(members,
                         by = 'msno')
rm(train_v2)

Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(reg_to_today = as.Date('2017-02-28', format = '%Y-%m-%d') - Train_v2_MST$registration_init_time)



# test_v2
Test_v2_MST <- 
  test_v2 %>%  left_join(members,
                         by = 'msno')
rm(test_v2)

Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(reg_to_today = as.Date('2017-03-31', format = '%Y-%m-%d') - Test_v2_MST$registration_init_time)
Test_v2_MST %>% NROW(); test_v2 %>% NROW() 

step_count <- c(step_count, '3')

#### ... trans 데이터 통합 ####
intersect(trans, trans_v2)  # NULL

Trans_MST <- rbind(trans, trans_v2)
rm(trans); rm(trans_v2)

most_common_pay_via = function(data){
  as.numeric(sort(table(data), decreasing = TRUE)[1])
}  

Trans_1701_MST <- 
  Trans_MST %>% 
  filter(between(transaction_date, 20170101, 20170131)) %>%
  arrange(desc(msno, transaction_date)) %>% 
  group_by(msno) %>% 
  summarise(Pay_Method = most_common_pay_via(payment_method_id),
            Pay_Plan_Day = mean(payment_plan_days),
            Plan_Price = mean(plan_list_price),
            Diff_Price = mean(plan_list_price) - mean(actual_amount_paid),
            Auto_Renew = first(is_auto_renew),
            Cancel = first(is_cancel),
            Expire_Next_Month = ifelse(first(membership_expire_date) <= '2017-02-28', 'Y', 'N'),
            Trans_CNT = n(),
            Trans_PlanDay = (n()) * mean(payment_plan_days))

Trans_1701_summary <- 
  Trans_MST %>% 
  mutate(Year_Month = substr(Trans_MST$transaction_date, 1, 7)) %>% 
  filter(between(transaction_date, '2016-11-01', '2017-01-31')) %>% 
  group_by(msno) %>% 
  summarise(CNT = n_distinct(Year_Month))

Trans_1701_MST <- 
  Trans_1701_MST %>% 
  left_join(Trans_1701_summary,
            by = 'msno')
Trans_1701_MST %>% NROW(); Trans_1701_MST$msno %>% unique() %>% NROW()
rm(Trans_1701_summary)


Trans_1701_MST <- 
  Trans_1701_MST %>% 
  mutate(Trans_CNT_Mean = Trans_CNT / CNT,      # 한 달에 평균 몇 번 transaction
         Trans_PlanDay = Trans_PlanDay / CNT)    # 한 달 평균 transaction * 평균 기간


Trans_1702_MST <- 
  Trans_MST %>% 
  filter(between(transaction_date, 20170201, 20170228)) %>% 
  arrange(desc(msno, transaction_date)) %>% 
  group_by(msno) %>% 
  summarise(Pay_Method = mean(payment_method_id),
            Pay_Plan_Day = mean(payment_plan_days),
            Plan_Price = mean(plan_list_price),
            Diff_Price = mean(plan_list_price) - mean(actual_amount_paid),
            Auto_Renew = first(is_auto_renew),
            Cancel = first(is_cancel),
            Expire_Next_Month = ifelse(first(membership_expire_date) <= 20170331, 'Y', 'N'),
            Trans_CNT = n(),
            Trans_PlanDay = (n()) * mean(payment_plan_days))
Trans_1702_MST %>% glimpse()

Trans_1702_summary <- 
  Trans_MST %>% 
  mutate(Year_Month = substr(Trans_MST$transaction_date, 1, 7)) %>% 
  filter(between(transaction_date, 20161201, 20170228)) %>% 
  group_by(msno) %>% 
  summarise(CNT = n_distinct(Year_Month))

Trans_1702_MST <- 
  Trans_1702_MST %>% 
  left_join(Trans_1702_summary,
            by = 'msno')
Trans_1702_MST %>% NROW(); Trans_1702_MST$msno %>% unique() %>% NROW()
rm(Trans_1702_summary)

Trans_1702_MST <- 
  Trans_1702_MST %>% 
  mutate(Trans_CNT_Mean = Trans_CNT / CNT,      # 한 달에 평균 몇 번 transaction
         Trans_PlanDay = Trans_PlanDay / CNT)    # 한 달 평균 transaction * 평균 기간

# Trans_1702_MST %>% 
#   filter(Trans_CNT_Mean == 1) %>% NROW() / Trans_1702_MST %>% NROW()


step_count <- c(step_count, '4')


Trans_1703_MST <- 
  Trans_MST %>% 
  filter(between(transaction_date, 20170301, 20170331)) %>% 
  arrange(desc(msno, transaction_date)) %>% 
  group_by(msno) %>% 
  summarise(Pay_Method = mean(payment_method_id),
            Pay_Plan_Day = mean(payment_plan_days),
            Plan_Price = mean(plan_list_price),
            Diff_Price = mean(plan_list_price) - mean(actual_amount_paid),
            Auto_Renew = first(is_auto_renew),
            Cancel = first(is_cancel),
            Expire_Next_Month = ifelse(first(membership_expire_date) <= 20170430, 'Y', 'N'),
            Trans_CNT = n(),
            Trans_PlanDay = (n()) * mean(payment_plan_days))



Trans_1703_summary <- 
  Trans_MST %>% 
  mutate(Year_Month = substr(Trans_MST$transaction_date, 1, 7)) %>% 
  filter(between(transaction_date, 20170101, 20170331)) %>% 
  group_by(msno) %>% 
  summarise(CNT = n_distinct(Year_Month))


Trans_1703_MST <- 
  Trans_1703_MST %>% 
  left_join(Trans_1703_summary,
            by = 'msno')
Trans_1703_MST %>% NROW(); Trans_1703_MST$msno %>% unique() %>% NROW() 
rm(Trans_1703_summary)

Trans_1703_MST <- 
  Trans_1703_MST %>% 
  mutate(Trans_CNT_Mean = Trans_CNT / CNT,      # 한 달에 평균 몇 번 transaction
         Trans_PlanDay = Trans_PlanDay / CNT)    # 한 달 평균 transaction * 평균 기간

rm(Trans_MST)
step_count <- c(step_count, '5')
setdiff(colnames(Train_MST), colnames(Trans_1701_MST))


colnames(Trans_1701_MST)
## Train
Train_MST <- 
  Train_MST %>% 
  left_join(Trans_1701_MST,
            by = 'msno')
Train_MST %>% NROW(); Train_MST$msno %>% unique() %>% NROW() 

## Test
Test_MST <- 
  Test_MST %>% 
  left_join(Trans_1702_MST,
            by = 'msno')
Test_MST %>% NROW(); Test_MST$msno %>% unique() %>% NROW() 


## Train_v2
Train_v2_MST <- 
  Train_v2_MST %>% 
  left_join(Trans_1702_MST,
            by = 'msno')
Train_v2_MST %>% NROW(); Train_v2_MST$msno %>% unique() %>% NROW() 
Trans_1702_MST$msno

## Test_v2
Test_v2_MST <- 
  Test_v2_MST %>% 
  left_join(Trans_1703_MST,
            by = 'msno')
test_v2 %>% NROW(); Test_v2_MST %>% NROW(); Test_v2_MST$msno %>% unique() %>% NROW() 

step_count <- c(step_count, '6')


# fwrite(Trans_1701_MST, "Trans_1701_MST.csv")
# fwrite(Trans_1702_MST, "Trans_1702_MST.csv")
# fwrite(Trans_1703_MST, "Trans_1703_MST.csv")
fwrite(Trans_MST, "Trans_MST.csv")
# rm(Trans_1701_MST); rm(Trans_1702_MST); rm(Trans_1703_MST); rm(Trans_MST)


#####################
#### 3. 파생변수 ####
#####################

Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'Y',
                           ifelse(city == 1, 'Y', 'N')))

Train_v2_MST <-
  Train_v2_MST %>% 
  mutate(Gender_YN = ifelse(is.na(gender), 'N', 'Y'))

Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'N',
                         ifelse(bd <= 0, 'N', 'Y')))

Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13'))))

Train_v2_MST$Auto_Renew <- as.character(Train_v2_MST$Auto_Renew)
Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(Auto_Renew = ifelse(is.na(Auto_Renew), 'X', Auto_Renew))
Train_v2_MST$Auto_Renew <- as.factor(Train_v2_MST$Auto_Renew)



Train_v2_MST$Cancel <- as.character(Train_v2_MST$Cancel)
Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(Cancel = ifelse(is.na(Cancel), 'X', Cancel))
Train_v2_MST$Cancel <- as.factor(Train_v2_MST$Cancel)




step_count <- c(step_count, '7')


Train_MST <- 
  Train_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'Y',
                           ifelse(city == 1, 'Y', 'N')))

Train_MST <-
  Train_MST %>% 
  mutate(Gender_YN = ifelse(is.na(gender), 'N', 'Y'))

Train_MST <- 
  Train_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'N',
                         ifelse(bd <= 0, 'N', 'Y')))

Train_MST <- 
  Train_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13'))))

Train_MST$Auto_Renew <- as.character(Train_MST$Auto_Renew)
Train_MST <- 
  Train_MST %>% 
  mutate(Auto_Renew = ifelse(is.na(Auto_Renew), 'X', Auto_Renew))
Train_MST$Auto_Renew <- as.factor(Train_MST$Auto_Renew)



Train_MST$Cancel <- as.character(Train_MST$Cancel)
Train_MST <- 
  Train_MST %>% 
  mutate(Cancel = ifelse(is.na(Cancel), 'X', Cancel))
Train_MST$Cancel <- as.factor(Train_MST$Cancel)




step_count <- c(step_count, '8')


Test_MST <- 
  Test_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'Y',
                           ifelse(city == 1, 'Y', 'N')))

Test_MST <-
  Test_MST %>% 
  mutate(Gender_YN = ifelse(is.na(gender), 'N', 'Y'))

Test_MST <- 
  Test_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'N',
                         ifelse(bd <= 0, 'N', 'Y')))

Test_MST <- 
  Test_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13'))))

Test_MST$Auto_Renew <- as.character(Test_MST$Auto_Renew)
Test_MST <- 
  Test_MST %>% 
  mutate(Auto_Renew = ifelse(is.na(Auto_Renew), 'X', Auto_Renew))
Test_MST$Auto_Renew <- as.factor(Test_MST$Auto_Renew)

Test_MST$Cancel <- as.character(Test_MST$Cancel)
Test_MST <- 
  Test_MST %>% 
  mutate(Cancel = ifelse(is.na(Cancel), 'X', Cancel))
Test_MST$Cancel <- as.factor(Test_MST$Cancel)






Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(City1_YN = ifelse(is.na(city), 'Y',
                           ifelse(city == 1, 'Y', 'N')))

Test_v2_MST <-
  Test_v2_MST %>% 
  mutate(Gender_YN = ifelse(is.na(gender), 'N', 'Y'))



Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(Age_YN = ifelse(is.na(bd), 'N',
                         ifelse(bd <= 0, 'N', 'Y')))


Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(Reg_Via = ifelse(is.na(registered_via), 'X',
                          ifelse(between(registered_via, 3, 4), 'Reg_3_4', 
                                 ifelse(registered_via == 7, 'Reg_7', 'Reg_9_13'))))


Test_v2_MST$Auto_Renew <- as.character(Test_v2_MST$Auto_Renew)
Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(Auto_Renew = ifelse(is.na(Auto_Renew), 'X', Auto_Renew))
Test_v2_MST$Auto_Renew <- as.factor(Test_v2_MST$Auto_Renew)

Test_v2_MST$Cancel <- as.character(Test_v2_MST$Cancel)
Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(Cancel = ifelse(is.na(Cancel), 'X', Cancel))
Test_v2_MST$Cancel <- as.factor(Test_v2_MST$Cancel)



Train_MST <- 
  Train_MST %>% 
  mutate(Age = ifelse(is.na(bd) | bd < 0, 0, 
                      ifelse(bd > 50, 50, bd)))

Train_v2_MST <- 
  Train_v2_MST %>% 
  mutate(Age = ifelse(is.na(bd) | bd < 0, 0, 
                      ifelse(bd > 50, 50, bd)))


Test_MST <- 
  Test_MST %>% 
  mutate(Age = ifelse(is.na(bd) | bd < 0, 0, 
                      ifelse(bd > 50, 50, bd)))

Test_v2_MST <- 
  Test_v2_MST %>% 
  mutate(Age = ifelse(is.na(bd) | bd < 0, 0, 
                      ifelse(bd > 50, 50, bd)))
step_count <- c(step_count, '9')



save.image("KKBOX_KimYongKyeom.RData")



