##############################
#### Define variable name ####
##############################
VarName <- c('Pay_Method', 
             # 'Age',
             'Gender_YN',
             'Pay_Plan_Day', 
             'Plan_Price',
             'Diff_Price', 
             'Expire_Next_Month',
             'Cancel',
             'Auto_Renew',
             # 'City1_YN',
             # 'Reg_Via',
             # 'CNT',
             'Trans_PlanDay',  'Trans_CNT_Mean', 
             'ratio_25_diff', 'avg_ratio_75_up', 'avg_secs')
Train_MST %>% colnames()
###########################################
#### Train / Validation / Test Set  #######
###########################################
Train$Auto_Renew %>% table()
Train$Auto_Renew %>%  levels()


#### .. 변수별 결측치  ####
sapply(Train_MST, function(y) sum(length(which(is.na(y)))))

Train <- Train_MST[, union(VarName, 'is_churn')]
# sapply(Train, function(y) sum(length(which(is.na(y)))))

Validation <- Test_MST[, union(VarName, c('is_churn', 'msno'))]
# sapply(Validation, function(y) sum(length(which(is.na(y)))))

Train_v2_MST <- as.tibble(fread('~/Train_v2_MST.csv'))
Validation <- Validation %>% inner_join(transmute(Train_v2_MST, msno, is_churn), by = 'msno')
rm(Train_v2_MST)

Validation$is_churn.x <- NULL
Validation$is_churn <- Validation$is_churn.y
Validation$is_churn.y <- NULL
# Validation %>% glimpse()


sapply(Train, function(y) sum(length(which(is.na(y)))))
#### .. 결측치 처리 ####
Train[is.na(Train$Pay_Method), 'Pay_Method'] <- Train$Pay_Method %>% median(na.rm = T)
Train[is.na(Train$Plan_Price), 'Plan_Price'] <- Train$Plan_Price %>% median(na.rm = T)
Train[is.na(Train$Pay_Plan_Day), 'Pay_Plan_Day'] <- Train$Pay_Plan_Day %>% median(na.rm = T)
Train[is.na(Train$Trans_PlanDay), 'Trans_PlanDay'] <- Train$Trans_PlanDay %>% median(na.rm = T)
# Train[is.na(Train$CNT), 'CNT'] <- Train$CNT %>% median(na.rm = T)

Train[is.na(Train$Trans_CNT_Mean), 'Trans_CNT_Mean'] <- Train$Trans_CNT_Mean %>% median(na.rm = T)
# Train[is.na(Train$avg_ratio_25), 'avg_ratio_25'] <- 0
Train[is.na(Train$avg_ratio_75_up), 'avg_ratio_75_up'] <- 0
# Train[is.na(Train$avg_unq), 'avg_unq'] <- 0
Train[is.na(Train$avg_secs), 'avg_secs'] <- 0

Train[is.na(Train$Diff_Price), 'Diff_Price'] <- Train$Diff_Price %>% median(na.rm = T)

# Train <- Train[complete.cases(Train), ]
sapply(Train, function(y) sum(length(which(is.na(y)))))
Train %>% glimpse()


Validation[is.na(Validation$Pay_Method), 'Pay_Method'] <- Validation$Pay_Method %>% median(na.rm = T)
Validation[is.na(Validation$Plan_Price), 'Plan_Price'] <- Validation$Plan_Price %>% median(na.rm = T)
Validation[is.na(Validation$Pay_Plan_Day), 'Pay_Plan_Day'] <- Validation$Pay_Plan_Day %>% median(na.rm = T)
Validation[is.na(Validation$Trans_PlanDay), 'Trans_PlanDay'] <- Validation$Trans_PlanDay %>% median(na.rm = T)
# Validation[is.na(Validation$CNT), 'CNT'] <- Validation$CNT %>% median(na.rm = T)

Validation[is.na(Validation$Trans_CNT_Mean), 'Trans_CNT_Mean'] <- Validation$Trans_CNT_Mean %>% median(na.rm = T)
# Validation[is.na(Validation$avg_ratio_25), 'avg_ratio_25'] <- 0
Validation[is.na(Validation$avg_ratio_75_up), 'avg_ratio_75_up'] <- 0
# Validation[is.na(Validation$avg_unq), 'avg_unq'] <- 0
Validation[is.na(Validation$avg_secs), 'avg_secs'] <- 0

Validation[is.na(Validation$Diff_Price), 'Diff_Price'] <- Validation$Diff_Price %>% median(na.rm = T)

sapply(Validation, function(y) sum(length(which(is.na(y)))))



Train$Gender_YN <- as.factor(Train$Gender_YN)
Train$City1_YN <- as.factor(Train$City1_YN)
Train$Reg_Via <- as.factor(Train$Reg_Via)
Train$is_churn <- as.factor(Train$is_churn)
Train$is_churn <- as.character(Train$is_churn)
Train <- Train %>% mutate(is_churn = ifelse(is_churn == 0, 'Renew', 'Churn'))
Train$is_churn <- as.factor(Train$is_churn)
Train$Auto_Renew <- as.factor(Train$Auto_Renew)
Train$Expire_Next_Month <- as.factor(Train$Expire_Next_Month)
Train$Cancel <- as.factor(Train$Cancel)

Train %>% glimpse()

Validation$Gender_YN <- as.factor(Validation$Gender_YN)
Validation$City1_YN <- as.factor(Validation$City1_YN)
Validation$Reg_Via <- as.factor(Validation$Reg_Via)
Validation$is_churn <- as.factor(Validation$is_churn)
Validation$is_churn <- as.character(Validation$is_churn)
Validation <- Validation %>% mutate(is_churn = ifelse(is_churn == 0, 'Renew', 'Churn'))
Validation$is_churn <- as.factor(Validation$is_churn)
Validation$Auto_Renew <- as.factor(Validation$Auto_Renew)
Validation$Expire_Next_Month <- as.factor(Validation$Expire_Next_Month)
Validation$Cancel <- as.factor(Validation$Cancel)

Validation %>% glimpse()

# Train_BackUp <- Train
# Train <- Train_BackUp
## Upsampling
Train_SUB <- Train %>% filter(is_churn == 'Churn')
Train_Down <- rbind(Train, Train_SUB, Train_SUB)

rm(Train_SUB)
# Train <- upSample(Train, Train$Auto_Renew)   # 967,837 => 2,624,475
Train_Down <- downSample(Train_Down, Train_Down$is_churn)
Train_Down$is_churn %>% table()



###########################
#### 1. caret package  ####
###########################


#### .. With caret package  ####
## parallel processing
library(doParallel)
detectCores()
cl <- makeCluster(7)
registerDoParallel(cl)
stopCluster(cl)

## cross validation
control <- trainControl(
  method  = "repeatedcv",
  repeats = 3,
  # number = 3,
  search  = "grid",
  allowParallel = TRUE,
  sampling = "up",
  classProbs = TRUE,
  returnData = FALSE,
  returnResamp = 'none',
  savePredictions = "final")

## preprocess
preProc <- c("scale",
             "center",
             "corr")

## Validation idx
# validation_idx <- createDataPartition(c(Validation$is_churn), p = 0.3)$Resample1




##############################
#### 2. Model Comparison  ####
##############################

Test_YN <- Train_MST$is_churn

result_matrix <- function(Pred_YN){
  confusionMatrix(      data = Pred_YN, 
                        reference = Test_YN)["table"]}

result_accuracy <- function(Pred_YN){
  (confusionMatrix(     data = Pred_YN, 
                        reference = Test_YN)["overall"] %>% 
     as.data.frame())["Accuracy", ]}

result_specificity <- function(Pred_YN){
  (confusionMatrix(     data = Pred_YN, 
                        reference = Test_YN)["byClass"] %>%
     as.data.frame())["Specificity", ]}

result_sensitivity <- function(Pred_YN){
  (confusionMatrix(     data = Pred_YN, 
                        reference = Test_YN)["byClass"] %>%
     as.data.frame())["Sensitivity", ]}


result_auc <- function(Prob){
  preds <- prediction(Prob, Test_YN)
  performance(preds, "auc")@y.values[[1]]
}





################






#### .. 2.1 Accuracy  ####
Logistic_ACC <- result_accuracy(Pred_Final_Logistic_YN)
DT_ACC       <- result_accuracy(Pred_Valid_DT_YN)
RF_ACC       <- result_accuracy(Pred_Final_RF_YN)
Adaboost_ACC <- result_accuracy(Pred_Final_Adaboost_YN)

#### .. 2.2 Specificity  ####
Logistic_SPE <- result_specificity(Pred_Final_Logistic_YN)
DT_SPE       <- result_specificity(Pred_Valid_DT_YN)
RF_SPE       <- result_specificity(Pred_Final_RF_YN)
Adaboost_SPE <- result_specificity(Pred_Final_Adaboost_YN)


#### .. 2.3 Sensitivity  ####
Logistic_SEN <- result_sensitivity(Pred_Final_Logistic_YN)
DT_SEN       <- result_sensitivity(Pred_Valid_DT_YN)
RF_SEN       <- result_sensitivity(Pred_Final_RF_YN)
Adaboost_SEN <- result_sensitivity(Pred_Final_Adaboost_YN)


#### .. 2.4 AUC  ####
# Logistic_AUC <- result_auc(Pred_Test_Logistic)
# DT_AUC       <- result_auc(Pred_DT)
# RF_AUC       <- result_auc(Pred_Test_RF)
# Adaboost_AUC <- result_auc(Pred_Adaboost)



#### .. 2.5 logLoss  ####
LogLoss_Logistic
LogLoss_DT
LogLoss_RF
LogLoss_Adaboost


#### .. 2.5 Visualization  ####

PRED_PERFORMANCE <- 
  data.frame(Model       = c("Logistic", "Tree", "RF", "Boost"),
             Accuracy    = c(Logistic_ACC, DT_ACC, RF_ACC, Adaboost_ACC),
             Specificity = c(Logistic_SPE, DT_SPE, RF_SPE, Adaboost_SPE),
             Sensitivity = c(Logistic_SEN, DT_SEN, RF_SEN, Adaboost_SEN),
             AUC         = c(Logistic_AUC, DT_AUC, RF_AUC, Adaboost_AUC),
             LogLoss     = c(0.134, 0.324, 0.256, 0.138))


theme_update(plot.title = element_text(hjust = 0.5))

ACC_Plot <- PRED_PERFORMANCE %>% 
  mutate(Accuracy = round(Accuracy, 3)) %>% 
  ggplot(aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(position ="dodge", stat = "identity") +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none')+
  scale_x_discrete(limits = c("Logistic", "Tree", "RF", "Boost")) +
  ggtitle("Accuracy") + labs(x = '', y = '') + 
  geom_text(aes(label = Accuracy), size = 5, hjust = 0.5, vjust = -1, position = "stack") 


SPE_Plot <- PRED_PERFORMANCE %>% 
  mutate(Specificity = round(Specificity, 3)) %>% 
  ggplot(aes(x = Model, y = Specificity, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none')+
  scale_x_discrete(limits = c("Logistic", "Tree", "RF", "Boost")) +
  ggtitle("Specificity") + labs(x = '', y = '') + 
  geom_text(aes(label = Specificity), size = 5, hjust = 0.5, vjust = -1, position = "stack") 


SEN_Plot <- PRED_PERFORMANCE %>% 
  mutate(Sensitivity = round(Sensitivity, 3)) %>% 
  ggplot(aes(x = Model, y = Sensitivity, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none')+
  scale_x_discrete(limits = c("Logistic", "Tree", "RF", "Boost")) +
  ggtitle("Sensitivity") + labs(x = '', y = '') +
  geom_text(aes(label = Sensitivity), size = 5, hjust = 0.5, vjust = -1, position = "stack") 


AUC_Plot <- PRED_PERFORMANCE %>% 
  mutate(AUC = round(AUC, 3)) %>%
  ggplot(aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none')+
  scale_x_discrete(limits = c("Logistic", "Tree", "RF", "Boost")) +
  ggtitle("AUC") + labs(x = '', y = '') +
  geom_text(aes(label = AUC), size = 5, hjust = 0.5, vjust = -1, position = "stack") 


LogLoss_Plot <- PRED_PERFORMANCE %>% 
  mutate(LogLoss = round(LogLoss, 3)) %>%
  ggplot(aes(x = Model, y = LogLoss, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none')+
  scale_x_discrete(limits = c("Logistic", "Tree", "RF", "Boost")) +
  ggtitle("Logloss") + labs(x = '', y = '') +
  geom_text(aes(label = LogLoss), size = 5, hjust = 0.5, vjust = -1, position = "stack") 


Model_Compare_Plot <- gridExtra::grid.arrange(ACC_Plot, SEN_Plot, SPE_Plot, LogLoss_Plot,
                                              ncol = 4)

VarName

