
##############################
#### .. 1.4 Random forest ####
##############################

## parallel processing
library(parallel)
library(doParallel)
detectCores()
cl <- makeCluster(6)
registerDoParallel(cl)

## cross validation
control <- trainControl(
  # method  = "cv",
  # repeats = 3,
  # number = 3,
  # search  = "grid",
  allowParallel = TRUE,
  # sampling = "up",
  verboseIter = TRUE,
  classProbs = TRUE,
  returnData = FALSE,
  returnResamp = 'none',
  savePredictions = "final")

## preprocess
preProc <- c("scale",
             "center",
             # "spatialSign",
             "corr",
             "zv")

sapply(Train, function(x) sum(length(which(is.na(x)))))


getModelInfo()$rf$parameters
# rftGrid <- expand.grid(mtry = c(3, 4))

library(pROC)
Fit_RF <- readRDS("Fit_RF.rds")



Train_Down <- Train_Down[, c(VarName, 'is_churn')]

Fit_RF <- readRDS('Fit_RF.rds')
system.time(
  Fit_RF <- train(x = Train_Down[, VarName],
                  y = Train_Down$is_churn,
                  # xtest = Validation[validation_idx, VarName],
                  # ytest = Validation[validation_idx, 'is_churn'],
                  method = "rf",
                  metric = "Accuracy",
                  trControl  = control,
                  preProcess = preProc,
                  # tuneGrid   = rftGrid,
                  ntree = 1000,
                  importance = TRUE,
                  na.action = na.omit,
                  proximity = FALSE,
                  linout = TRUE,
                  strata = 'Auto_Renew',
                  sampsize = NROW(Train_Down) / 3
  )
)
saveRDS(Fit_RF, "Fit_RF.rds")
save.image("KKBOX_KimYongKyeom.RData")

library(randomForest)
system.time(
  Fit_RF <-
    randomForest(is_churn ~ Pay_Method + Pay_Plan_Day + Plan_Price + Diff_Price + Auto_Renew +
                   Trans_CNT_Mean + Trans_PlanDay + ratio_25_diff + avg_ratio_75_up + avg_secs,
                 data = Train_Down,
                 ntree = 10000,
                 mtry = 4,   
                 importance = TRUE,
                 na.action = na.omit,
                 proximity = FALSE,
                 strata = 'Auto_Renew',
                 sampsize = NROW(Train_Down) / 2)
)
saveRDS(Fit_RF, "Fit_RF.rds")


Fit_RF
ggplot(Fit_RF)
Fit_RF$preProcess$method

## Prediction
Pred_Valid_RF <- predict(Fit_RF,
                         newdata = Validation[validation_idx, ],
                         type = 'prob')
## Variable importance 
randomForest::varImpPlot(Fit_RF, type = 2)
varImp(Fit_RF)

## 모형적합 시각화
Err_RF <- Fit_RF$err.rate %>% 
  tbl_df() %>% 
  mutate(index = row_number()) %>% 
  gather(범주, 오류율, -index)

Err_RF %>% 
  ggplot(aes(x = index, y = 오류율, color = 범주)) +
  geom_line() +
  labs(x = "", y = "오류율")

## 5.1. 노드 갯수
treesize(Fit_RF) %>% 
  as_tibble() %>% 
  rename(num_nodes = value) %>% 
  ggplot(aes(x = num_nodes)) +
  geom_histogram(aes(y = ..count..), 
                 fill = "grey", color = "black") + 
  geom_density(aes(y = ..density.. * 150000), color = "black") +
  scale_fill_grey() +
  labs(x = "나무모형 노드 갯수", y = "나무모형 빈도수")


## 모형에 변수가 나타난 횟수
varUsed(Fit_RF) %>% 
  as_tibble() %>% 
  mutate(변수명 = VarName) %>% 
  ggplot(aes(x = 변수명, y = value)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y= "", title = "변수가 나무모형에 포함된 빈도수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


#### .... 최적의 Cut off 지점 찾기   ####
Pred_Test_RF <- 
  predict(Fit_RF, 
          newdata = Validation[-validation_idx, ],
          type = "prob")
Pred_Test_RF <- as.data.frame(Pred_Test_RF)
Pred_Test_RF <- Pred_Test_RF$Renew

Pred_Test_RF                                   # 예측확률
Validation[-validation_idx, "is_churn"]         # 실제

preds           <- prediction(Pred_Test_RF, Validation[-validation_idx, "is_churn"])
perf_graph      <- performance(preds, measure = "tpr", x.measure = "tnr")
perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr")
# plot(perf_cut_point)   # ROC graph
perf_acc_cutoff <- performance(preds, "acc", "cutoff"); 
# plot(perf_acc_cutoff)

plot(perf_graph@alpha.values[[1]], 
     perf_graph@x.values[[1]], 
     type = 'n', xlab = 'cutoff', ylab = 'sensitivity or specificity')
lines(perf_graph@alpha.values[[1]], 
      perf_graph@y.values[[1]], 
      type = 's')                         # 민감도 sensitivity
lines(perf_graph@alpha.values[[1]], 
      perf_graph@x.values[[1]], 
      type = 's', col = 2)                # 특이도 specificity
legend(.1, .4, c('민감도 sensitivity', '특이도 specificity'), 
       lty = c(1, 1), col = 1:2, cex = .9, bty = 'n')

# Optimal Cut-off Point 
opt_mat         <- print(opt.cut(perf_cut_point, preds))
cutoff_RF <- opt_mat[3, 1]
# 0.3650000

#### .... 최종 예측    ####
Pred_Test_RF_YN <- ifelse(Pred_Test_RF >= cutoff_RF, 'Renew', 'Churn')
Pred_Test_RF_YN <- ifelse(Pred_Test_RF >= 0.5, 'Renew', 'Churn')

Pred_Test_RF_YN <- as.factor(Pred_Test_RF_YN)


Test_Result_RF <- data.frame(Pred_Test_RF,
                             Pred_Test_RF_YN,
                             Validation[-validation_idx, 'is_churn'])
colnames(Test_Result_RF) <- c("Prob", "Pred", "True")

confusionMatrix(   data   = Test_Result_RF$Pred, 
                   reference = Test_Result_RF$True)


LogLoss_RF <- LogLoss_Fun(Test_Result_RF,
                          Test_Result_RF$Prob,
                          Test_Result_RF$Pred,
                          Test_Result_RF$True)
print(LogLoss_RF)
# 0.7439936






library(h2o)
## initialize
h2o.init(nthreads = -1) 


# parameter_valid_rf <- for(g in 1:NROW(grid)){
#   for(k in 1:3) {
#     validation_idx <- caret::createDataPartition(c(Train_1702_MST$Auto_Renew), p = 0.15)$Resample1
#     validation <- Train_1702_MST[validation_idx, union(VarName, 'is_churn')]
#     
#     train    <- Train_1702_MST[-validation_idx, union(VarName, 'is_churn')]
#     test_idx <- caret::createDataPartition(c(train$Auto_Renew), p = 0.15)$Resample1
#     
#     test <- train[test_idx, union(VarName, 'is_churn')]
#     train <- train[-test_idx, union(VarName, 'is_churn')]
#     
#     # 모델 훈련
#     m <- h2o.randomForest(x = VarName,
#                           y = 'is_churn',
#                           training_frame   = as.h2o(train),
#                           ntrees = grid[g, "ntree"],
#                           mtries = grid[g, "mtry"])
#     # 예측
#     predicted_valid <- predict(m, newdata = as.h2o(validation))
#     
#     preds           <- prediction(predicted_valid$Renew, validation$is_churn)
#     perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr")
#     opt_mat         <- print(opt.cut(perf_cut_point, preds))
#     cutoff_RF       <- opt_mat[3, 1]
#     
#     predicted <- predict(m, newdata = as.h2o(test))
#     
#     pred_YN <- ifelse(predicted$Renew >= cutoff_RF, 'Renew', 'Churn')
#     
#     confMAT <- caret::confusionMatrix(     data = pred_YN,
#                                            reference = test$is_churn)
#     
#     # 성능 평가
#     return(data.frame(          g = g,
#                                 accuracy = confMAT$overall["Accuracy"],
#                                 specificity = confMAT$byClass["Specificity"],
#                                 sensitivity = confMAT$byClass["Sensitivity"]))
#   }
# }


#### .... With h2o  ####

library(h2o)

## initialize
h2o.init(nthreads = -1) 
# the number of CPUs used. -1 means use all CPUs on the host (Default).
h2o.shutdown(prompt = F)


test       <- Validation[-validation_idx, union(VarName, 'is_churn')]
test$is_churn <- as.factor(test$is_churn)

# 모델 훈련
Fit_RF_1703 <- h2o.randomForest(x = VarName,
                                y = 'is_churn',
                                training_frame    = as.h2o(Train),
                                validation_frame  = as.h2o(Validation[validation_idx, ]),
                                nfolds    = 3,
                                ntrees    = 1000,
                                max_depth = 30,
                                keep_cross_validation_predictions = TRUE,
                                stopping_rounds = 5,
                                stopping_metric = 'AUC')
Fit_RF_1703_Path <- h2o.saveModel(object = Fit_RF_1703, path = getwd(), force = TRUE)
save.image("~/KKBOX_YongKyeom.RData")

h2o.varimp_plot(Fit_RF_1703)
Fit_RF_1703 %>% summary()

h2o.auc(Fit_RF_1703, train = TRUE)
h2o.auc(Fit_RF_1703, valid = TRUE)



# 예측
Fit_RF_1703     <- h2o.loadModel(Fit_RF_1703_Path)
predicted_valid <- predict(Fit_RF_1703_Saved, 
                           newdata = as.h2o(Validation[validation_idx, union(VarName, 'is_churn')]))
predicted_valid <- predicted_valid$Renew %>% as.vector()

preds           <- prediction(predictions = predicted_valid, 
                              labels = Validation[validation_idx, 'is_churn'])
perf_graph      <- performance(preds, measure = "tpr", x.measure = "tnr")
perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr")
# plot(perf_cut_point)
perf_acc_cutoff <- performance(preds, "acc", "cutoff"); 
# plot(perf_acc_cutoff)
opt_mat         <- print(opt.cut(perf_cut_point, preds))
cutoff_RF_1     <- opt_mat[3, 1]
# 0.5261003

# AUC curve
# plot(performance(preds, measure = "tpr", x.measure = "fpr"))
# ACC curve
# plot(performance(preds, "acc", "cutoff"))

plot(perf_graph@alpha.values[[1]], 
     perf_graph@x.values[[1]], 
     type = 'n', xlab = 'cutoff', ylab = 'sensitivity or specificity')
lines(perf_graph@alpha.values[[1]], 
      perf_graph@y.values[[1]], 
      type = 's')                         # 민감도 sensitivity
lines(perf_graph@alpha.values[[1]], 
      perf_graph@x.values[[1]], 
      type = 's', col = 2)                # 특이도 specificity
legend(.2, .4, c('민감도 sensitivity', '특이도 specificity'), 
       lty = c(1, 1), col = 1:2, cex = .9, bty = 'n')



predicted_1703 <- predict(Fit_RF_1703, 
                          newdata = as.h2o(Validation[-validation_idx, union(VarName, 'is_churn')]))
predicted_1703 <- predicted_1703$Renew %>% as.vector()

pred_YN   <- ifelse(predicted_1703 >= cutoff_RF_1, 'Renew', 'Churn')
pred_YN   <- ifelse(predicted_1703 >= 0.5, 'Renew', 'Churn')

Test_Result_RF_1 <- data.frame(predicted_1703,
                               pred_YN,
                               test$is_churn)
colnames(Test_Result_RF_1) <- c("Prob", "Pred", "True")
Test_Result_RF_1$Pred <- as.factor(Test_Result_RF_1$Pred)
Test_Result_RF_1$True <- as.factor(Test_Result_RF_1$True)

confusionMatrix(     data = Test_Result_RF_1$Pred,
                     reference = Test_Result_RF_1$True)


#### .... Logloss  ####
LogLoss_RF <- 
  LogLoss_Fun(Validation[-validation_idx, union(VarName, 'is_churn')], 
              Test_Result_RF_1$Prob, 
              Test_Result_RF_1$Pred, 
              Test_Result_RF_1$True)
print(LogLoss_RF)
# 0.2455738





