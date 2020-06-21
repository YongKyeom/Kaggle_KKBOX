
######################################
#### .. 1.3 Adaboost (Boosting)   ####
######################################

# modelLookup("xgbTree")
# 
# xgbGrid <- expand.grid(
#   nrounds = 1000,
#   max_depth = c(6, 8, 10),
#   eta = c(0.001, 0.005, 0.01),
#   gamma = 1,
#   colsample_bytree = 0.5,
#   min_child_weight = 6
# )
# 
# xgbTrControl <- trainControl(
#   method = "repeatedcv",
#   number = 3,
#   # repeats = 3,
#   allowParallel = TRUE,
#   sampling = "up",
#   classProbs = TRUE,
#   returnData = FALSE,
#   returnResamp = 'none',
#   savePredictions = "final"
#   )
# 
# xgbTrain <- train(
#   x = Train[, VarName],
#   y = Train$is_churn,
#   xtest = Validation[validation_idx, VarName],
#   ytest = Validation[validation_idx, 'is_churn'],
#   trControl = xgbTrControl,
#   tuneGrid = xgbGrid,
#   method = "xgbTree",
#   metric = "Accuracy"
# )



VarName
library(gbm)
Train$is_churn2 <- ifelse(Train$is_churn == 'Renew', 0, 1)
Train$is_churn2 %>% table(); Train$is_churn2 %>% class()

library(gbm)

Fit_Adaboost <- readRDS('Fit_Adaboost_10000.rds')
system.time(
  Fit_Adaboost <- gbm(is_churn2 ~ Auto_Renew + Pay_Method + Pay_Plan_Day 
                      + Plan_Price + Diff_Price + Trans_PlanDay + Trans_CNT_Mean 
                      + ratio_25_diff + avg_ratio_75_up + avg_secs,
                      data = Train,
                      distribution = "adaboost",
                      # cv.folds = 3,
                      keep.data = FALSE,
                      n.trees = 500,
                      shrinkage = 0.01,
                      n.cores = 8,
                      verbose = TRUE)
)
rm(dtrain)
Fit_Adaboost %>% summary()

Ada_best.iter <- gbm.perf(Fit_Adaboost, method = "cv")
#  개 나무를 만드는게 Best
print(Fit_Adaboost)
Fit_Adaboost %>% summary()

# 분류확률
Pred_Valid_Adaboost <-
  predict(Fit_Adaboost,
          newdata = Validation[validation_idx, ],
          type = "response",
          # n.trees = Ada_best.iter,
          n.trees = 1000
  )


#### .... Validation 예측    ####
Pred_Valid_Adaboost_YN <- ifelse(Pred_Valid_Adaboost >= 0.5, 'Churn', 'Renew')
Pred_Valid_Adaboost_YN <- as.factor(Pred_Valid_Adaboost_YN)

Valid_YN <- Validation[validation_idx, 'is_churn']

confusionMatrix(reference = Valid_YN$is_churn,
                data   = Pred_Valid_Adaboost_YN)

LogLoss_Fun(Valid_YN,
            1 - Pred_Valid_Adaboost,
            Pred_Valid_Adaboost_YN,
            Valid_YN)
# 0.2489925

Pred_Adaboost %>% NROW()
#### .... 최적의 Cut off 지점   ####
Pred_Adaboost <-
  predict(Fit_Adaboost,
          newdata = Validation[-validation_idx, ],
          type = "response",
          n.trees = Ada_best.iter
  )

preds           <- prediction(Pred_Adaboost, Validation[-validation_idx, 'is_churn'])
perf_graph      <- performance(preds, measure = "tpr", x.measure = "tnr");
perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr"); 
# plot(perf_cut_point)  # ROC Curve
perf_acc_cutoff <- performance(preds, "acc", "cutoff"); 
# plot(perf_acc_cutoff)
# perf_cut_point@alpha.values <- list(c(0.1, 0.5134238 ,0.1701300))

opt_mat         <- print(opt.cut(perf_cut_point, preds))
cutoff_Adaboost <- opt_mat[3, 1]
# 0.3200304

# 민감도 & 특이도 그래프
plot(perf_graph@alpha.values[[1]],
     perf_graph@x.values[[1]],
     type='n', xlab='cutoff', ylab='sensitivity or specificity')
lines(perf_graph@alpha.values[[1]],
      perf_graph@y.values[[1]],
      type = 's')                         # 민감도 sensitivity
lines(perf_graph@alpha.values[[1]],
      perf_graph@x.values[[1]],
      type = 's', col = 2)                # 특이도 specificity
legend(.4, .8, c('민감도 sensitivity', '특이도 specificity'),
       lty = c(1, 1), col = 1:2, cex = .9, bty = 'n')


#### .... 최종 예측    ####
Pred_Adaboost_YN <- ifelse(Pred_Adaboost >= cutoff_Adaboost, 'Churn', 'Renew')
Pred_Adaboost_YN <- ifelse(Pred_Adaboost >= 0.5, 'Churn', 'Renew')
Pred_Adaboost_YN <- as.factor(Pred_Adaboost_YN)

confusionMatrix(reference = Test_YN$is_churn,
                data   = Pred_Adaboost_YN)

#### .... logLoss  ####
Test_Adaboost_Result <- data.frame(Pred_Adaboost,
                                   Pred_Adaboost_YN,
                                   Test_YN$is_churn)
colnames(Test_Adaboost_Result) <- c("Prob", "Pred", "True")

LogLoss_Adaboost <- LogLoss_Fun(Test_Adaboost_Result,
                                1 - Test_Adaboost_Result$Prob,
                                Test_Adaboost_Result$Pred,
                                Test_Adaboost_Result$True)
print(LogLoss_Adaboost)









