
##############################
#### .. 2.2 Decision tree ####
##############################

# getModelInfo()$rpart$parameters
# 
# rpartGrid <- expand.grid(cp = c(0.01, 0.03))
# 
# ## cross validation
# control <- trainControl(
#   method  = "repeatedcv",
#   repeats = 3,
#   # number = 3,
#   search  = "grid",
#   allowParallel = TRUE,
#   # sampling = "up",
#   classProbs = TRUE,
#   returnData = FALSE,
#   returnResamp = 'none',
#   savePredictions = "final")
# 
# 
# Fit_tree <- train(x = Train[, VarName],
#                   y = Train$is_churn,
#                   xtest = Validation[validation_idx, VarName],
#                   ytest = Valid_YN$is_churn,
#                   method     = "rpart",
#                   metric     = "Accuracy",
#                   trControl  = control,
#                   tuneGrid   = rpartGrid
#                   # maximize = 'logLoss'
#                   )
# saveRDS(Fit_tree, "Fit_tree.rds")
# save.image("KKBOX_KimYongKyeom.RData")
# Fit_tree
# ggplot(Fit_tree)
# 
# # visualization
# rpart.plot::rpart.plot(Fit_tree$finalModel)
# rpart.plot::rpart.plot(Fit_tree$finalModel, fallen.leaves = F)

library(rpart)
# Training with rpart package
rm(Fit_DT)
Fit_DT <- readRDS("Fit_DT.rds")
Fit_DT <- 
  rpart(is_churn ~ Auto_Renew + Pay_Method + Pay_Plan_Day 
        + Plan_Price + Diff_Price + Trans_PlanDay + Trans_CNT_Mean 
        + ratio_25_diff + avg_ratio_75_up + avg_secs,
        data = Train,
        method = "class",
        xval = 5,        # number of cross-validations.
        control = rpart.control(minsplit = 10)
  )
saveRDS(Fit_DT, "Fit_DT.rds")
summary(Fit_DT)
attributes(Fit_DT)
print(Fit_DT$cptable)
print(Fit_DT)
rpart.plot::rpart.plot(Fit_DT)


#### .... prune: DT Model selection   ####
opt <- which.min(Fit_DT$cptable[, "xerror"])
cp.opt <- Fit_DT$cptable[opt, "CP"]
Fit_DT_Prune <- prune(Fit_DT, cp = cp.opt)
saveRDS(Fit_DT_Prune, "Fit_DT_Prune.rds")

Fit_DT_Prune <- readRDS("Fit_DT_Prune.rds")
rpart.plot::rpart.plot(Fit_DT_Prune)

rm(Fit_DT)

#### .... Validation performance  ####
Pred_Valid_tree <- predict(Fit_DT_Prune,
                           newdata = Validation[validation_idx, ])
Pred_Valid_tree <- as.data.frame(Pred_Valid_tree)
Pred_Valid_tree <- Pred_Valid_tree$Renew


#### .... Validation 예측    ####
Pred_Valid_DT_YN <- ifelse(Pred_Valid_tree >= 0.5, 'Renew', 'Churn')
Pred_Valid_DT_YN <- as.factor(Pred_Valid_DT_YN)

Valid_YN <- Validation[validation_idx, 'is_churn']

confusionMatrix(reference = Valid_YN$is_churn,
                data   = Pred_Valid_Adaboost_YN)



#### .... 최적의 Cut off 지점   ####
Test_YN <- Validation[-validation_idx, 'is_churn']
Pred_DT <- predict(Fit_DT_Prune,
                   newdata = Validation[-validation_idx, ])
Pred_DT <- as.data.frame(Pred_DT)
Pred_DT <- Pred_DT$Renew

preds           <- prediction(Pred_DT, Validation[-validation_idx, 'is_churn'])
perf_graph      <- performance(preds, measure = "tpr", x.measure = "tnr");
perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr"); 
# plot(perf_cut_point)   # ROC Curve
perf_acc_cutoff <- performance(preds, "acc", "cutoff"); 
# plot(perf_acc_cutoff)

opt_mat         <- print(opt.cut(perf_cut_point, preds))
cutoff_DT      <- opt_mat[3, 1]
# 0.6342062


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
legend(.35, .5, c('민감도 sensitivity', '특이도 specificity'),
       lty = c(1, 1), col = 1:2, cex = .9, bty = 'n')



## 최종 예측

Pred_DT_YN <- ifelse(Pred_DT >= 0.5, 'Renew', 'Churn')
Pred_DT_YN <- ifelse(Pred_DT >= cutoff_DT, 'Renew', 'Churn')
Pred_DT_YN <- as.factor(Pred_DT_YN)

confusionMatrix(     data = Pred_DT_YN,
                     reference = Test_YN$is_churn)


#### .... logLoss  ####
Test_DT_Result <- data.frame(Pred_DT,
                             Pred_DT_YN,
                             Validation[-validation_idx, 'is_churn'])
colnames(Test_DT_Result) <- c("Prob", "Pred", "True")

LogLoss_DT <- LogLoss_Fun(Test_DT_Result,
                          Test_DT_Result$Prob,
                          Test_DT_Result$Pred,
                          Test_DT_Result$True)
print(LogLoss_DT)


