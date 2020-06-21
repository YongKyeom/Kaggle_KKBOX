

#### .. With caret package  ####
## parallel processing
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



#####################################
#### .. 1.1 Logistic regression  ####
#####################################


# rm(Fit_Logistic)
Fit_Logistic <- readRDS("Fit_Logistic.rds")
## Fit logistic regression
system.time(
  Fit_Logistic <- train(x = Train[, VarName],
                        y = Train$is_churn,
                        # xtest = Validation[validation_idx, VarName],
                        # ytest = Valid_YN,
                        method = "glm",
                        family = "binomial",
                        metric = "Accuracy",
                        trControl  = control,
                        preProcess = c("zv", "corr")
  )
)
saveRDS(Fit_Logistic, "Fit_Logistic.rds")
Fit_Logistic %>% summary()
Fit_Logistic$preProcess$method

library(h2o)
## initialize
h2o.init(nthreads = -1, max_mem_size = "50G")
# 
# Fit_Logistic <- h2o.glm(x = VarName,
#                         y = 'is_churn',
#                         training_frame    = as.h2o(Train[, union(VarName, 'is_churn')]),
#                         validation_frame  = as.h2o(Validation[validation_idx, union(VarName, 'is_churn')]),
#                         family            = "binomial",
#                         nfolds            = 5)

Pred_Valid_Logistic <- 
  predict(Fit_Logistic, 
          newdata = Validation[validation_idx, ],
          type = "prob")$Renew
# type = "response"일 경우 0~1 사이의 확률 


#### .... 최적의 Cut off 지점 찾기   ####
# ref. https://goo.gl/QGC455
library(ROCR)
library(pROC)

# performance
Pred_Valid_Logistic                                  # 예측확률
Validation[validation_idx, "is_churn"]               # 실제

preds           <- prediction(Pred_Valid_Logistic, Validation[validation_idx, "is_churn"])
perf_graph      <- performance(preds, measure = "tpr", x.measure = "tnr")
perf_cut_point  <- performance(preds, measure = "tpr", x.measure = "fpr"); plot(perf_cut_point)
perf_acc_cutoff <- performance(preds, "acc", "cutoff"); plot(perf_acc_cutoff)

plot(perf_graph@alpha.values[[1]], 
     perf_graph@x.values[[1]], 
     type = 'n', xlab = 'cutoff', ylab = 'sensitivity or specificity')
lines(perf_graph@alpha.values[[1]], 
      perf_graph@y.values[[1]], 
      type = 's')                         # 민감도 sensitivity
lines(perf_graph@alpha.values[[1]], 
      perf_graph@x.values[[1]], 
      type = 's', col = 2)                # 특이도 specificity
legend(.3, .4, c('민감도 sensitivity', '특이도 specificity'), 
       lty = c(1, 1), col = 1:2, cex = .9, bty = 'n')

# Optimal Cut-off Point 
opt.cut <- function(perf, pred){
  cut.ind <- mapply(FUN = function(x, y, p){
    d <- (x - 0)^2 + (y - 1)^2
    ind <- which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1 - x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

opt_mat         <- print(opt.cut(perf_cut_point, preds))
cutoff_Logistic <- opt_mat[3, 1]
# 0.05640359

Fit_Logistic %>% summary()
Validation %>% colnames()


#### .... 최종 예측    ####
Pred_Test_Logistic <- 
  predict(Fit_Logistic, 
          newdata = Validation[-validation_idx, ],
          type = "prob")$Renew

Pred_Test_Logistic_YN <- ifelse(Pred_Test_Logistic >= cutoff_Logistic, 'Renew', 'Churn')
Pred_Test_Logistic_YN <- ifelse(Pred_Test_Logistic >= 0.5, 'Renew', 'Churn')

Pred_Test_Logistic_YN <- as.factor(Pred_Test_Logistic_YN)

Test_Logistic_Result <- data.frame(Pred_Test_Logistic,
                                   Pred_Test_Logistic_YN,
                                   Validation[-validation_idx, 'is_churn'])
colnames(Test_Logistic_Result) <- c("Prob", "Pred", "True")

confusionMatrix(   data   = Test_Logistic_Result$Pred, 
                   reference = Test_Logistic_Result$True)


LogLoss_Fun <- function(data, Pred_prob, Pred, True){
  N <- NROW(data);
  true      <- ifelse(True == 'Renew', 1, 0)
  Pred_prob <- ifelse(Pred_prob == 0, 0.0000000001, Pred_prob)
  
  logloss = -1/N * sum(true * log(Pred_prob), (1 - true) * log(1 - Pred_prob))
  return(logloss)
}

LogLoss_Logistic <- LogLoss_Fun(Test_Logistic_Result, 
                                Test_Logistic_Result$Prob, 
                                Test_Logistic_Result$Pred, 
                                Test_Logistic_Result$True)
print(LogLoss_Logistic)
# 0.7467814

