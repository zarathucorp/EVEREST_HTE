library(ranger)
# set as working directory
train_x_learner_fit <- list(
  xf0 = readRDS("xf0.rds"),
  xf1 = readRDS("xf1.rds"),
  mapping0 = readRDS("mapping0.rds"),
  mapping1 = readRDS("mapping1.rds"),
  yhat0 = readRDS("yhat0.rds"),
  yhat1 = readRDS("yhat1.rds")
)

## Predict score
predict_x_learner_fit <- predict_x_learner(test_imputed[, c(covariates_factor, covariates_numeric)], W = 1, FALSE, TRUE)
covariates_factor <- c('SEX',  "DIABETES", "DIABETES.INSULIN", "DYSLIPIDEMIA", "HTN", "PRIOR.MI", "PRIOR.PCI", "PRIOR.STROKE", "stable.cad", "unstable.cad", "nstemi",
                       "stemi", "MVD.YN", "PCI.LM.YN", "SMOKING", "MVD.PCI.YN")
covariates_numeric <- c( "AGE", "IMPLANTED.STENT.NO", "TOTAL.STENT.LENGTH",  "BASELINE.HEMOGLOBIN", "WBC.COUNT", "BMI", "STENT.LESION.NO")
# w 는 1이던 2던 상관 없음. 
# 하나하나가 score.

++ test_imputed 를 보내주면 -> 가명처리 해서 보내줄 예정
++ score 가 나오면 

# > covariates_factor
#  [1] "SEX"              "DIABETES"         "DIABETES.INSULIN" "DYSLIPIDEMIA"     "HTN"              "PRIOR.MI"         "PRIOR.PCI"        "PRIOR.STROKE"     "stable.cad"       "unstable.cad"     "nstemi"
# [12] "stemi"            "MVD.YN"           "PCI.LM.YN"        "SMOKING"          "MVD.PCI.YN"
# > covariates_numeric
# [1] "AGE"                 "IMPLANTED.STENT.NO"  "TOTAL.STENT.LENGTH"  "BASELINE.HEMOGLOBIN" "WBC.COUNT"           "BMI"                 "STENT.LESION.NO"

## Helper functions
get_oob_predictions <- function(X, forest, mapping) {
  raw_preds <- predict(forest, X, predict.all = TRUE)$predictions
  final_preds <- vector("numeric", length = dim(X)[1])
  inbag_counts <- forest$inbag.counts

  for (i in 1:dim(X)[1]) {
    if (mapping[i] == 0 || i > length(mapping)) {
      final_preds[i] <- mean(raw_preds[i, ])
    } else {
      temp_preds <- vector("list", length = forest$num.trees)
      for (j in 1:forest$num.trees) {
        if (inbag_counts[j][[1]][mapping[i]] == 0) {
          temp_preds[[j]] <- raw_preds[i, j]
        }
      }
      final_preds[i] <- mean(unlist(Filter(is.numeric, temp_preds)))
    }
  }
  return(final_preds)
}

predict_x_learner <- function(X, W, estimate_propensities, predict_oob) {
  if (predict_oob) {
    preds_1 <- get_oob_predictions(X, train_x_learner_fit$xf1, train_x_learner_fit$mapping1)
    preds_0 <- get_oob_predictions(X, train_x_learner_fit$xf0, train_x_learner_fit$mapping0)
  } else {
    preds_1 <- predict(train_x_learner_fit$xf1, X)$predictions
    preds_0 <- predict(train_x_learner_fit$xf0, X)$predictions
  }

  if (estimate_propensities) {
    propf <- ranger(W ~ .,
      data = data.frame(X, W = W),
      min.node.size = 1
    )
    ehat <- propf$predictions
    preds <- (1 - ehat) * preds_1 + ehat * preds_0
  } else {
    preds <- 0.5 * preds_1 + 0.5 * preds_0
  }
  return(preds = preds)
}
