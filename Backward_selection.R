backwards <- function(model,scope){
  nextIter <- TRUE
  while(formula(model) != scope$lower & nextIter){
    nextIter <- FALSE
    modelAIC <- AIC(model)
    vars <- all.vars(formula(model))
    y <- vars[1]
    xPreds <- vars[2:length(vars)]
    previousModel <- model               
    previousModelAIC <- modelAIC
    for(predictor in xPreds){
      newModel <- update(model, as.formula(paste(".~. -",predictor)))
      newModelAIC <- AIC(newModel)
      if (previousModelAIC > newModelAIC){
        previousModel <- newModel
        previousModelAIC <- newModelAIC
        #add a flag 
        nextIter <- TRUE
      }
    }
    model <- previousModel
    modelAIC <- previousModelAIC
  }
  return(model)
}

library(readxl)
credit_data <- data.frame(read_excel("C:/Users/vasir/Downloads/default of credit card clients.xls",col_names=TRUE))

factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
credit_data[factor_vars]<-lapply(credit_data[factor_vars],function(x) as.factor(x))

trainPct <- .8

library('caret')
set.seed(550)
inTrain <- createDataPartition(y = credit_data$default.payment.next.month, p = trainPct, list = FALSE)
autoTrain <- credit_data[inTrain,]
autoTest <- credit_data[-inTrain,]
stopifnot(nrow(autoTrain) + nrow(autoTest) == nrow(credit_data))

form='default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6'
formula<-as.formula(form)
glm.fit<-glm(formula,data=autoTrain,family = binomial(link="logit"))
backwards(glm.fit,scope = list(lower=default.payment.next.month~1,upper=formula))
