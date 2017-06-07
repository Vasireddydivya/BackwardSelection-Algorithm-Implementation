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