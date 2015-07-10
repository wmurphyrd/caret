predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL, ...)
{
  if(!is.null(preProc)) newdata <- predict(preProc, newdata)
  out <- method$predict(modelFit = modelFit,
                        newdata = newdata,
                        submodels = param, ...)
  ##convert to character with classification
  out
}


