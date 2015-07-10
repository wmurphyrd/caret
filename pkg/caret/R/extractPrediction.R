

extractPrediction <- function(models,
                              testX = NULL,
                              testY = NULL,
                              unkX = NULL,
                              unkOnly = !is.null(unkX) & is.null(testX),
                              verbose = FALSE, ...)
{

  objectNames <- names(models)
  if(is.null(objectNames)) objectNames <- paste("Object", 1:length(models), sep = "")

  if(!unkOnly) {
    trainX <- models[[1]]$trainingData[,!(colnames(models[[1]]$trainingData) %in% ".outcome"), drop = FALSE]
    trainY <- models[[1]]$trainingData$.outcome
  }
  obsLevels <- levels(models[[1]])

  if(verbose)
  {
    cat("Number of training samples:", length(trainY), "\n")
    cat("Number of test samples:    ", length(testY), "\n\n")
  }

  pred <- obs <- modelName <- dataType <- objName <- NULL
  if(!is.null(testX))
  {
    #if(!is.data.frame(testX)) testX <- as.data.frame(testX)
    hasNa <- apply(testX, 1, function(data) any(is.na(data)))
    if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n")
  }

  for(i in seq(along = models))
  {
    if(!unkOnly)
    {
      tempTrainPred <- predictionFunction(models[[i]]$modelInfo,
                                          models[[i]]$finalModel,
                                          trainX,
                                          models[[i]]$preProcess, ...)
      if(verbose) cat(models[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n")
      #convert factors and shape into matrices
      if(is.list(tempTrainPred)) tempTrainPred <-
          sapply(tempTrainPred, function(x) {if(is.factor(x)) as.character(x) else x})
      if(is.null(dim(tempTrainPred))) tempTrainPred <- matrix(tempTrainPred, ncol = 1)

      if(models[[i]]$modelType == "Classification")
      {
        pred <- rbind(pred, tempTrainPred)
        obs <- c(obs, as.character(trainY))
      } else {

        tempTrainPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                         bounds =  models[[i]]$control$predictionBounds,
                                         limits =  models[[i]]$yLimit,
                                         pred = tempTrainPred)
        pred <- rbind(pred, tempTrainPred)
        obs <- c(obs, trainY)
      }

      modelName <- c(modelName, rep(models[[i]]$method, nrow(tempTrainPred)))
      objName <- c(objName, rep(objectNames[[i]], nrow(tempTrainPred)))
      dataType <- c(dataType, rep("Training", nrow(tempTrainPred)))

      if(!is.null(testX) & !is.null(testY))
      {
        if(any(colnames(testX) == ".outcome"))
          testX <- testX[, colnames(testX) != ".outcome", drop = FALSE]

        tempTestPred <- predictionFunction(models[[i]]$modelInfo,
                                           models[[i]]$finalModel,
                                           testX,
                                           models[[i]]$preProcess, ...)
        if(verbose) cat(models[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")
        #convert factors and shape into matrices
        if(is.list(tempTestPred)) tempTestPred <-
          sapply(tempTestPred, function(x) {if(is.factor(x)) as.character(x) else x})
        if(is.null(dim(tempTestPred))) tempTestPred <- matrix(tempTestPred, ncol = 1)

        if(models[[i]]$modelType == "Classification")
        {
          pred <- rbind(pred, tempTestPred)
          obs <- c(obs, as.character(testY))
        } else {
          tempTestPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                          bounds =  models[[i]]$control$predictionBounds,
                                          limits =  models[[i]]$yLimit,
                                          pred = tempTestPred)
          pred <- rbind(pred, tempTestPred)
          obs <- c(obs, testY)
        }

        modelName <- c(modelName, rep(models[[i]]$method, nrow(tempTestPred)))
        objName <- c(objName, rep(objectNames[[i]], nrow(tempTestPred)))
        dataType <- c(dataType, rep("Test", nrow(tempTestPred)))

      }
      if(verbose) cat("\n")
    }
    if(!is.null(unkX))
    {
      if(any(colnames(unkX) == ".outcome"))
        unkX <- unkX[, colnames(unkX) != ".outcome", drop = FALSE]
      tempUnkPred <- predictionFunction(models[[i]]$modelInfo,
                                        models[[i]]$finalModel,
                                        unkX,
                                        models[[i]]$preProcess, ...)
      if(verbose) cat(models[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")
      #convert factors and shape into matrices
      if(is.list(tempUnkPred)) tempUnkPred <-
        sapply(tempUnkPred, function(x) {if(is.factor(x)) as.character(x) else x})
      if(is.null(dim(tempUnkPred))) tempUnkPred <- matrix(tempUnkPred, ncol = 1)


      if(models[[i]]$modelType == "Classification")
      {
        pred <- rbind(pred, tempUnkPred)
        obs <- c(obs, rep("", nrow(tempUnkPred)))
      } else {
        tempUnkPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                       bounds =  models[[i]]$control$predictionBounds,
                                       limits =  models[[i]]$yLimit,
                                       pred = tempUnkPred)
        pred <- rbind(pred, tempUnkPred)
        obs <- c(obs, rep(NA, nrow(tempUnkPred)))
      }

      modelName <- c(modelName, rep(models[[i]]$method, nrow(tempUnkPred)))
      objName <- c(objName, rep(objectNames[[i]], nrow(tempUnkPred)))
      dataType <- c(dataType, rep("Unknown", nrow(tempUnkPred)))

    }
    if(verbose) cat("\n")
  }

  rownames(pred) <- NULL
  if(models[[1]]$modelType == "Classification")
  {
    pred <- lapply(as.data.frame(pred, check.names=FALSE, stringsAsFactors=FALSE),
                   factor, levels=obsLevels)
    obs <- factor(obs, levels = obsLevels)
    out <- data.frame(obs = obs,
                      pred = pred[[1]],
                      model = modelName,
                      dataType = dataType,
                      object = objName)
    #add any remaining prediction columns and prepend names with "pred."
    if(length(pred)>1) {
      if(is.null(names(pred))) names(pred) <- as.character(seq_len(length(pred)))
      out <- cbind(out, pred=pred[-1])
    }
  } else {
    out <- data.frame(obs = obs,
                      pred = pred[ , 1],
                      model = modelName,
                      dataType = dataType,
                      object = objName)
    #add remaining prediction columns and prepend names with "pred."
    if(ncol(pred) > 1) out <- cbind(out, pred=pred[ , -1])
  }
  out
}

#modified to handle pred as a matrix for when intervals are included
#casting the apply results as.matrix prevents length 1 inputs from becoming vectors
trimPredictions <- function(pred, mod_type, bounds, limits) {
  if(mod_type == "Regression" && is.logical(bounds) && any(bounds)) {
    if(bounds[1]) pred <-
        as.matrix(apply(pred, 2, function(x) ifelse(x < limits[1], limits[1], x)))
    if(bounds[2]) pred <-
        as.matrix(apply(pred, 2, function(x) ifelse(x > limits[2], limits[2], x)))
  }
  if(mod_type == "Regression" && is.numeric(bounds) && any(!is.na(bounds))) {
    if(!is.na(bounds[1])) pred <-
        as.matrix(apply(pred, 2, function(x) ifelse(x < bounds[1], bounds[1], x)))
    if(!is.na(bounds[2])) pred <-
        as.matrix(apply(pred, 2, function(x) ifelse(x > bounds[2], bounds[2], x)))
  }
  pred
}
