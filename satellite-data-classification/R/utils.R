PlotPCs <- function(image.pca, top = 3){
  # function to plot PCs with a bar plot
  
  # Arguments:
  #   - image.pca: PCA output
  #   - top: number of top PCs to see
  # Output:
  #   - pc.plot: ggplot of the principal components
  
  top.vectors <- top # number of top vectors to see
  principal.vectors <- as.matrix(image.pca$rotation)
  pc.row.names <- row.names(principal.vectors)
  row.names(principal.vectors) <- 1:length(pc.row.names)
  principal.vectors <- principal.vectors[,1:top.vectors]
  principal.vectors <- melt(principal.vectors)
  
  pc.plot <- principal.vectors %>%
    ggplot() +
    geom_bar(stat="identity", position = "dodge", aes(x = Var1, y = abs(value), fill = factor(Var2)), size = 1, alpha = 1) +
    theme_bw()+
    labs(fill = "") +
    xlab("Principal Components") +
    ylab("Values") +
    scale_x_discrete(labels = pc.row.names, limits = 1:8) +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 20))
  return(pc.plot)
}

PlotPCProjection <- function(image, image.coords, image.pca, pc){
  # Plotting PC projections
  # Arguments:
  # - image: image file without the coordinates and labels
  # - image.coords: coordinates of the image file
  # - image.pca: the pca fit of the image
  # - pc: the principal component we want to project on
  # Output:
  # - pc.plot: the ggplot of the projection
  projection <- scale(data.matrix(image), image.pca$center,
                      image.pca$scale) %*% image.pca$rotation[,pc]
  projection <- cbind(projection, image.coords)
  
  # PC projection
  pc.plot <- projection %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = projection),
               size = 0.5, alpha = 0.3) +
    scale_colour_gradientn(colours = c("yellow","green","blue")) +
    theme_bw()+
    labs(color="PC") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 20))
  return(pc.plot)
}

SplitTrainValidate <- function(input.data, percent){
  # Splits data into training/test and validating
  # Arguments:
  #   - input.data: desired input data
  #   - percent: percentage of split (for training set)
  # Output:
  #   - output.data: list of training and validation data with 
  #                 indices for training
  
  # Drop unneeded columns for model building
  input.data <- input.data %>%
    select(-c(x.coord, y.coord, image))
  
  # Train/test/validation split
  train.indx <- sample(nrow(input.data), percent * nrow(input.data), replace = FALSE)
  train.set <- input.data[train.indx,]
  valid.set <- input.data[-train.indx,]
  
  # Output the split data
  output.data <- list(train.set, valid.set, train.indx)
  return(output.data)
}

ImagePreprocess <- function(image.file1, image.file2, image.file3){
  # Model pre-processing - combine the 3 data frames and drop the 0s
  # Arguments:
  #   - image.file1: first image
  #   - image.file2: second image
  #   - image.file3: third image
  # Output:
  #   - prediction.accuracies: output of prediction accuracies
  
  # Create new column that corresponds to which image
  image.file1$image = 1
  image.file2$image = 2
  image.file3$image = 3
  
  # Combine the data frames, drop the no labels
  all.images <- do.call("rbind", list(image.file1, image.file2, image.file3))
  all.images <- all.images %>%
    filter(exp.label != 0) %>%
    mutate(exp.label = as.factor(exp.label))
  all.images$exp.label <- droplevels(all.images$exp.label)
  
  return(all.images)
}

ModelCrossValidation <- function(train.set, k.fold = 10, model.name,
                                 lr.threshold = 0.5){
  # Run cross validation for different models
  # Arguments:
  #   - train.set: the set of training data (which only includes the 
  #               predictor variables)
  #   - k.fold: the number of CV folds we want to use
  #   - model.name: the model we want to fit with
  #   - lr.threshold: threshold for classification 
  #                 (only used for logistic regression)
  # Output:
  #   - prediction.accuracies: output of prediction accuracies
  
  # k-fold CV split
  folds.indx <- sample(rep(1:k.fold, length.out = nrow(train.set)))
  
  # Calculate prediction accuracy for each iteration
  prediction.accuracies <- rep(NA, k.fold)
  
  for (k in 1:k.fold) {
    # Split into train and test
    test.indx <- which(folds.indx == k)
    train.data <- train.set[-test.indx, ]
    test.data <- train.set[test.indx, ]
    
    if (model.name == "randomForest"){ # Fitting with RF
      model <- randomForest(exp.label ~ ., data = train.data)
      preds <- predict(model, test.data)
    } else if (model.name == "SVM"){ # Fitting with SVM
      model <- svm(exp.label ~ ., data = train.data)
      preds <- predict(model, test.data)
    } else {  # Fitting with Logistic
      model <- glm(exp.label ~ ., data = train.data, family = "binomial")
      preds <- predict(model, test.data, type = "response")
      preds <- 2 * as.integer(preds > lr.threshold) - 1 # Responses are -1 or 1
    }
    pred.accuracy <- sum(preds == test.data$exp.label)/(nrow(test.data))
    prediction.accuracies[k] = pred.accuracy
  }
  
  # Output vector of prediction accuracies
  return(prediction.accuracies)
}

ModelFinalValidation <- function(train.set, valid.set, model.name,
                                 lr.threshold = 0.5){
  # Run final validation for different models
  # Arguments:
  #   - train.set: the set of training data (which only includes the 
  #               predictor variables)
  #   - valid.set: the set of validation data (which only includes the
  #               predictor variables)
  #   - lr.threshold: threshold for classification 
  #                 (only used for logistic regression)
  #   - model.name: the model we want to fit with
  # Output:
  #   - valid.output: list of output for predictions, accuracy, 
  #                 probability and truth
  
  # Train desired model on the whole training set
  # Use predictions to calculate prediction accuracy and plot ROC curve
  if (model.name == "randomForest"){ # Fitting with RF
    final.model <- randomForest(exp.label ~ ., data = train.set)
    final.preds <- predict(final.model, valid.set)
    final.probs <- predict(final.model, valid.set, type = "prob")[,"1"]
  } else if (model.name == "SVM"){ # Fitting with SVM
    final.model <- svm(exp.label ~ ., data = train.set, probability = TRUE)
    final.preds <- predict(final.model, valid.set, 
                           decision.values = TRUE, probability = TRUE)
    final.probs <- attr(final.preds, "probabilities")[,"1"]
  } else {  # Fitting with Logistic
    final.model <- glm(exp.label ~ ., data = train.set, family = "binomial")
    final.probs <- predict(final.model, valid.set, type = "response")
    final.preds <- 2 * as.integer(final.probs > lr.threshold) - 1 
    # Responses are -1 or 1
  }
  
  # Calculate the final accuracy
  final.accuracy <- sum(final.preds == valid.set$exp.label) / 
    (nrow(valid.set))
  
  # Labeled validation set expert labels
  truth <- valid.set$exp.label == 1
  
  # Output of relevant validation data
  valid.output <- list(predictions = final.preds, accuracy = final.accuracy,
                       probabilities = final.probs, truth = truth)
  return(valid.output)
}


CalculateTPR <- function(threshhold, preds, truth) {
  # A function to calculate the true positive rate
  # Borrowed and modified from Zoe Vernon's section

  # Arguments:
  #   - threshold: a value above which the prediction is defined to be 1
  #   - preds: a vector of predicted probabilities
  #   - truth: a 0, 1 vector of true classes
  # Returns: 
  #   - tpr: a number between 0 and 1 corresponding to the TPR
  
  tpr <- as.numeric(sum(preds[truth] > threshhold) / sum(truth))
  return(tpr)
}

CalculateFPR <- function(threshold, preds, truth) {
  # A function to calculate the false positive rate
  # Borrowed and modified from Zoe Vernon's section
  
  # Arguments:
  #   - threshold: a value above which the prediction is defined to be 1
  #   - preds: a vector of predicted probabilities
  #   - truth: a 0, 1 vector of true classes
  # Returns: 
  #   - tpr: a number between 0 and 1 corresponding to the FPR
  
  fpr <- as.numeric(sum(preds[!truth] > threshold) / sum(!truth))
  return(fpr)
}

PlotROC <- function(preds, truth) {
  # Print an ROC curve for a fitted model.
  # Arguments:
  #  - preds: numeric predictions on the held out data
  #  - truth: true classifications of the held out data (same length as preds)
  # Output:
  #  - roc.plot: ggplot of ROC curve
  
  # The following code calculates the true positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  tprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateTPR, 
                 preds, truth)
  
  # the following code calculates the false positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  fprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateFPR, 
                 preds, truth)
  
  # plot an ROC curve for the model 
  roc.plot <- ggplot() +
      geom_line(aes(x = fprs, y = tprs), color = "cornflowerblue") +
      geom_abline(aes(slope = 1, intercept = 0)) +
      theme_bw()+
      labs(fill = "") +
      xlab("FPR") +
      ylab("TPR") +
      theme(text = element_text(size=20),
            plot.title = element_text(size = 20))

  return(roc.plot)
}

ModelAUC <- function(preds, truth) {
  # Calculate the AUC for a fitted model.
  # Arguments:
  #  - preds: numeric predictions on the held out data
  #  - truth: true classifications of the held out data (same length as preds)
  # Output:
  #  - auc: the AUC for a fitted model
  
  # First calculate the TPR using each of the true negative (group 0) predicted 
  # probabilities as a threshold
  roc.jumps <-
    sapply(preds[!truth],
           FUN = function(threshold) { 
             CalculateTPR(threshold, preds, truth) 
           })
  # Calculate the average of these false positive rates
  auc <- sum(roc.jumps) / sum(!truth)
  
  return(auc)
}