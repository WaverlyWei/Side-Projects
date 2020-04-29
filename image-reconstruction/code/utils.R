## utils.R contains all the auxilliary functions 

SplitTV <- function(data, train_percent){
  # Splits data into training, testing and validation
  # Arguments:
  #   - data: input data
  #   - train_percent: percentage of training set 
  # Output:
  #   - output: list of training, testing, validation data
  
  # split training data
  train.idx <- sample(nrow(data), train_percent * nrow(data), replace = FALSE)
  train.set <- data[train.idx,]
  
  # split the rest of the data into testing/validation
  rest.data <- data[-train.idx,]
  
  # testing/validation both 20%, so each takes 50% of the rest of the data
  test.idx <- sample(nrow(rest.data), 0.5 * nrow(rest.data), replace = FALSE)
  test.set <- rest.data[test.idx,]
  val.set <- rest.data[-test.idx,]
  
  # output the split data
  output <- list(train.set, test.set, val.set)
  return(output)
}


ModSelect <- function(X.train, Y.train,X.val,Y.val,Model, Criteria){
  # Run models with corresponding selection criteria
  # Arguments:
  #   - X.train: training features
  #   - Y.train: training repsonse
  #   - X.val: validation features
  #   - Y.val: validation response
  #   - Model: lasso,ridge
  #   - Criteria: CV, ESCV, AIC, AICC, BIC
  # Output:
  #   - output: list of correlation, lambda for each 
  #             model-criteria pair. beta, number of 
  #             features are optional return values 
  
  if (Model == "lasso"){
    if (Criteria == "CV"){
      # select lambda using cv
      cv_fit <- cv.glmnet(X.train,Y.train)
      # run lasso with selected lambda
      lasso_fit <- glmnet(X.train,Y.train,lambda = cv_fit$lambda.min )
      # do prediction on validation set 
      lasso_pred <- predict.glmnet(lasso_fit,newx = X.val)
      # get correlation measure 
      lasso_cv_cor <- cor(lasso_pred,Y.val)
      # return lambda 
      lasso_cv_lambda <- cv_fit$lambda.min
      # return number of selected features 
      lasso_cv_var <- cv_fit$nzero
      # point check 
      cat("CV")
      return(list(lasso_cv_cor,lasso_cv_lambda,lasso_cv_var))
      
      }else if(Criteria == "ESCV"){
        # select lambda using escv
        escv_fit <- escv.glmnet(X.train,Y.train)
        # using selected lambda to run lasso 
        lasso_escv_fit <- glmnet(X.train,Y.train,lambda = escv_fit$lambda.escv )
        # predict on validation set 
        lasso_escv_pred <- predict.glmnet(lasso_escv_fit,newx = X.val)
        # get correlation measure 
        lasso_escv_cor <- cor(lasso_escv_pred,Y.val)
        # return escv lambda 
        lasso_escv_lambda <- escv_fit$lambda.escv
        # return betas
        lasso_escv_beta <- lasso_escv_fit$beta
        # point check 
        cat("ESCV")
        return(list(lasso_escv_cor,lasso_escv_lambda,lasso_escv_beta))
        
      }else if(Criteria == "AIC"){
        # get initial fit
        fit <- glmnet(X.train, Y.train) 
        # apply AIC calculation
        tLL <- fit$nulldev - deviance(fit)
        k <- fit$df
        n <- fit$nobs
        AIC <- -tLL+2*k
        # select lambda corresponding to min AIC 
        lasso_aic_lambda <- fit$lambda[which(AIC == min(AIC))]
        
        # run lasso with aic selected lambda
        lasso_aic_fit <- glmnet(X.train,Y.train, lambda = lasso_aic_lambda)
        # predict on validation set 
        lasso_aic_pred <- predict.glmnet(lasso_aic_fit,newx = X.val)
        # get correlation measure 
        lasso_aic_cor <- cor(lasso_aic_pred,Y.val)
        # remove na's 
        lasso_aic_cor <- ifelse(is.na(lasso_aic_cor),0,lasso_aic_cor)
        # point check 
        cat("aic")
        return(list(lasso_aic_cor,lasso_aic_lambda))
        
      }else if(Criteria == "AICc"){
        # initial fit 
        fit <- glmnet(X.train, Y.train) 
        # calculate aicc 
        tLL <- fit$nulldev - deviance(fit)
        k <- fit$df
        n <- fit$nobs
        AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
        # select lambda with min aicc
        lasso_aicc_lambda <- fit$lambda[which(AICc == min(AICc))]
        # run lasso with aicc selected lambda 
        lasso_aicc_fit <- glmnet(X.train,Y.train, lambda = lasso_aicc_lambda)
        # predict on validation set 
        lasso_aicc_pred <- predict.glmnet(lasso_aicc_fit,newx = X.val)
        # get correlation measure 
        lasso_aicc_cor <- cor(lasso_aicc_pred,Y.val)
        # remove na's
        lasso_aicc_cor <- ifelse(is.na(lasso_aicc_cor),0,lasso_aicc_cor)
        # point check 
        cat("AICc")
        return(list(lasso_aicc_cor,lasso_aicc_lambda))
        
      }else {   #BIC 
        # get init fit 
        fit <- glmnet(X.train, Y.train) 
        # calculate bic 
        tLL <- fit$nulldev - deviance(fit)
        k <- fit$df
        n <- fit$nobs
        BIC<-log(n)*k - tLL
        # select lambda corresponding to min bic 
        lasso_bic_lambda <- fit$lambda[which(BIC == sort(BIC)[2])]
        # run lasso with bic selected lambda
        lasso_bic_fit <- glmnet(X.train,Y.train, lambda = lasso_bic_lambda)
        # predict on validation set 
        lasso_bic_pred <- predict.glmnet(lasso_bic_fit,newx = X.val)
        # get correlation measure 
        lasso_bic_cor <- cor(lasso_bic_pred,Y.val)
        # remove na's 
        lasso_bic_cor <- ifelse(is.na(lasso_bic_cor),0,lasso_bic_cor)
        # point check 
        cat("bic")
        return(list(lasso_bic_cor,lasso_bic_lambda))
        }
    
  }else if (Model == "ridge"){
    if (Criteria == "CV"){
      # use cv to select lambda
      ridge_cv <- cv.glmnet(X.train, Y.train, alpha = 0)
      # run ridge with selected lambda 
      ridge_cv_fit <- glmnet(X.train, Y.train, alpha = 0, lambda = ridge_cv$lambda.min)
      # predict on val set 
      ridge_cv_pred <- predict.glmnet(ridge_cv_fit,newx = X.val )
      # get correlation measure 
      ridge_cv_cor <- cor(ridge_cv_pred,Y.val)
      # return lambda 
      ridge_cv_lambda <- ridge_cv$lambda.min
      # point check 
      cat("ridge_cv")
      return(list(ridge_cv_cor,ridge_cv_lambda))
      
    }else if(Criteria == "ESCV"){
      # use escv to select lambda
      ridge_escv <- escv.glmnet(X.train,Y.train,alpha = 0)
      # run ridge with selected lambda 
      ridge_escv_fit <- glmnet(X.train,Y.train,alpha = 0, lambda = ridge_escv$lambda.escv)
      # predict on val set 
      ridge_escv_pred <- predict.glmnet(ridge_escv_fit,newx = X.val)
      # get correlation measure 
      ridge_escv_cor <- cor(ridge_escv_pred,Y.val)
      # return lambda
      ridge_escv_lambda <- ridge_escv$lambda.escv
      # point check 
      cat("ridge_escv")
      return(list(ridge_escv_cor,ridge_escv_lambda))
      
    }else if(Criteria == "AIC"){
      # get init fit 
      fit <- glmnet(X.train, Y.train,alpha = 0) 
      # calculate aic 
      tLL <- fit$nulldev - deviance(fit)
      k <- fit$df
      n <- fit$nobs
      AIC <- -tLL+2*k
      # select lambda corresponding to min aic 
      ridge_aic_lambda <- fit$lambda[which(AIC == min(AIC))]
      # run ridge with aic selected lambda
      ridge_aic_fit <- glmnet(X.train,Y.train, lambda = ridge_aic_lambda,alpha = 0)
      # predict on val set 
      ridge_aic_pred <- predict.glmnet(ridge_aic_fit,newx = X.val)
      ridge_aic_cor <- cor(ridge_aic_pred,Y.val)
      # remove na's
      ridge_aic_cor <- ifelse(is.na(ridge_aic_cor),0,ridge_aic_cor)
      # point check 
      cat("r_aic")
      return(list(ridge_aic_cor,ridge_aic_lambda))
      
      
    }else if(Criteria == "AICc"){
      # get init fit 
      fit <- glmnet(X.train, Y.train,alpha = 0) 
      # calculate aicc
      tLL <- fit$nulldev - deviance(fit)
      k <- fit$df
      n <- fit$nobs
      AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
      # select lambda corresponding to min aic 
      ridge_aicc_lambda <- fit$lambda[which(AICc == min(AICc))]
      # run ridge with aicc selected lambda 
      ridge_aicc_fit <- glmnet(X.train,Y.train, 
                               lambda = ridge_aicc_lambda,alpha = 0)
      ridge_aicc_pred <- predict.glmnet(ridge_aicc_fit,newx = X.val)
      ridge_aicc_cor <- cor(ridge_aicc_pred,Y.val)
      # remove na's 
      ridge_aicc_cor <- ifelse(is.na(ridge_aicc_cor),0,ridge_aicc_cor)
      cat("r_AICc")
      return(list(ridge_aicc_cor,ridge_aicc_lambda))
      
    }else{ # RIDGE BIC
      # get init fit 
      fit <- glmnet(X.train, Y.train,alpha = 0) 
      # calculate bic 
      tLL <- fit$nulldev - deviance(fit)
      k <- fit$df
      n <- fit$nobs
      BIC<-log(n)*k - tLL
      # select lambda corresponding to min bic 
      ridge_bic_lambda <- fit$lambda[which(BIC == sort(BIC)[2])]
      # run ridge with bic selected lambda 
      ridge_bic_fit <- glmnet(X.train,Y.train, 
                              lambda = ridge_bic_lambda,alpha = 0)
      
      ridge_bic_pred <- predict.glmnet(ridge_bic_fit,newx = X.val)
      ridge_bic_cor <- cor(ridge_bic_pred,Y.val)
      # remove na's 
      ridge_bic_cor <- ifelse(is.na(ridge_bic_cor),0,ridge_bic_cor)
      # point check 
      cat("r_bic")
      return(list(ridge_bic_cor,ridge_bic_lambda))
    }
  }
}

Transform <- function(dat,Vox){
  # Transform data into appropriate matrix form 
  # to feed into glmnet 
  # Arguments:
  #   - dat: input data
  #   - Vox: voxel name 
  # Output:
  #   - output: transformed data
  
  # tranform into matrix form 
  dat <- matrix(unlist(dat), nrow=nrow(dat), byrow=F)
  # give column names 
  if( ncol(dat) > 1){ # if it's response data
    colnames(dat) <- c(paste0("V", 1:ncol(fit_feat)))
  }else{ # if it's feature data
    colnames(dat) <- Vox
  }
  # return transformed and names data   
  return(dat)
}


Feature <- function(X.train,Y.train,Criteria){
  # Select features corresponding to nonzero beta
  # Arguments:
  #   - X.train: training features
  #   - Y.train: training response
  #   - Criteria: CV, ESCV
  # Output:
  #   - nnzeros: index of features
  # EC-CV LASSO
  if (Criteria == "ESCV"){
  escv_fit <- escv.glmnet(X.train,Y.train)
  lasso_escv_fit <- glmnet(X.train,Y.train,lambda = escv_fit$lambda.escv )
  # return the name of selected variables 
  sparse.mat <- lasso_escv_fit$beta
  }else{
    cv_fit <- cv.glmnet(X.train,Y.train)
    lasso_cv_fit <- glmnet(X.train,Y.train,lambda = cv_fit$lambda.min )
    # return the name of selected variables 
    sparse.mat <- lasso_cv_fit$beta
    cat('CV')
  }
  
  nnzeros <-  which(sparse.mat[,1]!=0)
  return(nnzeros)
  
}


Bootstrap<- function(dat,N){
  # Perform bootstrap 
  # Arguments:
  #   - dat: input data
  #   - N: number of features we want to look at 
  # Output:
  #   - boot.cor: correlation of each boostrap
  #   - beta: list of betas for each voxel 
  #   - boot.feat: name of features 
  boot.feat <- list()
  betas <- list()
  cors <- c()
  for (i in 1: N){

    # feature columns 
    n <- ncol(fit_feat) 
    # Voxel column
    idx <- n + i
    # Voxel name
    Vox <- paste0("Voxel",i)
    
    boot.dat <- dat[sample(nrow(dat),nrow(dat),replace = T),]
    # 0.6 :0.2 :0.2
    split.boot <- SplitTV(boot.dat,0.6)
    boot.train <- split.boot[[1]]
    names(boot.train) <- c(paste0("V", 1:ncol(fit_feat)),paste0("Voxel", 1:ncol(resp_dat)) )
    boot.test <- split.boot[[2]]
    names(boot.test) <- c(paste0("V", 1:ncol(fit_feat)),paste0("Voxel", 1:ncol(resp_dat)) )
    boot.val<- split.boot[[3]]
    names(boot.val) <- c(paste0("V", 1:ncol(fit_feat)),paste0("Voxel", 1:ncol(resp_dat)) )
    
    # get training features 
    X.train <- Transform(boot.train[,1:n],Vox)
    # get ith voxel 
    Y.train <- Transform(data.frame(boot.train[,idx]),Vox)
    
    # get testing features 
    X.test <- Transform(boot.test[,1:n],Vox)
    # get ith voxel 
    Y.test <- Transform(data.frame(boot.test[,idx]),Vox)
    
    # get val features 
    X.val <- Transform(boot.val[,1:n],Vox)
    # get ith voxel 
    Y.val <- Transform(data.frame(boot.val[,idx]),Vox)
    
    boot.pred <- ModSelect(X.train,Y.train,
                          X.test,Y.test,X.val,Y.val,"lasso","ESCV")
    
    boot.cor <- boot.pred[[1]]
    sparse.mat <- boot.pred[[3]]
    nnzeros <-  which(sparse.mat[,1]!=0)
    beta <- sparse.mat[sparse.mat[,1]!=0,]
    boot.feat[[i]] <- nnzeros
    betas[[i]] <- beta
    cors <- c(cors,boot.cor)
  }
  
  return(list(boot.feat,betas,cors))
}


RF <- function(train.set, test.set,idx){
  # Further process escv results and run random forest 
  # for better prediction 
  # Arguments:
  #   - train: trainning data
  #   - test: test data
  #   - idx: voxel index
  # Output:
  #   - output: correlation after randomforest 
  
  # feature columns 
  n <- ncol(fit_feat) 
  Vox <- paste0("Voxel",i)
  
  Y.train <- Transform(data.frame(train.set[,idx]),Vox)
  Y.test<- Transform(data.frame(test.set[,idx]),Vox)
  
  # get training features 
  X.train <- Transform(train.set[,1:n],"Voxel1")
  X.test<- Transform(test.set[,1:n],"Voxel1")
  
  selected <- Feature(X.train,Y.train,Criteria = "ESCV")
  X.train.new <- X.train[,selected]
  X.test.new <- X.test[,selected]
  
  train.rf <- cbind(X.train.new,Y.train)
  test.rf <- cbind(X.test.new,Y.test)
  
  names(train.rf)[ncol(train.rf)] = "Voxel"
  
  rf_fit <- train(Voxel ~ ., 
                  data = train.rf, 
                  method = "ranger")
  
  rf_pred <- predict(rf_fit, X.test.new)
  rf_cor <- cor(rf_pred,Y.test)
  return(rf_cor)
}




# General functions for handling images and Gabor features
RotateImageVector <- function(image.vec) {
  # Rotate a raw image vector into the
  # correct orientation for viewing.
  
  # There are this many rows and columns,
  # and transpose and re-ordering gives it the correct
  # orientation.
  kRows <- 128
  return(t(matrix(image.vec, nrow = kRows)[kRows:1,]))
}


ReadImage <- function(number) {
  # Read a single line from fit <- stim.csv into
  # a matrix that can be plotted as an image.
  #
  # Args:
  #  - number: Which image number to load, starting
  #            with 1.
  #
  # Returns:
  #  - A matrix containing the raw image pixels.
  
  # The first line contains column names, so we always
  # skip at least one line.
  img <- scan("data/fit_stim.csv",
              skip = number, nlines = 1, sep=",")
  
  return(RotateImageVector(img))
  
}

ReadRealBasisFunction <- function(number) { 
  # Read a single column from real <- wav.csv into 
  # a matrix that can be plotted as an image.  This 
  # uses a bash command and may not work for windows users.  
  # 
  # Args: 
  #  - number: Which basis function to load, starting 
  #            with 1. 
  # 
  # Returns: 
  #  - A matrix containing the basis function.  NB: 
  #    I am not 100% sure that the rotation is the same 
  #    for the basis function as for the images. 
  
  # Use bash to read a single column into a text file. 
  # Note that this may not work for windows users. 
  temp.file <- tempfile() 
  system(paste("cat data/real_wav.csv | cut -d, -f ", 
               number, " > ", temp.file)) 
  basis.data <- read.csv(temp.file, header=T)[[1]] 
  return(RotateImageVector(basis.data)) 
} 
