## Example of running the analysis for full dataset on SCF cluster

# Set path of export:
# path <- ""

# Load in useful packages
library(caret)
library(tidyverse)
library(knitr)
library(stringr)
library(ggplot2)
library('ggpubr') # you need to download this for SCF cluster
library(ggpubr)
library(lubridate)
library(dplyr)
library(reshape2)
library(plotly)
library(png)
library(gridExtra)
library(grid)

# Functions
library(aod)
library(e1071)
library(randomForest)


# Load images and functions
source("R/load_data.R")
source("R/utils.R") 

# Load data
all.images <- ImagePreprocess(image.file1, image.file2, image.file3)
all.images <- all.images[, c('exp.label', 'ndai', 'corr', 'df',
                             'x.coord', 'y.coord', 'image')]

# Subsampling
# all.images <- all.images[sample(nrow(all.images), 20000),]

# Split up into train/test and validate
all.images.list <- SplitTrainValidate(all.images, 0.7)
all.images.train <- all.images.list[[1]]
all.images.valid <- all.images.list[[2]]
all.images.train.indx <- all.images.list[[3]]

# Running CV
set.k.fold = 5

rf.CV <- ModelCrossValidation(all.images.train, 
                              k.fold = set.k.fold, 
                              model.name = "randomForest")
svm.CV <- ModelCrossValidation(all.images.train, 
                               k.fold = set.k.fold, 
                               model.name = "SVM")
lr.CV <- ModelCrossValidation(all.images.train, 
                              k.fold = set.k.fold, 
                              model.name = "logistic")

# Collating to make CV accuracy:
accuracies.CV <- data.frame(cbind(rf.CV, svm.CV, lr.CV))
write.csv(accuracies.CV, paste0(path,"extra/results_CV_best_features_scf.csv"))

#######################################################################

# Fit the final model
rf.final <- ModelFinalValidation(all.images.train, all.images.valid,
                                 model.name = "randomForest")
svm.final <- ModelFinalValidation(all.images.train, all.images.valid,
                                  model.name = "SVM")
lr.final <- ModelFinalValidation(all.images.train, all.images.valid,
                                 model.name = "logistic")

# Collating to make final accuracy:
accuracies.final <- data.frame(cbind(rf.final = rf.final$accuracy, 
                                     svm.final = svm.final$accuracy, 
                                     lr.final = lr.final$accuracy))
write.csv(accuracies.final, paste0(path,"extra/results_final_best_features_scf.csv"))

# ROC calculation
rf.tprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateTPR, 
                  rf.final$probabilities, rf.final$truth)
rf.fprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateFPR, 
                  rf.final$probabilities, rf.final$truth)

svm.tprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateTPR, 
                   svm.final$probabilities, svm.final$truth)
svm.fprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateFPR, 
                   svm.final$probabilities, svm.final$truth)

lr.tprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateTPR, 
                  lr.final$probabilities, lr.final$truth)
lr.fprs <- sapply(seq(0, 1, length.out = 1000), FUN = CalculateFPR, 
                  lr.final$probabilities, lr.final$truth)

# Collating to make a csv
roc.final <- data.frame(cbind(rf.tprs, rf.fprs,
                              svm.tprs, svm.fprs,
                              lr.tprs, lr.fprs))
roc.final[1,] <- 1 # Need to adjust (it should all be 1 for first row)
write.csv(roc.final, paste0(path,"extra/roc_best_features_scf.csv"))

# Model AUC calculation
rf.auc <- ModelAUC(rf.final$probabilities, rf.final$truth)
svm.auc <- ModelAUC(svm.final$probabilities, svm.final$truth)
lr.auc <- ModelAUC(lr.final$probabilities, lr.final$truth)

auc.final <- data.frame(cbind(rf.auc, svm.auc, lr.auc))
write.csv(auc.final, paste0(path,"extra/auc_best_features_scf.csv"))

# Plotting
all.images.pred <- cbind(all.images[-all.images.train.indx, ],
                         rf.pred = rf.final$predictions,
                         svm.pred = svm.final$predictions,
                         lr.pred = lr.final$predictions)

# Plot predictions for all images
for(image.num in 1:3){
  # Random Forest prediction
  rf.plot <- all.images.pred %>% 
    filter(image == image.num) %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = factor(rf.pred)),
               size = 1, alpha = 0.5) +
    theme_bw()+
    scale_color_manual(values = c("red","blue")) +
    labs(color="Labels") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 25)) + 
    ggtitle(paste0("RF prediction on Image ", image.num))
  
  # SVM prediction
  svm.plot <- all.images.pred %>% 
    filter(image == image.num) %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = factor(svm.pred)),
               size = 1, alpha = 0.5) +
    theme_bw()+
    scale_color_manual(values = c("red","blue")) +
    labs(color="Labels") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 25)) + 
    ggtitle(paste0("SVM prediction on Image ", image.num))
  
  # Logistic Regression prediction
  lr.plot <- all.images.pred %>% 
    filter(image == image.num) %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = factor(lr.pred)),
               size = 1, alpha = 0.5) +
    theme_bw()+
    scale_color_manual(values = c("red","blue")) +
    labs(color="Labels") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 25)) + 
    ggtitle(paste0("LR prediction on Image ", image.num))
  
  ggarrange(rf.plot, svm.plot, lr.plot, ncol = 3, 
            common.legend = TRUE, legend = "right") + 
  ggsave(paste0(path, "extra/best-prediction-img", image.num, ".png"), 
           scale = 1, width = 20, height = 6)
}

# Plot final prediction for RF
p <- list()
for(image.num in 1:3){
  # Random Forest prediction
  p[[image.num]] <- all.images.pred %>% 
    filter(image == image.num) %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = factor(rf.pred)),
               size = 1, alpha = 0.5) +
    theme_bw()+
    scale_color_manual(values = c("red","blue")) +
    labs(color="Labels") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 25)) + 
    ggtitle(paste0("RF prediction on Image ", image.num))
}
ggarrange(p[[1]], p[[2]], p[[3]], ncol = 3, 
          common.legend = TRUE, legend = "right") + 
  ggsave(paste0(path, "extra/rf-best-final-prediction.png"), 
         scale = 1, width = 20, height = 6)



#################################################
# RF specific scripts

# Plot x, y, classification error for RF
p <- list()
for(image.num in 1:3){
  # Random Forest prediction
  p[[image.num]] <- all.images.pred %>% 
    filter(image == image.num) %>%
    ggplot() +
    geom_point(aes(x = x.coord, y = y.coord, color = (rf.pred == exp.label)),
               size = 1, alpha = 0.5) +
    theme_bw()+
    scale_color_manual(values = c("red","blue")) +
    labs(color="Correct Label") +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=20),
          plot.title = element_text(size = 25)) + 
    ggtitle(paste0("Image ", image.num, " Classification Error")) 
}

ggarrange(p[[1]], p[[2]], p[[3]], ncol = 3, 
          common.legend = TRUE, legend = "right") + 
  ggsave(paste0(path, "extra/rf-best-prediction-mismatch.png"), 
       scale = 1, width = 20, height = 6)


# Plot kernel density for model covariates, separated by properly/mis-classified for RF
df.plot <- ggplot(all.images.pred, aes(x = df, fill = (rf.pred == exp.label) )) +
  geom_density(alpha = 0.25) + ggtitle('KDE of Radiance DF') +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20)) +
  scale_fill_manual(values = c('red', 'blue')) +
  labs(fill = 'Correct Label')

ndai.plot <- ggplot(all.images.pred, aes(x = ndai, fill = (rf.pred == exp.label) )) +
  geom_density(alpha = 0.25) + ggtitle('KDE of NDAI') +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20)) +
  scale_fill_manual(values = c('red', 'blue')) +
  labs(fill = 'Correct Label')

corr.plot <- ggplot(all.images.pred, aes(x = corr, fill = (rf.pred == exp.label) )) +
  geom_density(alpha = 0.25) + ggtitle('KDE of Corr') +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20)) +
  scale_fill_manual(values = c('red', 'blue')) +
  labs(fill = 'Correct Label')

ggarrange(df.plot, ndai.plot, corr.plot, ncol = 3, 
          common.legend = TRUE, legend = "right") + 
  ggsave(paste0(path, "extra/rf-best-KDE.png"), 
         scale = 1, width = 20, height = 6)


# Confusion matrix generation for random forest
confusion.matrix <- confusionMatrix(all.images.pred$rf.pred, 
                                    all.images.pred$exp.label,
                                    positive = '1')

# Export confusion matrix
write.csv(confusion.matrix$table, paste0(path,"extra/rf_best_confusion_scf.csv"))

# Export sensitivity and specificity
rf.sens.spec <- cbind(confusion.matrix$byClass[1], confusion.matrix$byClass[2])
colnames(rf.sens.spec) <- c("Sensitivity", "Specificity")
rownames(rf.sens.spec) <- NULL
write.csv(rf.sens.spec, paste0(path,"extra/rf_best_sens_spec_scf.csv"))
