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

# Subsampling
# all.images <- all.images[sample(nrow(all.images), 20000),]

#############################################
# Subsampling analysis to see how stable the parameter importance are:
 
m = 0.2 # subsample proportion
n.subsample = round(m * nrow(all.images))
n.subsample.iter = 100
rf.importance <- list()

for(n.iter in 1:n.subsample.iter){
  # Subsample
  all.images.subsample <- all.images[sample(nrow(all.images), n.subsample),]
  all.images.subsample <- all.images.subsample[, c('exp.label', 'ndai', 'corr', 'df')]
  
  # Determine importance
  rf.model <- randomForest(exp.label ~ ., 
                           data = all.images.subsample, 
                           importance = TRUE)
  rf.importance[[n.iter]] <- cbind(importance(rf.model), N = n.iter)
}
rf.importance <- Reduce(rbind, rf.importance)
write.csv(rf.importance, paste0(path,"extra/rf_feature_stability_scf.csv"))


# Evaluate prediction for unlabeled images
p <- list()
for(image.num in 1:3){
  # Split up into train/test and validate
  all.images.test.indx <- which(all.images$image == image.num)
  all.images.train <- all.images[-all.images.test.indx,]
  all.images.test <- all.images[all.images.test.indx,]
  
  all.images.train <- all.images.train[, c('exp.label', 'ndai', 'corr', 'df')]
  all.images.test <- all.images.test[, c('exp.label', 'ndai', 'corr', 'df')]
  
  # Fit the final model
  rf.final <- ModelFinalValidation(all.images.train, all.images.test,
                                   model.name = "randomForest")
  
  # Plotting
  all.images.pred <- cbind(all.images[all.images.test.indx, ],
                           rf.pred = rf.final$predictions)
  
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
    ggtitle(paste0("Image ", image.num, ": Accuracy = ", round(rf.final$accuracy,5)))
}

ggarrange(p[[1]], p[[2]], p[[3]], ncol = 3, 
          common.legend = TRUE, legend = "right") + 
  ggsave(paste0(path, "extra/rf-best-final-unlabeled-prediction.png"), 
         scale = 1, width = 20, height = 6)