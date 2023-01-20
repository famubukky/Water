install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("e1071")
install.packages('tidyverse')
install.packages("corrplot")
install.packages("DataExplorer")
library(caret)
library(dplyr)
library(ggplot2)
library(e1071)
library(tidyverse)
library(corrplot)
library(DataExplorer)
water_potability <-read.csv("water_potability.csv", header = TRUE)
str(water_potability)
water_potability$Potability <- as.factor(water_potability$Potability) 
#Remove missing values
complete.cases(water_potability)
na_values <- which(!complete.cases(water_potability))
water <- water_potability[-na_values, ]
anyNA(water)
summary(water)

#normalize data
p1<-preProcess(water[,c(1:10)], method=c("center", "scale"))
water1 <- predict(p1, water[,c(1:10)])
summary(water1)
#Perform Eploratory Data Analysis
qplot(water1$Hardness, 
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Hardness", 
      xlab = "ph",  
      fill=I("black"), 
      col=I("yellow"), 
      )
plot_bar(water_potability)
plot_histogram(water)
#Correlation plot
correlations <- cor(water1[,1:9])
corrplot(correlations, 
            method = "square",
                  outline = T,
                  addgrid.col = "darkgray",
                  order="hclust",
                  mar = c(0,0,0,2),
                  addrect = 4,
                  rect.col = "black",
                  rect.lwd = 5,
                  cl.pos = "b",
                  tl.col = "red",
                  tl.cex = 0.5,
                  cl.cex = 0.5)# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(correlations, cutoff = 0.6, verbose = TRUE, names = TRUE)
highlyCorrelated

#sampling
set.seed(777)
water3 <- water1 %>%
  group_by(Potability) %>%
  sample_n(600)
plot_bar(water3)
plot_bar(water_potability)
plot_histogram(water3)
qplot(water1$Turbidity, 
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Turbidity", 
      xlab = "ph",  
      fill=I("black"), 
      col=I("yellow"), 
)
#train dataset
intrain <- createDataPartition(y = water3$Potability, p = 0.7, list = FALSE)
training <- water3[intrain, ] 
testing <- water3[-intrain, ]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(Potability ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(table(test_pred, testing$Potability))
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Potability ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneGrid = grid,tuneLength= 10)
svm_Linear_Grid
plot(svm_Linear_Grid)
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$Potability))


