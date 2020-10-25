rm(list=ls())

ADOS <- read.csv("C:/Users/shrey/Downloads/covid_machine_learning_model/COVID-19_Case_Surveillance_Public_Use_Data.csv", sep=',', header = T)

ADOS_1 <- ADOS

ADOS$hosp_yn[ADOS$hosp_yn == "Yes"] <- 1
ADOS$hosp_yn[ADOS$hosp_yn == "No"] <- 0
ADOS$hosp_yn[ADOS$hosp_yn == "Missing"] <- 0.5
ADOS$hosp_yn[ADOS$hosp_yn == "Unknown"] <- 0.5

ADOS$icu_yn[ADOS$icu_yn == "Yes"] <- 1
ADOS$icu_yn[ADOS$icu_yn == "No"] <- 0
ADOS$icu_yn[ADOS$icu_yn == "Missing"] <- 0.5
ADOS$icu_yn[ADOS$icu_yn == "Unknown"] <- 0.5

ADOS$death_yn[ADOS$death_yn == "Yes"] <- 1
ADOS$death_yn[ADOS$death_yn == "No"] <- 0
ADOS$death_yn[ADOS$death_yn == "Missing"] <- 0.5
ADOS$death_yn[ADOS$death_yn == "Unknown"] <- 0.5

ADOS$hosp_yn <- as.integer(ADOS$hosp_yn)
ADOS$icu_yn <- as.integer(ADOS$icu_yn)
ADOS$death_yn <- as.integer(ADOS$death_yn)

ADOS$risk_factor <- ADOS$hosp_yn + ADOS$icu_yn + ADOS$death_yn


ADOS$cdc_report_dt <- gsub('/', '', ADOS$cdc_report_dt)
ADOS$cdc_report_dt <- as.numeric(ADOS$cdc_report_dt)

ADOS$medcond_yn[ADOS$medcond_yn == "Yes"] <- 1
ADOS$medcond_yn[ADOS$medcond_yn == "No"] <- 0
ADOS$medcond_yn[ADOS$medcond_yn == "Missing"] <- 0.5
ADOS$medcond_yn[ADOS$medcond_yn == "Unknown"] <- 0.5
ADOS$medcond_yn <- as.integer(ADOS$medcond_yn)

ADOS <- ADOS[-c(2,3,4,8,9,10)]

write.table(ADOS,file="C:/Users/shrey/Downloads/covid_machine_learning_model/covid.csv",row.names=FALSE)

cor.test(ADOS$cdc_report_dt, ADOS$risk_factor)

#=======================================================================================================

ADOS_1 <- read.csv("C:/Users/shrey/Downloads/covid_machine_learning_model/covid.csv", sep=' ', header = T)
ADOS_1$sex <- as.factor(ADOS_1$sex)
ADOS_1$age_group <- as.factor(ADOS_1$age_group)
ADOS_1$Race.and.ethnicity..combined. <- as.factor(ADOS_1$Race.and.ethnicity..combined.)
ADOS_1$risk_factor <- as.factor(ADOS_1$risk_factor)

levels(ADOS_1$Race.and.ethnicity..combined.)
levels(ADOS_1$age_group)
summary(ADOS_1)

dataset <- ADOS_1

library(caret)
library(caTools)
library(C50)
split = sample.split(dataset$risk_factor, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dataset$risk_factor <- na.omit(dataset$risk_factor)

nfolds <- 2
trControl <- trainControl(method  = "cv",
                          number  = nfolds)
fit <- train(form=risk_factor ~ .,
             data = training_set,
             method     = "C5.0",
             na.action = na.pass,
             trControl  = trControl,
             tuneLength = 2, #5
             control = C5.0Control(earlyStopping = FALSE),
             metric     = "Accuracy")

plot(fit)
saveRDS(fit, "c50_model.rds")

#=================================================================================================
#Saving the model and making prediction based on input

library(shiny)
test_set <- read.csv('C:/Users/shrey/Downloads/hackgt/test_short.csv', sep=' ', header = T)
test_set$sex <- as.factor(test_set$sex)
test_set$age_group <- as.factor(test_set$age_group)
test_set$Race.and.ethnicity..combined. <- as.factor(test_set$Race.and.ethnicity..combined.)

my_model <- readRDS("c50_model.rds")
summary(fit)

# Predicting the Test set results
y_pred = predict(my_model, newdata = test_set) #test_set is the csv file consisting of the variables without the final prediction
summary(y_pred) #prediction made by the machine learning model

# Making the Confusion Matrix
cm = table(test_set[,6], y_pred)

cm
#print("=====================================Random Forest=====================================")
#library(ggplot2)
#library(lattice)
#library(caret)
confusionMatrix(cm)


plot(my_model)

levels(ADOS$hosp_yn)
summary(ADOS)
levels(ADOS$Race.and.ethnicity..combined.)
