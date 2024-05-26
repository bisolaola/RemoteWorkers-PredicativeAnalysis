####################################################################################################################################
set.seed(2022)

####################################################################################################################################
###calling required libraries##
library(readxl)
library(skimr) 
library(dplyr)    
library(ggplot2)
library(stargazer)
library(glmnet)
library(caret) 
library(lattice)
library(rpart) 
library(psych)
library(lavaan)
library(apaTables)
library(tidyverse)
library(flexplot)
library(RColorBrewer)
library(corrplot)
library(gridExtra)

###set the working directory and call the data set###
setwd("/Users/bisolaolagoke/Desktop/BU/Data")

data <- read_excel("ewcs6_2015_ukda_1904.xlsx")
###43850 observations and 374 variables 

####################################################################################################################################
#####data transformation and restriction####

#OUTCOME VARIABLE
#1. Job Satisfaction (Q88)
rw_data <- mutate(rw_data, js = Q88) %>%
  mutate(js = ifelse(js == 1, 4, 
          ifelse(js == 2, 3,
           ifelse(js == 3, 2,
           ifelse(js == 4, 1, js)))))

table(rw_data$js)

#EXPLANATORY VARIABLE 

#2. Autonomy 

#CronbaAlpha
alpha(rw_data[,c("Q61n", "Q61i")]) 
#result is 0.71

#merging the variables 
rw_data <- mutate(rw_data, autonomy = ifelse(Q61n == 1 & Q61i == 1 , 5, 
                                      ifelse(Q61n == 2 &Q61i == 2, 4,
                                      ifelse(Q61n == 3 &Q61i == 3, 3,   
                                      ifelse(Q61n == 4 &Q61i == 4, 2,
                                      ifelse(Q61n == 5 &Q61i == 5, 1, Q61n))))))
table(rw_data$autonomy)

#3. feedback
table(rw_data$Q63e)
rw_data <- mutate(rw_data, feedback = Q63e) %>%
  mutate(feedback = ifelse(feedback== 1, 5, 
                    ifelse(feedback== 2, 4,
                    ifelse(feedback == 3, 3,
                    ifelse(feedback== 4, 2,
                     ifelse(feedback == 5, 1,feedback))))))

#4. task significance 
table(rw_data$Q90e)
rw_data <- rename(rw_data, tasks_sign = Q90e) 

# 5. motivation 
table(rw_data$Q89e) 
rw_data <- mutate(rw_data, motivation = Q89e)  %>%
  mutate(motivation = ifelse(motivation == 1, 5, 
                      ifelse(motivation == 2, 4,
                      ifelse(motivation == 3, 3,
                      ifelse(motivation == 4, 2, 
                      ifelse(motivation == 5, 1, motivation))))))

##10036 observations and 74observations#

#final data
rw_data <- select(rw_data, js, autonomy, feedback, tasks_sign, motivation, salary, advance, workitself,  
                  growth, recogn, achievement, int_relatnshp, supervision, workcondn, security,
                  gender, agegroup, qualification, contract_type, tasks_sign, tenure)

#################################################################################################################################
################
###regression

model2 <- lm(formula = js  ~ salary + advance  +  workitself + growth + recogn + achievement +
             int_relatnshp + supervision + workcondn + security + gender + agegroup + qualification +
             contract_type + tenure, data = rw_data) 
summary(model2)

##########################################################
####path analysis 

###using Percentile Bootstrap Approach
set.seed(2200)

model1a <- lm(motivation ~ autonomy + feedback + tasks_sign
              + gender + agegroup + qualification + contract_type + tenure, data = rw_data)
summary(model1a)

model1b <- lm(js ~  autonomy  + feedback + tasks_sign + motivation 
              + gender + agegroup + qualification + contract_type + tenure, data = rw_data)
summary(model1b)

######bootstrapping (resampling method)
set.seed(2022)
library(mediation)

#####Boostrapping 
resulta <- mediate(model1a, model1b, treat =  "autonomy", mediator = 
                     "motivation", boot = TRUE, sims=5000)
summary(resulta)

require(flexplot)

added.plot(js~gender + autonomy, data= rw_data, method="lm")
mediate_plot(js~motivation + autonomy, data= rw_data )


################################################################################################################################
#####regression###

##LASSO

#set seed 
set.seed(2022)

###adding cid
rw_data <- mutate(rw_data, cid = row_number())

#test and train data
train <- rw_data %>% sample_frac(0.8)
test  <- anti_join(rw_data, train, by = "cid")

#######specifying model using 10-fold cross-validation 
ctrospec <- trainControl(method = "cv", number = 10, savePredictions ="all")

###training lasso regression model 

#create vector of potiental lambda values (regularization turning parameter
#or turning parameter of interest)
lambda_vector <- 10^seq(5, -5, length = 500)

#specifying Lasso estimated using the train data and 
#the 10-fold cross validation
myformula <- formula (js  ~ autonomy + feedback + tasks_sign + motivation + salary + advance + workitself +  
                      growth + recogn + achievement + int_relatnshp + supervision + workcondn + security +
                      gender + agegroup + qualification + contract_type + tasks_sign + tenure, data = train) #remove motivation

###using glmnet function 
model3a <- train(myformula, 
                data = train,
                preProcess=c("center", "scale"),
                method = "glmnet", tuneGrid=expand.grid(alpha =1, lambda = lambda_vector), 
                trainControl=ctrospec, na.action=na.omit)

set.seed(2022)

model3a$bestTune 
model3a$bestTune$lambda

#LASSO coefficient (parameter estimates)
co <- coef(model3a$finalModel, mode3a1$bestTune$lambda)
round(coef(model3a$finalModel, model3a$bestTune$lambda), 3)

log(model3a$bestTune$lambda)

#variable importance
varImp (model3a)
ggplot(varImp (model3a))

#the outcome when we give it new data
predict1 <- predict(model3a, newdata=test) 

set.seed(2022)
#model performance
performance1 <- data.frame(RMSE = RMSE(predict1, test$js), 
                           Rsquared = R2(predict1, test$js))

###############################################################################################################################
#####data visualization 
#####demography overview 

rw_data <- rw_data %>% mutate(Gender = ifelse(gender == "1","Male","Female"))

rw_data <- rw_data %>% mutate(contract_type2 = ifelse(contract_type == 1, "Parttime","Fulltime"))


#demograph
ggplot(data = rw_data, mapping = aes(x = ..count../1, y = qualification)) + geom_bar() + 
  scale_fill_manual(labels = c("O-levels", "BSc", "MSc", "PhD"))
values = c("#118ab2","#073b4c", "#06d6a0", "#355070")  + 
  theme(legend.title = element_blank(), legend.position = "none") +
  labs( x= "Degree", y = "Propostion of Workers", 
        title = "Qualification of Remote Workers")


ggplot(data = rw_data, mapping = aes(x = ..count../1, y =Gender, fill = factor(contract_type))) + 
  geom_bar(position = "fill")+
  scale_fill_manual(labels = c("Fulltime", "Parttime"),
                    values = c("#98c1d9","#1b4965"))  + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs( x= "Proposition (%)", y = "Gender", 
        title = "Contract Type by Gender")

ggplot(data = rw_data, mapping = aes(x =agegroup, fill = factor(gender))) + geom_bar()+
  scale_fill_manual(labels = c("Female", "Male"), 
                    values = c("#005f73","#94d2bd"))  + 
  theme(legend.title = element_blank()) +
  labs( x= "Age Group", y = "No of Workers", 
        title = "Age Group by Gender")

#job satisfaction  
ggplot(rw_data, aes(x = factor(js), fill = factor(js)))+ 
  geom_bar() + 
  scale_fill_manual(labels = c("Very Unsatisfied", "Unsatisifed", "Satisfied", "Very Satisfied"), 
                    values = c("#118ab2","#073b4c", "#89b0ae", "#98c1d9"))  + 
  theme(legend.title = element_blank(), legend.position = "bottom", axis.text.x=element_blank()) +
  labs( x= "Satisfaction Score", y = "No. of workers", 
        title = "Satisfaction Score of Remote Workers")


#mediator - motivation
ggplot(data = rw_data, mapping = aes(x = factor(motivation), fill = factor(motivation))) + geom_bar() +
  scale_fill_manual(labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", 
                               "Strongly Agree"), 
                    values = c("#118ab2","#073b4c", "#98c1d9", "#38a3a5", "#89b0ae"))  + 
  theme(legend.title = element_blank(), legend.position = "bottom", axis.text.x=element_blank()) +
  labs( x= "Motivation", y = "No of Workers", 
        title = "Motivation Assessment of Remote Workers")

#job satisfaction and demography 

ggplot(data = rw_data, mapping = aes(x =Gender, fill = factor(js))) + geom_bar(position = "fill") +
  facet_wrap(~ contract_type2) +
  scale_fill_manual(labels = c("Very Satisfied", "Unsatisfied", "Satisfied", "Very Satisfied"), 
                    values = c("#118ab2","#073b4c", "#89b0ae", "#98c1d9"))  + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs( x= "gender", y = "Proportion of Workers (%)", 
        title = "Job Satisfaction by Gender and Contract")

ggplot(data = rw_data, mapping = aes(x = ..count../1, y = agegroup, fill = factor(js))) + geom_bar(position = "fill") +
  scale_fill_manual(labels = c("Very Satisfied", "Unsatisfied", "Satisfied", "Very Satisfied"), 
                    values = c("#118ab2","#073b4c", "#89b0ae", "#98c1d9"))  + 
  theme(legend.title = element_blank()) +
  labs( x= "Proportion of Workers (%)", y = "Age Group", 
        title = "Job Satisfaction by Age Group")

ggplot(data = rw_data, mapping = aes(x = qualification , fill = factor(js))) + geom_bar(position = "fill") +
  scale_fill_manual(labels = c("Very Satisfied", "Unsatisfied", "Satisfied", "Very Satisfied"), 
                    values = c("#118ab2","#073b4c", "#89b0ae", "#98c1d9"))  + 
  theme(legend.title = element_blank(),  legend.position = "bottom") +
  labs( x= "Qualification", y = "Proportion of Workers (%)", 
        title = "Job Satisfaction by Qualification")

