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

#6. advancement - Q89b
table(rw_data$Q89b)
rw_data <- mutate(rw_data, advance = Q89b)  %>%
  mutate(advance = ifelse(advance == 1, 5, 
                   ifelse(advance == 2, 4,
                   ifelse(advance == 3, 3,
                   ifelse(advance == 4, 2, 
                   ifelse(advance == 5, 1, advance))))))

####7. work itself - Q18d, Q49a, Q49b , Q51, Q53e

#CronbaAlpha
alpha(rw_data[,c("Q18d", "Q49a", "Q49b" , "Q51", "Q53e")])
#result is 0.70

rw_data<- rw_data %>% mutate(Q18d = ifelse(Q18d == "1" | Q18d == "2", 0,1), 
                             Q49a = ifelse(Q49a == "6" | Q49a == "7", 1,0),
                             Q49b = ifelse(Q49b == "6" | Q49b == "7", 1, 0),
                             Q51 = ifelse(Q51 == "1" | Q51 == "2", 0,1), 
                             Q53e = ifelse(Q53e == "1", 0,1)) %>%
mutate(workitself = ifelse(Q18d == 1 | Q49a == 1 | Q49b == 1 | Q51 == 1| Q53e == 1, 1, 0))
table(rw_data$workitself)

####8. growth - Q65a, Q65b, Q65d, Q95f, Q53f
table(rw_data$Q53f)
#CronbaAlpha
alpha(rw_data[,c("Q65a", "Q65b", "Q65d", "Q65c", "Q95f", "Q53f")])
#result is 0.73


rw_data<- rw_data %>% mutate(Q65a = ifelse(Q65a == "1", 1, 0), 
                             Q65b = ifelse(Q65b == "1", 1, 0),
                             Q65c = ifelse(Q65c == "1", 1, 0),
                             Q65d = ifelse(Q65d == "1", 1, 0), 
                             Q95f = ifelse(Q95f == "4"| Q95f == "5", 0,1), 
                             Q53f = ifelse(Q53f == "1", 1, 0)) %>%
mutate(rw_data, growth = ifelse(Q65a == 1 |Q65b == 1 |Q65c == 1
                                |Q65d == 1|Q95f == 1|Q53f == 1, 1, 0))
table(rw_data$growth)

####9. recognition - Q63b, Q89c, Q70a

#CronbaAlpha
alpha(rw_data[,c("Q63b", "Q89c", "Q70a")])
#result is 0.78

rw_data <- mutate(rw_data, recogn = ifelse(Q63b == 1 & Q89c == 1 & Q70a == 1, 5, 
                                       ifelse(Q63b == 2 &Q89c == 2 & Q70a == 2, 4,
                                       ifelse(Q63b == 3 &Q89c == 3 & Q70a == 3, 3,   
                                       ifelse(Q63b == 4 &Q89c == 4 & Q70a == 4, 2,
                                       ifelse(Q63b == 5 &Q89c == 5 & Q70a == 5, 1, Q63b))))))
table(rw_data$recogn)

###10. achievement - Q89a
table(rw_data$Q89a)
rw_data <- mutate(rw_data, achievement = Q89a)  %>%
  mutate(achievement = ifelse(achievement == 1, 5, 
                  ifelse(achievement == 2, 4,
                   ifelse(achievement == 3, 3,
                   ifelse(achievement == 4, 2, 
                   ifelse(achievement == 5, 1, achievement))))))
table(rw_data$achievement)


#######################hygienic#######
####11. interpersonal relationship - Q89d, Q70e

#CronbaAlpha
alpha(rw_data[,c("Q89d", "Q70e")])
#result is 0.71

rw_data <- mutate(rw_data, int_relatnshp = ifelse(Q89d == 1 & Q70e == 1, 1, 
                                    ifelse(Q89d == 2 & Q70e == 2, 2,
                                    ifelse(Q89d == 3 & Q70e == 3, 3,   
                                    ifelse(Q89d == 4 & Q70e == 4, 5,
                                    ifelse(Q89d == 5 & Q70e == 5, 5, Q89d))))))
table(rw_data$int_relatnshp)

####12. salary - Q18b
table(rw_data$Q18b)
rw_data <- rename(rw_data, salary = Q18b)  
table(rw_data$salary)


#####13. supervision - Q65c, Q63d, Q63f, Q61b

#CronbaAlpha
alpha(rw_data[,c("Q63d", "Q63f", "Q61b")])
#result is 77
rw_data<- rw_data %>% mutate(Q63d = ifelse(Q63d == "1"| Q63d == "2", 0, 1),
                             Q63f = ifelse(Q63f == "1"| Q63f == "2", 0, 1), 
                             Q61b = ifelse(Q61b == "4"| Q61b == "5", 1, 0)) %>%
mutate(rw_data, supervision = ifelse(Q63d == 1 |Q63f == 1 |Q61b == 1, 1, 0))
table(rw_data$supervision)

#14. working condition - Q101d
table(rw_data$Q101d)
rw_data <- mutate (rw_data, workcondn = ifelse(Q101d == "1", 1,0)) 

#security - Q81a, Q81b, Q81c, Q72a, Q72b, Q72c, Q72d, Q72e, Q72f, Q72g, Q80a, 
#Q80b, Q80c, Q80d

#CronbaAlpha
alpha(rw_data[,c("Q81a", "Q81b", "Q81c", "Q72a", "Q72b", "Q72c", "Q72d", "Q72e", 
                  "Q72f", "Q72g", "Q80a", "Q80b", "Q80c", "Q80d")])
#result is 0.73

rw_data<- rw_data %>% mutate(Q81a = ifelse(Q81a == "1", 1,0), 
                             Q81b = ifelse(Q81b == "1", 1, 0),
                             Q81c = ifelse(Q81c == "1", 1, 0),
                             Q72a = ifelse(Q72a == "1", 1, 0),
                             Q72b = ifelse(Q72b == "1", 1, 0),
                             Q72c = ifelse(Q72c == "1", 1, 0),
                             Q72d = ifelse(Q72d == "1", 1, 0),
                             Q72e = ifelse(Q72e == "1", 1, 0),
                             Q72f = ifelse(Q72f == "1", 1, 0),
                             Q72g = ifelse(Q72g == "1", 1, 0),
                             Q80a = ifelse(Q80a == "1", 1, 0),
                             Q80b = ifelse(Q80b == "1", 1, 0),
                             Q80c = ifelse(Q80c == "1", 1, 0), 
                             Q80d = ifelse(Q80d == "1", 1, 0)) %>%
mutate(rw_data, security = ifelse(Q81a == 1 |Q81b == 1 |Q81c == 1
                                             |Q72a == 1|Q72b == 1|Q72c == 1
                                             |Q72d == 1|Q72e == 1|Q72f == 1
                                             |Q72g == 1|Q80a == 1|Q80b == 1
                                             |Q80c == 1|Q80d == 1, 1, 0))
table(rw_data$security)

#CONTROL VARIABLES 

#15. Age (Q2b) 
table(rw_data$Q2b)
####dropped age greater 70 because of retirement 
rw_data <- rw_data %>% filter (Q2b <= "70") %>%
  mutate(agegroup = ifelse(Q2b <= 24, "15-24",
                    ifelse(25 <= Q2b & Q2b <= 34,  "25-34",
                    ifelse(35 <= Q2b & Q2b <= 44, "35-44",
                    ifelse(45 <= Q2b & Q2b <= 54, "45-54",
                    ifelse(55 <= Q2b & Q2b <= 64, "55-64",
                    ifelse(Q2b >=65, "65 and Older", Q2b)))))))
table(rw_data$agegroup)

#16. Gender (Q2a)
rw_data <- rw_data %>% mutate(gender = ifelse(Q2a == "1", 1,0))
## 1 represents male and 0 female 
table(rw_data$gender)

#17. Qualification (Q106)
table(rw_data$Q106)
rw_data <- mutate(rw_data, qualification = ifelse(Q106 == 1 | Q106 == 2 | Q106 == 3 | 
                                                    Q106 == 4 | Q106 == 5,  "O-Level and Below",
                                           ifelse(Q106 == 6 | Q106 == 7 , "BSc",
                                           ifelse(Q106 == 8, "MSc",
                                           ifelse(Q106 == 9, "PhD", Q106)))))

rw_data$qualification <- rw_data$qualification %>% factor (levels = c("O-Level and Below", "BSc", "MSc","PhD"))
table(rw_data$qualification)

#18. Tenure (Q17)
table(rw_data$Q17) #leave as continuous
rw_data <- rename(rw_data, tenure = Q17)
table(rw_data$tenure) 

#19. Contract (Q2d)
table(rw_data$Q2d) #nominal variable
rw_data <- mutate(rw_data, contract_type = ifelse(Q2d =="1", 1, 0)) # 1 for parttime and 0 for fulltime 
table(rw_data$contract_type)
skim(rw_data)

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

plot(resulta, group.plot = TRUE)#appendix
#####visusal

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

