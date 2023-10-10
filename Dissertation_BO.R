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
setwd("/Users/bisolaolagoke/Desktop/BU/Thesis/Data")

data <- read_excel("ewcs6_2015_ukda_1904.xlsx")
###43850 observations and 374 variables 

skim(data)
str(data)
####################################################################################################################################
###select variables and filter observations###
rw_data <- select(data, Q88, Q35e, Q35b, Q35c, Q35d, Q34, Q35f, Q2b, Q2a, Q106, Q17, Q2d, Q89a,
                  Q89d, Q18b, Q71a, Q71b, Q65c, Q101d, Q81a, Q81b, Q81c, Q72a, Q72b, 
                  Q72c, Q72d, Q72e, Q72f, Q72g, Q80a, Q80b, Q80c, Q80d, Q70e, Q61n, Q61i, Q90e,
                  Q18d, Q63e, Q89b, Q49a, Q49b, Q51, Q53e, Q65a, Q65b, Q65d, 
                  Q95f, Q63b, Q89c, Q70a, Q63d, Q63f, Q89e, Q61b, Q53f)


######variable selected######
# remote working - Q35e, Q35b, Q35c, Q35d, Q34, Q35f

#JCM factors 
#autonomy - Q61n, Q61i
#task significance - Q90e
#feedback - Q63e

#mediator
#motivation - Q89e

####Dual theory
#motivation 
#advancement - Q89b
#work itself - Q48, Q51, Q53e, Q56, Q18d
#growth - Q65a, Q65b, Q65d, Q66, Q67a, Q67c, Q96f
#recognition - Q63b, Q89c, Q70a
#achievement - Q89a

#hygienic
#interpersonal relationship - Q89d, Q70e
#salary - Q8a, Q18b
#supervision - Q65c, Q63d, Q63f, Q61b
#working condition - Q101d
#security - Q81a, Q81b, Q81c, Q72a, Q72b, Q72c, Q72d, Q72e, Q72f, Q72g, Q80a, 
#Q80b, Q80c, Q80d

###control variables 
#age - Q2b 
#gender - Q2a
#qualification - Q106
#tenure - Q17
#contract - Q2d

####################################################################################################################################
###filtering out remote workers#
#based on definition is working anywhere else other than from the office. 
#working anywhere else with the use of technology

#1. Q35e
table(rw_data$Q35e) #drop outlier 8 and 9
rw_data <- rw_data %>% filter(Q35e != "8", Q35e != "9")

#2. Q35b
table(rw_data$Q35b) #drop outlier 8 and 9
rw_data <- rw_data %>% filter(Q35b != "8", Q35b != "9")

#3. Q35c
table(rw_data$Q35c) #drop outlier 8 and 9
rw_data <- rw_data %>% filter(Q35c != "8", Q35c != "9")

#4. Q35d
table(rw_data$Q35d) #drop outlier 8 and 9
rw_data<- rw_data %>% filter(Q35d != "8", Q35d != "9")

#5. Q35f
table(rw_data$Q35f) #drop outlier 8 and 9
rw_data<- rw_data %>% filter(Q35f != "8", Q35f != "9")

#6. Q34
table(rw_data$Q34) #drop outlier 8 and 9
rw_data <- rw_data %>% filter(Q34 != "8", Q34 != "9")

rw_data <- rw_data %>% mutate(Q35e = ifelse(Q35e == "1" | Q35e == "2"| Q35e == "3"| Q35e == "4", 1,0), 
                              Q35b = ifelse(Q35b == "1" | Q35b == "2"| Q35b == "3"| Q35b == "4", 1,0),
                              Q35c = ifelse(Q35c == "1" | Q35c == "2"| Q35c == "3"| Q35c == "4", 1,0), 
                              Q35d = ifelse(Q35d == "1" | Q35d == "2"| Q35d == "3"| Q35d == "4", 1,0), 
                              Q35f = ifelse(Q35f == "1" | Q35f == "2"| Q35f == "3"| Q35f == "4", 1,0),
                              Q34 = ifelse(Q34 == "1", 1,0))

###check consistency 
#CronbaAlpa
alpha(rw_data[,c("Q35e", "Q35b", "Q35c", "Q35d", "Q34", "Q35f")])
#result is 0.74

####merge
rw_data<- rw_data %>%
  mutate(remotew = ifelse(Q35b == 1 | Q35e == 1 | Q35c == 1 | Q35d == 1| Q35f == 1| Q34 == 1, 1, 0))
table(rw_data$remotew) 

#select remote work from data

rw_data <- filter(rw_data, remotew == "1")
table(rw_data$remotew)

### a total of 26175 observation and 57 variables
str(rw_data)
skim(rw_data)
#1212 NAs in Q106
#1945 NAs in Q2d
#6237 NAs in Q101d
#6237 NAs in Q70e
#6237 NAs in Q70b
#6237 NAs in Q71a
#6237 NAs in Q70b
#6237 NAs in Q65a
#7125 NAs in Q63e
#7125 NAs in Q63f
####################################################################################################################################
##### data cleaning####

#drop outliers and missing variables 
#7 - Not applicable  
#8 or 888 - DK/no opinion 
#9 or 999 - Refusal 

####1. 
table(rw_data$Q88) #drop outlier 8 and 9
rw_data <- rw_data %>% filter(Q88 != "8", Q88 != "9")

#1. 
table(rw_data$Q61n) #drop outlier 7, 8 and 9
rw_data <- rw_data %>% filter(Q61n != "8", Q61n != "9", Q61n != "7")

#2. 
table(rw_data$Q61i) #drop outlier 7, 8 and 9
rw_data <- rw_data %>% filter(Q61i != "8", Q61i != "9", Q61i != "7")

#3.
table(rw_data$Q90e)
rw_data <- rw_data %>% filter(Q90e != "8", Q90e != "9")

#4. 
table(rw_data$Q63e) #drop 7,8,9
rw_data <- rw_data %>% filter(Q63e != "7", Q63e != "8", Q63e != "9")

#5.
table(rw_data$Q89e) #drop 7,8,9
rw_data <- rw_data %>% filter(Q89e != "8", Q89e != "9", Q89e != "7")

#6.
table(rw_data$Q89b) #drop 7,8,9
rw_data <- rw_data %>% filter(Q89b != "7", Q89b != "8", Q89b != "9")

#7.
table(rw_data$Q18d) #drop 8,9
rw_data <- rw_data %>% filter(Q18d != "8", Q18d != "9")

#8.
table(rw_data$Q49a) #drop 8,9
rw_data <- rw_data %>% filter(Q49a != "8", Q49a != "9")

#9.
table(rw_data$Q49b) #drop 8,9
rw_data <- rw_data %>% filter(Q49b != "8", Q49b != "9")

#10
table(rw_data$Q51) #drop 8,9
rw_data <- rw_data %>% filter(Q51 != "8", Q51 != "9")

#11
table(rw_data$Q53e) #drop 8,9
rw_data <- rw_data %>% filter(Q53e != "8", Q53e != "9")

#12
table(rw_data$Q65a) #drop 8,9
rw_data <- rw_data %>% filter(Q65a != "8", Q65a != "9")

#13
table(rw_data$Q65b) #drop 8,9
rw_data <- rw_data %>% filter(Q65b != "8", Q65b != "9")

#14
table(rw_data$Q65d) #drop 8,9
rw_data <- rw_data %>% filter(Q65d != "8", Q65d != "9")

#15
table(rw_data$Q95f) #drop 8,9
rw_data <- rw_data %>% filter(Q95f != "8", Q95f != "9")

#16
table(rw_data$Q53f) #drop 8,9
rw_data <- rw_data %>% filter(Q53f != "8", Q53f != "9")

#17
table(rw_data$Q61d) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q61d != "7", Q61d != "8", Q61d != "9")

#18
table(rw_data$Q63b) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q63b != "7", Q63b != "8", Q63b != "9")

#19
table(rw_data$Q89c) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q89c != "7", Q89c != "8", Q89c != "9")

#20
table(rw_data$Q70a) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q70a != "7", Q70a != "8", Q70a != "9")

#21
table(rw_data$Q89a) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q89a != "7", Q89a != "8", Q89a != "9")

#22
table(rw_data$Q89d) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q89d != "7", Q89d != "8", Q89d != "9")

#23
table(rw_data$Q70e) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q70e != "7", Q70e != "8", Q70e != "9")

#24
table(rw_data$Q18b) #drop 8,9
rw_data <- rw_data %>% filter(Q18b != "8", Q18b != "9")

#25
table(rw_data$Q65c) #drop 8,9
rw_data <- rw_data %>% filter(Q65c != "8", Q65c != "9")

#26
table(rw_data$Q63d) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q63d != "7", Q63d != "8", Q63d != "9")

#27
table(rw_data$Q63f) #drop 8,9
rw_data <- rw_data %>% filter(Q63f != "7", Q63f != "8", Q63f != "9")

#28
table(rw_data$Q61b) #drop 7, 8,9
rw_data <- rw_data %>% filter(Q61b != "7", Q61b != "8", Q61b != "9")

#29
table(rw_data$Q101d) #drop 8,9 and NA
rw_data <- rw_data %>% filter(Q101d != "8", Q101d != "9", Q101d != ".")

#30
table(rw_data$Q81a) #drop 8,9
rw_data <- rw_data %>% filter(Q81a != "8", Q81a != "9")

table(rw_data$Q81b) #drop 8,9
rw_data <- rw_data %>% filter(Q81b != "8", Q81b != "9")

table(rw_data$Q81c) #drop 8,9
rw_data <- rw_data %>% filter(Q81c != "8", Q81c != "9")

table(rw_data$Q72a) #drop 8,9
rw_data <- rw_data %>% filter(Q72a != "8", Q72a != "9")

table(rw_data$Q72b) #drop 8,9
rw_data <- rw_data %>% filter(Q72b != "8", Q72b != "9")

table(rw_data$Q72c) #drop 8,9
rw_data <- rw_data %>% filter(Q72c != "8", Q72c != "9")

table(rw_data$Q72d) #drop 8,9
rw_data <- rw_data %>% filter(Q72d != "8", Q72d != "9")

table(rw_data$Q72e) #drop 8,9
rw_data <- rw_data %>% filter(Q72e != "8", Q72e != "9")

table(rw_data$Q72f) #drop 8,9
rw_data <- rw_data %>% filter(Q72f != "8", Q72f != "9")

table(rw_data$Q72g) #drop 8,9
rw_data <- rw_data %>% filter(Q72g != "8", Q72g != "9")

table(rw_data$Q80a) #drop 8,9
rw_data <- rw_data %>% filter(Q80a != "8", Q80a != "9")

table(rw_data$Q80b) #drop 8,9
rw_data <- rw_data %>% filter(Q80b != "8", Q80b != "9")

table(rw_data$Q80c) #drop 8,9
rw_data <- rw_data %>% filter(Q80c != "8", Q80c != "9")

table(rw_data$Q80d) #drop 8,9
rw_data <- rw_data %>% filter(Q80d != "8", Q80d != "9")

table(rw_data$Q2b) #drop outlier 888 and 999
rw_data <- rw_data %>% filter(Q2b != "888", Q2b != "999")

table(rw_data$Q2a) #drop 9
rw_data <- rw_data %>% filter(Q2a != "9")

skim(rw_data$Q106) ## drop NAs 
table(rw_data$Q106) #drop outlier 88, 99 and unknown qualification from 10 to 20
rw_data <- rw_data %>% filter(Q106 < 10, Q106 != ".")

table(rw_data$Q17) #dropped outliers 77, 88, 99 and 999
rw_data <- rw_data %>% filter(Q17 < 77)

skim(rw_data$Q2d) #drop NAs
table(rw_data$Q2d) #drop outliers 8 and 9
rw_data <- rw_data %>% filter(Q2d < 8, Q2d != ".")

skim(rw_data)

###10045 observation and 57 variables 

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

#######################################################################
#####Descriptive Statistics###

rw_data %>% summarise(avg_js = mean(js),min_js = min(js), 
                      max_js = max(js), S.D_js = sd(js)) 
median(rw_data$js)

rw_data %>% summarise(avg_autonomy = mean(autonomy),min_autonomy = min(autonomy), 
                      max_autonomy = max(autonomy), S.D_autonomy = sd(autonomy)) 
median(rw_data$autonomy)

rw_data %>% summarise(avg_feedback = mean(feedback),min_feedback = min(feedback), 
                      max_feedback = max(feedback), S.D_feedback = sd(feedback)) 
median(rw_data$feedback)

rw_data %>% summarise(avg_tasks_sign = mean(tasks_sign),min_tasks_sign = min(tasks_sign), 
                      max_tasks_sign = max(tasks_sign), S.D_tasks_sign = sd(tasks_sign)) 
median(rw_data$tasks_sign)

rw_data %>% summarise(avg_motivation = mean(motivation),min_motivation = min(motivation), 
                      max_motivation = max(motivation), S.D_motivation = sd(motivation))
median(rw_data$motivation)

rw_data %>% summarise(avg_salary = mean(salary),min_salary = min(salary), 
                      max_salary = max(salary), S.D_salary = sd(salary))
median(rw_data$salary)

rw_data %>% summarise(avg_advance = mean(advance),min_advance = min(advance), 
                      max_advance = max(advance), S.D_advance = sd(advance))
median(rw_data$advance)

rw_data %>% summarise(avg_workitself = mean(workitself),min_workitself = min(workitself), 
                      max_workitself = max(workitself), S.D_workitself = sd(workitself))
median(rw_data$workitself)

rw_data %>% summarise(avg_growth = mean(growth),min_growth = min(growth), 
                      max_growth = max(growth), S.D_growth = sd(growth))
median(rw_data$growth)

rw_data %>% summarise(avg_recogn = mean(recogn),min_recogn = min(recogn), 
                      max_recogn = max(recogn), S.D_recogn = sd(recogn))

median(rw_data$recogn)

rw_data %>% summarise(avg_achievement = mean(achievement),min_achievement = min(achievement), 
                      max_achievement = max(achievement), S.D_achievement = sd(achievement))
median(rw_data$achievement)

rw_data %>% summarise(avg_int_relatnshp = mean(int_relatnshp),min_int_relatnshp = min(int_relatnshp), 
                      max_int_relatnshp = max(int_relatnshp), S.D_int_relatnshp = sd(int_relatnshp))
median(rw_data$int_relatnshp)

rw_data %>% summarise(avg_supervision = mean(supervision),min_supervision = min(supervision), 
                      max_supervision = max(supervision), S.D_supervision = sd(supervision))
median(rw_data$supervision)

rw_data %>% summarise(avg_workcondn = mean(workcondn),min_workcondn = min(workcondn), 
                      max_workcondn = max(workcondn), S.D_workcondn = sd(workcondn))
median(rw_data$workcondn)

rw_data %>% summarise(avg_security = mean(security),min_security = min(security), 
                      max_security = max(security), S.D_security = sd(security))
median(rw_data$security)

rw_data %>% summarise(avg_gender = mean(gender),min_gender = min(gender), 
                      max_gender = max(gender), S.D_gender = sd(gender))

rw_data %>% summarise(avg_contract_type = mean(contract_type),min_contract_type = min(contract_type), 
                      max_contract_type = max(contract_type), S.D_contract_type = sd(contract_type)) 

rw_data %>% summarise(avg_tenure = mean(tenure),min_g = min(tenure), 
                      max_tenure = max(tenure), S.D_tenure = sd(tenure))
median(rw_data$tenure)

skim(rw_data$agegroup)
skim(rw_data$qualification)

#demography frequency and percentage 
prop.table(table(rw_data$agegroup))*100
table(rw_data$agegroup)

prop.table(table(rw_data$gender))*100
table(rw_data$gender)

prop.table(table(rw_data$qualification))*100
table(rw_data$qualification)

prop.table(table(rw_data$contract_type))*100
table(rw_data$contract_type)


#################################################################################################################################
###pearson correlation 
#select variables 

rw_pears <- dplyr::select(rw_data, js, autonomy, feedback, tasks_sign, motivation, salary, advance, workitself,  
                          growth, recogn, achievement, int_relatnshp, supervision, workcondn, security,
                          tasks_sign) 

corr.test(rw_pears, method = "pearson")

cormatrix <- corr.test(rw_pears, method = "pearson")
print(cormatrix, short= FALSE)

apa.cor.table(rw_pears, "pearson cor table.doc")


cor_data<-cor(rw_pears)
head(round(rw_pears,2))

##Graph of the pearson correlation
corrplot(cor_data, method="number",type="lower")

#############################################################
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


resultf <- mediate(model1a, model1b, treat =  "feedback", mediator = 
                     "motivation", boot = TRUE, sims=5000)
summary(resultf)

resultt <- mediate(model1a, model1b, treat =  "tasks_sign", mediator = 
                     "motivation", boot = TRUE, sims=5000)
summary(resultt)


plot(resultt, group.plot = TRUE) #appendix
plot(resultf, group.plot = TRUE)#appendix
plot(resulta, group.plot = TRUE)#appendix
#####visusal

require(flexplot)

added.plot(js~gender + autonomy, data= rw_data, method="lm")
mediate_plot(js~motivation + autonomy, data= rw_data )

added.plot(js~gender + feedback, data= rw_data, method="lm")
mediate_plot(js~motivation + feedback, data= rw_data )

added.plot(js~gender + tasks_sign, data= rw_data, method="lm")
mediate_plot(js~motivation + tasks_sign, data= rw_data )


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

####Linear Regression to predict 

#using the lm funtion 
model3b <- train(myformula, 
                data = train,
                preProcess=c("center", "scale"),
                method = "lm", 
                trainControl=ctrospec, na.action=na.omit)
print(model3b)
summary(model3b)


model3b$finalModel$coefficients

#compare_model performance of k-fold cross validation on train data 
model3b_list <- list(model3a, model3b)
resamp2 <- resamples(model3b_list)
summary(resamp2)

#compare model using paired-samples t-test
compare_models(model3a, model3b, metric = "RMSE")
compare_models(model3a, model3b, metric = "Rsquared")

ggplot(varImp (model3b))

####prediction outcome 
predict2 <- predict(model3b, newdata=test)

set.seed(2022)
###model performance 

performance2 <- data.frame(RMSE = RMSE(predict2, test$js), 
                           Rsquared = R2(predict2, test$js))
summary(performance2)

###compare both models 
compare <- matrix(c(performance1$RMSE, performance1$Rsquared, 
                    performance2$RMSE, performance2$Rsquared), ncol=2, byrow =TRUE)

colnames(compare)<-c("RMSE", "R-square")
rownames(compare)<-c("LASSO Regression", "OLS MLR")

comp<- as.table(compare)

comp<round(compare, 2)


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

