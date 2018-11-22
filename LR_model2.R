##Attempt 1: removed variables, ID, poutcome, month
## Attempt2: Removed variables ID, month


library(dplyr)
## Linux
bankdata_train = read.csv('Dropbox/Edvancer/CDAP/Projects/banking/bank-full_train.csv', stringsAsFactors = F)
bankdata_test = read.csv('Dropbox/Edvancer/CDAP/Projects/banking/bank-full_test.csv', stringsAsFactors = F)

##Windows
bankdata_train = read.csv('c:/Users/HP/Dropbox/Edvancer/CDAP/Projects/banking/bank-full_train.csv', stringsAsFactors = F)
bankdata_test = read.csv('c:/Users/HP/Dropbox/Edvancer/CDAP/Projects/banking/bank-full_test.csv', stringsAsFactors = F)

bankdata_test$y = NA

bankdata_test$data = 'test'
bankdata_train$data = 'train'

bankdata_all = rbind(bankdata_train, bankdata_test)

#View(bankdata_all)

#n_distinct(bankdata_all$ID) # Has 45211 unique values, so we can drop this variable

bankdata_all = bankdata_all %>%
  select(-ID, -month)

## Printing the names of categorical variables

names(bankdata_all)[sapply(bankdata_all, function(x) is.character(x))]

##Printing the number of categories in each categorical column in a dataframe

PrintCategories = function(dataframe, pred_var) {
  for (i in 1:(ncol(dataframe)-1)){
    if (class(dataframe[,i]) == 'character'){
      if(names(dataframe)[i] != pred_var){
        print(paste('Number of categories in', colnames(dataframe)[i] , ':' , n_distinct(unique(dataframe[,i]))))
        ##In the previous line, unique returns a vector, data frame or array like x with duplicate elements/rows removed.
      }
    }
  }
}

PrintCategories(bankdata_all, 'y')



#table(bankdata_all[,'y'])
table(bankdata_all[,'marital'])
table(bankdata_all[,'education'])
#table(bankdata_all[,'default'])
#table(bankdata_all[,'housing'])
#table(bankdata_all[,'loan'])
table(bankdata_all[,'contact'])
table(bankdata_all[,'month'])
table(bankdata_all[,'poutcome'])

## Function for creating dummies

CreateDummies = function(data, var, freq_cutoff=0){
  t=table(data[,var])
  t = t[t>freq_cutoff]
  t = sort(t)
  categories = names(t)[-1]
  
  for (cat in categories) {
    name = paste(var,cat, sep='_')
    name = gsub(" ", "", name)
    name = gsub("-", "_", name)
    name = gsub("\\?", "Q", name)
    name = gsub("<", "LT_", name)
    name = gsub("\\+", "", name)
    
    data[, name] = as.numeric(data[,var]==cat)
  }
  
  data[,var] = NULL
  return(data)
}

#columns to create dummy variables for
char_columns = sapply(bankdata_all, is.character)
cat_cols = names(bankdata_all)[char_columns]
cat_cols = cat_cols[!cat_cols %in% c('y', 'data')]

for (col in cat_cols){
  bankdata_all = CreateDummies(bankdata_all, col, 100)
}

# converting the resonse variable 'y' into numeric

bankdata_all$y = as.numeric(bankdata_all$y == 'yes')

head(bankdata_all)

#Splitting train and test data

bankdata_train = bankdata_all %>%
  filter(data == 'train') %>%
  select(-data)

bankdata_test = bankdata_all %>%
  filter(data == 'test') %>%
  select(-data, -y)

#Splitting train data into 2 parts for 

set.seed(2)
s = sample(1:nrow(bankdata_train), 0.8*nrow(bankdata_train))
bankdata_train1 = bankdata_train[s,]
bankdata_train2 = bankdata_train[-s,]

#Eliminating variables with huge multicollinearity

library(car)

for_vif = lm(y~., data = bankdata_train1)

sort(vif(for_vif), decreasing = T)[1:3]

for_vif = lm(y~. -job_blue_collar, data = bankdata_train1)

sort(vif(for_vif), decreasing = T)[1:3]

#Since our VIF has come less than 10, let's start building the model now. 

log_fit = glm(y~. -job_blue_collar, data = bankdata_train1, family = 'binomial')

log_fit = step(log_fit)

summary(log_fit)

log_fit = glm(y~. -job_blue_collar -day -job_housemaid -marital_single -education_primary, data = bankdata_train1, family = 'binomial')

log_fit = step(log_fit)

summary(log_fit)

log_fit = glm(y~. -job_blue_collar -day -job_housemaid -marital_single -education_primary -pdays -job_unemployed -job_services,
              data = bankdata_train1, family = 'binomial')

log_fit = step(log_fit)

summary(log_fit)

log_fit = glm(y~. -job_blue_collar -day -job_housemaid -marital_single -education_primary 
              -pdays -job_unemployed -job_services -education_secondary,
              data = bankdata_train1, family = 'binomial')

log_fit = step(log_fit)

summary(log_fit)

formula(log_fit)

#removing the variables with high p-value and forming a new formula

log_fit = glm(y ~ balance + duration + campaign + job_student + job_retired + 
                job_admin. + job_technician + job_management + marital_married + 
                education_tertiary + housing_yes + loan_no + contact_unknown + 
                poutcome_other + poutcome_failure + poutcome_unknown, data = bankdata_train1, family = 'binomial')

log_fit = step(log_fit)

summary(log_fit)

library(pROC)

val.score = predict(log_fit, newdata = bankdata_train2, type= 'response')
auc_score = auc(roc(bankdata_train2$y, val.score))

auc_score #attempt2 0.8833

library(ggplot2)

mydata = data.frame(y = bankdata_train2$y, val.score = val.score)
ggplot(mydata, aes(y = y, x= val.score, color = factor(y))) + geom_point() + geom_jitter()

# Calculating model evaluation metrics for bankdata_train2 using log_fit. This should actually be 

train2.score = predict(log_fit, newdata = bankdata_train2, type = 'response')
real = bankdata_train2$y
cutoffs = seq(0.001, 0.999, 0.001)

cutoff_data = data.frame(cutoff = 99, Sn=99, Sp=99, KS=99, F5=99, F.1 = 99, M=99)

for (cutoff in cutoffs) {
  predicted = as.numeric(train2.score>cutoff)
  
  TP = sum(real ==1 & predicted ==1)
  TN = sum(real ==0 & predicted ==0)
  FP = sum(real ==0 & predicted ==1)
  FN = sum(real == 1 & predicted == 0)
  
  P = TP + FN 
  N = TN + FP 
  
  Sn = TP /P
  Sp = TN/N
  precision = TP/(TP+FP)
  recall = Sn
  
  KS = (TP/P) - (FP/N)
  F5 = (26*precision*recall)/((0.01*precision)+recall)
  F.1 = (1.01*precision*recall)/((0.1*precision)+recall)
  
  M = (4*FP+FN)/(5*(P+N))
  
  cutoff_data = rbind(cutoff_data, c(cutoff, Sn, Sp, KS, F5, F.1, M))
}

cutoff_data = cutoff_data[-1,]

#Plotting Specificity
ggplot(cutoff_data, aes(x=cutoff, y = Sp)) + geom_line()

#Plotting all 

library(tidyr)

cutoff_long = cutoff_data%>%
  gather(Measure,Value,Sn:M)


ggplot(cutoff_long, aes(x=cutoff, y = Value, color=Measure)) + geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]
which.max(cutoff_data$KS) #attempt1 Ks = 136 #attempt2 Ks = 98
my_cutoff # Attempt1 = 0.135 #attempt2 = 0.098

##Building the model on entire training data

for_vif = lm(y~., data = bankdata_train)

sort(vif(for_vif), decreasing = T)[1:3]

for_vif = lm(y~. -job_blue_collar, data = bankdata_train)

sort(vif(for_vif), decreasing = T)[1:3]

log_fit_final = glm(y~. -job_blue_collar, data = bankdata_train, family = 'binomial')

log_fit_final = step(log_fit_final)

summary(log_fit_final)

log_fit_final = glm(y~. -job_blue_collar -age -day -job_technician, data = bankdata_train, family = 'binomial')

log_fit_final = step(log_fit_final)

summary(log_fit_final)

formula(log_fit_final)

log_fit_final = glm(y ~ balance + duration + campaign + job_student + job_housemaid + 
                      job_retired + job_admin. + job_management + marital_single + 
                      marital_married + education_primary + education_tertiary + 
                      housing_yes + loan_no + contact_unknown + poutcome_other + 
                      poutcome_failure + poutcome_unknown, data = bankdata_train, family = 'binomial')

# testing the model on the entire training data to see how well it has predicted the response variable

## Here we get the percentage probability that a given response is 1/0
train.prob.score = predict(log_fit_final, newdata = bankdata_train, type = 'response') 

## Here we compare the predicted probability with the actual output variable in training data
auc_score = auc(roc(bankdata_train$y, train.prob.score))
auc_score # Attempt2 =0.8913

# Calculating model evaluation metrics for bankdata_train2 using log_fit_final. This should actually be 

train2.score = predict(log_fit_final, newdata = bankdata_train, type = 'response')
real = bankdata_train$y
cutoffs = seq(0.001, 0.999, 0.001)

cutoff_data = data.frame(cutoff = 99, Sn=99, Sp=99, KS=99, F5=99, F.1 = 99, M=99)

for (cutoff in cutoffs) {
  predicted = as.numeric(train2.score>cutoff)
  
  TP = sum(real ==1 & predicted ==1)
  TN = sum(real ==0 & predicted ==0)
  FP = sum(real ==0 & predicted ==1)
  FN = sum(real == 1 & predicted == 0)
  
  P = TP + FN 
  N = TN + FP 
  
  Sn = TP /P
  Sp = TN/N
  precision = TP/(TP+FP)
  recall = Sn
  
  KS = (TP/P) - (FP/N)
  F5 = (26*precision*recall)/((0.01*precision)+recall)
  F.1 = (1.01*precision*recall)/((0.1*precision)+recall)
  
  M = (4*FP+FN)/(5*(P+N))
  
  cutoff_data = rbind(cutoff_data, c(cutoff, Sn, Sp, KS, F5, F.1, M))
}

cutoff_data = cutoff_data[-1,]

#Plotting Specificity
ggplot(cutoff_data, aes(x=cutoff, y = Sp)) + geom_line()

#Plotting all 

library(tidyr)

cutoff_long = cutoff_data%>%
  gather(Measure,Value,Sn:M)


ggplot(cutoff_long, aes(x=cutoff, y = Value, color=Measure)) + geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]
which.max(cutoff_data$KS) #attempt2 Ks = 110
my_cutoff # Attempt1 = 0.11


#Now let's predict the precentage probability that a given response is 1/0 in test data

test.prob.score = predict(log_fit_final, newdata =  bankdata_test, type = 'response')

setwd('Dropbox/Edvancer/CDAP/Projects/banking/') #Linux
setwd('/Users/HP/Dropbox/Edvancer/CDAP/Projects/banking') #Windows
#write.csv(test.prob.score, 'Thileepan_Paulraj_P5_prob_score.csv', row.names = F)

## Converting 1/0 to Yes/No 
test.predicted.numeric = as.numeric(test.prob.score>my_cutoff)
test.predicted = ifelse(test.predicted.numeric ==0, 'No', 'Yes')

any(is.na(bankdata_train))
any(is.na(bankdata_test))
any(is.na(test.prob.score))
any(is.na(test.predicted.numeric))
any(is.na(test.predicted))

#Writing the predicted file

write.csv(test.predicted, 'Thileepan_Paulraj_P5_part2.csv', row.names = FALSE)
