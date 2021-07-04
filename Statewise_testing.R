library(tidyverse)
setwd("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject")
statewise_testing = read.csv("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject\\data_files\\StatewiseTestingDetails.csv")
data = statewise_testing

# Data Cleaning
statewise_testing$Date = as.Date(statewise_testing$Date)
statewise_testing$TotalSamples = as.numeric(statewise_testing$TotalSamples)
statewise_testing$Negative = as.numeric(statewise_testing$Negative)
statewise_testing$Positive = as.numeric(statewise_testing$Positive)

for (i in 1:length(statewise_testing$State)) {
  if (is.na(statewise_testing$Positive[i])) {
    statewise_testing$Positive[i] = statewise_testing$TotalSamples[i] - statewise_testing$Negative[i]
  } else if (is.na(statewise_testing$Negative[i])) {
    statewise_testing$Negative[i] = statewise_testing$TotalSamples[i] - statewise_testing$Positive[i]
  }
}

statewise_testing_zero = statewise_testing
statewise_testing_zero[is.na(statewise_testing_zero)] = 0

# Correlation Tests
column_names = colnames(statewise_testing)
cor_tests_results1 = list()
counter = 1
for (col in column_names) {
  if (class(unlist(statewise_testing[col])) %in% c("numeric", "date")) {
    cor_tests_results1[[counter]] = eval(parse(text = 
                                    paste("cor.test(unlist(statewise_testing['", 
                                    as.character(col), 
                                    "']), statewise_testing$Positive, method = 'pearson')", 
                                    sep = "")))
  }
  counter = counter + 1
}
print(cor_tests_results1)

# Grouping Data by states
states_data = statewise_testing_zero %>% group_by(across(c("State"))) %>% 
  summarise(
    TotalSamples = sum(TotalSamples), 
    Negative = sum(Negative),
    Positive = sum(Positive)
  )
sum_cases_state = data.frame(State = states_data$State, Cases = states_data$Positive) #Final

# Average Daily Positive Tests
data_avg = statewise_testing %>% group_by(across(c("Date"))) %>% 
  summarise(
    TotalSamples = sum(TotalSamples), 
    Negative = sum(Negative),
    Positive = sum(Positive)
  )
avg_pos = sum(data_avg$Positive)/length(data_avg$Positive)
avg_pos

# Average Daily Negative Tests
avg_neg = sum(data_avg$Negative)/length(data_avg$Negative)
avg_neg
