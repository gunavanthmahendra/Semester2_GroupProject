library(tidyverse)
library(scales)
library(lubridate)

## Setting working directory and reading data
setwd("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject")
data = read.csv("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject\\data_files\\covid_19.csv")
data$Date = as.Date(data$Date, "%d-%m-%Y")

## Defining required functions

colfunc = colorRampPalette(c("blue", "green"))

colfunc1 = colorRampPalette(c("red", "yellow"))

date_generator = function(date, nday) {
  date_list = c(as.Date(date))
  for (i in 1:nday) {
    date_list = c(date_list, as.Date(date) + days(1))
    date = as.Date(date) + days(1)
  }
  return(as.character(date_list))
}

## Aggregating the data based on Country and Date.
cleaned_data = data %>% group_by(across(c("Country.Region", "Date"))) %>% 
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active = sum(Active),
)

country_data = cleaned_data %>% group_by(across(c("Country.Region"))) %>%
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active = sum(Active),
  )

data_r = cleaned_data %>% group_by(across(c("WHO.Region"))) %>%
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active = sum(Active),
  )
## Adding in the WHO Region column to the cleaned data.
cleaned_data["WHO.Region"] = ""
n = 1
while (n <= nrow(cleaned_data)) {
  cleaned_data["WHO.Region"][n,] = data$`WHO.Region`[which(data["Country.Region"] == as.character(cleaned_data["Country.Region"][n,]), arr.ind=TRUE)[1]]
  n = n+1
}

## Defining our seasons
winter = c(as.Date("2020-01-22"): as.Date("2020-02-29"))
summer = c(as.Date("2020-03-01"): as.Date("2020-05-31"))
monsoon = c(as.Date("2020-06-01"): as.Date("2020-07-27"))

## Grouping by WHO Regions and Date.
regional_data = cleaned_data %>% 
  group_by(across(c("WHO.Region", "Date"))) %>%
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active = sum(Active),
  )  
regional_data["Season"] = ""
i = 1
while (i <= nrow(data.frame(regional_data))) {
  if (regional_data$Date[i] %in% winter) {
    regional_data$Season[i] = "Winter"
  } 
  else if (regional_data$Date[i] %in% summer) {
    regional_data$Season[i] = "Summer"
  } 
  else if (regional_data$Date[i] %in% monsoon) {
    regional_data$Season[i] = "Monsoon"
  }
  i = i + 1
}

# Master Plot
base_line_plot = ggplot(regional_data, mapping = aes(x = Date, y = Confirmed))

# Overall Graph
base_line_plot + geom_smooth(color = "#C70039", se = FALSE) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) + 
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "", y = "Confirmed Cases")

# Seasonal Graph
base_line_plot + geom_smooth(mapping = aes(color = Season), se = FALSE) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) + 
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "", y = "Confirmed Cases")

## Defining Regional datasets:

africa = filter(regional_data, WHO.Region == 'Africa')
americas = filter(regional_data, WHO.Region == 'Americas')
eastern_mediterranean = filter(regional_data, WHO.Region == 'Eastern Mediterranean')
europe = filter(regional_data, WHO.Region == 'Europe')
south_east_asia = filter(regional_data, WHO.Region == 'South-East Asia')
western_pacific = filter(regional_data, WHO.Region == 'Western Pacific')

## Correlation Tests: Ran loop and appended the results to lists.
deathtorecovery = list()
activetodeath = list()
counter = 0
for (df in list(africa, americas, eastern_mediterranean, europe, south_east_asia, western_pacific)) {
  counter = counter + 1
  eval(parse(text = paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_deaths = unlist(df$Deaths)", sep = "")))
  eval(parse(text = paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_recovered = unlist(df$Recovered)", sep = "")))
  eval(parse(text = paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_active = unlist(df$Active)", sep = "")))
  eval(parse(text = 
         paste("deathtorecovery[[counter]] = cor.test(", 
               paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_deaths", sep = ""),
               ", ",
               paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_recovered", sep = ""),
               ", method = 'pearson'",
               ")",
               sep = ""
         )
  ))
  eval(parse(text = 
         paste("activetodeath[[counter]] = cor.test(", 
               paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_active", sep = ""),
               ", ",
               paste(str_replace_all(as.character(df[1,1]), "[' '-]", ""), "_deaths", sep = ""),
               ", method = 'pearson'",
               ")",
               sep = ""
         )
  ))
}
deathtorecovery

activetodeath 
## Region specific hypothesis test for South East Asia on 01-06-2020 vs Previous Period

region_hypo = cleaned_data %>% filter(WHO.Region == "South-East Asia")
region_hypo["Recovery Rate"] = region_hypo$Recovered/(region_hypo$Recovered + region_hypo$Deaths) # Recovery Rate = Recovered/(Recovery + Deaths)

# Filtering Date on 01-06-2020
data_june_one = region_hypo %>% filter(Date == as.Date("2020-06-01"))

# Filtering data from 22-01-2020 to 31-05-2020 and finding mean recovery rate
prev_period_data = region_hypo %>% filter(Date >= as.Date("2020-01-22") & Date <= as.Date("2020-05-31"))
mean_prev_period = mean(prev_period_data$`Recovery Rate`)
mean_prev_period
# Performing t-test
t.test(data_june_one$`Recovery Rate`, mu = mean_prev_period)

## Hypothesis test for mean deaths: March 2020 vs June 2020 for South East Asia

# Filtering for March and June and taking mean deaths for June
march2020 = region_hypo %>% filter(Date >= as.Date("2020-03-01") & Date <= as.Date("2020-03-31"))
june2020 = region_hypo %>% filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-06-30"))
mean_june2020 = mean(june2020$Deaths)
mean_june2020
t.test(march2020$Deaths, mu = mean_june2020, alternative = "greater")

## Linear Model

# Checking for Normality of Variance using QQ Plots

# Region Wise QQ Plot from Regional Dataset
ggplot(regional_data, aes(sample = regional_data$Deaths)) + 
  stat_qq(colour = colfunc(1128)) + 
  geom_qq_line(colour = colfunc1(12)) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) +
  theme_bw() +
  theme(strip.background = element_rect(colour="black", fill="white")) + 
  labs(x = "Theoretical", y = "Deaths") + 
  ggtitle("From Grouped Regional Data") + 
  theme(plot.title = element_text(hjust = 0.5))

# Country Wise QQ Plot from Original dataset
ggplot(cleaned_data, aes(sample = cleaned_data$Deaths)) + 
  stat_qq() + 
  geom_qq_line() + 
  facet_wrap(~ Country.Region, scales = c("free")) +
  labs(y = "Deaths")

# Region Wise QQ Plot from Original dataset
ggplot(cleaned_data, aes(sample = cleaned_data$Deaths)) + 
  stat_qq(colour = colfunc(35156)) + 
  geom_qq_line(colour = colfunc1(12)) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) +
  theme_bw() +
  theme(strip.background = element_rect(colour="black", fill="white")) + 
  labs(x = "Theoretical", y = "Deaths") + 
  ggtitle("From Cleaned Data") + 
  theme(plot.title = element_text(hjust = 0.5))

# Preparing Model using Regional Dataset
regional_data$WHO.Region = as.factor(regional_data$WHO.Region)
deaths_model = lm(Deaths ~ WHO.Region + Date, data = regional_data)
summary(deaths_model)

# Parameters here are Region and Date
new_data = data.frame(WHO.Region = unique(regional_data$WHO.Region))

# Prediction
list_africa = vector("numeric", 0) # Making lists to store daily predictions
list_america = vector("numeric", 0)
list_east_mediterranean = vector("numeric", 0)
list_europe = vector("numeric", 0)
list_southeast_asia = vector("numeric", 0)
list_west_pacific = vector("numeric", 0)

dates = date_generator(as.Date("2020-08-01"), 29)

# Looping through predictions on each day and appending to the list
for (d in dates) {
  print(d)
  new_data["Date"] = as.Date(d)
  prediction = predict.lm(deaths_model, newdata = new_data)
  list_africa = c(list_africa, as.numeric(prediction[1]))
  list_america = c(list_america, as.numeric(prediction[2]))
  list_east_mediterranean = c(list_east_mediterranean, as.numeric(prediction[3]))
  list_europe = c(list_europe, as.numeric(prediction[4]))
  list_southeast_asia = c(list_southeast_asia, as.numeric(prediction[5]))
  list_west_pacific = c(list_west_pacific, as.numeric(prediction[6]))
  }

# Taking sum of daily predictions for each region to get total predicted deaths for October.
aug_prediction = c(
  round(sum(list_africa), 0),
  round(sum(list_america), 0), 
  round(sum(list_east_mediterranean), 0), 
  round(sum(list_europe), 0),
  round(sum(list_southeast_asia), 0), 
  round(sum(list_west_pacific), 0)
)

aug_prediction

# Residual Analysis
ggplot(regional_data, mapping = aes(x = deaths_model$fitted.values, y = deaths_model$residuals)) + 
  geom_point(colour = colfunc(1128)) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) + 
  theme_bw() +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  labs(x = "Fitted Values", y = "Residuals") + 
  ggtitle("Residuals vs Fitted Values") + 
  theme(plot.title = element_text(hjust = 0.5))

# QQ Plot
ggplot(regional_data, mapping = aes(sample = deaths_model$residuals)) + 
  stat_qq(colour = colfunc(1128)) + 
  geom_qq_line(colour = "red") +
  theme_bw() + 
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  ggtitle("Residual QQ Plot") + 
  theme(plot.title = element_text(hjust = 0.5))

## Pie Charts
pie_data = regional_data %>% group_by(across(c("WHO.Region"))) %>% 
  summarise(
    Deaths = sum(Deaths),
    Confirmed = sum(Confirmed),
    Recovered = sum(Recovered),
    Active = sum(Active)
  )

# Deaths
ggplot(pie_data, aes(x="", y=Deaths, fill=WHO.Region)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  ggtitle("Deaths") +
  labs(x = "", y = "") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))

# Confirmed
ggplot(pie_data, aes(x="", y=Confirmed, fill=WHO.Region)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  ggtitle("Confirmed") +
  labs(x = "", y = "") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))

# Active
ggplot(pie_data, aes(x="", y=Active, fill=WHO.Region)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  ggtitle("Active") +
  labs(x = "", y = "") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))

# Recovered
ggplot(pie_data, aes(x="", y=Recovered, fill=WHO.Region)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  ggtitle("Recovered") +
  labs(x = "", y = "") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))

# Analysing Variance for the month of May and August.

# Making predictions for confirmed cases for August 2020.
cases_model = lm(Confirmed ~ WHO.Region + Date, data = regional_data)
new_data_c = data.frame(WHO.Region = unique(regional_data$WHO.Region))
list_africa_cases = vector("numeric", 0) # Making lists to store daily predictions
list_america_cases = vector("numeric", 0)
list_east_mediterranean_cases = vector("numeric", 0)
list_europe_cases = vector("numeric", 0)
list_southeast_asia_cases = vector("numeric", 0)
list_west_pacific_cases = vector("numeric", 0)

dates_c = date_generator(as.Date("2020-08-01"), 29)

# Looping through predictions on each day and appending to the list
for (d in dates_c) {
  new_data_c["Date"] = as.Date(d)
  prediction = predict.lm(cases_model, newdata = new_data)
  list_africa_cases = c(list_africa_cases, as.numeric(prediction[1]))
  list_america_cases = c(list_america_cases, as.numeric(prediction[2]))
  list_east_mediterranean_cases = c(list_east_mediterranean_cases, as.numeric(prediction[3]))
  list_europe_cases = c(list_europe_cases, as.numeric(prediction[4]))
  list_southeast_asia_cases = c(list_southeast_asia_cases, as.numeric(prediction[5]))
  list_west_pacific_cases = c(list_west_pacific_cases, as.numeric(prediction[6]))
}

# Taking sum of daily predictions for each region to get total predicted deaths for August.
aug_prediction_c = c(
  sum(list_africa_cases),
  sum(list_america_cases), 
  sum(list_east_mediterranean_cases), 
  sum(list_europe_cases), 
  sum(list_southeast_asia_cases), 
  sum(list_west_pacific_cases)
)
aug_prediction_c

# Making the dataframe for confirmed cases in August and May
regions = c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")
august_cases = data.frame(WHO.Region = rep(regions, times = rep(30, 6)), 
                          Date = rep(dates_c, 6), 
                          Confirmed = c(
                          list_africa_cases, 
                          list_america_cases,
                          list_east_mediterranean_cases,
                          list_europe_cases,
                          list_southeast_asia_cases,
                          list_west_pacific_cases
                          ))

may_cases = regional_data %>% filter(Date >= as.Date("2020-05-01"), Date <= as.Date("2020-05-30"))

# Performing Analysis of Variance
anova(lm(may_cases$Confirmed ~ august_cases$Confirmed))

# Correlation between Confirmed and Active
regional_correlations = vector("numeric", 0)
for (region in regions) {
  cor_data = regional_data %>% filter(WHO.Region == region)
  regional_correlations = c(regional_correlations, cor(cor_data$Confirmed, cor_data$Active))
}
regional_correlations

ggplot(cleaned_data, mapping = aes(x = Confirmed, y = Active)) +
  geom_point(color = "#FA8072") + 
  geom_line(color = "blue", linetype = 1) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) +
  theme_bw() +
  labs(x = "Confirmed Cases", y = "Active Cases") + 
  ggtitle("Active Cases vs Confirmed Cases") + 
  theme(plot.title = element_text(hjust = 0.5), strip.background = element_rect(colour="black", fill="white")) + 
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)
