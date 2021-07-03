setwd("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject")
data = read.csv("C:\\Users\\guna3\\Desktop\\Semester2_GroupProject\\data_files\\covid_19.csv")
data$Date = as.Date(data$Date, "%d-%m-%Y")
## Aggregating the data based on Country and Date.
cleaned_data = data %>% group_by(across(c("Country.Region", "Date"))) %>% 
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

## Grouping by Season and WHO Regions
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
base_line_plot + geom_smooth(color = "#C70039") + 
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

## Region specific hypothesis test for South East Asia on 01-06-2020 vs Previous Period

region_hypo = cleaned_data %>% filter(WHO.Region == "South-East Asia")
region_hypo["Recovery Rate"] = region_hypo$Recovered/(region_hypo$Recovered + region_hypo$Deaths)

# Filtering Date on 01-06-2020
data_june_one = region_hypo %>% filter(Date == as.Date("2020-06-01"))

# Filtering data from 22-01-2020 to 31-05-2020 and finding mean recovery rate
prev_period_data = region_hypo %>% filter(Date >= as.Date("2020-01-22") & Date <= as.Date("2020-05-31"))
mean_prev_period = mean(prev_period_data$`Recovery Rate`)

# Performing t-test
t.test(data_june_one$`Recovery Rate`, mu = mean_prev_period)

## Hypothesis test for mean deaths: March 2020 vs June 2020 for South East Asia

# Filtering for March and June and taking mean deaths for June
march2020 = region_hypo %>% filter(Date >= as.Date("2020-03-01") & Date <= as.Date("2020-03-31"))
june2020 = region_hypo %>% filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-06-30"))
mean_june2020 = mean(june2020$Deaths)
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
  labs(y = "Deaths")

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
  labs(y = "Deaths")

# Preparing Model using Regional Dataset
deaths_model = lm(Deaths ~ WHO.Region + Date, data = regional_data)
new_data = data.frame(WHO.Region = unique(regional_data$WHO.Region))

# Prediction
new_data["Date"] = as.Date("2020-08-01")
predict.lm(deaths_model, newdata = new_data)

# Residual Analysis
ggplot(regional_data, mapping = aes(x = regional_data$Date, y = deaths_model$residuals)) + 
  geom_point(colour = colfunc(1128)) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) + 
  theme_bw() +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "Date", y = "Deaths")

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