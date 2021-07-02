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
seasonal_data = cleaned_data
seasonal_data["Season"] = ""
i = 1
while (i <= nrow(data.frame(seasonal_data))) {
  if (seasonal_data$Date[i] %in% winter) {
    seasonal_data$Season[i] = "Winter"
  } 
  else if (seasonal_data$Date[i] %in% summer) {
    seasonal_data$Season[i] = "Summer"
  } 
  else if (seasonal_data$Date[i] %in% monsoon) {
    seasonal_data$Season[i] = "Monsoon"
  }
  i = i + 1
}

regional_data = seasonal_data %>% 
  group_by(across(c("WHO.Region", "Date"))) %>%
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active = sum(Active),
  )  

# Making the plots
base_line_plot = ggplot(regional_data, mapping = aes(x = Date, y = Confirmed))

base_line_plot + geom_smooth(color = "#C70039", se = FALSE) + 
  facet_wrap(~ WHO.Region, nrow = 2, scales = c("free")) + 
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "", y = "Confirmed Cases")
