library(tidyverse)
## Reading the csv file
insurance_data<- read.csv(file.choose())
head(insurance_data)

## data cleaning
data=insurance_data

data$age= as.numeric(data$age)

data$sex = as.factor(data$sex)
#data$sex = as.numeric(data$sex)

data$bmi = as.numeric(data$bmi)

data$children = as.numeric(data$children)

data$smoker= as.factor(data$smoker)
#data$smoker=as.numeric(data$smoker)

data$region = as.factor(data$region)
#data$region = as.numeric(data$region)

data["log_charges"] = log(data$charges)
head(data)

## 1 
regional_insurance = insurance_data
regional_insurance$smoker= as.factor(data$smoker)
regional_insurance$smoker=as.numeric(data$smoker)
regional_insurance$charges = as.numeric(regional_insurance$charges)
regional_insurance = data.frame(regional_insurance %>% group_by(across(c("region"))) %>% 
  summarise(
    age = mean(age),
    bmi = mean(bmi),
    smoker = sum(smoker),
    charges = sum(charges)
    
  ))
regional_insurance[
  with(regional_insurance, order(regional_insurance$charges, decreasing = TRUE)),
]

# From the data we see that the southeast contributes the most to the total charges collected 
# and has the highest mean BMI and highest number of smokers. 
# Similarly, we could target regions based on the charges collected historically, number of smokers and average BMI.
# Given the data it would be reasonable to tagger the southeast.

## 2

smokers = (
  filter(insurance_data, smoker == "yes")
)
smokers

non_smokers = (
  filter(insurance_data, smoker == "no")
)
non_smokers

smoking_data = insurance_data %>% group_by(across(c("smoker"))) %>%
  summarise(
    age = mean(age),
    bmi = mean(bmi),
    charges = sum(charges)
  )
smoking_data[
  with(smoking_data, order(charges, decreasing = TRUE)),
]

# We see that non_smokers are responsible for more charges paid, 
# but this could also be down to the greater number of non_smokers in the data set.
# We perform a Hypothesis test to check if smokers should be charged more.

# H0: Mean smokers charges = Mean non-smokers charges
# H1: Mean smokers charges > Mean non-smokers charges

t.test(smokers$charges, non_smokers$charges, alternative = "greater")

# There is sufficient evidence to reject the null hypothesis at the 95% level. 
# Hence it is reasonable to assume that, smokers pay more charges on average.
# It would be advisable to charge smokers more.

## 3 Pricing model
## checking the fit for linear model

## checking for normality
## As BMI is the only continuous variable we check the normality for only bmi
ggplot(data = data, mapping = aes(x = bmi)) + 
  geom_density(fill = "#FA8072", color = "#C70039") + 
  theme_bw() + 
  labs(x = "BMI", y = "Density")


## BMI is normally distributed where the concentration of the data is in
## the center and thinner at the tails.
pricing_model<- lm(charges ~ age + sex + bmi + children + 
                    smoker + region, data = data)

summary(pricing_model)

## xi= age,sex,bmi,children,smoker,region
## Y= charge
## the linear equation is 
## the linear model is
## charge= -35151.14+(-353.64)(region)+23820(smoker)+479.37(children)+332.57(bmi)
##         =(-.131.11)(sex)+257.29(age)


##4. correlation tests
cor_data = data
for (col in colnames(cor_data)) {
  if (is.factor(unlist(cor_data[col]))) {
    print(paste(col, " is a factor"))
    eval(parse(text = paste("cor_data$", col, " = ", "as.numeric(cor_data$", col, ")", sep = "")))
    
  }
}

## Hypothesis for the correlation tests
## H0: there is not correlation ie correlation=0
## H1; there exists some correlation and i.e., correlation=!0

## a) charges ~ age
cor_test_age <- cor.test(cor_data$charges, cor_data$age)
cor_test_age

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and age and
## it is not equal to zero.
## Though as the value of correlation is only 0.3 there is only a weak correlation




## 5 GLM model 
model_data = data %>% group_by(across(c("region","smoker", "sex"))) %>%
  summarise(
    log_charges = mean(log_charges), 
    age = mean(age), 
    children = sum(children), 
    bmi = mean(bmi)
    )

predictor_model = glm(log_charges ~  smoker + sex + children +
                                    sex:smoker + 
                                    region:smoker, 
                                data = model_data)

summary(predictor_model)

dropterm(predictor_model, sort = TRUE)

# Checking Model Fit

model_plot = ggplot(data = model_data, mapping =  aes(x = fitted(predictor_model), y = residuals(predictor_model))) 

# Residuals vs Fitted

model_plot + 
  geom_point(color = colfunc(1)) + 
  theme_bw() +
  ggtitle("Residuals vs Fitted") +
  labs(x = "Fitted Values", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5))

# QQ Plot
ggplot(mapping = aes(sample = residuals(predictor_model))) +
  geom_qq(color = colfunc(1)) + 
  geom_qq_line(color = "red") + 
  theme_bw() + 
  ggtitle("QQ Plot") +
  labs(x = "Theoretical", y = "Sample") + 
  theme(plot.title = element_text(hjust = 0.5))


