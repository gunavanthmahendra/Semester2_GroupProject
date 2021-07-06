library(tidyverse)
## Reading the csv file
insurance_data<- read.csv(file.choose())
head(insurance_data)

## data cleaning
data=insurance_data

data$age= as.numeric(data$age)

data$sex = as.factor(data$sex)
data$sex = as.numeric(data$sex)

data$bmi = as.numeric(data$bmi)

data$children = as.numeric(data$children)

data$smoker= as.factor(data$smoker)
data$smoker=as.numeric(data$smoker)

data$region=as.factor(data$region)
data$region=as.numeric(data$region)

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
pricing_model<- lm(data$charge~data$age+ data$sex +data$bmi + data$children + 
                    data$smoker + data$region)

summary(pricing_model)

##xi= age,sex,bmi,children,smoker,region
##Y= charge
## the linear equation is 
## the linear model is
## charge= -35151.14+(-353.64)(region)+23820(smoker)+479.37(children)+332.57(bmi)
##         =(-.131.11)(sex)+257.29(age)


##4. correlation tests

## Hypothesis for the correlation tests
## H0: there is not correlation ie correlation=0
## H1; there exists some correlation and i.e., correlation=!0

## a) charges ~ age
cor_test_age<- cor.test(data$charges,data$age)
cor_test_age

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and age and
## it is not equal to zero.
## Though as the value of correlation is only 0.3 there is only a weak correlation

## b) Charges~ BMI
cor_test_BMI<- cor.test(data$charges, data$bmi)
cor_test_BMI
##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and BMI and
## it is not equal to zero.
## Though as the value of correlation is only 0.2 there is only a weak correlation

## c) Charges children
cor_test_children<- cor.test(data$charges, data$children)
cor_test_children

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and Children and
## it is not equal to zero.
## Though, as the value of correlation is 0.06 there is a very weak correlation

## d) Charges smoker
cor_test_smoker<- cor.test(data$charges, data$smoker)
cor_test_smoker

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and smoker and
## it is not equal to zero.
## As the value of correlation is 0.8 there is a very strong correlation

## e) Charges~ Region
cor_test_region<- cor.test(data$charges, data$region)
cor_test_region
##As the P value is lesser than 0.08 we don't have enough evidence to reject 
## H0 and conclude that there is no correlation between Charges and region and
## it is equal to zero.


## 5 GLM model 
pricing_model_after_changes = glm(log_charges ~ age + sex + bmi + children + smoker + region +
                                 age * smoker +
                                 age * sex +
                                 bmi * smoker + 
                                 age * children +
                                 age * region, 
                                data = data)

summary(pricing_model_after_changes)

##xi= age,sex,bmi,smoker
##Y= charge
## the linear equation is 
## the linear model is
## charge= -35151.14+23833.87(smoker)323.05(bmi)+(-109.11)(sex)+259.45(age)

glm(log_charges ~ age + sex + bmi + children + smoker + region +
      age * smoker +
      age * sex +
      bmi * smoker + 
      age * children +
      age * region, 
    data = data)