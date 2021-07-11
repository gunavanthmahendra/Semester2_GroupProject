library(tidyverse)
library(MASS)
library(stats)
## Reading the csv file
insurance_data = read.csv(file.choose())
head(insurance_data)

## data cleaning
data = insurance_data

data$age= as.numeric(data$age)

data$sex = as.factor(data$sex)
#data$sex = as.numeric(data$sex)

data$bmi = as.numeric(data$bmi)

data$children = as.numeric(data$children)

data$smoker= as.factor(data$smoker)
#data$smoker=as.numeric(data$smoker)

data$region = as.factor(data$region)
#data$region = as.numeric(data$region)

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
ex = data.frame(regional_insurance[
  with(regional_insurance, order(regional_insurance$charges, decreasing = TRUE)),
])
write.csv(ex, "ex.csv")
# From the data we see that the southeast contributes the most to the total charges collected 
# and has the highest mean BMI and highest number of smokers. 
# Similarly, we could target regions based on the charges collected historically, number of smokers and average BMI.
# Given the data it would be reasonable to tagger the southeast.

## 2

smokers = (
  filter(insurance_data, smoker == "yes")
)
nrow(smokers)

non_smokers = (
  filter(insurance_data, smoker == "no")
)
nrow(non_smokers)

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
pricing_model = lm(charges ~ age + bmi + smoker, data = data)

summary(pricing_model)

# Plots for model
ggplot(data = data, mapping = aes(x = fitted(pricing_model), y = residuals(pricing_model))) + 
  geom_point(col = colfunc(1338)) + 
  theme_bw() + 
  ggtitle("Residuals vs Fitted") + 
  labs(x = "Fitted Values", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, mapping = aes(sample = pricing_model$residuals)) + 
  geom_qq(color = colfunc(1338)) + 
  geom_qq_line(color = colfunc1(1)) +
  ggtitle("QQ Plot of Residuals") + 
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  theme(plot.title = element_text(hjust = 0.5))
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

## b) Charges~ BMI
cor_test_BMI <- cor.test(cor_data$charges, cor_data$bmi)
cor_test_BMI
##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and BMI and
## it is not equal to zero.
## Though as the value of correlation is only 0.2 there is only a weak correlation

## c) Charges children
cor_test_children<- cor.test(cor_data$charges, cor_data$children)
cor_test_children

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and Children and
## it is not equal to zero.
## Though, as the value of correlation is 0.06 there is a very weak correlation

## d) Charges smoker
cor_test_smoker<- cor.test(cor_data$charges, cor_data$smoker)
cor_test_smoker

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and smoker and
## it is not equal to zero.
## As the value of correlation is 0.8 there is a very strong correlation

## e) Charges~ Region
cor_test_region<- cor.test(cor_data$charges, cor_data$region)
cor_test_region
##As the P value is lesser than 0.08 we don't have enough evidence to reject 
## H0 and conclude that there is no correlation between Charges and region and
## it is equal to zero.

##As the P value is lesser than 0.05 we  have enough evidence to reject 
## H0 and conclude that there is correlation between Charges and age and
## it is not equal to zero.
## Though as the value of correlation is only 0.3 there is only a weak correlation

## 5 GLM model 
data["log_charges"] = log(data$charges)
model_data = data %>% group_by(across(c("age", "sex", "smoker", "region"))) %>%
  summarise(
    log_charges = mean(log_charges),
    children = sum(children), 
    bmi = mean(bmi),
    charges = mean(charges)
    )

predictor_model = glm(log_charges ~  smoker + sex + age + bmi +
                                    sex:smoker + 
                                    region:smoker + 
                                    age:smoker +
                                    age:bmi, 
                                data = model_data)

summary(predictor_model)

dropterm(predictor_model, sort = TRUE)

# Checking Model Fit

model_plot = ggplot(data = model_data, mapping =  aes(x = fitted(predictor_model), y = residuals(predictor_model))) 

# Residuals vs Fitted

model_plot + 
  geom_point(color = colfunc(1)) + 
  geom_hline(yintercept = mean(residuals(predictor_model)), color = "red") +
  theme_bw() +
  ggtitle("Residuals vs Fitted") +
  labs(x = "Fitted Values", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = model_data, mapping = aes(sample = residuals(predictor_model))) + 
  geom_qq(color = colfunc(566)) + 
  geom_qq_line(color = colfunc1(1)) +
  ggtitle("QQ Plot of Residuals") + 
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  theme(plot.title = element_text(hjust = 0.5))

# Sample Predictions

prediction_data = data.frame(region = unique(model_data$region), 
                             smoker = c("yes", "yes", "no", "no"), 
                             sex = c("male", "female", "male", "female"), 
                             age = c(35, 50, 40, 19), 
                             bmi = c(23, 30, 27, 37))

prediction_data$sex = as.factor(prediction_data$sex)
prediction_data$smoker = as.factor(prediction_data$smoker)
prediction_data$region = as.factor(prediction_data$region)
predicted_charges = exp(predict.glm(predictor_model, newdata = prediction_data))
predictions = prediction_data
predictions["Estimated Charge"] = predicted_charges

regional_effect = data.frame(region = unique(model_data$region), 
                             smoker = "yes", 
                             sex = "male", 
                             age = 35, 
                             bmi = 25)

regional_effect$smoker = as.factor(regional_effect$smoker)
regional_effect$sex = as.factor(regional_effect$sex)
regional_effect$region = as.factor(regional_effect$region)
regional_predict = exp(predict.glm(predictor_model, newdata = regional_effect))
regional_effect["Estimated Charge"] = regional_predict

# Exporting the Results

write.csv(regional_effect, "regional_prediction.csv")
write.csv(predictions, "predictions.csv")
