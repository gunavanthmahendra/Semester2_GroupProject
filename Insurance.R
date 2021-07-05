## Reading the csv file
insurance_data<- read.csv(choose.files())
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

head(data)


## 3 Pricing model
## checking the fit for linear model

## checking for normality
## As BMI is the only continuous variable we check the normality for only bmi
ggdensity(data$bmi, xlab ="bmi")
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
pricing_model_after_changes<-lm(data$charge~data$age+ data$sex +data$bmi+data$smoker )
summary(pricing_model_after_changes)

##xi= age,sex,bmi,smoker
##Y= charge
## the linear equation is 
## the linear model is
## charge= -35151.14+23833.87(smoker)323.05(bmi)+(-109.11)(sex)+259.45(age)








