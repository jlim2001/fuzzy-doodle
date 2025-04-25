##### ST1131 ASSIGNMENT 2 #####
getwd()
table = read.csv("hdb-2012-to-2014.csv")
attach(table)
summary(table)

##### summarise response variable #####
#Create Histogram of resale price to see if it is normally distributed
hist(resale_price)
#Transformation of response variable 
hist(log(resale_price))


##### Explanatory Variables to be investigated #####
#month, town, flat type, flate model, lease commence date, storey range, floor area

##### Check association between response variable and other variable #####

# floor area 
floor_area = lm(log(resale_price)~floor_area_sqft, data = table)
summary(floor_area)
plot(floor_area_sqft, log(resale_price), col = 2)
title('log Resale Price vs floor area')
cor(floor_area_sqft, log(resale_price))

# month 
month_variable = lm(log(resale_price)~factor(month), data = table)
summary(month_variable)
boxplot(log(resale_price) ~ factor(month) )
title('log Resale Price vs Month')

# lease date 
lease_date = lm(log(resale_price)~factor(lease_commence_date), data = table)
summary(lease_date)
boxplot(log(resale_price) ~ factor(lease_commence_date) )
title('log Resale Price vs Lease Commence Date')

# flat type 
type_of_flat = lm(log(resale_price)~factor(flat_type), data = table)
summary(type_of_flat)
boxplot(log(resale_price) ~ factor(flat_type) )
title('log Resale Price vs Flat Type')

# town
town_area = lm(log(resale_price)~factor(town), data = table)
summary(town_area)
boxplot(log(resale_price) ~ factor(town) )
title('log Resale Price vs town')

# storey range
storey = lm(log(resale_price)~factor(storey_range), data = table)
summary(storey)
boxplot(log(resale_price) ~ factor(storey_range) )
title('log Resale Price vs Storey Range')

# flat model
model_type = lm(log(resale_price)~factor(flat_model), data = table)
summary(model_type)
boxplot(log(resale_price) ~ factor(flat_model) )
title('log Resale Price vs Flat Model')


##### Propose Regressors, fit the model , #####
#report the goodness of fit #
#floor_area_sqft, flat_type, town, storey_range, flat_model, lease significant from 1994 onwards
M1 = lm(log(resale_price) ~ floor_area_sqft + flat_type + flat_model + town + storey_range + month + lease_commence_date + floor_area_sqft * flat_type, table)
summary(M1)
anova(M1)
#Adj Rsq: 0.9198
# 91.98% of the variation in resale_price is explained by this regression model. 



##### Check m1 using residual plots. #####

#test for normality
SR1 = rstandard(M1)
qqnorm(SR1)
qqline(SR1, col = "red")
shapiro.test(SR1[1:5000])

hist(SR1)

# Linearity 
plot(floor_area_sqft, log(resale_price))
title('log Resale Price vs Floor Area')

#constant variance 
plot(floor_area_sqft, SR1)

plot(M1$fitted.values, SR1)
title('Constant Variance Assumption')

#cooks distance
plot(cooks.distance(M1))
title('Cooks Distance')

##### Test M2 removed month and story range 
M2 = lm(log(resale_price) ~ floor_area_sqft + town + flat_type + flat_model + lease_commence_date + floor_area_sqft * flat_type, table)
summary(M2)
anova(M2) 

# 87.61% of the variation in resale_price is explained by this regression model. 

#test for normality
SR2 = rstandard(M2)
qqnorm(SR2)
qqline(SR2, col = "red")
shapiro.test(SR2[1:5000])

hist(SR2)

# Linearity 
plot(floor_area_sqft, log(resale_price))

#constant variance 
plot(M3$fitted.values, SR2)

#cooks distance
plot(cooks.distance(M2))

##### check m3 ##### remove flat model
M3 = lm(log(resale_price) ~ floor_area_sqft + town + flat_type + lease_commence_date + floor_area_sqft * flat_type, table)
summary(M3)
anova(M3)

# 87.04% of the variation in resale_price is explained by this regression model.

#test for normality
SR3 = rstandard(M3)
qqnorm(SR3)
qqline(SR3, col = "red")
shapiro.test(SR3[1:5000])

hist(SR3)

# Linearity 
plot(floor_area_sqft, log(resale_price))

#constant variance 
plot(M3$fitted.values, SR3)

#cooks distance
plot(cooks.distance(M3))


##### m4 ##### - removed lease commence date
M4 = lm(log(resale_price) ~ floor_area_sqft + town + flat_type + floor_area_sqft * flat_type, table)
summary(M4)
anova(M4)

# 85.84% of the variation in resale_price is explained by this regression model.

#test for normality
SR4 = rstandard(M4)
qqnorm(SR4)
qqline(SR4, col = "red")
shapiro.test(SR4[1:5000])

hist(SR4)

# Linearity 
plot(floor_area_sqft, log(resale_price))

#constant variance 
plot(M4$fitted.values, SR4)

#cooks distance
plot(cooks.distance(M4))




#test m5 #remove associated variable floor_area_sqft * flat_type
M5 = lm(log(resale_price) ~ floor_area_sqft + flat_type + town, table)
summary(M5)
anova(M5)

# 85.43% of the variation in resale_price is explained by this regression model.

#test for normality
SR5 = rstandard(M5)
qqnorm(SR5)
qqline(SR5, col = "red")
shapiro.test(SR5[1:5000])

hist(SR5)

# Linearity 
plot((floor_area_sqft), log(resale_price))
title('log Resale Price vs Floor Area')

#constant variance 
plot(M5$fitted.values, SR5)
title('Constant Variance')
#cooks distance
plot(cooks.distance(M5))
title("Cook's Distance")

# Finding influential points twice the mean Cook's Distance 
which(SR5>3 |SR5< (-3))
C = cooks.distance(M5)
which(C>2*mean(C))
influential_points <- c(which(C>2*mean(C)))
new_data <- table[-influential_points,]

##### m6 ###### remove influential points
M6 = lm(log(resale_price) ~ floor_area_sqft + flat_type + town, new_data)
summary(M6)
anova(M6)

# 88.99% of the variation in resale_price is explained by this regression model.

#test for normality
SR6 = rstandard(M6)
qqnorm(SR6)
qqline(SR6, col = "red")
shapiro.test(SR6[1:5000])

hist(SR6)


# Linearity 
plot((floor_area_sqft), log(resale_price))
title('log Resale Price vs Floor Area')

#constant variance 
plot(M6$fitted.values, SR6)
title('Constant Variance')
#cooks distance
plot(cooks.distance(M6))
title("Cook's Distance")

