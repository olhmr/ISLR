# Chapter 1, Applied Exercises

## Question 8 ##

# 8.a. and 8.b. aren't fully applicable as we're loading the data straight from the ISLR package,
# where these steps have already been performed. We do the steps below:
#install.packages("ISLR")
library(ISLR)
college <- ISLR::College
fix(college)

# 8.c.i.
summary(college)

# 8.c.ii.
pairs(college[, 1:10])

# 8.c.iii.
plot(data = college, Outstate ~ Private)

# 8.c.iv.
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college)
plot(data = college, Outstate ~ Elite)

# 8.c.v.
par(mfrow = c(2,2))
hist(x = college$Outstate, bins = 50)
hist(x = college$Accept, bins = 10)
hist(x = college$Personal, bins = 25)
hist(x = college$PhD, bins = 30)

# 8.c.vi.
# There appears to be some correlation between the out of state tuition and the proportion of new students
# from the top 10 and top 25 percent of their high school classes, although in the case of top 10 percent, 
# peak outstate values are reached much quicker than for top 25 percent.
par(mfrow = c(1, 2))
plot(data = college, Outstate ~ Top10perc)
plot(data = college, Outstate ~ Top25perc)

# There also appears to be a trend where a higher percentage of alumni donating is correlated with higher 
# out of state tuition.
plot(data = college, Outstate ~ perc.alumni)

# However, there appears to be no correlation between book costs and personal costs, although there are 
# a couple of outliers for both variables that may be worth investigating.
plot(data = college, Books ~ Personal)

# Private colleges generally have a lower student-to-faculty ratio, although there are plenty of outliers 
# and the highest student-to-faculty ratio is found in a private college.
plot(data = college, S.F.Ratio ~ Private)

## Question 9 ##
auto <- ISLR::Auto
sum(is.na(auto)) # Check that no NAs

# 9.a.
str(auto)
unique(auto$cylinders)
unique(auto$origin)
# mpg, displacement, horsepower, weight, acceleration, and year appear quantitative
# cylinders, origin, and name appear qualitative
# cylinders and origin are numeric, but the limited number of options for each indicate that they are 
# categorial. year could also feasibly be considered categorial, but since there could be value in a 
# quantitative regression run on year we will consider it as quantitative.

# 9.b.
lapply(X = auto[, c(1, 3, 4, 5, 6, 7)], FUN = range)

# 9.c.
lapply(X = auto[, c(1, 3, 4, 5, 6, 7)], FUN = mean)
lapply(X = auto[, c(1, 3, 4, 5, 6, 7)], FUN = sd)

# 9.d.
auto_sub <- auto[-(10:85), ]
lapply(X = auto_sub[, c(1, 3, 4, 5, 6, 7)], FUN = range)
lapply(X = auto_sub[, c(1, 3, 4, 5, 6, 7)], FUN = mean)
lapply(X = auto_sub[, c(1, 3, 4, 5, 6, 7)], FUN = sd)

# 9.e.
pairs(auto)
par(mfrow = c(2,2))
plot(data = auto, mpg ~ weight)
plot(data = auto, mpg ~ year)
plot(data = auto, weight ~ year)
plot(data = auto, acceleration ~ year)
# Heavier vehicles generally get fewer miles per gallon, but mpg has improved in later generations of cars.
# It is true that newer cars are generally lighter, but it seems there may be some other cause of improved
# mpg performance - perhaps in engine efficiency. Acceleration has remained mostly constant in newer
# generations of cars, suggesting that improved mpg performance is not coming at the cost of weaker engines.

# 9.f.
pairs(auto)
# Moving on from the insights from 9.e., there definitely seems to be a correlation between displacement, 
# horsepower, weight, year, and cylinders with mpg. However, there is definitely some synergy effects here, 
# since displacement, horsepwower, weight, and, to some extent, cylinders, have an almost perfectly linear 
# relationship with eachother. As such, we would likely only use one of these variables as the predictor,
# in addition to year.
# An argument could be made that origin is also of interest, but the correlation appears to be quite weak.

## Question 10 ##
library(MASS)
boston <- MASS::Boston

# 10.a.
?MASS::Boston
# 506 rows, 14 columns
# Rows are observations, where each row is one town
# Columns are predictors such as crime rate, tax rate, etc., along with the response medv (median house value)

# 10.b.
pairs(boston)
par(mfrow = c(2, 2))
plot(data = boston, medv ~ rm)
plot(data = boston, medv ~ crim)
plot(data = boston, dis ~ rad)
plot(data = boston, age ~ dis)
# unsurprisingly, higher average rooms by house appears associated with higher median house prices. Again 
# unexpectedly, higher crime rates appear to preclude high house values. The radial highways appear to be 
# quite close to the city centre, as the towns with the best access to the highways had the shortest 
# distance to employment centres. In addition, the oldest houses tended to be closer to the centres, 
# suggesting Boston was small and grew outwards, as opposed to being an amalgamation of several smaller
# cities.

# 10.c. 
