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
