# Chapter 1, Applied Exercises

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
