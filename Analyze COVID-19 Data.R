# Remove all the previous variables that were loaded
rm(list=ls())

# Install Hmisc library
install.packages("Hmisc")
# Import the library
library(Hmisc)

# Read-in the data from our files; this will look different
# depending on where the data is stored in your computer
data <- read.csv("~/BioinformaticsLab/R Scripts/COVID-19 R Project/COVID19_line_list_data (1).csv")

# Use the Hmisc command describe() on the data
# There a couple of things that we observe in the death description -> there are
# 14 distinct values, which doesn't make sense since we are only stating if 
# there was a death (1) or not (0). Some entries write the date of the death.
# We need to clean this up. 

# We will create a dummy in the data that will only state if there was a
# death or not. This states that where the cell doesn't have a value of 0, put
# it as a 1.
data$death_dummy <- as.integer(data$death != 0)

# We can check if the values only have 1 and 0 in the dummy column:
unique(data$death_dummy)

# In order to calculate the death rate, we need to use the counts from the dummy
# column and the total number of people that contracted COVID:

# death rate
sum(data$death_dummy == 1) / nrow(data) #0.05806452

# AGE
# claim: people who die are older than people who survive

# We are going to create a subset of the death data and life data
dead = subset(data, death_dummy == 1)
life = subset(data, death_dummy == 0)

# Now we are going to calculate the mean age of the people that died 
# and people that survived
mean(dead$age) # NA
mean(life$age) # NA

# We get NA for both of these commands because there are 242 values
# missing for the age column. To manage this, we are going to use 
# this command instead:
mean(dead$age, na.rm = TRUE) # 68.58621
mean(life$age, na.rm = TRUE) # 48.07229

# Difference
68.58621 - 48.07229 # 20.51392; is this stat sign.?

# We are going to use a t-test to found out if this difference
# statistically significant
t.test(life$age, dead$age, alternative = "two.sided", conf.level = 0.95)

# From the Welch's 2 sample t-test, we see that the p-val < 0.05
# p-val = 2.2e-16, which is ~0. We can reject the null hypothesis
# ("There is no statistical difference in average age between the life and
# dead"). Meaning that this difference in average age is statistically significant.

# GENDER
# Claim: Women are more likely to die from COVID-19 than men.

# Make gender subsets
male = subset(data, gender == "male")
female = subset(data, gender == "female")

# Average
mean(male$death_dummy, na.rm = TRUE) # 0.08461538
mean(female$death_dummy, na.rm = TRUE) # 0.03664921

# Difference
0.08461538 - 0.03664921 # 0.04796617; is this stat sign.?

t.test(male$death_dummy, female$death_dummy, alternative = "two.sided", conf.level = 0.95)
# 95% confidence that males have from 1.7% to 7.8% higher chance of dying.

# p-val is 0.002105, which is less than 0.05, meaning we reject the null
# hypothesis, of there being no difference of death rates between males and females.

# We see that the difference in death rates between males and females is
# statistically significant.

# The claim is false in this case and rather that males have a higher chance of
# dying than females.