library("tidyverse")
library("tibble")

###    Question 8  ###
#8) a) 
College <- read.csv("College.csv")
View(College)

#8) b) ----
College <- College %>% rename(college_name = X)
View(College) # I wanted to make sure that the column was renamed 

#8) c) 
summary(College)
College %>% summarise(across(where(is.numeric),
                             .fns = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
                           pivot_longer(cols = everything(),
                                   names_sep = "_",
                                   names_to  = c("variable", ".value"))
install.packages("GGally")
library("GGally") 
College_num_col <- College %>% select(where(is.numeric)) 
view(College_num_col)
ggpairs(College_num_col) 
install.packages("ggpairs")
pairs(College_num_col) #not useful as they are small and can't really see the relationship between the two varioables

ggplot(data=College, aes(y= Enroll, x= Accept, fill=Outstate ) ) + 
  geom_boxplot()
# there are  a lot of outliers starting from a little below 2000
College <- College %>% mutate(elite = case_when(Top10perc > 50 ~ "Yes",
                                                Top10perc <= 50 ~ "No") ) %>% glimpse()
College %>% group_by(elite) %>% summarise( avg_enrollement = mean(Enroll, na.rm = TRUE))
#the average enrollement of the elite colleges is 1061
ggplot(data=College, aes(y= Apps, x= elite, fill=Outstate ) ) + 
  geom_boxplot()
#there are a significant difference in terms of outliers between the elite and non-elite colleges
#the outliers in the elite colleges are lower 
p1 <- ggplot(College, aes(x = Apps)) + 
  geom_histogram(bins = 60) 
p1 #a right sekwed histogram, the applications reach their peak above 200
p2 <- ggplot(data = College) +
     geom_bar(mapping = aes(x = Enroll))
p2 #enrollement is equal among all the variables except for three times that we may consider as outliers         
p3 <- ggplot(College, aes(x = Accept)) + 
  geom_histogram(bins = 50) 
p3 #also a right sekwed histogram, the accepetence in colleges reaches its peak a little below 250
p4 <- ggplot(data = College, mapping = aes(x = PhD)) +
  geom_freqpoly(binwidth = 0.1)
p4 #people who are pursuing a doctorate degree are significantly high 

library("gridExtra")

grid.arrange(p1, p2, p3, p4, nrow = 2)

###  Question 9 ###

#9)a)
Auto <- read_csv("Auto.csv")
View(Auto)
Auto <- na.omit(Auto) #to remove na variables 
View(Auto)
# name and origin are  qualitatives
Auto_numeric = subset(Auto, select = -c(origin,name) )
view(Auto_numeric)
#9)b) 
range(Auto_numeric)
#9)c)
summary(Auto_numeric) #summary stat of columns 
#9)d)
new_auto <- Auto_numeric[-c(10:85),] #to remove rows from 10 to 85
new_auto
view(new_auto)
summary(new_auto)
# 9)e)
r1 <- ggplot(data = Auto_numeric) +
  geom_bar(mapping = aes(x = acceleration))
r1 #most of the data is close to or higher than a frequency of 10         
r2 <- ggplot(Auto_numeric, aes(x = mpg)) + 
  geom_histogram(bins = 50) 
r2 #seems as a right skewed histogram with a frequency above 15 
#9) f)
#the acceleration variable might be useful since it calculates speed and velocity,
#the two features taht affect the mpg

r3 <- ggplot2::ggplot(data= Auto_numeric) + 
  ggplot2::geom_point(mapping = aes(x = mpg, y = acceleration))
r3
#there is a linear (positive) relationship between the two variables

### Question 10 ###
#10) a) 
library(MASS)

?Boston
dim(Boston)
# 506 rows and 14 columns

# 10) b) pairplot 

boston_numeric <- Boston %>% dplyr::select(where(is.numeric)) 
view(boston_numeric)
ggpairs(boston_numeric)
#ggpairs is not available in theserver, even after I have downloaded it
#so can't extract the relationship bewteen the variables using ggpairs which won't let me solve question 10) c) 
# 10) c)
l1 <- ggplot2::ggplot(data= boston_numeric) + 
  ggplot2::geom_point(mapping = aes(x = medv, y = crim))
l1
# I have tried different variables in x with the y variable fixed on crime,
#however: little to no relationship was found between the variables

#10) d)
range(boston_numeric$crim, na.rm = TRUE)
#the highest crime rate is 88.97620
which(boston_numeric == 88.97620, arr.ind=TRUE)
#to know which row/suburb has the highest crime rates
range(boston_numeric$tax, na.rm = TRUE)
#the highest tax rate is 711 
which(boston_numeric == 711, arr.ind=TRUE)
#to know which rows/suburbs have the highest tax rates 
range(boston_numeric$ptratio, na.rm = TRUE)
which(boston_numeric == 22.0, arr.ind=TRUE)
#to know which rows/subrbs have the highest Pupil-teacher ratios
#10) e) 
?dplyr::count
boston_numeric %>% count(chas)
# there are 35 suburbs that bound the Charels River

#10) f) 
median(boston_numeric$ptratio)
#the median is 19.05 
#10) g) 
min(Boston$medv)
# the lowest is 5
which(Boston$medv == 5, arr.ind=TRUE)
#row/suburb 399 and 406 
Boston[ c(399,406), ]
summary(Boston)
#the crime rates in both rows are higher that the mean, both suburbs don't bound the river
#the average numbers of rooms per house in both subrubs are  lower than the overall mean
#Consequently, both subrubs are the worst of the overall suburbs in Boston
#10) h) 
more_than_seven <- subset(boston_numeric, rm > 7)
nrow(more_than_seven) 
#64 suburbs 
more_than_eight <- subset(boston_numeric, rm > 8)
nrow(more_than_eight)
#13 suburbs
