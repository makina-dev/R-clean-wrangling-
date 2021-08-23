library(tidyr)
library(tidyverse)
library(ggplot2)
glimpse(the_counted_2015)
names(the_counted_2015)
dim(the_counted_2015)
names(the_counted_2015)[5]= "origin"
names(the_counted_2015)[9]= "adress"
names(the_counted_2015)[12]="grouping"
names(the_counted_2015)[13]="jurisdiction"
names(the_counted_2015)[1]= "id"
names(the_counted_2015)
sum(is.na(the_counted_2015$id))
sum(is.na(the_counted_2015$name))
sum(is.na(the_counted_2015$age))
sum(is.na(the_counted_2015$gender))
sum(is.na(the_counted_2015$origin))
sum(is.na(the_counted_2015$month))
sum(is.na(the_counted_2015$day))
sum(is.na(the_counted_2015$year))
sum(is.na(the_counted_2015$adress))
sum(is.na(the_counted_2015$city))
sum(is.na(the_counted_2015$state))
sum(is.na(the_counted_2015$grouping))
sum(is.na(the_counted_2015$jurisdiction))
sum(is.na(the_counted_2015$armed))
my_data_new=na.omit(the_counted_2015)
names(my_data_new)
my_data_new= my_data_new %>% mutate(age=as.numeric(age))
str(my_data_new)
boxplot(my_data_new$age)
boxplot(my_data_new$id)
boxplot(my_data_new$day)
glimpse(my_data_new)
my_data_new=my_data_new %>% mutate( origin=factor(origin),state=factor(state), grouping=factor(grouping))
glimpse(my_data_new)
the_counted_2015$gender[the_counted_2015$gender== "Female"]= "0"
the_counted_2015$gender[the_counted_2015$gender== "Male"]= "1"
my_data_new %>% ggplot(aes(gender)) + geom_bar()+theme_bw() +labs(title = "Death rates by police according to gender")
my_data_new %>% ggplot(aes(origin)) + geom_bar()+theme_bw() +labs(title = "Death rates by police according to origin")
my_data_new %>% ggplot(aes(grouping)) + geom_bar()+theme_bw() +labs(title = "Death rates by police according to grouping")
my_data_new %>% ggplot(aes(age)) + geom_histogram(binwidth = 5)+theme_bw() +labs(title = "Death rates by police according to age")
my_data_new %>% ggplot(aes(state)) + geom_bar()+theme_bw() +labs(title = "Death rates by police according to states")
prop.table(table(my_data_new$origin))
prop.table(table(my_data_new$gender))
my_data_new=my_data_new %>% mutate(gender=character(gender))
glimpse(my_data_new)
my_data_new %>% ggplot(aes(age, gender)) + boxplot()
my_data_new=my_data_new %>% mutate(replace(my_data_new$gender,male=1, female=0))
glimpse(the_counted_2015)
the_counted_2015$gender[the_counted_2015$gender== "Male"]= "1"
my_data_new$gender[which(is.na(my_data_new$gender))] = 1
the_counted_2015$gender[the_counted_2015$gender== "Female"]= "0"
t=the_counted_2015 %>% mutate(gender=as.numeric(gender))
t
lm(age~gender, data = the_counted_2015)
the_counted_2015=the_counted_2015%>% mutate(age=as.numeric(age))
the_counted_2015%>%ggplot(aes(age, gender)) + geom_count()
