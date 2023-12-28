#Exercise 1
#section 1
n <-  100
x <- n*(n+1)/2
x

sum(seq(1, 1000))

log(sqrt(100),10)

log(exp(2.56))

sum(1/seq(1, 1000)^2)

#section 2
library(dslabs)

data("murders")
str(murders)
colnames(murders)
class(murders$abb)
class(murders$region)
length(levels(murders$region))

table(murders$region)

#section 3

temp <-  c(35, 88, 42, 84, 81, 30)
city <-  c("Beijing", "Lagos", "Paris", "Rio de Janeiro", 
           "San Juan", "Toronto")

names(temp) <-  city
temp[1:3]
temp[c("Paris", "San Juan")]

seq <- 12:73
seq

odd <- seq(1, 100, 2)
odd

length(seq(6, 55, 4/7))

a <- seq(1, 10, 0.5)
class(a)

b <- seq(1, 10)
class(b)

class(a<-1)
class(a <- 1L)

x <- c("1", "3", "5")
as.numeric(x)

#Section 4
pop <- murders$population

sort(pop, decreasing = FALSE) #turns sorted values 
order(pop, decreasing = FALSE) #turns indexes of sorted values

min(pop)
which.min(pop)
which.max(pop)
pop[which.min(pop)]

#to find state name has min population
murders$state[which.min(pop)]
murders$state[which.min(murders$population)]

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro",
          "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

ranks <- rank(city_temps$temperature)
my_df <-data.frame(ranks=ranks,city_temps$name)

ranks2 <- rank(murders$population)
my_df2 <- data.frame(ranks=ranks2, name=murders$state)

my_df2[order(my_df2$ranks), ]
head(my_df2[order(my_df2$ranks), ])
tail(my_df2[order(my_df2$ranks), ])

#Section 5

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro",
          "San Juan", "Toronto") 
names(temp) <- city

temp_fahrenheit <- (city_temps$temperature - 32)*(5/9)

data("na_example")
str(na_example)

mean(na_example)
mean(na_example, na.rm = TRUE)

