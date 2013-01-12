data = read.csv(file = "/.../Computing for Data Analysis/Week1/hw1_data.csv")
# "..." is the directory in your computer; masked here for privacy
head(data)
# Q1(D): Ozone Solar.R Wind Temp Month Day
# Q2(C):
#    Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
data[1:2,]
# Q3(B): 153
dim(data)[1] 
# Q4(B):
#     Ozone Solar.R Wind Temp Month Day
# 152    18     131  8.0   76     9  29
# 153    20     223 11.5   68     9  30
data[152:153,]
# Q5(C): 21
data[47, ]$Ozone
# Q6(B): 37
Na = data[is.na(data$Ozone), ]
dim(Na)[1]
# Q7(A): 42.12931
mean(data$Ozone, na.rm = T)
# Q8(D): 212.8
sub = data[data$Ozone > 31 & data$Temp > 90, ]
mean(sub$Solar.R, na.rm = T)
# Q9(D): numeric
class(data$Month) # integer
data.class(data$Month) # numeric
# Q10(D): 79.1
sub1 = data[data$Month == 6, ]
mean(sub1$Temp, na.rm = T)
#### some good clarifications ###
x <- 1L
data.class(x)
# [1] "numeric"
class(x)
# [1] "integer"
mode(x)
# [1] "numeric"
typeof(x)
# [1] "integer
attr(x, 'class') <- 'class'
data.class(x)
# [1] "class"
class(x)
# [1] "class"
mode(x)
# [1] "numeric"
typeof(x)
# [1] "integer"
data <- c(0L,1L,2L)
is.integer(data) #TRUE
is.numeric(data) #TRUE
# integer is numeric; numeric is not necessarily integer
data <- c(0,1,2)
is.integer(data) #FALSE
is.numeric(data) #TRUE
