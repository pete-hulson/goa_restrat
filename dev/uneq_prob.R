

install.packages('TeachingSampling')
install.packages('samplingbook')

library(TeachingSampling)
library(samplingbook)


m1=400
m2=200
n1=100
n2=50
sd1=10
sd2=20
s1=rnorm(n1, mean = m1, sd = sd1)
s2=rnorm(n2, mean = m2, sd = sd2)
area1=1000
area2=2000
p1=rep(n1/area1,n1)
p2=rep(n2/area2,n2)
total=area1*m1+area2*m2 
pop_mean=total/(area1+area2)
pop_mean
mean(c(s1,s2))

HT(c(s1,s2),c(p1,p2))
total
HH(c(s1,s2),c(p1,p2))

?HH
?HT

?rnorm

htestimate(c(s1,s2), 3000, pk=c(p1,p2), method = 'hh')
mean(c(s1,s2))


# Uses the Lucy data to draw a simple random sample without replacement
data(Lucy)
attach(Lucy)

N <- dim(Lucy)[1]
n <- 400
sam <- sample(N,n)
# The vector of inclusion probabilities for each unit in the sample
pik <- rep(n/N,n)
# The information about the units in the sample is stored in an object called data
data <- Lucy[sam,]
attach(data)
names(data)
# The variables of interest are: Income, Employees and Taxes
# This information is stored in a data frame called estima
estima <- data.frame(Income, Employees, Taxes)
HT(estima, pik)
pik
