exp(2)
round(exp(1),2)
log(3)
log(3,10)
choose(5,2)
factorial(5) #silnia
pi
sin(pi/2)
seq(1,10)
1:10
#Data types
class(5) # "numeric"
class("a") # "character"
class(TRUE) #logical
x <- 1:5 #integer
as.character(x)
#Vector
wek <- c(3,4,5)
is.vector(wek)
class(wek) # give us type of elements
wek[1] # from one
length(wek)
#we can hae diffrent types in vector
#matrix
m <- matrix(5:8,nrow = 2, ncol = 2)
m
dim(m) #row/col
m[1,1]
#list
l <- list("ala", 3 , TRUE)
l <- list(imie = "ala", wiek=10, wzrost=152)
l
l$imie
#factor like categoires of dataframe in python
plec <-c('K','M','K')
class(plec)
plec <- as.factor(plec)
levels(plec)
table(plec) # how many in our categories = value.counts() , tanle of requently
#Dataframe
df <- data.frame(col1 = c(1,3,5),col2 = c('c','f','d'))
df
dim(df)
names(df)
df[ ,1]
df$col1
rownames(df)
rownames(df) <- c('w1','w2','w3')
ncol(iris)
head(iris)
str(iris)
table(iris$Species)
str(mtcars)
# 6 Operations on vectors
w <- c(2,4,5,6)
round(sqrt(w))
w^2
w1 <-c(1,0,1,1)
w+w1
mean(w)
var(w)
sd(w)
summary(w)
head(cars)
mean(cars$speed)
# If, else
i <- 40
if (cars$speed[i] <= 15){
  print('slow')
} else {
  print('fast')
}

ifelse(cars$speed[i] <= 15,print('slow'),print('fast'))

i <- 30
if (cars$speed[i] <= 15){
  print('slow')
} else if (cars$speed[i] < 20){
  print('avreage')
} else {
  print('fast')
}
cars$speed[i]
#loop
for (i in 1:5){
  print(i)
}
dis <- cars$dist
sum <- dis[1]
n <- length(dis)

for (i in 1:(n-1)){
  sum <- sum + dis[i+1] 
}
sum/n

i <-0
while(i < 10){
  i<- i + 1
  print(i)
}