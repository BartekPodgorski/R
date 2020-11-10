attach(cars)
names(cars)
View(cars)
speed
length(speed)
summary(speed)
var(speed)
sd(speed)
#visualization
plot(speed)
plot(speed, type='p',col='blue',cex=0.5,
     xlab ="Nr. oberwacji",
     y_lab ="Wartosc zmiennej",
     main = "Zmienna Speed")
#hisogram
hist(speed, freq=FALSE,breaks=10)
str(hist(speed))
abline(v = mean(speed),col='red')
#Boxplot
boxplot(speed)
#hist &density
hist(speed,freq=FALSE)
lines(density(speed),col='red')
