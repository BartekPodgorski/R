install.packages("devtools")
library(devtools)
install_github("pbiecek/Diagnoza") 
library(Diagnoza)
data("gospodarstwa")
data("gospodarstwaDict")
data("osoby")
data("osobyDict")
install.packages("weights")
library(weights)
#L4 Jaki jest najnizszy miesieczny dochód netto w zl potrzebny do zwiazania konca z koncem przez Pana(i) gospodarstwo domowe?
#View(colnames(gospodarstwa))
#View(gospodarstwaDict)
#View(colnames(osoby))
#View(osobyDict)
#EX1
Profit1<-as.vector(gospodarstwa[,"hl4"])
tmp<-gospodarstwa[,c("f2015","hl4")]
tmp<-na.omit(tmp)

tmp<-gospodarstwa[,c("f2015", "waga_gd_2015","hl4")]
tmp<-na.omit(tmp)
attach(tmp)
#mean
wtd.mean(hl4,weights=waga_gd_2015)
#hist
wtd.hist(hl4,weight = waga_gd_2015, breaks=250,  xlim=c(0,10000), ylim =c(0,3000),
         main="The lowest monthly net income (in PLN) needed to make ends meet for household",
         xlab="Net income in PLN",ylab = "Quantity"  )
#sdpop

sdpop<-function(x,y){sqrt(wtd.var(x,weights=y))}
sdpop(hl4,waga_gd_2015)
sd(waga_gd_2015*hl4)
#skewness
skewness<-mean((hl4-wtd.mean(hl4, weights = waga_gd_2015))^3)/(sdpop(hl4, waga_gd_2015))^3
skewness
#kurtosis
kurtosis<-mean((hl4-wtd.mean(hl4, weights = waga_gd_2015))^4)/(sdpop(hl4, waga_gd_2015))^4-3
kurtosis
#quantile
wtd.quantile(hl4, weights = waga_gd_2015)
wtd.quantile(hl4, 0.95,weights=waga_gd_2015)
wtd.quantile(hl4, 0.05, weights=waga_gd_2015)
detach(tmp)

#Ex2
area<-gospodarstwa[,c("waga_gd_2000","ah8","ah15_2")]
area<-na.omit(area)
attach(area)
#scatter plot
plot(ah15_2, ah8, ylab = "Area of a household", xlab = "Fixed costs and rent")
#SD line
area.mean<-wtd.mean(ah8,waga_gd_2000)
cost.mean<-wtd.mean(ah15_2,waga_gd_2000)
area<-sdpop(ah8, waga_gd_2000)
cost<-sdpop(ah15_2,waga_gd_2000)
abline(area.mean - cost.mean * area / cost, area /cost, col='red')
#correlation
area.cor<-wtd.cor(ah8,ah15_2,waga_gd_2000)
area.cor
#Ex3
plot(ah15_2, ah8, ylab = "Area of a household", xlab = "Fixed costs and rent")
#regression 
lm1<- lm(ah15_2~ah8, weights = waga_gd_2000)
a1<-lm1$coefficients[1]
b1<-lm2$coefficients[2]
lm2<- lm(ah8~ah15_2, weights = waga_gd_2000)
abline(a=-a1/b1,b=1/b1, col='blue')
abline(lm2, col='green')
#chart of resuiduals
plot(ah15_2, resid(lm2), xlab = "Fixed costs and rent", ylab = "Residuals")
summary(lm2)
summary(lm1)
resid(lm2)
detach(area)
#Ex4
span<-c(2000,2003,2005,2007,2009,2011)
psych<-osoby[,c("waga_2000_ind","waga_2003_ind","waga_2005_ind","waga_2007_ind",
                "waga_2009_ind","waga_2011_ind", "ap84","bp71","cp78","dp75","ep70","fp72")]
psych<-na.omit(psych)
attach(psych)
y0<-wpct(ap84, waga_2000_ind)[1]
y1<-wpct(bp71, waga_2003_ind)[1]
y2<-wpct(cp78, waga_2005_ind)[1]
y3<-wpct(dp75, waga_2007_ind)[1]
y4<-wpct(ep70, waga_2009_ind)[1]
y5<-wpct(fp72, waga_2011_ind)[1]
yes<-c(y0, y1, y2, y3, y4, y5)
plot(span, yes, ylab="Percentage of people who visited psychologist")
lines(span,yes)

#CAGR
mult<-(yes[length(yes)]/yes[1])^(1/11)
lines(c(2000:2011), yes[1]*mult^(0:11),col="red")
#fixed index
fixed<-yes/yes[1]*100
plot(span[1:length(yes)], fixed, xlab = "years", ylab = "fixed index")
lines(span[1:length(yes)], fixed)
#chain index
chain<-yes[2:length(yes)]/yes[1:length(yes)-1]*100
plot(span[2:length(yes)],chain, xlab = "span")
lines(span[2:length(yes)],chain)
detach(psych)