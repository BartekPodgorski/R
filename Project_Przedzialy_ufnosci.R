#Ile przeciętnie godzin w tygodniu Pan pracuje? hp114
library(weights)
load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osoby.rda"))
load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osobyDict.rda"))

#Ile przeciętnie godzin w tygodniu Pan pracuje? hp114
#I would like to check average of being in work per weak in 2015 with confidence intervals
#Ex1
hours<- osoby[,c('hp114','waga_2015_ind')]
hours<-na.omit(hours)
sqrt(length(hours$hp114))
mean<-wtd.mean(hours$hp114, weights = hours$waga_2015_ind)
sd<-sqrt(wtd.var(hours$hp114, weights = hours$waga_2015_ind))
left<-mean-1.96*(sd/sqrt(length(hours$hp114)))
right<-mean+1.96*(sd/sqrt(length(hours$hp114)))
errbar(x=0,yminus=left,yplus=right,y=mean,xlab='Ile przeciętnie godzin w tygodniu Pan pracuje?')
#The confidence interval of being in work per weak in 2015 is between 41.12729 and 41.53219 hours.
#Ex2
#Pana własny (osobisty) dochód miesięcznynetto (na rękę) średnio z ostatnich trzech miesięcy wyniósł hp65
#I would like to check average month income from last three month in 2015 per pearson. 
#I would like to check that this income is greater than 2200 PLN with 95% confidence(mu=2200, alpha=0.05)
income<-osoby[,c('hp65','waga_2015_ind')]
income<-na.omit(income)
wtd.t.test(income$hp65,weight = income$waga_2015_ind, alternative = 'greater', y=2200)
#After check it we can say that  income is not greater than 2200 and it is about 2033.71PLN
#Ex3
#Jakiego dochodu miesięcznienetto (na rękę) spodziewa się Pan za dwa lata? hp67
#I would like to check average month income predicted in  2017 per pearson. 
#I would like to check that this income will be greater than 3000 PLN with 95% confidence(mu=3000, alpha=0.05)
income2<-osoby[,c('hp66','waga_2015_ind')]
income2<-na.omit(income2)
wtd.t.test(income2$hp66,weight = income2$waga_2015_ind, alternative = 'greater', y=3000)
#After check it we can say that  income is not greater than 3000 and it is about 2594.92 PLN