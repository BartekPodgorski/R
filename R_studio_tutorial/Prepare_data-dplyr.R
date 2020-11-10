#load data from file
getwd()
setwd('C:/Users/user/Desktop/R_analiza/vnozar')
sale <- read.csv('sprzedaz.csv',header=TRUE)
View(sale)
head(sale)
str(sale)

sale$Sprzedaz2013 <-as.numeric(sale$Sprzedaz2013)
sale$Sprzedaz2014 <-as.numeric(sale$Sprzedaz2014)

sale <-sale[-1]
dim(sale)
sale$Produkt <- as.factor(gsub('Produkt ', 'P', sale$Produkt))
#Agrregation
#1What is the total sale in 2013 for each product?
sale1 <- aggregate(sale$Sprzedaz2013 ~ sale$Produkt,FUN = sum)
head(sale1)
colnames(sale1)<-c('Product','sale2013')
attach(sale) #we don't have to writesale$
#What is the total margin in 2013 for every product with divide in categories
sale1b <- aggregate(Marza2013 ~ Produkt +Kategoria,FUN=sum )
head(sale1b)

#3What is the avreage sale in 2014 for each sector?
sale2 <- aggregate(Sprzedaz2014 ~ Segment,FUN = mean)
head(sale2)
#4What is the largest number of items, which sold in 2014 for each category and sectors?
sale3 <- aggregate(Sztuki2014 ~ Kategoria+Segment, FUN= max)
sale3
#5all descriptive statistics for number of items in 2014 by products
sale4 <- aggregate(Sztuki2014, by=list(Produkt),FUN=summary)
sale4
detach(sale)
#filter of date
#only transction for product P1
sub1 <- subset(sale, Produkt == 'P1')
head(sub1)
nrow(sub1)
#only trancastion for category 'Skin care' , which sale is bigger than mean sale in 2014
sub2 <- subset(sale, Sprzedaz2014 > mean(Sprzedaz2014) & Kategoria == 'Skin care')
head(sub2)
nrow(sub2)
#Only 10% of the best transactions with the highest sale in 2013
sub3 <- subset(sale, Sprzedaz2013 >= quantile(Sprzedaz2013,0.9))
head(sub3)
nrow(sub3)/nrow(sale)
#only category
sale[,'Kategoria']
#Products and item2013
sale[,c('Produkt','Sztuki2013')]
#only column 1 and 8
sale[,c(1,8)]
#remove last column
sale[,-ncol(sale)]
#remove 2 and 3
sale[,-c(2,3)]
#10 first transaction
sale[1:10,]
#remove first 10
sale[-c(1:10),]
#Condition on vectors
Produkt[Sprzedaz2014 = max(Sprzedaz2014)]
#Filter the list
x <- list(a=list(10,12,14), b=c(3.14,2.81))
x$a
x$b
x$a[2]
x$b[1]
#Sort of data
Sprzedaz2014[1:20]
sort(Sprzedaz2014[1:20])
sort(Sprzedaz2014[1:20], decreasing=TRUE)
#order
sort1 <- sale[order(Produkt),]
head(sort1)
#sort by amount sold item in 2014 and product descending
sort2 <- sale[order(Sztuki2014, Produkt, decreasing = TRUE),]
head(sort2)
#sort by margin 2014 ascending and sale in 2014 decreasing
sort3 <- sale[order(Marza2014, -Sprzedaz2014),] #minus make 
head(sort3,3)
detach(sale)
#Merge data
opak <- read.csv2('opakowanie.csv')
str(opak)
head(opak)
#relation one to many
sale_opak <- merge(sale,opak,by = 'Produkt')
head(sale_opak)
nrow(sale_opak)
#different keys
names(opak)[1] <- 'Prod'
sale_opak1 <- merge(sale,opak,by.x = 'Produkt', by.y='Prod')
nrow(sale_opak1)
#package dplyr
install.packages('dplyr')
library(dplyr)
attach(sale)
select(sale, Produkt, Kategoria)
filter(sale,Produkt == 'P1')
#pipeline
x <- 4
log(x)
exp(log(x))
x %>% log() %>% exp()
sale %>% select(Produkt,Kategoria)
sale %>% filter(Produkt == 'P1')
#How many items of P2 was sold in 2014?
sum(select(filter(sale, Produkt == 'P2'), Sztuki2014))
sale %>% filter(Produkt == 'P2') %>% select(Sztuki2014) %>% sum()
#Create new dataframe contains total margin in 2014 for each sector
sale %>% group_by(Segment) %>% summarise(suma = sum(Marza2013)) %>%
  arrange(desc(suma))  
#Create new dataframe contains mean amount of itmes in 2013 by product ,sd 
sale %>% group_by(Produkt) %>% 
  summarise(sr = mean(Sztuki2013), od.s = sd(Sztuki2013),
            dol = sr-od.s, gora = sr+od.s) %>% arrange(desc(sr))
#Missing values(NA)

miss <- data.frame(kol1 = sample(c(1,3,4,NA),30,replace=TRUE),
                   kol2 = sample(c(1,3,4,NA),30,replace=TRUE))
nrow(miss)
View(miss)
summary(miss)
#Param na.rm
max(miss) #NA
max(miss,na.rm=TRUE) # ignore NA
#remove NA
miss1 <- na.omit(miss)
View(miss1)
nrow(miss1)
#Impute 0
is.na(miss) #logic mask
miss[is.na(miss)] <- 0 #refill NA by 0
#Impute median
m1 <- median(miss1$kol1)
m2 <- median(miss1$kol2)
miss$kol1[is.na(miss$kol1)] <- m1
miss$kol2[is.na(miss$kol2)] <- m2