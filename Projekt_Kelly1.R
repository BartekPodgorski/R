#Projekt numer 1  
# Short description:

# Simulate two people: one playing 20% (Kelly strategy), another a different strategy (say, 10%, 30% ...).
# Assume they are playing with the same coin. In the simulation, what was the percentage of cases
# when the final bankroll of the Kelly player was higher than the final bankroll of the other person?

#I added few functions to the program:
#1)It calculates for 3 strategies
#2)It calculates average bankrolls for each stragegy for each toss
#3)It draws a plot of a average bankroll for each strategy after each toss
#4)Is is made to change variables in simple way so we can test diffrent scenarios

#Summary:
#after testing this program a few times i noticed that 20% probably the most optimal strategy to play
#what is interesting, avrage bankroll with betting 30% grows much faster than in Kelly strategy
#but there is higher probability that after (for example 100 toses) you will end with less money betting 30% than 20%
#in conclusion, if you bet higher than 20% the game takes a rule: high risk high reward. It means that u can win much more
#or lose, setting 20% the safest strategy if the game is abut 80 toses long or more, then the longer the game the better
#Kelly strategy becomes, for early game 30% and 10% gives better results


# robimy monete

value<-c(-1,1)                    

prob<-c(.4,.6)            

coinA<-data.frame(value, prob)    


#liczniki zwyciestw
win.strategy <- 0          
win.Kelly <- 0  

n<-10           #liczba rzutow
x<-100          #liczba symulacji
strategy <- .25
Kelly <- .2


for (j in 1:x) #petla ktora sie wykonuje x razy
{
  
  #rzucamy moneta, niech rozpocznie sie hazard
  sample1<-sample(coinA$value, size=n, prob=coinA$prob, replace=TRUE)
  
  bankroll.strat<-rep(0,n+1)      
  bankroll.Kelly<-rep(0,n+1)      
  #stowa na start
  bankroll.strat[1]<-100  
  bankroll.Kelly[1]<-100
  
  for (j in 1:n) {
    
    #liczenie hajsu po rzucie
    bankroll.strat[j+1]<-bankroll.strat[j]*(1+strategy*sample1[j]);      
    bankroll.Kelly[j+1]<-bankroll.Kelly[j]*(1+Kelly*sample1[j]);
  }
  
  #zliczanie zwyciestw
  
  if (bankroll.Kelly[n+1] > bankroll.strat[n+1])
  {
    win.Kelly =  win.Kelly + 1
  }
  if (bankroll.Kelly[n+1] < bankroll.strat[n+1])
  {
    win.strategy =  win.strategy + 1
  }
}

print((win.Kelly/x)*100)                
print((win.strategy/x)*100)


v <- c(win.Kelly, win.strategy)                  
s<-c(strategy,Kelly)

barplot(v,s, xlab = "factor", names.arg=c("Kelly strategy", ".25 strategy"))