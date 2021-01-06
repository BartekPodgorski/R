#Instalacja pakiet

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("ggplot2")
#install.packages("wordcloud")

###Corpus Preprocessing
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)

#Setting working directory
setwd("C:/Users/user/Desktop/Projekt_Opinion")
#getwd()
Data<-read.csv("Opinie.csv",
               header = FALSE,
               sep = ",",  # or ";"
               strip.white = TRUE, 
               fill = TRUE, 
               comment.char = "#",
               stringsAsFactors = FALSE,
)
#Matrix from Brand Database
MyData = as.data.frame.matrix(Data)
colnames(MyData)<-("Review")
MyData_1 <- MyData[,1] 
n<-length(MyData_1)
n

docs <-VCorpus(x = VectorSource(MyData_1),
               readerControl = list(reader=readPlain,
                                    language="en"))

#___________by sentences_____________________
library(tokenizers)
MyData_22<-tokenize_sentences(MyData_1)
length(MyData_22)
wf1=NULL
for (i in 1:n) {
  c <- data.frame(document=as.character(MyData_22[[i]][]))
  wf1=rbind(wf1,c)
}
n<-length(wf1$document)
n
#write.csv(wf1,'Sentences.csv', row.names=FALSE)

Data_Sentences<-read.csv("Sentences.csv",
                         header = TRUE,
                         sep = ",",  # or ";"
                         strip.white = TRUE, 
                         fill = TRUE, 
                         comment.char = "#",
                         stringsAsFactors = FALSE 
)
#Matrix from Brand Database
Data_Sentences = as.data.frame.matrix(Data_Sentences) 

docs <-VCorpus(x = VectorSource(Data_Sentences[,1]),
               readerControl = list(reader=readPlain,
                                    language="en"))

#_________________Preprocessing_____________________________

# number of terms in Corpus
docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)
dtm
#write.csv(as.matrix(dtm),file="DTM.csv")

rownames(dtm)<-seq(1,n)

# comments length
doc_length <- as.data.frame(rowSums(as.matrix(dtm)))
#write.csv(as.matrix(doc_length),file="Doc_length.csv")

#Lenghts
max_length<-max(doc_length)
max_length
min_length<-min(doc_length)
min_length
aver_length<-mean(rowSums(as.matrix(dtm)))
aver_length

#pre-processing
getTransformations()
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers) 

for (j in seq(docs)) { 
  docs[[j]] <- gsub("/", " ", docs[[j]]) 
  docs[[j]] <- gsub("-", " ", docs[[j]]) 
  docs[[j]] <- gsub("â€", " ", docs[[j]]) 
  docs[[j]] <- gsub("’", " ", docs[[j]]) 
  docs[[j]] <- gsub("“", " ", docs[[j]]) 
  docs[[j]] <- gsub("…", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]]) 
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])  
}  

docs <- tm_map(docs, tolower) 

#length(stopwords("english")) 
#stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("English"))

StW<-read.table("StopWords.txt")
StWW<-as.character(StW$V1) 
docs <- tm_map(docs, removeWords, StWW) 

docs <- tm_map(docs, PlainTextDocument) 
docs <- tm_map(docs, stripWhitespace) 

for (j in seq(docs)) { 
  docs[[j]]<-stemDocument(docs[[j]], language = "english") 
} 

docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)

rownames(dtm)<-seq(1,n)
#write.csv(as.matrix(dtm),file="DocumentTermMatrix.csv")
dtm
#normalized matrix
xx<-rowSums(as.matrix(dtm))
dtm_Norm<-dtm/xx
dtm_Norm
dtm_Norm_m<-as.data.frame(as.matrix(dtm_Norm))
#write.csv(dtm_Norm_m, file="DocumentTermMatrixNorm.csv")


#___________Histogtram of Frequency_________________
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 30)
tail(freq, 15)

d <- data.frame(word = names(freq),freq=freq)
head(d, 30)
mk<-min(head(freq, 30))

wf=data.frame(word=names(freq),freq=freq)  
p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Keywords for Opinions") + labs(x="Words",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1, size=16))
p

#___________________Building Wordcloud___________________________

d <- data.frame(word = names(freq),freq=freq)
set.seed(1234)
dev.new(width = 100, height = 100, unit = "px")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#_____________________Bigrams_____________________________________
NgramTokenizer = function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs, control = list(tokenize = NgramTokenizer))

#___________Histogtram of Bigrams__________________________________
#___________Calculating the Frequency______________________________

freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE)
head(freq_n, 15)
mk<-min(head(freq_n, 15))
tail(freq_n, 15)   
m<-as.matrix(dtm_n)
#write.csv(m, file="N_DocumentTermMatrix.csv")

#___________Building the Histogtram (zipf law)___________________
wf=data.frame(word=names(freq_n),freq=freq_n) 

p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bi-grams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=10))
p

#wordcloud
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq_n), freq_n, max.words=100, rot.per=0.2, colors=dark2)   

#___________topics modelling____________________________________
#install.packages("topicmodels")
library(topicmodels) 
library(lsa)

raw.sum=apply(dtm,1,FUN=sum) #sum by raw for each raw of the table

#number of rows with a zero's sum
mmm<-nrow(dtm[raw.sum==0,])
mmm

#if mmm=0, only create dtm2 and NN (number of rows in DTM)
# if mmm>0, delete the rows with zero's sum form corpus

if (mmm==0) {
  dtm2<-dtm
  NN<-nrow(dtm)
  NN
} else {
  dtm2<-dtm[raw.sum!=0,]
  NN<-nrow(dtm2)
}
#number of comments before deleting
n
#number of comments after deleting
NN
#new matrix dtm2
dtm2
#__________LDA Topic Modelling______________________
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5

ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)

#____topics keywords___________________________
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms
#write.csv(ldaOut.terms,file="C:/Users/user/Desktop/Projekt_Opinion/TopicsToTerms_1.csv")

#___topics probability per document_____________
topicProbabilities <- as.data.frame(ldaOut@gamma)

#___Topic proportion over Corpus________________
col.sum=apply(topicProbabilities,2,FUN=sum) # 1 - rows sum, 2- columns sum
col.sum<-as.matrix(col.sum)
dim(col.sum)
sum.TP=col.sum/sum(col.sum)
sum.TP
#write.csv(topicProbabilities,file="C:/Users/user/Desktop/Projekt_Opinion/TopicProbabilities_1.csv")

#___topics by Documents_________________________
ldaOut.topics <- as.matrix(topics(ldaOut))
#View(rownames(ldaOut.topics))
#dtm -> dtm2 
rownames(ldaOut.topics)<-as.character(rownames(dtm2)) 
ldaOut.topics
#write.csv(ldaOut.topics,file="C:/Users/user/Desktop/Projekt_Opinion/DocsToTopics_1.csv")

#___________Topics Data Frame building_______________________________
nrow(ldaOut.topics)
Comment<-seq(1, NN, by=1)
wf=data.frame(Comment=Comment, Topics=ldaOut.topics)
kk<-nrow(dtm2)
kk

#Building Sub-Corpus of Topic 1
topic1<-wf[wf[2] == 1,]  
topic1$Comment
length(topic1$Comment)
Data_Sentences$document[13]

kk1<-nrow(topic1)
kk1

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==1) {               # Tu zmieniac XD
    list1<-c(list1,i)}
  i=i+1
}
list1
wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(Comment[[i]]))
      wf1=rbind(wf1,c)
    } 
  }
}
wf1

Topic_1_docs <- Corpus(VectorSource(as.character(wf1$document)))
mycorpus_dataframe_1 <- data.frame(text=wf1$document, stringsAsFactors=TRUE)
#write.csv(mycorpus_dataframe_1,'Topic 1_Comments.csv', row.names=FALSE) #File with documents for Topic 1


#Building Sub-Corpus of Topic 2
topic2<-wf[wf[2] == 2,]
topic2$Comment
length(topic2$Comment)
Data_Sentences$document[2]
kk2<-nrow(topic2)
kk2

list2<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==2) {               # Tu zmieniac XD
    list2<-c(list2,i)}
  i=i+1
}
list2
wf2=NULL
for (i in 1:kk) {
  for (j in 1:kk2) {
    if (i==list2[j]){
      c <- data.frame(file=as.character(wf[list2[j],1]),document=as.character(Comment[[i]]))
      wf2=rbind(wf2,c)
    } 
  }
}
wf2

Topic_2_docs <- Corpus(VectorSource(as.character(wf2$document)))
mycorpus_dataframe_2 <- data.frame(text=wf2$document, stringsAsFactors=TRUE)
#write.csv(mycorpus_dataframe_2,'Topic 2_Comments.csv', row.names=FALSE) #File with documents for Topic 2


#Building Sub-Corpus of Topic 3
topic3<-wf[wf[2] == 3,]
topic3$Comment
Data_Sentences$document[1]
length(topic3$Comment)
kk3<-nrow(topic3)
kk3

list3<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==3) {               # Tu zmieniac XD
    list3<-c(list3,i)}
  i=i+1
}
list3
wf3=NULL
for (i in 1:kk) {
  for (j in 1:kk3) {
    if (i==list3[j]){
      c <- data.frame(file=as.character(wf[list3[j],1]),document=as.character(Comment[[i]]))
      wf3=rbind(wf3,c)
    } 
  }
}
wf3

Topic_3_docs <- Corpus(VectorSource(as.character(wf3$document)))
mycorpus_dataframe_3 <- data.frame(text=wf3$document, stringsAsFactors=TRUE)
#write.csv(mycorpus_dataframe_3,'Topic 3_Comments.csv', row.names=FALSE) #File with documents for Topic 3


#Building Sub-Corpus of Topic 4
topic4<-wf[wf[2] == 4,]
topic4$Comment
Data_Sentences$document[14]
length(topic4$Comment)
kk4<-nrow(topic4)
kk4

list4<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==4) {               # Tu zmieniac XD
    list4<-c(list4,i)}
  i=i+1
}
list4
wf4=NULL
for (i in 1:kk) {
  for (j in 1:kk4) {
    if (i==list4[j]){
      c <- data.frame(file=as.character(wf[list4[j],1]),document=as.character(Comment[[i]]))
      wf4=rbind(wf4,c)
    } 
  }
}
wf4

Topic_4_docs <- Corpus(VectorSource(as.character(wf4$document)))
mycorpus_dataframe_4 <- data.frame(text=wf4$document, stringsAsFactors=TRUE)
#write.csv(mycorpus_dataframe_4,'Topic 4_Comments.csv', row.names=FALSE) #File with documents for Topic 4

#Building Sub-Corpus of Topic 5
topic5<-wf[wf[2] == 5,]
topic5$Comment
Data_Sentences$document[5790]
length(topic5$Comment)
kk5<-nrow(topic5)
kk5

list5<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==5) {               # Tu zmieniac XD
    list5<-c(list5,i)}
  i=i+1
}
list5
wf5=NULL
for (i in 1:kk) {
  for (j in 1:kk5) {
    if (i==list5[j]){
      c <- data.frame(file=as.character(wf[list5[j],1]),document=as.character(Comment[[i]]))
      wf5=rbind(wf5,c)
    } 
  }
}
wf5

Topic_5_docs <- Corpus(VectorSource(as.character(wf5$document)))
mycorpus_dataframe_5 <- data.frame(text=wf5$document, stringsAsFactors=TRUE)
#write.csv(mycorpus_dataframe_5,'Topic 5_Comments.csv', row.names=FALSE) #File with documents for Topic 5

########

#____________________SENTIMENT ANALYSIS__________________________________
library("plyr")
library("stringr")

neg=scan("Lexicon/negative-words.txt", what="character", comment.char=";" )
pos=scan("Lexicon/positive-words.txt", what="character", comment.char=";" )

for (j in seq(neg)) { 
  neg[[j]]<-stemDocument(neg[[j]], language = "english") 
} 

for (j in seq(pos)) { 
  pos[[j]]<-stemDocument(pos[[j]], language = "english") 
} 

#__________Initialization of the Sentiment analysis Procedure_______________

score.sentiment = function(docs, pos.words, neg.words, .progress='none')
{
  scores = laply(docs_s, function(docs, pos.words, neg.words) {
    
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=docs)
  return(scores.df)
}

#Topic1
V <- levels(mycorpus_dataframe_1$text)
X <- as.numeric(V)
docs_w_1 <- Data_Sentences$document[X]
#write.csv(docs_w_1,'Topic 1_Comments.csv', row.names=FALSE)

result=c()
docs<-docs_w_1 
m1=c()

for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  m1<- rbind(m1,newRow1)
}

#Statistics
summary(m1$Score)
minn<-min(m1$Score)
minn
maxx<-max(m1$Score)
maxx
mmm<-maxx-minn
mmm

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w_1),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
dev.new(width = 100, height = 100, unit = "px")
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Clustering
library(cluster)
library(fpc)
tdmr<-removeSparseTerms(dtm1w, 0.96)
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

#Histogram_1
h<-hist(m1$Score, main="Histogram for the Sentiment by Topic 1", xlab="Scores", 
        ylab="Number of of Opinions",right=FALSE,border="blue", col="green",
        freq=TRUE,las=1,xlim=c(minn,maxx),breaks=mmm)

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
#m1$Score
#h$count

#Histogram_2
hist(m1$Score, main="Histogram for the Sentiment by Topic 1", xlab="Scores", 
     ylab="Probability", border="blue", col="green",prob = TRUE,right=FALSE,
     xlim=c(minn,maxx),breaks=mmm)

lines(density(m1$Score))
m11<-as.matrix(m1)
#write.csv(m11, file="Sent_1.csv")

#Division into positive and negative opinions

pos1<-m1[m1$Score>=1,]
length(pos1$Score)

neu1<-m1[(m1$Score<1)&(m1$Score>=0),]
length(neu1$Score)

neg1<-m1[m1$Score<0,]
length(neg1$Score)

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

pos_docs_1_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
#write.csv(pos_docs_1_dataframe,'Pos_Topic 1_Comments.csv', row.names=FALSE)

neg_docs_1_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
#write.csv(neg_docs_1_dataframe ,'Neg_Topic 1_Comments.csv', row.names=FALSE)

neu_docs_1_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
#write.csv(neu_docs_1_dataframe ,'Neu_Topic 1_Comments.csv', row.names=FALSE)


###############################################################
#Topic2
V <- levels(mycorpus_dataframe_2$text)
X <- as.numeric(V)
docs_w_2 <- Data_Sentences$document[X]
#write.csv(docs_w_2,'Topic 2_Comments.csv', row.names=FALSE)

result=c()
docs<-docs_w_2 
m2=c()

for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  m2<- rbind(m2,newRow1)
}

#Statistics
summary(m2$Score)
minn<-min(m2$Score)
minn
maxx<-max(m2$Score)
maxx
mmm<-maxx-minn
mmm

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w_2),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Clustering
library(cluster)
library(fpc)
tdmr<-removeSparseTerms(dtm1w, 0.96)
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

#Histogram_1
h<-hist(m2$Score, main="Histogram for the Sentiment by Topic 2", xlab="Scores", 
        ylab="Number of of Opinions",right=FALSE,border="blue", col="green",
        freq=TRUE,las=1,xlim=c(minn,maxx),breaks=mmm)

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
#m2$Score
#h$count

#Histogram_2
hist(m2$Score, main="Histogram for the Sentiment by Topic 2", xlab="Scores", 
     ylab="Probability", border="blue", col="green",prob = TRUE,right=FALSE,
     xlim=c(minn,maxx),breaks=mmm)

lines(density(m2$Score))
m22<-as.matrix(m2)
#write.csv(m22, file="Sent_2.csv")

#Division into positive and negative opinions

pos1<-m2[m2$Score>=1,]
length(pos1$Score)

neu1<-m2[(m2$Score<1)&(m2$Score>=0),]
length(neu1$Score)

neg1<-m2[m2$Score<0,]
length(neg1$Score)

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
dev.new(width = 100, height = 100, unit = "px")
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

pos_docs_2_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
#write.csv(pos_docs_2_dataframe,'Pos_Topic 2_Comments.csv', row.names=FALSE)

neg_docs_2_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
#write.csv(neg_docs_2_dataframe ,'Neg_Topic 2_Comments.csv', row.names=FALSE)

neu_docs_2_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
#write.csv(neu_docs_2_dataframe ,'Neu_Topic 2_Comments.csv', row.names=FALSE)


######################################################################
#Topic3
V <- levels(mycorpus_dataframe_3$text)
X <- as.numeric(V)
docs_w_3 <- Data_Sentences$document[X]
#write.csv(docs_w_3,'Topic 3_Comments.csv', row.names=FALSE)

result=c()
docs<-docs_w_3 
m3=c()

for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  m3<- rbind(m3,newRow1)
}

#Statistics
summary(m3$Score)
minn<-min(m3$Score)
minn
maxx<-max(m3$Score)
maxx
mmm<-maxx-minn
mmm

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w_3),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Clustering
library(cluster)
library(fpc)
tdmr<-removeSparseTerms(dtm1w, 0.96)
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

#Histogram_1
h<-hist(m3$Score, main="Histogram for the Sentiment by Topic 3", xlab="Scores", 
        ylab="Number of of Opinions",right=FALSE,border="blue", col="green",
        freq=TRUE,las=1,xlim=c(minn,maxx),breaks=mmm)

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
#m1$Score
#h$count

#Histogram_2
hist(m3$Score, main="Histogram for the Sentiment by Topic 3", xlab="Scores", 
     ylab="Probability", border="blue", col="green",prob = TRUE,right=FALSE,
     xlim=c(minn,maxx),breaks=mmm)

lines(density(m3$Score))
m33<-as.matrix(m3)
#write.csv(m33, file="Sent_3.csv")

#Division into positive and negative opinions

pos1<-m3[m3$Score>=1,]
length(pos1$Score)

neu1<-m3[(m3$Score<1)&(m3$Score>=0),]
length(neu1$Score)

neg1<-m3[m3$Score<0,]
length(neg1$Score)

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

pos_docs_3_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
#write.csv(pos_docs_3_dataframe,'Pos_Topic 3_Comments.csv', row.names=FALSE)

neg_docs_3_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
#write.csv(neg_docs_3_dataframe ,'Neg_Topic 3_Comments.csv', row.names=FALSE)

neu_docs_3_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
#write.csv(neu_docs_3_dataframe ,'Neu_Topic 3_Comments.csv', row.names=FALSE)


###############################################################
#Topic4
V <- levels(mycorpus_dataframe_4$text)
X <- as.numeric(V)
docs_w_4 <- Data_Sentences$document[X]
#write.csv(docs_w_4,'Topic 4_Comments.csv', row.names=FALSE)

result=c()
docs<-docs_w_4 
m4=c()

for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  m4<- rbind(m4,newRow1)
}

#Statistics
summary(m4$Score)
minn<-min(m4$Score)
minn
maxx<-max(m4$Score)
maxx
mmm<-maxx-minn
mmm

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w_4),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Clustering
library(cluster)
library(fpc)
tdmr<-removeSparseTerms(dtm1w, 0.98)
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

#Histogram_1
h<-hist(m4$Score, main="Histogram for the Sentiment by Topic 4", xlab="Scores", 
        ylab="Number of of Opinions",right=FALSE,border="blue", col="green",
        freq=TRUE,las=1,xlim=c(minn,maxx),breaks=mmm)

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
#m4$Score
#h$count

#Histogram_2
hist(m4$Score, main="Histogram for the Sentiment by Topic 4", xlab="Scores", 
     ylab="Probability", border="blue", col="green",prob = TRUE,right=FALSE,
     xlim=c(minn,maxx),breaks=mmm)

lines(density(m4$Score))
m44<-as.matrix(m4)
#write.csv(m44, file="Sent_4.csv")

#Division into positive and negative opinions

pos1<-m4[m4$Score>=1,]
length(pos1$Score)

neu1<-m4[(m4$Score<1)&(m4$Score>=0),]
length(neu1$Score)

neg1<-m4[m4$Score<0,]
length(neg1$Score)

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

pos_docs_4_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
#write.csv(pos_docs_4_dataframe,'Pos_Topic 4_Comments.csv', row.names=FALSE)

neg_docs_4_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
#write.csv(neg_docs_4_dataframe ,'Neg_Topic 4_Comments.csv', row.names=FALSE)

neu_docs_4_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
#write.csv(neu_docs_4_dataframe ,'Neu_Topic 4_Comments.csv', row.names=FALSE)


###################################################################
#Topic5
V <- levels(mycorpus_dataframe_5$text)
X <- as.numeric(V)
docs_w_5 <- Data_Sentences$document[X]
#write.csv(docs_w_5,'Topic 5_Comments.csv', row.names=FALSE)

result=c()
docs<-docs_w_5 
m5=c()

for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  m5<- rbind(m5,newRow1)
}

#Statistics
summary(m5$Score)
minn<-min(m5$Score)
minn
maxx<-max(m5$Score)
maxx
mmm<-maxx-minn
mmm

#WORDCLOUD
docc <-VCorpus(x = VectorSource(docs_w_5),
               readerControl = list(reader=readPlain,
                                    language="en"))
docc<-tm_map(docc, removeWords, stopwords("english"))
docc<-tm_map(docc, removeWords, c("will","get","told","just","never","the","back","now","one"))
docc<-tm_map(docc, removePunctuation)
docc <- tm_map(docc, stripWhitespace) 
library(SnowballC) 

for (j in seq(docc)) { 
  docc[[j]]<-stemDocument(docc[[j]], language = "english") 
} 
dtm1w<-TermDocumentMatrix(docc)
m<-as.matrix(dtm1w)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Clustering
library(cluster)
library(fpc)
tdmr<-removeSparseTerms(dtm1w, 0.96)
d <- dist(tdmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

#Histogram_1
h<-hist(m5$Score, main="Histogram for the Sentiment by Topic 5", xlab="Scores", 
        ylab="Number of of Opinions",right=FALSE,border="blue", col="green",
        freq=TRUE,las=1,xlim=c(minn,maxx),breaks=mmm)

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
#m5$Score
#h$count

#Histogram_2
hist(m5$Score, main="Histogram for the Sentiment by Topic 5", xlab="Scores", 
     ylab="Probability", border="blue", col="green",prob = TRUE,right=FALSE,
     xlim=c(minn,maxx),breaks=mmm)

lines(density(m5$Score))
m55<-as.matrix(m5)
#write.csv(m55, file="Sent_1.csv")

#Division into positive and negative opinions

pos1<-m5[m5$Score>=1,]
length(pos1$Score)

neu1<-m5[(m5$Score<1)&(m5$Score>=0),]
length(neu1$Score)

neg1<-m5[m5$Score<0,]
length(neg1$Score)

pos_docs_1 <- Corpus(VectorSource(pos1$Documents))
neu_docs_1 <- Corpus(VectorSource(neu1$Documents))
neg_docs_1 <- Corpus(VectorSource(neg1$Documents))

#WORDCLOUD POSITIVE
pos_docs_1<-tm_map(pos_docs_1, removeWords, stopwords("english"))
pos_docs_1<-tm_map(pos_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
pos_docs_1<-tm_map(pos_docs_1, removePunctuation)
pos_docs_1 <- tm_map(pos_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(pos_docs_1)) { 
  pos_docs_1[[j]]<-stemDocument(pos_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(pos_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEUTRAL
neu_docs_1<-tm_map(neu_docs_1, removeWords, stopwords("english"))
neu_docs_1<-tm_map(neu_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neu_docs_1<-tm_map(neu_docs_1, removePunctuation)
neu_docs_1 <- tm_map(neu_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neu_docs_1)) { 
  neu_docs_1[[j]]<-stemDocument(neu_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neu_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#WORDCLOUD NEG

neg_docs_1<-tm_map(neg_docs_1, removeWords, stopwords("english"))
neg_docs_1<-tm_map(neg_docs_1, removeWords, c("will","get","told","just","never","the","back","now","one"))
neg_docs_1<-tm_map(neg_docs_1, removePunctuation)
neg_docs_1 <- tm_map(neg_docs_1, stripWhitespace) 
library(SnowballC) 

for (j in seq(neg_docs_1)) { 
  neg_docs_1[[j]]<-stemDocument(neg_docs_1[[j]], language = "english") 
} 
dtmpos<-TermDocumentMatrix(neg_docs_1)
m<-as.matrix(dtmpos)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,12)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

pos_docs_5_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F)
#write.csv(pos_docs_5_dataframe,'Pos_Topic 5_Comments.csv', row.names=FALSE)

neg_docs_5_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F)
#write.csv(neg_docs_5_dataframe ,'Neg_Topic 5_Comments.csv', row.names=FALSE)

neu_docs_5_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F)
#write.csv(neu_docs_5_dataframe ,'Neu_Topic 5_Comments.csv', row.names=FALSE)

