#Required packages

#install.packages("stm") 
#install.packages("quanteda")  
#install.packages("stringr",repos = "http://cran.us.r-project.org") 
#install.packages("igraph") 
#install.packages("stmCorrViz") 
#install.packages("plotly")
#install.packages("tidytext")

#Lib ppackages

library(stm) 
library(quanteda)
library(stringr)
library(igraph) 
library(stmCorrViz)
library(ggplot2)


#A. Reading in data from a “spreadsheet”
setwd('C:/Users/user/Desktop/Projekt_Structural_modeling/final')
data <- read.csv2("sentiment_comments.csv")
#View(data)

#B. Cleaning the data
data<-data[!is.na(data$comment_body),]
data$comment_body = str_replace_all(data$comment_body, "/"," ")
data$comment_body = str_replace_all(data$comment_body, "&#x27;|&quot;|&#x2F;", "'")
data$comment_body = str_replace_all(data$comment_body, "<a(.*?)>", " ")
data$comment_body = str_replace_all(data$comment_body, "&gt;|&lt;|&amp;", " ")
data$comment_body = str_replace_all(data$comment_body, "&#[:digit:]+;", " ")
data$comment_body = str_remove_all(data$comment_body, "<[^>]*>")
docss<-data$comment_body
remove.words <- c("dont","will","can","one","also", "get", "-", "donâ€™t" , "itâ€™" , "canâ€™t")
length(docss)
#View(docss)
#C. The textProcessor function
processed <- textProcessor(docss, metadata=data, lowercase=TRUE, removestopwords=TRUE, 
                           removenumbers=TRUE, removepunctuation=TRUE, stem=TRUE, 
                           wordLengths=c(3, Inf), sparselevel=1, language="en", 
                           verbose=TRUE, onlycharacter= FALSE, striphtml=FALSE, 
                           customstopwords=remove.words)
meta<-processed$meta
#View(meta)
vocab<-processed$vocab
#View(vocab)
docs<-processed$document
#View(docs)
#docs[1]
length(docs)

rr<-processed$docs.removed 
length(rr)

# building the list of documents correspondent to docss 
if (identical(rr, integer(0))){ 
  z<-docss 
} else { 
  z<-docss[-rr] }

#D. The prepDocuments function
plotRemoved(processed$documents, lower.thresh = seq(1, 25, by = 1))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     verbose=FALSE, lower.thresh = 2) 

docs <- out$documents 
vocab <- out$vocab 
meta <-out$meta
out$words.removed
out$docs.removed
out$tokens.removed
out$wordcounts

# building the list of documents correspondent to docs
rr1<-out$docs.removed 
rr1 
rr1 <- as.numeric(rr1) 
rr1

if (identical(rr1, numeric(0))){ 
  z 
} else { 
  z<-z[-rr1] 
}
#z
length(z)

#.3 ASSESSING THE MODELS
#slowa do usuniecia donâ€™t , itâ€™ , canâ€™t
#ASSESSING THE MODELS (10, 15, 20, 25, 30 topics)
set.seed(1234)
dev.new(width = 100, height = 100, unit = "px")
#10
model_NP_10Prrateby<-stm(docs, vocab, K=10, data=meta, init.type = "Spectral")
model_NP_10Prrateby
#labelTopics(model_NP_10Prrateby,n=15) #show top 15 words
plot.STM(model_NP_10Prrateby,main="Top Topics for 10 topics model", n=7) #show top topics

#15
model_NP_15Prrateby<-stm(docs, vocab, K=15, data=meta, init.type = "Spectral") 
model_NP_15Prrateby
#labelTopics(model_NP_15Prrateby,n=15) #show top 15 words
plot.STM(model_NP_15Prrateby,main="Top Topics for 15 topics model", n=7) #show top topics

#20
model_NP_20Prrateby<-stm(docs, vocab, K=20, data=meta, init.type = "Spectral") 
model_NP_20Prrateby
#labelTopics(model_NP_20Prrateby,n=15) #show top 15 words
plot.STM(model_NP_20Prrateby,main="Top Topics for 20 topics model", n=7) #show top topics

#25
model_NP_25Prrateby<-stm(docs, vocab, K=25, data=meta, init.type = "Spectral") 
#model_NP_25Prrateby
#labelTopics(model_NP_25Prrateby,n=15) #show top 15 words
plot.STM(model_NP_25Prrateby,main="Top Topics for 25 topics model", n=7) #show top topics

#30
model_NP_30Prrateby<-stm(docs, vocab, K=30, data=meta, init.type = "Spectral") 
#model_NP_30Prrateby
#labelTopics(model_NP_20Prrateby,n=15) #show top 15 words
plot.STM(model_NP_30Prrateby,main="Top Topics for 30 topics model", n=7) #show top topics

#Coherence-Exclusivity CHECKING
M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(model_NP_10Prrateby), semanticCoherence(model=model_NP_10Prrateby, docs), "Mod10"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model_NP_15Prrateby), semanticCoherence(model=model_NP_15Prrateby, docs), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model_NP_20Prrateby), semanticCoherence(model=model_NP_20Prrateby, docs), "Mod20")) 
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model_NP_25Prrateby), semanticCoherence(model=model_NP_25Prrateby, docs), "Mod25")) 
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model_NP_30Prrateby), semanticCoherence(model=model_NP_30Prrateby, docs), "Mod30"))

mean_Exc10<-mean(exclusivity(model_NP_10Prrateby)) 
mean_Exc10 
mean_Coh10<-mean(semanticCoherence(model=model_NP_10Prrateby, docs)) 
mean_Coh10

mean_Exc15<-mean(exclusivity(model_NP_15Prrateby)) 
mean_Exc15 
mean_Coh15<-mean(semanticCoherence(model=model_NP_15Prrateby, docs)) 
mean_Coh15

mean_Exc20<-mean(exclusivity(model_NP_20Prrateby)) 
mean_Exc20 
mean_Coh20<-mean(semanticCoherence(model=model_NP_20Prrateby, docs)) 
mean_Coh20

mean_Exc25<-mean(exclusivity(model_NP_25Prrateby)) 
mean_Exc25 
mean_Coh25<-mean(semanticCoherence(model=model_NP_25Prrateby, docs)) 
mean_Coh25

mean_Exc30<-mean(exclusivity(model_NP_30Prrateby)) 
mean_Exc30 
mean_Coh30<-mean(semanticCoherence(model=model_NP_30Prrateby, docs)) 
mean_Coh30

ModsExSem<-rbind(M10ExSem, M15ExSem, M20ExSem, M25ExSem, M30ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")
ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model)) +geom_point(size = 2, alpha = 0.7) + geom_text(aes(label=K), nudge_x=.05, nudge_y=.05) + labs(x = "Semantic coherence", y = "Exclusivity", title = "Comparing exclusivity and semantic coherence")
plotexcoer

#A. Model selection and search

model_NP_15Prrateby<-stm(docs, vocab, K=15, data=meta, init.type = "Spectral", verbose=FALSE) 
model_NP_15Prrateby 
labelTopics(model_NP_15Prrateby) 
capture.output(labelTopics(model_NP_15Prrateby), file = "Topics KeyWords.txt")

#frex tylko distinct

#B. Explore the topics
par(mfrow = c(1,2))
plot(model_NP_15Prrateby, type = 'labels', labeltype = 'frex', main = 'FREX',text.cex=1.0, n=8) 
plot(model_NP_15Prrateby, type = 'labels', labeltype = 'prob', main = 'PROB',text.cex=1.0, n=8)

dev.off()
set.seed(1234)
dev.new(width = 100, height = 100, unit = "px")
cloud(model_NP_15Prrateby, topic = 1)
plot(model_NP_15Prrateby, type="summary", xlim=c(0,.4))

plot(model_NP_15Prrateby, type = "perspectives", topics = c(10,15), main = "Topic contrasts") # Topics #10 and #5

plot.STM(model_NP_15Prrateby, "hist") 
topicprop15<-model_NP_15Prrateby$theta 
topicprop15 
write.csv(topicprop15, file = "topicprop15.csv")


colSums(topicprop15)/1993


library(tidytext)
td_theta <- tidytext::tidy(model_NP_15Prrateby, matrix = "theta")
selectiontdthteta<-td_theta[td_theta$document%in%c(1:15),]#select the first 15 documents

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) + geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) + facet_wrap(~ document, ncol = 3) + labs(title = "Theta values per document", y = expression(theta), x = "Topic")
thetaplot1

library(dplyr)
#if (!require(devtools)) install.packages("devtools")
#install.packages("drlib", repos = "http://cran.us.r-project.org")
require(devtools)
#library("drlib") #14 strona
#install.packages("ggplot2")

td_beta <- tidytext::tidy(model_NP_15Prrateby)
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)
td_beta %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta), title = "Highest word probabilities for each topic", subtitle = "Different words are associated with different topics")

betaT1<-td_beta %>% 
  mutate(topic = paste0("Topic ", topic), 
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1") 

#beta values for topic 1 
betaplotT1<-ggplot(betaT1[betaT1$beta>0.005,], aes(term, beta, fill = as.factor(topic))) + geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta), title = "Word probabilities for Topic 1") 
#plot word probabilities higher than 0.005 for topic 1 
betaplotT1

hh<-findThoughts(model_NP_15Prrateby, z, n = 5, topics = 4)$docs[[1]] 
hh 
options(repr.plot.width=10, repr.plot.height=12, repr.plot.res=100) 
par(mfrow=c(1,2)) 
plot.STM(model_NP_15Prrateby, "labels",text.cex=1.) 
plotQuote(hh, width=100, maxwidth=1000, text.cex=1., main="Topic 4")

library(stmCorrViz) 
mod.out.corr <- topicCorr(model_NP_15Prrateby) 
plot(mod.out.corr)

stmCorrViz(model_NP_15Prrateby, "stm-interactive-correlation.html", documents_raw=data$documents, documents_matrix=out$documents)

#C. Topics Labeling
model_NP_15Prrateby_Sent<-stm(docs, vocab, prevalence=~sentiment, K=15, data=meta, init.type = "Spectral", verbose=FALSE)
model_NP_15Prrateby_Sent
labelTopics(model_NP_15Prrateby_Sent)

predict_topics_NP<-estimateEffect(formula =~ sentiment, stmobj = model_NP_15Prrateby_Sent, metadata = out$meta, uncertainty = "Global")
head(summary(predict_topics_NP))

dev.off()
dev.new(width = 100, height = 100, unit = "px")
plot.estimateEffect(predict_topics_NP, model= model_NP_15Prrateby_Sent, 
                    cov.value1="pos", cov.value2="neg", covariate="sentiment", 
                    method="difference", nsims = 100,
                    xlab="More Negative ..................................................................................................................................... More Positive", 
                    labeltype="custom", custom.labels=c(1:30),ci.level=.99)

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
library(tidystm)
effect00 <- extract.estimateEffect(x = predict_topics_NP, covariate = "sentiment", model = model_NP_15Prrateby_Sent, method = "pointestimate", )
knitr::kable(effect00)
write.csv(effect00, file = "effect15.csv")


#______________________Charts__________________________
library(stminsights)
effects <- get_effects(estimates = predict_topics_NP, variable = "sentiment", type = "pointestimate")                 
effects

# plot effects for topics 14 and 13 pos
effects %>% filter(topic == 7) %>% 
  ggplot(aes(x = value, y = proportion)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) + 
  coord_flip() + theme_light() + labs(x = 'Topic 7', y = 'Topic Proportion')

effects %>% filter(topic == 8) %>% 
  ggplot(aes(x = value, y = proportion)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) + 
  coord_flip() + theme_light() + labs(x = 'Topic 8', y = 'Topic Proportion')

#1.6 IDENTIFICATION THE TIME EFFECT

#a) Create the STM model model_NP_15_1Prrateby_year uses both the “Sentiment” variable as well as the “Year” variable (prevalence=~Sentiment+Year)
model_NP_15_1Prrateby_year<-stm(docs, vocab, prevalence=~sentiment+year, K=15, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_15_1Prrateby_year)
model_NP_15_1Prrateby_year

#Topics prevalence table
topicprop15_1<- model_NP_15_1Prrateby_year$theta
topicprop15_1
write.csv(topicprop15_1, file = "topicprop15_1.csv")
View(topicprop15_1)

#b) Estimate the relationship between Time and Topics

predict_topics_1_NP<-estimateEffect(formula = ~ sentiment + year, stmobj = model_NP_15_1Prrateby_year, metadata = out$meta, uncertainty = "Global")
predict_topics_1_NP

effect15_1 <- extract.estimateEffect(x = predict_topics_1_NP, covariate = "year", moderator = "sentiment", model = model_NP_15_1Prrateby_year, method = "pointestimate", moderator.value ="pos" )

effect15_1
knitr::kable(effect15_1)
write.csv(effect15_1, file = "effect15_1.csv")
effect15_2 <- extract.estimateEffect(x = predict_topics_1_NP, covariate = "year", moderator = "sentiment",model = model_NP_15_1Prrateby_year, method = "pointestimate", moderator.value ="neg")
knitr::kable(effect15_2)
write.csv(effect15_2, file = "effect15_2.csv")

#c) Build the plots the relationship between Time and Topics
effect <- lapply(c("neg", "pos"), function(i) { extract.estimateEffect(x = predict_topics_1_NP, covariate = "year", method = "continuous", model = model_NP_15_1Prrateby_year, labeltype = "prob", topic=15, n = 7, ci.level = 0.95, moderator = "sentiment", moderator.value = i) })
head(effect)
effect <- do.call("rbind", effect)
effect

## Plot it with ggplot2 and facet by topic instead.
ggplot(effect, aes(x = covariate.value, y = estimate, ymin = ci.lower, ymax = ci.upper, group = moderator.value, fill = factor(moderator.value))) + facet_wrap(~ label, nrow = 2) + geom_ribbon(alpha = .5) + geom_line() + scale_x_continuous(labels = function(x) ifelse(x == 1, "1\nREP", ifelse(x == 0, "0\nDEM", x))) + labs(x = "year", y = "Expected Topic Proportion", fill = "sentiment") + theme(legend.position = "bottom")

#1.7 IDENTIFICATION THE Company TYPE EFFECT
model_NP_15_2Prrateby<-stm(docs,vocab, prevalence=~sentiment+company_name, K=15, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_15_2Prrateby)
model_NP_15_2Prrateby
topicprop15_2<-model_NP_15_2Prrateby$theta
topicprop15_2
write.csv(topicprop15_2, file = "topicprop15_2.csv")


predict_topics_2_NP<-estimateEffect(formula = ~ sentiment + company_name, stmobj = model_NP_15_2Prrateby, metadata = out$meta, uncertainty = "Global")

plot.estimateEffect(predict_topics_2_NP, model=model_NP_15_2Prrateby, cov.value1="Apple", cov.value2="Samsung Electronics", covariate="company_name", method="difference", nsims = 100, xlab="Samsung ............................................................................... Apple", labeltype="custom", custom.labels=c(1:15),ci.level=.99)

model_NP_15_3Prrateby<-stm(docs,vocab, prevalence=~sentiment+company_name, K=15, data=meta, init.type = "Spectral", content=~company_name, verbose=FALSE)
labelTopics(model_NP_15_3Prrateby)
model_NP_15_3Prrateby

plot(model_NP_15_3Prrateby, type="perspectives", topics=7)
plot(model_NP_15_3Prrateby, type="perspectives", topics=1)

effect15_3 <- extract.estimateEffect(x = predict_topics_2_NP, covariate = "company_name", moderator = "sentiment", model = model_NP_15_2Prrateby, method = "pointestimate", moderator.value ="pos" )
effect15_3
knitr::kable(effect15_3)
write.csv(effect15_3, file = "effect15_3.csv")
effect15_3$estimate

View(effect15_3)
effect15_4 <- extract.estimateEffect(x = predict_topics_2_NP, covariate = "company_name", moderator = "Sentiment", model = model_NP_15_2Prrateby, method = "pointestimate", moderator.value ="neg")
knitr::kable(effect15_4)
write.csv(effect15_4, file = "effect15_4.csv")

effect15_4$estimate
View(effect15_4)
