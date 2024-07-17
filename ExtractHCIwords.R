################new testing words extraction###############
library(tm)
library(stringr)
require(plyr)
sd1=data.frame(tesingdata$V1)
View(sd1)

library(caret)
###train LVM model
Lm_model <- train(FullHCIfact$V1~FullHCIfact$V1, data=FullHCIfact, method = "Lm")
summary(lm_model)

###

myCorpus <- Corpus(VectorSource(as.character(sd1$tesingdata.V1))) 

inspect(myCorpus)

# remove anything other than English letters or space
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myStopwords <- c(stopwords('english'), "But", "somewhat","also","eyes")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
dar<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)
dar

ap=dar$text
me1=unlist(str_split(ap, '\\.+'))
me1=unlist(str_split(me1, '\\s+'))
me1
xc =as.matrix(me1)
xc

dhj =c(me1)
View(dhj)
dhj

fgj=FullHCIfact$V1
fgj
#####################
View(FullHCIfact$V1)
# Create the document term matrix
library(RTextTools)
dtmatrix1 <- create_matrix(FullHCIfact["V1"])
dtmatrix2 <- create_matrix(xc)
# Configure the training data
container <- create_container(dtmatrix1, FullHCIfact$V1, trainSize=1:17, virgin=FALSE)
container
# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)
model
xc
# create a prediction document term matrix
trace("create_matrix",edit=T)
predMatrix <- create_matrix(xc, originalMatrix=dtmatrix1)
predMatrix
# create the corresponding container
predSize = length(xc);
predSize
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize , virgin = TRUE)
predictionContainer
# predict
results <- classify_model(predictionContainer, model)
results
View(results)
svm1_classify <- classify_model(predictionContainer, model, "svm1")

da <-data.frame(xc)
#model=svm(data=d)
library(e1071)



summary(model)


###################

Attributesforweb=intersect(dhj,fgj)
View(Attributesforweb)
View(dhj)
ins=as.data.frame(Attributesforweb)
Attributesforweb=as.vector(Attributesforweb)

#########exporing data################
library(rio)
###set path

setwd(file.path("D:","research data set","asd"))
getwd()
export(ins, "HCIWORDS1.csv")


################################end comparison & save it as csv result######



myCorpus2 <- Corpus(VectorSource(as.character(me1))) 

############tdm
tdm <- TermDocumentMatrix(myCorpus2,control = list(wordLengths = c(1, Inf)))
tdm

(freq.terms <- findFreqTerms(tdm, lowfreq = 1))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df3 <- data.frame(term = names(term.freq), freq = term.freq)
df3
library(ggplot2)

ggplot(df3, aes(x = term, y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Count") + coord_flip()
findAssocs(tdm, "mining", 0.25)
library(graph)
library(Rgraphviz)
 source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)
library(Rcmdr)
######word cloud
library(RColorBrewer)
mp <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(mp), decreasing = T)
# colors
word.freq
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
library(wordcloud)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,random.order = F, colors = pal)
#sd=data.frame(me1)
#####clustering
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
mp1 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(mp1))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,hang =-1 )
rect.hclust(fit, k = 2)
#######################

#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

#cut into 2 subtrees � try 3 and 5
rect.hclust(fit,2)

library(fpc) 
library(cluster)
dtm <-DocumentTermMatrix(myCorpus2,control = list(wordLengths = c(1, Inf)))
myt <-as.matrix(dtm)

d <- dist(t(dtm), method="euclidian")
d
kfit <- kmeans(d, 5)   
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

library(clValid)
clmethods <- c("hierarchical","kmeans")
internval <- clValid(myt, nClust = 2:5, clMethods = clmethods, validation = "internal")

summary(internval)


###########topic modelling
dtm <- as.DocumentTermMatrix(tdm)
dtm
library(topicmodels)
lda <- LDA(dtm, k = 1) # find 8 topics
(term <- terms(lda, 6))

########cleaning
myCorpus <- Corpus(VectorSource(sd$tesingdata.v1))
myCorpus


removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus
View(myCorpus)
myCorpuscopy <-myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)

View(dar)
view(dar)
