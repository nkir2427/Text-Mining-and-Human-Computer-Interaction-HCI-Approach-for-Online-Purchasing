pos.senti=c(sentimentpolarity)
NROW(pos.senti)
NCOL(pos.senti)
View(pos.senti)
hci.words=c(attributeshci)
NROW(attributeshci)
NCOL(attributeshci)
View(hci.words)
hci.words$attribute[1]

#accessign

FinalDatasetCustProductCLuster

df <- data.frame( c( attributesone ))
View(df)
DF2 <- df[which(df$ID==1 ] 
df[1,]
df[,]
i=1
for (i in df) {
  print(df[i,])
}

j=5
i=1
#for loop for identify hci words in dataset
for (i in hci.words) {
  print(hci.words$attribute[i])
  print(i)
  
  
}
x<-c(2,4,6,8,10)
for(i in x)
{
  print(i)
}

hci.words$attribute[2]
data$V1[1]
require(stringr)
require(tm)
require(plyr)
length(corpusset)

#corpus set is break down into sentences
corpusset = c(testingcodeing$review)
str(corpusset)
View(corpusset)
mycorpustestre <-Corpus(VectorSource(testingcodeing$review))

a<-list(attributeshci$attribute)
View(a)
x <- list(attributeshci$attribute)
View(x)
x[1][1]
str(mycorpustestre)
View(mycorpustestre)
list1word = str_split(mycorpustestre, '\\.+')
#word.list2 = str_split(mycorpustest, '.+')
View(list1word)

j=5
i=1
#for loop for identify hci words in dataset
for (i in i:j) {
 
  print(i)
  print.value(i)
  hci.words[i]
}
hci.words[$attribute,2]
n=1
i=1
mydframe <- data.frame(hci.words)
for(i in i:5){
 for(n in n:1){

 if(i==1){
    mydframe[i,n]
  print(mydframe)
 }
  hci.words$attribute[2]
  neg.matches = match(list1word, hci.words)
}
}
for (i in 1:NROW(hci.words))
  d=c(attributeshci)
View(d)
y=NROW(d)
  ####################################
i=1
j=1
s = c("Graphics", "Color", "Accuracy", "Stability", "Camerafunction")
y=NROW(s)
print(y)
for(i in 1:5){
  for(j in 1:NROW(d)){
    
    if()
    print(d[1]) 
  }

 print(s[i]) 
  
}

print(list1word[1]) 

for(j in 1:NROW(list1word)){
  
  if()
    print(list1word[j]) 
}

View(s)
#################################

for (i in i:5) {
  for (n in hci.words) {
    
  }
  
}


for (i in 1:NROW(hci.words)) {
  print(hci.words[i, ])
  hci.words$attribute[i]
  hci.words[i, 1]
  i+1
  # do more things with the data frame...
}
######
scores = laply(sentences, function(sentence, pos.words, neg.words){
  vx=FinalDatasetCustProductCLuster$
 
  para=c(FinalDatasetCustProductCLuster)
  View(para)
para=c("Graphics are perfect.colors are good.Graphics are good")
toMatch <- c("Graphics")
unlist(strsplit(para,split="\\."))[grep(paste(toMatch, collapse="|"),unlist(strsplit(FinalDatasetCustProductCLuster,split="\\.")))]
unlist(strsplit(para,split="\\."))[grep(paste(toMatch, collapse="|"),unlist(strsplit(para,split="\\.")))]
sentences<-unlist(strsplit(para,split="\\."))
sentences[grep(paste(toMatch, collapse="|"),sentences)]

word.list = str_split(unlist(strsplit(para,split="\\."))[grep(paste(toMatch, collapse="|"),unlist(strsplit(para,split="\\.")))], '\\s+')
View(word.list)
pos.matches = match(word.list, "Perfect")
pos.matches
r=1
m=1
for(r in r:3){
  for(m in m:2){
    words<-word.list[r,m]
    function(word.list,pos.senti  ){
      pos.matches = match(words,pos.senti )
      pos.matches
    }
  }}






listwordi = str_split(my, '\\.+')



#########value#########
require(stringr)
require(tm)
require(plyr)
View(my)

list1word =c(str_split(my, '\\.+')) 
View(list1word)

library(rio)
export(m,"FinalDatasetCustProductCLuster.csv")
export(m,"FinalDatasetCustProductCLuster.txt")
matrix( list1word[ , 1 ]}

###


am<-toString(list1word)
View(am)
jk <- matrix(list1word,,1)

colnames(jk) <- 1:1
jk[,1]
View(jk)
##
d=scan(FinalDatasetCustProductCLuster.csv)
m<- as.data.frame(list1word)

dataframe<-data.frame(text=unlist(sapply(m, `[`, "content")), stringsAsFactors=F)
dataframe[1,]
library(qdap)
as.data.frame(crude)
View(dataframe)
library(tidyr)
View(mydata)
# selected sentences into vector

sentences<-unlist(strsplit(my,split="\\."))

View(sentences)
jkl<-c(listwordi[grep(paste("Graphics", collapse="|"),listwordi)])

############################################
view(jkl)
View(jkl)
unite(list1word, new, 1:5, sep='')
unite(list1word, new,1:5, sep ='' )
print(list1word)

datasetnew = c(testingcodeing)
para=c("Graphics are perfect.colors are good.Graphics are good")
my=datasetnew$review
print(my)
view(datasetnew)
#correct for loop
i=1
x=1
for(i in i:4){
  
  if(i==1){
   x = hci.words$attribute[i]
 # print(x)

  newone=str_split(unlist(strsplit(list1word,split="\\."))[grep(paste(x, collapse="|"),unlist(strsplit(list1word,split="\\.")))], '\\s+')
    View(newone)
    
    
    
  }
}














mycorpustestre <-Corpus(VectorSource(testingcodeing$review))

###########################

# sometimes a list() is one level of hierarchy
# sometimes a list() is one level of hierarchy too much
words = unlist(sentences)
View(words)
require(plyr)
require(stringr)
# compare our words to the dictionaries of positive & negative terms
pos.matches = match(word.list, "Perfect")
#neg.matches = match(words, neg.words)
# match() returns the position of the matched term or NA
# we just want a TRUE/FALSE:
pos.matches = !is.na(pos.matches)
#neg.matches = !is.na(neg.matches)
# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
score = sum(pos.matches)
return(score)
view(score)
View(pos.senti)
View(pos.matches)
#, pos.words, neg.words, .progress=.progress )
score = data.frame(score=score, text=sentences)
}, pos.words, neg.words, .progress=.progress )


