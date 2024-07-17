#pos.senti=c(sentimentpolarity)
#hci.words=c(attributeshci)

#hci.words$Attributesforweb[1]

#tesingdata$V1

##############################testing correct###############
require(stringr)
require(tm)
require(plyr)
library(tidyr)
library(reshape2)

library(rio)
#export(m,"FinalDatasetCustProductCLuster.csv")
#export(m,"FinalDatasetCustProductCLuster.txt")


##################################
#View(tesingdata)
#tesingdata
#vm =tesingdata$V1
#vm
#xyt=unlist(str_split(vm, '\\.+'))
#xyt=unlist(str_split(vm, '\\.+'))
#xyt





# selected sentences into vector



############################################
hci.words=c(attributes)

View(hci.words)


#correct for loop
i=1
for(i in i:5){
  
  if(i==1){
    dif1=fun3(i)
  }


 else if(i==2){
  
      dif2=fun3(i)
      dif2
      
       }

  if(i==3){
   
      dif3=fun3(i)
     
     
    }
  if(i==4){
      dif4=fun3(i)
      
    }
  
  if(i==5){
    
      dif5=fun3(i)
     
  }

  z <-as.vector(rbind(dif1,dif2,dif3,dif4,dif5))


  
  #######sort/order the vector 
 sortedvec <- z[order(z$score),]
 ###########export as csv

 
}


###########export the reults to csv##############
library(rio)
export(sortedvec,"modelresults.csv")
##########function for split down and match and get score of each sentences related hci factors

fun2<-function(n,i){
  #
  vm =tesingdata$V1
  
  xyt=unlist(str_split(vm, '\\.+'))
  
  x =hci.words$Attributesforweb[i]
  
  abc= xyt[grep(paste(x, collapse="|"),xyt)]
  
  o=sentiment$Sentiment[n]
  
  p=sentiment$Polarity[n]

  me=unlist(str_split(abc, '\\s+'))

  acd= me[grep(paste(o, collapse="|"),me)]

  dfgh=length(unlist(strsplit(acd," ")))

  score1=dfgh*p
  score1
  return(score1)
}


###########################function3 identify sentiment word and get score through funtion2

fun3<-function(i){
  
  n=1
for (n in n:5){
if(n==1){ 
  tya1=fun2(n,i)
  
 
}
  else if(n==2){ 
    tya2=fun2(n,i)
    #tya2
  }

  else if(n==3){ 
    tya3=fun2(n,i)
   
  } 

  else if(n==4){ 
    tya4=fun2(n,i)
 
  }

  else if(n==5){ 
    tya5=fun2(n,i)
  
  }
  

  attributes=hci.words$Attributesforweb[i]
  #attributes
  
  score=c(tya1+tya2+tya3+tya4+tya5)
  
  #score
  #df = data.frame(attributes,score)
  ###get diffrent attributes average#####
  Negative_Score=c(tya4+tya5)
  
  Negative_Score
  Positive_Count=c(tya1+tya2)
  Neutral_Score=c(tya3)
 
  full=Positive_Count +Neutral_Score -Negative_Score
  full
  Average=score/full
  Average
 # df11=data.frame(attributes,Positive_Count,Negative_Score)
 
  df = data.frame(attributes,Positive_Count,Negative_Score,Neutral_Score,score,Average)
  #df
   #afg=data.matrix(df)

}
  
  return(df)
 # return(df11)
}



sb <-as.vector(rbind(tya1,tya2,tya3,tya4,tya5))
sb
 ##############################ending scoring###########
########################################visualization
library(RColorBrewer)
library(ggplot2)
pal1 <- brewer.pal(9, "BrBG")
pal1 <- pal[-(1:4)]
###visualization
sortedvec[["sign"]] = ifelse(sortedvec[["score"]] >= 0, "positive", "negative")
pp<- ggplot(sortedvec, aes(x = attributes, y = score ,fill=sign )) +
  geom_bar(stat = "identity", colour="Black" ) +xlab("HCI Related Attributes") +
  ggtitle("Polarity Scores for each categories")+
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))+
  scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "red")) +
  ylab("Polarity Score") + coord_flip()+
  scale_y_continuous(breaks = round(seq(min(sortedvec$score), max(sortedvec$score), by = 1),20))

scale_x_continuous(breaks = c(FontSize,productdescription,ProductComparison), labels = c("Font Size,Product Description,Product Comparison"))
pp
counts <- table(sortedvec$Positive_Count, sortedvec$Negative_Score)

########################## Grouped Bar Plot########################


pp1<- ggplot(sortedvec, aes(x = attributes, y = Average ,fill=sign )) + geom_bar(stat = "identity", colour="Black" ) +xlab("HCI Related Attributes") +ggtitle("Range of scores for each categories")+
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))+scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "darkred")) + ylab("Polarity Score") + coord_flip()+ scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), labels = c("Worst", "Bad", "Fair","Good", "Very Good"))

pp1

pp3<- ggplot(sortedvec, aes(x = attributes, y = Positive_Count ,fill=sign )) + geom_bar(stat = "identity", colour="Black" )  +xlab("HCI Related Attributes") +scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "darkred")) + ylab("Polarity Score") + coord_flip()+ scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), labels = c("Worst", "Bad", "Fair","Good", "Very Good"))
pp3
############################## end############
###########positive and negative of each groups##### correct
df11=data.frame(sortedvec$attributes,sortedvec$Positive_Count,sortedvec$Negative_Score)
df11
data.m <- melt(df11, id.vars='sortedvec.attributes')
data.m
# plot everything
xyte <-ggplot(data.m, aes(sortedvec.attributes, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

xyte
xyte<- ggplot(data.m, aes(sortedvec.attributes,value )) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity" ) +xlab("HCI Related Attributes") +
  ggtitle("Polarity Scores for each categories")+
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))+ coord_flip()




library(plotly)

p3 <- plot_ly(sortedvec, y = ~attributes, x = ~Positive_Count, type = 'bar', name = 'Positive_Count') %>%
  add_trace(x = ~Negative_Score, name = 'Negative_Score') %>%
  layout(yaxis = list(title = 'Polarity Count'), barmode = 'group')
scale_y_continuous(breaks = round(seq(min(sortedvec$Negative_Score), max(sortedvec$Positive_Count), by = 1),20))
p3

ps <- plot_ly(sortedvec, x = ~attributes, y = ~score, type = 'bar', name = 'score') %>%
  add_trace(y = ~Negative_Score, name = 'Negative_Score') %>%
  layout(yaxis = list(title = 'Polarity Count'), barmode = 'stack')
ps
qp <- plot_ly(sortedvec, x = ~attributes, y = ~Negative_Score, type = 'bar', name = 'Negative_Score') %>%
  add_trace(y = ~Positive_Count, name = 'Positive_Count') %>%
  layout(yaxis = list(title = 'Polarity Count'), barmode = 'stack')

qp
########################################################


######3

