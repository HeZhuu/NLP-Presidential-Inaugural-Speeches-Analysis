library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)

#folder.path="../data/InauguralSpeeches/"
folder.path="C:/Users/Simon/Desktop/Zhuhe/Spr2017-Proj1-zhuuu-master/data/InauguralSpeeches/"
speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)

length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))


ff.all<-tm_map(ff.all, stripWhitespace)
ff.all<-tm_map(ff.all, content_transformer(tolower))
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)
tdm.all<-TermDocumentMatrix(ff.all)
tdm.tidy=tidy(tdm.all)
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))

dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
ff.dtm=tidy(dtm)
#ff.all<-tm_map(ff.all, stemDocument)

for(i in 1:length(speeches)){
#  #crude=stemDocument(ff.all[[i]])
#  crude=Corpus(VectorSource(ff.all[[i]]))
#  tdm <- TermDocumentMatrix(crude[1], list(wordLengths=c(3, Inf)))
#  m <- as.matrix(tdm)
#  v <- sort(rowSums(m),decreasing=TRUE)
#  d <- data.frame(word = names(v),freq=v)
  
  png(paste("C:/Users/Simon/Desktop/Zhuhe/Spr2017-Proj1-zhuuu-master/output/", prez.out[i], ".png", sep=""),
      width=300, height=300)
  wordcloud(ff.dtm$term[ff.dtm$document==speeches[i]],
            ff.dtm$count[ff.dtm$document==speeches[i]],
              scale=c(5,0.5),
              max.words=200,
              min.freq=1,
              random.order=FALSE,
              rot.per=0,
              use.r.layout=FALSE,
              random.color=FALSE,
              colors=brewer.pal(10,"Blues"), 
            main=prez.out[i])
  dev.off()
  
}

