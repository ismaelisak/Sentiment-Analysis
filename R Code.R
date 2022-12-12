## Credit Card Sentiment Analysis
library(tm)
library(tidyverse)
library(data.table)

setwd("/Users/ismaelisak/R Studio Files/Sentiment Analysis/Data/")
complaints <- fread("complaints.csv", nrows = 100000)

## Removing empty complaint narrative cells


narrative <- complaints %>% filter(`Consumer complaint narrative`!="")


write.csv(narrative, "narrative.csv", row.names=FALSE)
## Build corpus



corpus <- iconv(narrative$`Consumer complaint narrative`, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


## Clean text


corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeXs <- function(x) gsub('xxxx', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeXs))
cleanset <- tm_map(cleanset, stripWhitespace)

inspect(cleanset[1:5])



## Term document matrix


tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


## Bar plot


w <- rowSums(tdm)

which.max(w)
w <- subset(w, w>=1000)
barplot(w,
        las = 2,
        col = rainbow(50))



## Word cloud


library(wordcloud)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 100,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
head(w)
wordcloud2(w,
           size = 0.7,
           shape = 'triangle-forward',
           rotateRatio = 0.5,
           minSize = 1)


## Sentiment analysis


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)



## Obtain sentiment scores


s <- get_nrc_sentiment(content(cleanset))
head(s)


## Bar plot


barplot(colSums(s),
        las = 3,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Credit Complaints')


Companies <- narrative %>% group_by(Company)

Companies %>% count(as.factor(`Timely response?`))

table(Companies$`Timely response?`)

ordered_companies <- count(Companies$Company) %>% arrange(desc(freq))

ordered_companies$x[1:20]

Top_Companies <- Companies %>% filter(Company %in% ordered_companies$x[1:10])

count(Top_Companies$Company) %>% arrange(desc(freq))

temp <- count(Top_Companies$Company) %>% arrange(desc(freq))
ggplot(temp, aes(reorder(x,-freq), freq, fill=x)) +
  geom_col() +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) + labs(x="Companies")+guides(fill=guide_legend(title="Companies"))

  