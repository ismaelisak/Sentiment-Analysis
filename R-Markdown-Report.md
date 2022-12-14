Sentiment Analysis using R Studio
================

## Credit Reporting Agencies Complaints

“Your most unhappy customers are your greatest source of learning.”

-   Bill Gates

Making sense of customer complaints is a difficult task unless you use a
limited set of responses to choose from. Often that is frustrating to
those making the complaint because their particular issue might not be
listed. On the other hand it’s not feasible for companies to spend
hundreds of hours searching through massive amounts of complaints for
feedback.

That is where sentiment analysis comes in, and uses a predefined list of
words to gain the sentiment of each response. What’s great about this
approach is that it can work on millions of responses and distill them
into categories to better understand the overall feeling of the
respondents. This a great example of turning disorganized and messy data
into actionable information.

For my analysis I am going to use the [Consumer Complaint
Database](https://www.consumerfinance.gov/data-research/consumer-complaints/)
managed by the Consumer Financial Protection Bureau. I will first
organize the data, then create a word cloud of the most frequent words,
then get into the sentiment analysis. I will then summarize other
findings including the number of complaints by credit agency and closing
of cases.

## Code

Here is the [GitHub](https://github.com/ismaelisak/Sentiment-Analysis)
page with the R code but I will briefly highlight some key sections.

I first used the function fread() from the data.table library to import
the first million complaints.

``` r
complaints <- fread("complaints.csv", nrows = 100000)
```

I then removed the rows the lacked any consumer narrative and saved it
as narrative.

``` r
narrative <- complaints %>% filter(`Consumer complaint narrative`!="")
```

Then began with creating a corpus where the complaints are stored and
converted them to utf-8-mac, which made sure there weren’t any
inconvenient characters.

``` r
corpus <- iconv(narrative$`Consumer complaint narrative`, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
```

Then I went to cleaning the text by removing punctuation and
capitalization, along with the censoring of private information.

``` r
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
```

Then creating the Term Document Matrix and storing it as a matrix, where
it totals the frequency of each word.

``` r
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
```

Then creating the bar plot of words.

``` r
w <- rowSums(tdm)
w <- subset(w, w>=1000)
barplot(w,
        las = 2,
        col = rainbow(50))
```

Then the word clouds.

``` r
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
```

Followed by the sentiment analysis.

``` r
s <- get_nrc_sentiment(content(cleanset))
head(s)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Credit Complaints')
```

## Results

First we can look at the barplot created for most frequent words.

![](Data/Figure1.png)

We can see that most of the sentiments are positive, followed by trust,
and then negative. Which generally follows from our expectations, and
considering all the complaints received some sort of response from the
company that is expected.

Here is the word cloud we created from the most frequent words used.

![](Data/WordCloud2.png)

## Conclusion

In this report we made a sentiment analysis from the Consumer Financial
Protection Bureau complaints database. In this example we looked at all
credit reporting agencies together but it can be boiled down to specific
ones and analyze their particular sentiment scores.

Here is a graph showing the most frequent companies mentioned in the
complaints. It’s what you’d expect with the biggest 3 being Transunion,
Equifax, and Experian but there are many other smaller companies listed
here.

![](Data/Rplot01.png)
