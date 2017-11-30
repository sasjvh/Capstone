# download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "SwiftKey.zip")
# unzip("SwiftKey.zip")
# cmd<-system(paste("/RTools/bin/wc -l","final/en_US/en_US.blogs.txt"), intern=TRUE) 
# cmd<-strsplit(cmd, " ")[[1]][1] 
# cmd<-system(paste("/RTools/bin/wc -L","final/en_US/en_US.blogs.txt"), intern=TRUE) 
# cmd<-strsplit(cmd, " ")[[1]][1] 
con <- file("final/en_US/en_US.twitter.txt", "r")
f <- readLines(con, skipNul = TRUE)
close(con)
sum(grepl("love", f)) / sum(grepl("hate",f))
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", f))

library(dplyr)
text_df <- data_frame(line = 1:length(f), text = f)
library(tidytext)

text_df <- text_df %>%
  unnest_tokens(word, text)

text_df %>%
  count(word, sort = TRUE)

data(stop_words)

removeStops <- text_df %>%
  anti_join(stop_words)

removeStops %>%
  count(word, sort = TRUE)

bigrams <- data_frame(line = 1:length(f), text = f)
bigrams <- bigrams %>%
  unnest_tokens(ngram2, text, token = "ngrams", n = 2)
bigrams %>%
  count(ngram2, sort = TRUE)


corp <- Corpus(DirSource("final/en_US"),
               readerControl = list(reader = readPlain,
                                    language = "en_US",
                                    load = TRUE))

# Create function for graphing top 15 ngrams
graphTop15 <- function(textDataFrame, gramLength, titleText) {
  
  # Build ngrams
  df_ngram <- textDataFrame %>%
    unnest_tokens(ngram, text, token = "ngrams", n = gramLength) %>%
    count(ngram, sort = TRUE)
  
  # Graph top 15 ngrams
  df_ngram[1:15,] %>% mutate(ngram = reorder(ngram, n)) %>%
    ggplot(aes(ngram, n)) +
    geom_col() +
    labs(x = NULL, title = titleText) +
    coord_flip()
}

# Open a connection to the file
con <- file("final/en_US/en_US.twitter.txt", "r")

# Read the file into a data frame
textDataFrame <- data_frame(text = readLines(con, skipNul = TRUE))

# Close the connection
close(con)

graphTop15(textDataFrame, 1, "Most Frequent Twitter Unigrams")
graphTop15(textDataFrame, 2, "Most Frequent Twitter Bigrams")
graphTop15(textDataFrame, 3, "Most Frequent Twitter Trigrams")

sampleDF <- textDataFrame[sample(nrow(textDataFrame), replace = FALSE, size=0.1*nrow(textDataFrame)),]

df_ngram <- textDataFrame %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE) %>%
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
  group_by(word1, word2) %>%
  filter(n == max(n))

require(readtext)
require(quanteda)
require(data.table)
require(dplyr)
require(tidyr)
all_txt <- readtext("final/en_US/*.txt")
corp <- corpus(all_txt)
rm(all_txt)
sentcorpus <- corpus_reshape(corp, to = "sentences")
# Number of sentences:  4,744,428
# text3(twitter): 2,583,841
# text2(news): 140,515
# text1(blogs): 2,020,072
rm(corp)




load(file = 'sentcorpus')
samp <- corpus_sample(sentcorpus, 1000000)
rm(sentcorpus)
save(samp, file = 'samp')


load(file = 'samp')
bi <- dfm(samp, tolower = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, ngrams = 2, concatenator = ' ', verbose = TRUE)
rm(samp)
freq <- textstat_frequency(bi)[, c("feature", "frequency")] %>%
        separate(feature, c("word1", "word2"), sep = " ") %>%
        filter(frequency > 1) %>%
        group_by(word1) %>%
        filter(frequency == max(frequency)) %>%
        select(-frequency)
rm(bi)
bigrams <- as.data.table(freq)
rm(freq)
bigrams[word1 == 'of1', word2]
bigrams[word1 == 'to', word2]
length(bigrams[word1 == 'of1', word2]) == 0 # if nothing is found
save(bigrams, file = 'bigrams')
rm(bigrams)

# Trigrams
load(file = 'samp')
tri <- dfm(samp, tolower = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, ngrams = 3, concatenator = ' ', verbose = TRUE)
rm(samp)
freq <- textstat_frequency(tri)[, c("feature", "frequency")] %>%
  separate(feature, c("word1", "word2", "word3"), sep = " ") %>%
  filter(frequency > 1) %>%
  group_by(word1, word2) %>%
  filter(frequency == max(frequency)) %>%
  select(-frequency)
rm(tri)
trigrams <- as.data.table(freq)
rm(freq)
trigrams[word1 == 'quite' & word2 == 'some', word3]
trigrams[word1 == 'tow' & word2 == 'the', word3]
length(trigrams[word1 == 'quite' & word2 == 'some', word3]) == 0 # if nothing is found
save(trigrams, file = 'trigrams')
rm(trigrams)

# Fourgrams
load(file = 'samp')
ng <- dfm(samp, tolower = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, ngrams = 4, concatenator = ' ', verbose = TRUE)
rm(samp)
freq <- textstat_frequency(ng)[, c("feature", "frequency")] %>%
  separate(feature, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(frequency > 1) %>%
  group_by(word1, word2, word3) %>%
  filter(frequency == max(frequency)) %>%
  select(-frequency)
rm(ng)
fourgrams <- as.data.table(freq)
rm(freq)
fourgrams[word1 == 'quite' & word2 == 'some' & word3 == 'time', word4]
fourgrams[word1 == 'tow' & word2 == 'the' & word3 == 'line', word4]
length(fourgrams[word1 == 'quite' & word2 == 'some' & word3 == 'time', word4]) == 0 # if nothing is found
save(fourgrams, file = 'fourgrams')
rm(fourgrams)
