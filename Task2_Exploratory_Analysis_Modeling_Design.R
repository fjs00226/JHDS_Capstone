path<-"C:/Postdoc/Coursera/DScourse/Coursera-SwiftKey/final/en_US"
twitter<-readLines(paste(path,"/en_US.twitter.txt",sep=""),encoding = "UTF-8",skipNul = TRUE)
blog<-readLines(paste(path,"/en_US.blogs.txt",sep=""),encoding = "UTF-8",skipNul = TRUE)
news<-readLines(paste(path,"/en_US.news.txt",sep=""),encoding = "UTF-8",skipNul = TRUE)

library(plyr)
library(stringr) # str_to_lower
library(ggplot2)
library(data.table) # shift
library(tidyr)
library(dplyr)  # arrange
install.packages("textclean")
library(textclean) # replace_contraction

# functions that will be used for data analysis
# function 1: clean data
# input is a long character, output is a long character
clean_data<-function(input){
  #paragraph<-paste(input,collapse="") # paste lines
  para_v<-gsub("[0-9]","",input) # remove numbers
  # replace contraction (in textclean package, seems slow)
  para_v<-gsub("’","'",para_v) # remove non-english style "’"
  para_v<-replace_contraction(para_v, contraction.key = lexicon::key_contractions)
  para_v<-str_to_lower(para_v) # all lower case
  #para_v<- gsub('([[:alpha:]])\\1+', '\\1', para_v)# remove repeating letters
  # add a space at the beginning if there is none
  para_v<-gsub(pattern = "([a-z][[:punct:]]+)([a-z])", replacement = "\\1 \\2", para_v)
  # remove punctuation
  para_v<-gsub("[[:punct:]]+","",para_v)
  # replace one or more white space with one white space
  para_v<-gsub("  |   "," ",para_v)
  para_v<-gsub(" [b-hj-z] "," ",para_v) # remove single letter except a and i
  return(para_v)  
}

# function 2: n-gram tokenization, save result in a data frame
# ngram_df<-function(input, n){
#  df<-data.frame(matrix(ncol = 1, nrow = 0)) # build an empty df
#  length_input<-length(str_split(input," ")[[1]])
#  for (i in 1:length_input){
#    df<-rbind(df,word(sample_clean, i,i+n-1, sep=" ")) 
# word function is in the stringr package
#  }
#  colnames(df)<-"words"
#  return(df)
#} # DO NOT loop, too slow

# input is a long character after cleaning, n is n_gram 
# output is a dataframe with all possible phrases
# This is fast, but it does NOT work for n = 1
ngram_df2<-function(input, n){
  n <- n-1
  word_list <- lapply(shift(str_split(input," "), 0:n, type = 'lead'), na.omit)
  # shift() is in data.table package
  mn <- min(lengths(word_list))
  grams <- do.call(paste, lapply(word_list, head, mn))
  grams<-as.data.frame(grams)
  colnames(grams)<-"words"
  return(grams)
}

# function 3: get data frame in descending order of word frequency
# input is a data frame with all possible phrases
# output is a data frame of words and counts
wfreq_df<-function(input){
  wfreq_df<-input %>%
    count(words) %>%
    arrange(desc(n))
  colnames(wfreq_df)<-c("words","count")
  return(wfreq_df)
}

# function 4: plots words distribution for n_gram tokenization, n is the top n words/phrases
# input is a data frame of all words and counts, num is the number of phrases the plot shows
# output is a plot of top num of phrases and counts
plot_ngram<-function(input, num, ngram){
  wfreq_ngram_n<-input[1:num,]
  wfreq_ngram_n<-wfreq_ngram_n%>%
    arrange(wfreq_ngram_n$count)  # in dplyr package
  wfreq_ngram_n$words=factor(wfreq_ngram_n$words,levels=wfreq_ngram_n$words) 
  # plot distribution of top 10 words frequencies?
  g=ggplot(data=wfreq_ngram_n, mapping=aes(x=words,y=count))+
    geom_col(fill="#f68060", alpha=.6, width=.6)+
    ggtitle(paste("words_count_",ngram,"gram",collapse = ""))+
    coord_flip()
  return (g)
}
###########
# 1. Basic summary of the data
# line count
lcount_n<-length(news)
lcount_t<-length(twitter)
lcount_b<-length(blog)
# character count
ccount_n<-sum(nchar(news))
ccount_t<-sum(nchar(news))
ccount_b<-sum(nchar(news))
# preliminary summary
summary_word_line<-data.frame("items"=c("news","twitter","blog"),
                              "lines"=c(lcount_n,lcount_t,lcount_b),
                              "characters"=c(ccount_n,ccount_t,ccount_b))
# 2. n_gram tokenization
# combine 10% of each file to make a sample
set.seed(2333)
sample_10<-paste(sapply(list(news, twitter,blog), function(x) {sample(x,length(x)*0.1)}),collapse = "")
sample_10_clean<-clean_data(sample_10)
# words count
wcount<-lengths(gregexpr("\\W+", sample_10_clean)) + 1 # space+1
print(paste("The sample has",wcount,"words."))

# 1_gram tokenization
sample_10_1gram<-as.data.frame(str_split(sample_10_clean," "))
colnames(sample_10_1gram)<-"words"
# 2,3,4_gram tokenization
sample_10_2gram<-ngram_df2(sample_10_clean,2)
sample_10_3gram<-ngram_df2(sample_10_clean,3)
sample_10_4gram<-ngram_df2(sample_10_clean,4)
# 3. words frequency
wfreq_1gram<-wfreq_df(sample_10_1gram)
wfreq_2gram<-wfreq_df(sample_10_2gram)
wfreq_3gram<-wfreq_df(sample_10_3gram)
wfreq_4gram<-wfreq_df(sample_10_4gram)
# top 15 frequent words
wfreq_1gram_15<-wfreq_1gram[1:15,]
wfreq_2gram_15<-wfreq_2gram[1:15,]
wfreq_3gram_15<-wfreq_3gram[1:15,]
wfreq_4gram_15<-wfreq_4gram[1:15,]
top10<-data.frame("1_gram"=wfreq_1gram_15$words,"percent_frequency"=wfreq_1gram_15$count*100/wcount,
                  "2_gram"=wfreq_2gram_15$words,"percent_frequency"=wfreq_2gram_15$count*100/wcount,
                  "3_gram"=wfreq_3gram_15$words,"percent_frequency"=wfreq_3gram_15$count*100/wcount,
                  "4_gram"=wfreq_4gram_15$words,"percent_frequency"=wfreq_4gram_15$count*100/wcount)

# plot distribution of top 15 words frequencies?
plot_ngram(wfreq_1gram,15,1)
plot_ngram(wfreq_2gram,15,2)
plot_ngram(wfreq_3gram,15,3)
plot_ngram(wfreq_4gram,15,4)

# 3. How many unique words do you need in a frequency sorted dictionary to cover
# 50% of all word instances in the language? 90%?
# make a plot to demonstrate, coverage_vs_filter (cf)
freq_max = wfreq_1gram$count[1] 
q<-seq(from = 0, to = freq_max/15, by = 25)
wcount_1gram<-nrow(wfreq_1gram) # distinct words count of 1 gram

coverage<-c()
for (i in 1:length(q)){
  d<-sum(wfreq_1gram[which(wfreq_1gram$count>q[i]),"count"])*100/wcount
  coverage<-append(coverage,d)
}
cf<-data.frame("word_frequency"=c(q),"percent_coverage"=c(coverage))
ggplot(data=cf, mapping=aes(x=word_frequency,y=percent_coverage))+geom_point()+ggtitle("coverage_vs_wfreq")

cf$percent_coverage<-round(cf$percent_coverage,digit=0)
wcount_1gram_50<-wcount_1gram-max(which(cf$percent_coverage==50))
wcount_1gram_90<-wcount_1gram-max(which(cf$percent_coverage==90))
print(paste(wcount_1gram_90, "words are needed to cover 90 % of the language.",
            wcount_1gram_50, "words are needed to cover 90 % of the language."))
      
# 4. How to evaluate words from foreign language
# remove anything with a non a-z 0-9 character in it.

# 5. How to increase the coverage of words with smaller number of words in the dictionary
# stemming, normalize words into the root form


