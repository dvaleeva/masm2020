######## ANALYZING TWITTER DATA ########

# Workshop 'Mining and Analyzing Social Media'
# Prepared by Diliara Valeeva, University of Amsterdam
# 15 April, 2020
# Slides at https://slides.com/diliaravaleeva/masm2020


#### STRUCTURE ####

# In this tutorial, we will learn how to:
# 1. ANALYZE HASHTAGS AND SENTIMENTS OF TWEETS
# 2. EXPLORE TWITTER USERS AND FOLLOWERS

#### ABOUT TWITTER DEVELOPER ACCESS ####

# To scrape Twitter data, you need to create a developer account and an app.
# Follow the steps in slides: https://slides.com/diliaravaleeva/masm2020#/


#### 0. PREPARE THE WORKSPACE ####

## In this tutorial, we will use a few packages for text analysis, visualization, and Twitter scraping.
# Install these packages if you do not have them
install.packages('rtweet')
install.packages('ggplot2')
install.packages('syuzhet')

## Open these packages
library(rtweet)
library(ggplot2)
library(syuzhet)
# Also install 'httpuv' that would allow R to open your browser
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}


## Set the working directory
# The working directory is a folder where we will store data files and will send outputs
# To know what is your current directory, run
getwd()
# R will show where is the current directory

# If you want to change this directory, you can set the new one by indicating the path. For example,
setwd("/home/diliara/Dropbox/MASM2020")

# Another way of setting the current directory is on the top menu. Go to Session => Set Working Directory => Choose Directory
# And choose the folder


#### 1. CONNECTING TO YOUR TWITTER APP ####

# In your Twitter app dashboard, you will find the name of your application, API key, and API secret key
# You can find keys on the 'Keys and tokens' section
# Enter them below and remember to keep your API keys secret

# Enter the name of the app
appname <- "masm2020" # You might have entered a different name

## API key 
key <- "insert-here-api-key-numbers"

## API secret
secret <- "insert-here-api-secret-key-numbers"

## Connect to the app
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# Your browser will open a new page and will ask to authorize the app
# Press "Authorize app", it will say "Authentication complete. Please close this page and return to R."
# Go back to R Studio. Now you can scrape Twitter data using R!


#### 2. ANALYSIS OF HASHTAGS ####

## 2.1. EXPLORE TWEETS WITH HASHTAGS ##

## Search for recent 10,000 tweets using the #herdimmunity hashtag and do not include rewteets

herdimmunity <- search_tweets(q = "#herdimmunity", # containing this hashtag
                       n = 10000, # maximum number of tweets to return is 10,000
                       include_rts = FALSE, # do not include retweets
                       type = 'recent') # return recent tweets

# Twitter limit is 18,000 search results every 15 minutes, that's why we asked R to return no more than 10,000 tweets
# But if you want to get more data, set retryonratelimit = TRUE.
# For example:
# more_data <- search_tweets(q = "#herdimmunity", include_rts = FALSE, retryonratelimit = TRUE, type = 'recent').
# It will take more time to run and R will send calls to Twitter every 15 minutes. But you will get ALL the recent tweets with this hashtag

# Twitter returns data in a 'matrix' format
# It is easier to analyze data in a 'dataframe' format
# Turn matrix to R dataframe
herdimmunity <- as.data.frame(herdimmunity)

# View first three rows of our 'herdimmunity' dataset
head(herdimmunity, n=3)

# Names of columns
colnames(herdimmunity)
# As we can see, Twitter returns lots of different data about each tweet

# Who are the users who write about the herdimmunity: Show the list of unique usernames
unique(herdimmunity$screen_name)

# Show the list of 3 most retweeted tweets with this hashtag
herdimmunity[order(herdimmunity$retweet_count, decreasing=TRUE)[1:3],,drop=FALSE]

# If you want to save this dataset in your working directory:
write.csv(herdimmunity, "herdimmunity.csv")


## 2.2. SENTIMENT ANALYSIS OF HASHTAGS ##

# Sentiment analysis is a process of computationally categorizing opinions.
# Sentiments can be positive, negative, or neutral.
# We will look at tweets containing the #herdimmunity hashtag and explore their sentiments.

# We use our 'herdimmunity' dataset that contains tweets with the #herdimmunity hashtag

# The column 'text' of the 'herdimmunity' dataset contains the text of tweets
# Before the sentiment analysis, we need to clean the text of tweets a bit: remove URLs, hashtags, extra spaces, etc
herdimmunity_cleaned <- gsub("http.*","", herdimmunity$text)
herdimmunity_cleaned <- gsub("https.*","", herdimmunity_cleaned)
herdimmunity_cleaned <- gsub("#.*","", herdimmunity_cleaned) 
herdimmunity_cleaned <- gsub("@.*","", herdimmunity_cleaned)


# We first get the emotion score for each tweet.
# There are 10 emotions in ‘syuzhet’ package:
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive.

word.df <- as.vector(herdimmunity_cleaned) # Create a vector out of the herdimmunity tweets
emotion.df <- get_nrc_sentiment(word.df) # Assign emotions to each tweet 
emotion.df2 <- cbind(herdimmunity_cleaned, emotion.df) # Merge 'herdimmunity_cleaned' dataset with 'emotion.df' dataset
head(emotion.df2, n=20)

# Tweets were classified as belonging to the following emotions: anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive

# Getting the most positive sentiments
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive
# I have: "Ernesto's compassionate Sanctuary seems managed with utility (to produce the greater good), efficiency, fairness, liberty,
# WHO's principles for ethical preparation &amp; response to pandemic. 

# Getting the most negative sentiments
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative
# I have: "Might we venture that a la Kobe, the deeply sad circumstances of Boris’s current condition have made it “problematic”
# to discuss the flippancy of his government’s early response to the disease and the pain and suffering that caused?

## Split the dataset in positive, negative, and neutral tweets

# Subset of positive tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

# Subset of negative tweets tweets
negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

# Subset of neutral tweets tweets
neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

# Number of tweets in each category
length(positive.tweets)
length(negative.tweets)
length(neutral.tweets)



#### 3. ANALYSIS OF USERS ####

## 3.1. ANALYSIS OF FOLLOWERS ## 

## Get user IDs of accounts followed by Bernie Sanders
sanders_follows <- get_friends("BernieSanders")

## Who are these users that Sanders follows
sanders_follows_userinfo <- lookup_users(sanders_follows$user_id)

# What kins of information do we have about these users 
colnames(sanders_follows_userinfo)

# Where are they from?
table(sanders_follows_userinfo$country)


## Get user IDs of accounts following Bernie Sanders
sanders_followers <- get_followers("BernieSanders", n = 1000) # we put a limit of 1,000 users

## Who are these users
sanders_followers_userinfo <- lookup_users(sanders_followers$user_id)


## 3.2. ANALYSIS OF USER TIMELINE ACTIVITIES ## 

# Get timelines of B. Sanders and D. Trump
tmls <- get_timelines(c("BernieSanders", "realDonaldTrump"), n = 3000)

# Plot the frequency of their Twitter updates, starting from 1 Jan 2020
tmls %>%
  filter(created_at > "2020-01-01") %>%
  group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  geom_point() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses posted by Sanders and Trump")


#### MORE ####

# Read more about the options of the 'rtweet' package in the documentation: https://rtweet.info/

## If you want to do more exercises, here are some ideas:
# 1) Get the data about another related hashtag and compare its sentiments with the sentiments of #herdimmunity hashtag
# 2) Analyze the frequency of words and their relationships, as we did in the Youtube analysis
# 3) Analyze sentiments of video descriptions at Trump's Youtube channel. Also see the Youtube part of the workshop
