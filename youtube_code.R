
######## ANALYZING YOUTUBE DATA ########

# Workshop 'Mining and Analyzing Social Media'
# Prepared by Diliara Valeeva, University of Amsterdam
# 15 April, 2020
# Slides at https://slides.com/diliaravaleeva/masm2020


#### STRUCTURE ####

# In this tutorial, we will learn how to:
# 1. Analyze videos on Youtube channels
# 2. Analyze the text of Youtube video comments


#### ABOUT YOUTUBE DATA TOOLS ####

# To get Youtube data, we will use Youtube Data Tools: https://tools.digitalmethods.net/netvizz/youtube/index.php
# This is a collection of simple tools for extracting data from the YouTube platform via the YouTube API v3.

# Youtube Data Tools consists of 5 main modules:
# Module 1 – Channel Info: Here you can get data about the channel
# Module 2 – Channel Network: Here you can get data on networks of relations of a channel with other channels
# Module 3 – Video list: Here you can explore lists of videos in the channel
# Module 4 – Video network: Here you can get data on networks of relations between videos
# Module 5 – Video info: Here you can get information about the video and its comments

# If you need more information about Youtube Data Tools, you can refer to these materials:
# The blogpost http://thepoliticsofsystems.net/2015/05/exploring-youtube/
# FAQ https://tools.digitalmethods.net/netvizz/youtube/faq.php
# Introductory video: https://www.youtube.com/watch?v=sbErTW2MzCY


#### 0. PREPARE THE WORKSPACE ####

## In this tutorial, we will use a few packages for text analysis and visualization
# Install these packages if you do not have them installed yet
install.packages('tm')
install.packages('wordcloud')
install.packages('ggplot2')

## Open these packages
library(tm)
library(wordcloud)
library(ggplot2)

## Set the working directory
# The working directory is a folder where we will store data files and will send outputs
# To know what is your current directory, run
getwd()
# R will show where is the current directory

# If you want to change this directory, you can set the new one by indicating the path. For example,
setwd("/home/diliara/Dropbox/MASM2020")

# Another way of setting the current directory is on the top menu. Go to Session => Set Working Directory => Choose Directory
# And choose the folder


#### 1. ANALYZING VIDEOS IN A CHANNEL ####


## 1.1. DOWNLOAD THE DATA ##

# In Youtube Data Tools, we will use the module 'Video List' to explore videos on the Youtube channel of Donald Trump.
# The address of Trumps' channel is https://www.youtube.com/channel/UCAql2DyGU2un1Ei2nMYsqOA
# Each channel has its own ID, which is the last series of numbers in the Youtube URL
# The ID of Trump's channel is 'UCAql2DyGU2un1Ei2nMYsqOA'.

# Go to 'Video List' module
# This module has a few fields to fill in:
# In the 'Channel ID', enter: UCAql2DyGU2un1Ei2nMYsqOA 
# Iterations: 1
# Rank by: choose 'by relevance'
# Press the box that you are not the robot
# Press 'Submit'

# The page will load for some time and at the bottom of the page you will see the section 'Results'
# It will give you the .tab file at the very bottom of the page
# For example, my file is named 'videolist_channel394_2020_04_13-13_16_16.tab'
# Download this file to your working directory and go back to R Studio


## 1.2. EXPLORE THE DATASET WITH VIDEOS ## 

## Read this dataset with the list of videos
videolist <- read.csv('videolist_channel394_2020_04_13-13_16_16.tab', header = TRUE, sep = '\t')

# Show the first few columns of this dataset
head(videolist)

# Show variable names
colnames(videolist)

# Number of rows in the dataset (=videos)
nrow(videolist)

# As we can see, there are currently 392 videos on Trump's channel
# Youtube Data Tools gives us various information about these videos: titles, number of likes and dislikes, number of comments and views, dates of publishing etc)


## 1.3. EXPLORE THE STATISTICS OF VIDEOS ##

# Show the distribution of Trump's video counts
summary(videolist$viewCount)
# The least popular video was only 648 views

# Show the top 5 most popular videos, based on view counts
videolist[order(videolist$viewCount, decreasing=TRUE)[1:5],,drop=FALSE]
# The column 'videoTitle' shows the titles of the videos
# "Text BORDER to 88022" is the most popular video at Trump's channel
# It was published at 2019-01-05 (see the variable 'publishedAt')
# It was accepted negatively by the audience. The number of likes is 38,700 ('likeCount') and the number of dislikes is 139,170 ('dislikeCount')


# Show the top 3 least popular videos, based on view count
videolist[order(videolist$viewCount, decreasing=FALSE)[1:3],,drop=FALSE]
# Real News Updates are usually among the least popular videos

# Show the top 3 most likable videos
videolist[order(videolist$likeCount, decreasing=TRUE)[1:3],,drop=FALSE]


## 1.4. RELATIONSHIP BETWEEN LIKES AND DISLIKES ## 

# Drop 2 outliers with a large number of likes/dislikes because they skew the plot
videolist_subset <- videolist[which(videolist$dislikeCount < 20000), ]

# Setting the size of the plot
options(repr.plot.width=5, repr.plot.height=5)

# Plotting relationship between likes and dislikes
plot(videolist_subset$likeCount, videolist_subset$dislikeCount, xlab = 'Number of dislikes', ylab = 'Number of likes')



#### 2. ANALYZING COMMENTS OF VIDEOS ####


## 2.1. DOWNLOAD THE DATA ##

# Video "HOPE": https://www.youtube.com/watch?v=BSSrimkmxzI
# Video ID is 'BSSrimkmxzI' (last numbers of the URL).

# Go to tab 'Video Info'
# Video ID: Insert 'BSSrimkmxzI'
# Click "HTML Output' and be sure that you are not a robot
# After some processing, it will return 4 files
# We are interested in the second one, it will contain '_comments' text part in the name
# Example of the file name: 'videoinfo_BSSrimkmxzI_2020_04_13-13_51_38_comments.tab'
# Download the file to your working directory and go back to R Studio


## 2.2. PREPARE TEXT DATA ## 

# Read the dataset
comments <- read.csv('videoinfo_BSSrimkmxzI_2020_04_13-13_51_38_comments.tab', header = TRUE, sep = '\t')

# Explore the first few rows
head(comments)
# # Our main variable of interest here is in the 'text' column
# These are the comments and you will see something like:
# "We love them, we&#39;re with them, and we won&#39;t let them down? The lie detector just went off the hook."
# "Fuck Donald Trump:)"
# "We love you too Mr. President! Haters gonna hate, that means your doing something right!"

# Before the text analysis, we need to do some cleaning
comments$text <- as.character(comments$text) # to make sure that this column is the text
comments$text <- tolower(comments$text) # transform letters to lower case
comments$text <- removeNumbers(comments$text) # remove numbers
comments$text <- removePunctuation(comments$text) # remove punctuation
comments$text <- stripWhitespace(comments$text) # strip extra white spaces
comments$text <- removeWords(comments$text, stopwords("english")) # remove stop words: a, and, also, the, etc.


## 2.3. EXPLORE THE WORDS ##

# Out of the comments of this video, create one text corpus
corpus <- Corpus(VectorSource(comments$text)) 

# From the corpus, create the Document Text Matrix
# The Document Term Matrix is the matrix that lists all occurrences of words in the corpus
dtm <- DocumentTermMatrix(corpus)

# Show the frequency of the first 20 words in all the set of the comments
freq <- colSums(as.matrix(dtm))
head(freq, n=20)
# For example, the word 'love' is used 114 times while the word 'fuck' is used 11 times

# Sort words and frequency values
ord <- order(freq,decreasing=TRUE)
# Inspect the most often used words
freq[head(ord)]
# The most frequenlty used words in the comments of this 'HOPE' video are: trump, president, god, bless, thank, will


## 2.4. CREATE WORD PLOTS ##

# Plotting the frequency of words
wf = data.frame(term=names(freq),occurrences=freq) # create a dataframe from the 'freq' object

p <- ggplot(subset(wf, freq>50), aes(term, occurrences)) # plot words that occur more than 50 times
p <- p + geom_bar(stat='identity')
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# Plot a wordcloud of the most frequent terms
set.seed(8) # set a seed number to make a plot reproducable
wordcloud(names(freq),freq, min.freq=10) # Plot words that are used more than 10 times


## 2.5. EXPLORE WORD RELATIONSHIPS

# Find words that often appear together
findAssocs(dtm, c("god"), corlimit=0.7) # 'corlimit' is specifying a correlation limit
# If words always appear together, then correlation=1.0
# 'god' and 'bless' usually occur together

findAssocs(dtm, c("china"), corlimit=0.3) # 
# china co-occurs in comments together with: 'whole', 'saudi', 'several', 'accountable'. etc

findAssocs(dtm, c("pandemic"), corlimit=0.3)
# 'pandemic' co-occurs often with: 'coronavirus', 'aaron', 'according', 'america’s', etc 


#### 3. MORE #### 

# If you are interested in other modules, you can explore them further
# Note that Youtube Data Tools sometimes gives files in .gdf format.
# For example, the 'Video Info' module returns a comment network as a .gdf file.
#.gdf files are network data formats
# R can't open these files easily but you can use Gephi https://gephi.org/
# Gephi is a free software for visualization and analysis of networks


## If you want to do more exercises, here are some ideas:
# 1) Take one of the videos from Bernie Sanders' Youtube channel, compare the text of comments with a video from Trump's channel.
# 2) Explore the most popular words used in video descriptions. Look at their relationship with video characteristics.
# 3) Explore the relationship between the duration of videos (column 'durationSec') and other characteristics of videos.
