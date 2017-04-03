

##R script to auto generate custom vocabulary list



#####################
#
#Load libraries
#
#####################
library(devtools)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(roxygen2)
#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#install_url(slam_url)
library(slam)
library(tm)
library(NLP)
library(openNLP)
library(tidytext)

#Tidy Text reference
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html
#helpful   browseVignettes(package = "tidytext") 


##################################
#
#INGEST DATA FROM YOUR SOURCE TEXT
#
##################################
fileName <- "theletter.txt"
conn <- file(fileName,open="r")
lines<-readLines(conn)
lines<-lines[which(lines!="")]
lines1<-as.data.frame(lines)



#################################
#
#Create Corpus and Clean Text.  
#
#################################
sourcetext <- tm::Corpus(VectorSource(lines))
rm(lines)

#check text
#sourcetext[[3]]$content

# transformations
removeSlash <- function(x) gsub("/", " ", x)
removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
sourcetext <- tm::tm_map(sourcetext, content_transformer(tolower))
sourcetext <- tm::tm_map(sourcetext, removeNumbers)
sourcetext <- tm::tm_map(sourcetext, removeWords, stopwords("english"))
sourcetext <- tm::tm_map(sourcetext, removePunctuation)
sourcetext <- tm::tm_map(sourcetext, content_transformer(removeURL))
sourcetext1<- tm::TermDocumentMatrix(sourcetext, control = list(wordLengths = c(3,Inf)))
#sourcetext1$id <- rownames(sourcetext1) 



#################
#
#Break into words
#
#################
sourcetext2<-tidy(sourcetext1)
sourcetext2$count<-as.integer(sourcetext2$count)
sourcetext3<-as.data.frame(sourcetext2)
sourcetext3<-arrange(sourcetext3,desc(count))

text_singlewords<-as.data.frame(unique(sourcetext3$term))
colnames(text_singlewords)<-c("singlewords")
tail(text_singlewords) 
dim(text_singlewords) 



####################################
#
#Break corpus into N-grams
#
####################################
#break here into all 2 n-grams in text
text_ngrams<-lines1 %>%unnest_tokens(ngram, lines, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE) %>%
  separate(ngram, c("word1", "word2"), sep = " ")
text_ngrams$ngram = paste(text_ngrams$word1, text_ngrams$word2, sep=" ")
myvars<-c("ngram")
text_ngrams<-text_ngrams[myvars]
text_ngrams$ngram<-tolower(as.character(text_ngrams$ngram))
head(text_ngrams)
dim(text_ngrams)



############################################################
#
#Map words  to frequency and generate custom vocabulary list
#
############################################################
#a nice word and ngram frequency reference
#http://norvig.com/mayzner.html

#Intake list of common vocabulary words
#in this case, we're leveraging 100k most common words from 
#http://norvig.com/ngrams/ as republished here https://github.com/first20hours/google-10000-english
fileName_words <- "20kwords.txt"
conn1 <- file(fileName_words,open="r")
lines<-readLines(conn1)
lines<-lines[which(lines!="")]
lines1<-as.data.frame(lines)

#Put both lists into lower case
text_singlewords$singlewords<-tolower(as.character(text_singlewords$singlewords))
lines1$lines<-tolower(as.character(lines1$lines))

#Compare our corpus to common list, filter to uncommon words
text_singlewords$Iscommon<-text_singlewords$singlewords %in% lines1$lines
head(text_singlewords)
customvocabwords<-filter(text_singlewords,Iscommon==FALSE)
customvocabwords$Iscommon<-NULL
head(customvocabwords)
dim(customvocabwords)



##################################################################
#
#Map grams to frequency, generate custom vocabulary ngram list
#
##################################################################
#Input common 2 ngrams list
#leveraging Norvig's list commong 2 ngrams here http://norvig.com/ngrams/
input <- read.table(file='count_2w.txt')
input$ngram = paste(input$V1, input$V2, sep=" ")
myvars<-c("ngram")
input<-input[myvars]
input$ngram<-tolower(as.character(input$ngram))
dim(input)

#Compare our corpus to common list, filter to uncommon words
text_ngrams$Iscommon<-text_ngrams$ngram %in% input$ngram
head(text_ngrams)
customvocab_ngrams<-filter(text_ngrams,Iscommon==FALSE)
myvars<-c("ngram")
customvocab_ngrams<-customvocab_ngrams[myvars]
#get rid of more common ngrams
customvocab_ngrams<-customvocab_ngrams[!grepl("a ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl("and ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl("it ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl("his ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl("her ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl("i ", customvocab_ngrams$ngram),] 
customvocab_ngrams<-customvocab_ngrams[!grepl(" i", customvocab_ngrams$ngram),] 

customvocab_ngrams<-arrange(customvocab_ngrams,ngram)
head(customvocab_ngrams,10)
dim(customvocab_ngrams)



#####################################################################
#
#Write out lists of custom words and ngrams for review and recording
#
####################################################################
head(customvocab_ngrams)
head(customvocabwords)

write.csv(file="customvocab_ngrams.csv", x=customvocab_ngrams)
write.csv(file="customvocabwords.csv", x=customvocabwords)

#These lists will include the potential custom vocabulary you want to potentially record to create your custom vocabulary model in the 
#Custom Language Service.
