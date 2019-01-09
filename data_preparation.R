##### Packages##################################################################################

if (require(knitr)==FALSE) (install.packages('knitr'));library(knitr)
if (require(readtext)==FALSE) (install.packages('readtext'));library(readtext)
if (require(kableExtra)==FALSE) (install.packages('kableExtra'));library(kableExtra)
if (require(ngram)==FALSE) (install.packages('ngram'));library(ngram)
if (require(wordcloud)==FALSE) (install.packages('wordcloud'));library(wordcloud)
if (require(textstem)==FALSE) (install.packages('textstem'));library(textstem)
if (require(data.table)==FALSE) (install.packages('data.table'));library(data.table)
if (require(ggplot2)==FALSE) (install.packages('ggplot2'));library(ggplot2)
if (require(reshape2)==FALSE) (install.packages('reshape2'));library(reshape2)
if (require(tm)==FALSE) (install.packages('tm'));library(tm)
if (require(stringr)==FALSE) (install.packages('stringr'));library(stringr)
if (require(gridExtra)==FALSE) (install.packages('gridExtra'));library(gridExtra)
if (require(dplyr)==FALSE) (install.packages('dplyr'));library(dplyr)
if (require(xlsx)==FALSE) (install.packages('xlsx'));library(xlsx)




#### Main functions##########################################################################


uniqueCount <- function (text_content){
  
  return (length(unique(unlist(strsplit(text_content,split=" ")))))
  
}
#######################  Generate the statistics of the text files
getFilesStatistics<- function (path, news, blogs, tweets, verbose = F){
  
  conn <- file(paste(path, news, sep ='/'), "r") 
  dt_news <- readLines(conn)
  close(conn)
  
  conn <- file(paste(path, blogs, sep ='/'), "r") 
  dt_blogs <- readLines(conn)
  close(conn)
  
  conn <- file(paste(path, tweets, sep ='/'), "r") 
  dt_tweets <- readLines(conn) 
  close(conn)
  
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  header <- c("News", "Blogs", "Tweets") 
  if (verbose ==T) print("calculate number of lines...")
  nb_lines <- c(sum(table(dt_news)), sum(table(dt_blogs)), sum(table(dt_tweets)))
  if (verbose ==T) print("calculate length of longest line...")
  ln_lines <- c(max(nchar(dt_news)), max(nchar(dt_blogs)), max(nchar(dt_tweets)))
  if (verbose ==T) print("calculate length of shortest line...")
  shr_lines <- c(min(nchar(dt_news)), min(nchar(dt_blogs)), min(nchar(dt_tweets)))
  if (verbose ==T) print("calculate the number of words...")
  wrd_counts <- c(wordcount(dt_news), wordcount(dt_blogs), wordcount(dt_tweets))
  if (verbose ==T) print("calculate the number of unique words...")
  wrd_unique_counts <- c(uniqueCount(dt_news), uniqueCount(dt_blogs), uniqueCount(dt_tweets)) 
  
  corpus_stats <- data.frame(DATA_SRC=header, "Number.Lines"=nb_lines, "Length.Longest.Line"=ln_lines, "Length.Shortest.Line"=shr_lines, "Number.Words"=wrd_counts, "Number.Unique.Words" = wrd_unique_counts)
  corpus_stats$lexical.density  <- corpus_stats$Number.Unique.Words/corpus_stats$Number.Words
  names(corpus_stats)<-c("Source", "Number Lines", "Length Longest Line", "Length Shortest Line", "Number Words", "Number Unique Words","Lexical density")
  
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  
  if (verbose ==T) print(paste('operation time :', end_time - start_time))
  
  return (corpus_stats)
}

#######################  Sampling Data 
#For this exploratory data analysis specific need no specific error handling is developed
#Sample data out of a list of files with given ratios (files and ratios are 2 vctors of the same length)
sampleFile <- function(path, files, ratios){
  dt_sample <- list()
  # check the nullity of the vectors
  if (is.null(path)) return (-1)
  if (is.null(files)) return(-2)
  if (is.null(ratios)) ratios <- rep(0.1,length(files))
  for (i in 1:length(files) ){
    #print(paste("Sampling from file : ", file))
    conn <- file(paste(path,files[i], sep = "/"), "r") 
    dt_file <- readLines(conn)
    close(conn)
    
    nb_lines <- length(dt_file)
    
    sample_length <- round(ratios[i]*nb_lines )
    print(paste("Sample file ",files[i], " generated. Original file length : ", nb_lines," lines and sample length :", sample_length))
    smpl <-  sample(dt_file,size = sample_length ,replace = F)
    dt_sample[files[i]] <- paste(smpl, collapse="\n")
    
  }
  return(dt_sample)
}


# Write Sample to disk                                          
saveSample <- function (path, dt_sample, separate=T, file_name=NULL, ratio=1){
  if (is.null(path)) return (-1)
  dir.create(file.path(path, 'samples'), showWarnings = FALSE)
  wd <- getwd()
  setwd(paste(path,"samples", sep = "/"))
  if (!separate){
    file.create( file_name, showWarnings = FALSE)
    fileConn<-file(file_name)
    writeLines(dt_sample, fileConn)
    close(fileConn)
  }else{
    for(i in 1:length(dt_sample)){
      newfile = paste(gsub(pattern ="(\\.txt)",x=names(dt_sample)[i],replacement = "" ) , sprintf('%.2f',ratio) ,"smpl.txt", sep="_")
      file.create(newfile , showWarnings = FALSE)
      fileConn<-file(newfile)
      writeLines(dt_sample[[i]], fileConn)
      close(fileConn)
    }
    
  }
  
  setwd(wd)
  print('Sample saved successfully')
}

#generate statistics
sample_stats <- function (path, ratios, files, clean = F, profane_list){  
  wd <- getwd()
  setwd(paste(path,"samples", sep = "/"))
  print(getwd())
  wordstat_df <- data.frame(matrix(ncol=10, nrow = 0))
  
  for (i in 1:length(ratios)){
    
    ratio =sprintf('%.2f',ratios[i])
    news_smpl <- gsub("(\\.txt)", paste0("_",ratio,"_smpl.txt"), files[1])
    tweets_smpl <- gsub("(\\.txt)", paste0("_",ratio,"_smpl.txt"), files[2])
    blogs_smpl <- gsub("(\\.txt)", paste0("_",ratio,"_smpl.txt"), files[3])
    
    print(paste0("processing files" ,"_",ratio,"_smpl.txt"))
    
    conn <- file(news_smpl, "r") 
    dt_news_smpl <- readLines(conn)
    close(conn)
    
    conn <- file(blogs_smpl, "r") 
    dt_blogs_smpl <- readLines(conn)
    close(conn)
    
    conn <- file(tweets_smpl, "r") 
    dt_tweets_smpl <- readLines(conn) 
    close(conn)
    
    start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
    corpus <- VCorpus(VectorSource(concatenate(dt_news_smpl)))
    
    if (clean == T){
      corpus<- cleanDocument(corpus ,remove_sw = TRUE, profane_list = profane_list)
    }
    tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
    
    news_uwc <- dim(tdm)[1][1]
    news_wc <- sum(tdm)
    
    end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds    
    news_ts <- end_time - start_time
    
    start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
    corpus <- VCorpus(VectorSource(concatenate(dt_blogs_smpl)))
    if (clean == T){
      corpus<- cleanDocument(corpus,remove_sw = TRUE, profane_list = profane_list)
    }
    tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
    blogs_uwc <- dim(tdm)[1][1]
    blogs_wc <- sum(tdm)
    
    end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
    blogs_ts <- end_time - start_time
    
    start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
    corpus <- VCorpus(VectorSource(concatenate(dt_tweets_smpl)))
    if (clean == T){
      corpus<- cleanDocument(corpus,remove_sw = TRUE, profane_list = profane_list)
    }
    tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
    tweets_uwc <- dim(tdm)[1][1]
    tweets_wc <- sum(tdm)
    
    end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
    tweets_ts <- end_time - start_time
    
    wordstat_df  <-  rbind(wordstat_df, c(news_wc, news_uwc, news_ts, blogs_wc, blogs_uwc, blogs_ts, tweets_wc, tweets_uwc, tweets_ts, ratios[i] ))
    
  }
  names(wordstat_df ) <- c("News_wc","News_uwc","News_ts", "Blogs_wc", "Blogs_uwc", "Blogs_ts", "Tweets_wc", "Tweets_uwc", "Tweets_ts", "ratio") 
  wordstat_df['Total_wc']  <- apply(wordstat_df[,c("News_wc", "Blogs_wc", "Tweets_wc")], 1, sum)
  wordstat_df['Total_uwc']  <- apply(wordstat_df[,c("News_uwc", "Blogs_uwc", "Tweets_uwc")], 1, sum)
  wordstat_df['Total_ts']  <- apply(wordstat_df[,c("News_ts", "Blogs_ts", "Tweets_ts")], 1, sum)
  
  setwd(wd)
  print('operation successfully completed')
  return (wordstat_df)
}

# Files statistics
# Get the count of the words before and after cleaning  for the complete corpus
getFileStats <- function (clean = F, profane_list){
  locale <- "en_US"
  path <-paste("C:/Moh/00-DataScience/capstone/data", locale, sep = "/")
  news <- paste(locale, "news.txt", sep = ".")
  tweets <- paste(locale, "twitter.txt", sep = ".")
  blogs <- paste(locale, "blogs.txt", sep = ".")
  
  fileStats_df <- data.frame(matrix(ncol=10, nrow = 0))
  
  print ('Open files...')
  
  conn <- file(paste(path,news, sep = "/"), "r") 
  dt_news <- readLines(conn)
  close(conn)
  
  conn <- file(paste(path,blogs, sep = "/"), "r") 
  dt_blogs <- readLines(conn)
  close(conn)
  
  conn <- conn <- file(paste(path,tweets, sep = "/"), "r")  
  dt_tweets <- readLines(conn) 
  close(conn)
  
  print ('Processing News ...')
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  corpus <- VCorpus(VectorSource(concatenate(dt_news)))
  
  if (clean == T){
    corpus<- cleanDocument(corpus, remove_sw = TRUE, profane_list = profane_list)
  }
  tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds    
  news_uwc <- dim(tdm)[1][1]
  news_wc <- sum(tdm)
  news_ts = end_time - start_time
  
  
  print ('Processing  blogs...')
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  corpus <- VCorpus(VectorSource(concatenate(dt_blogs)))
  
  if (clean == T){
    corpus<- cleanDocument(corpus,remove_sw = TRUE, profane_list = profane_list)
  }
  tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds    
  blogs_uwc <- dim(tdm)[1][1]
  blogs_wc <- sum(tdm)
  blogs_ts = end_time - start_time
  
  
  print ('Processing  tweets...')
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  corpus <- VCorpus(VectorSource(concatenate(dt_tweets)))
  
  if (clean == T){
    corpus<- cleanDocument(corpus,remove_sw = TRUE, profane_list = profane_list)
  }
  tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds    
  tweets_uwc <- dim(tdm)[1][1]
  tweets_wc <- sum(tdm)
  tweets_ts = end_time - start_time
  
  fileStats_df  <-  rbind(fileStats_df, c(news_wc, news_uwc, news_ts, blogs_wc, blogs_uwc, blogs_ts, tweets_wc, tweets_uwc, tweets_ts, 1 ))
  names(fileStats_df ) <- c("News_wc","News_uwc","News_ts", "Blogs_wc", "Blogs_uwc", "Blogs_ts", "Tweets_wc", "Tweets_uwc", "Tweets_ts", "ratio") 
  fileStats_df['Total_wc']  <- apply(fileStats_df[,c("News_wc", "Blogs_wc", "Tweets_wc")], 1, sum)
  fileStats_df['Total_uwc']  <- apply(fileStats_df[,c("News_uwc", "Blogs_uwc", "Tweets_uwc")], 1, sum)
  fileStats_df['Total_ts']  <- apply(fileStats_df[,c("News_ts", "Blogs_ts", "Tweets_ts")], 1, sum)
  
  return (fileStats_df)
}

#generate a frequency table 
#remove_sw : Remove stop words
#profane_list  list of the profane words to filter out
#verbose : verbose mode activated
getFreqTable <- function (path, verbose = F, prefix = NULL, profane_list =NULL, remove_sw =FALSE){
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  if (verbose == T){
    print('loading data...')
  }
  conn <- file(path, "r")
  data <- readLines(conn)
  close(conn)
  data <- concatenate(data)
  
  if (verbose == T){
    print('creating and cleaning corpus...')
  }
  
  # create a corpus 
  
  corpus <- VCorpus(VectorSource(data))
  data <-  NULL
  corpus<- cleanDocument(corpus, remove_sw = remove_sw, profane_list= profane_list)
  
  tdm <- as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
  tdm_df = as.data.frame(tdm)
  tdm_df$word <- rownames(tdm_df)
  rownames(tdm_df) <- NULL
  tdm_df <- tdm_df[, c(2,1)]
  names(tdm_df) <- c('word', paste0('occur',ifelse(is.null(prefix),'','_'),prefix))
  tdm_df[paste0('freq',ifelse(is.null(prefix),'','_'),prefix)] <- tdm_df[2]/sum(tdm_df[2])
  
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  if (verbose == T){
    print(paste("The document has been processed in ",end_time - start_time, "ms" ))
    print(paste("After cleansing the documents the obtained dictionary contains ",dim(tdm_df)[1][1], "words" ))
  }
  
  return (tdm_df)
}

#######################  Manage profane words
profaneWords <- function(file){
  profane_dict <- fread(file, header = F, fill=T)
  profane_words <- as.vector(profane_dict[['V1']])
  profane_list <- paste('\\b',profane_words, '\\b', sep='',collapse = "|")
  return(profane_list)
}

#######################  Clean Text
cleanText <- function(x, simulate=FALSE){
  replacement<-rep(" ",7)
  if (simulate == TRUE){
    replacement<-c("_EMAIL_", "_URL_", "_USERNAME_", "_PUNCT_", "_NUMB_", "_NONALPHA", " ")
  }
  # replace emails by spaces
  x<- gsub("[a-z.0-9._%.]{2,}@[a-z.0-9.]+\\.[a-z]{2,4}",replacement[1],x, ignore.case = TRUE)
  # Replace URLS by spaces
  x <-gsub("www.[a-z.0-9._%.]{2,}",replacement[2],x, ignore.case = TRUE)
  # Replace usernames by spaces
  x <- gsub("@[a-z.0-9._.]+", replacement[3], x, ignore.case = TRUE)
 
  # expand contracted words
  x <- expandContractions(x)
  #replace punctuations by spaces
  x <- gsub("[[:punct:] ]+", replacement[4], x, ignore.case = TRUE)
  
  #replace numbers by spaces
  x <- gsub("[0-9]+", replacement[5], x, ignore.case = TRUE)
  

  #replace non alpha by spaces
  x <- gsub("[^a-zA-Z0-9 ]+", replacement[6], x, ignore.case = TRUE)
  
  #clean up superfluous spaces
  x <- gsub("[ ]{2,}",replacement[7], x, ignore.case = TRUE)
 
  return(x)
}
########### add start of line and end of line to text
tagText <- function(x){

  x <- gsub(x, pattern ="(^)(.*)($)" , replacement ="\\1<SL>\\2<EL>\\3" )
  return (x)
}

removeNonAlpha <- function(x) gsub("[^a-zA-Z0-9 ]"," ",x)

cleanDocument <- function(docs, remove_sw = FLASE, profane_list=NULL ){
  
  docs <- tm_map(docs, content_transformer(cleanText))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, content_transformer(tagText))
  
  
  if(remove_sw == TRUE){
    docs <- tm_map(docs, removeWords, stopwords(kind = "en"))
  }
    
  if (!is.null(profane_list)){
    docs <- tm_map(docs, removeWords, profane_list)
  }

  return (docs)
}


# Expand all contractions from a sentence , can be improved
expandContractions <- function(sentence)
{
  # assumes that text already converted into lower case
  for(i in  1:dim(contraction_df)[1]){
    sentence <- gsub(x = sentence, pattern =as.character(contraction_df[i,]$CONTRACTION) , replacement = as.character(contraction_df[i,]$EXPANSION), ignore.case = TRUE)
  }
  
  return (sentence)
}
#generate n-grams
generateNgrams <- function(text_content,n=1 ,verbose = FALSE, nfirst=0){
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  if (verbose == TRUE) print('generating the ngrams...')
  ng <-ngram(str, n)
  if (verbose == TRUE) print('extracting words and frequencies')
  ng <-get.phrasetable(ng)
  if (verbose == TRUE) print('re-ording frequencies from highest to lowest')
  ng<- ng[order(ng$freq, decreasing = T),]
  if (nfirst>0) ng <- head(ng, nfirst)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15) # get the current time in milliseconds
  if (verbose == TRUE) print(paste('operation ended successfully within ', end_time - start_time))
  return (ng)
}

###Data initialization#####################################################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data_path <-"./data"
locale <- "en_US"
folder <-paste(data_path, locale, sep = "/")
file_profane <-paste0(folder,"/resources/profanity_list.txt")

news <- paste(locale, "news.txt", sep = ".")
tweets <- paste(locale, "twitter.txt", sep = ".")
blogs <- paste(locale, "blogs.txt", sep = ".")
files<- c(news, tweets, blogs)

# List of contractions and their corresponding expansion 
#Ref : https://gist.github.com/nealrs/96342d8231b75cf4bb82
contraction_df <- read.csv(file = paste0(folder,'/resources/contractions.csv'), header = T, sep = ';')

##Statistics preparation ###################################################################

# Generate the initial statistics for the files
file_stats<- getFilesStatistics(path = folder, news, blogs, tweets, verbose = T)
#write.xlsx(x = file_stats, paste0(stats_path, "/file_stats.xlsx"), sheetName = 'FileStats', row.names = FALSE)
write.csv(file_stats, paste0(folder, "/stats/file_stats.csv"), row.names = FALSE)


# Read Profane Words
profane_words <- as.vector(fread(file_profane, header = F, fill=T)[['V1']])

# Generate several files for the calculation of the word frequencies (raw text and clean text)
news_df <-getFreqTable(paste(folder, news, sep='/'), prefix = 'news', profane_list=profane_words, remove_sw = TRUE)
#head(news_df)
blogs_df<-getFreqTable(paste(folder, blogs, sep='/'), verbose = T, prefix = 'blogs', profane_list=profane_words, remove_sw = TRUE)
#head(blogs_df)
tweets_df<- getFreqTable(paste(folder, tweets, sep='/'), verbose = T, prefix = 'tweets', profane_list=profane_words, remove_sw = TRUE)
#head(tweets_df)

#joining the data in one table

dict_df <- news_df %>% full_join(blogs_df, by='word')%>% full_join(tweets_df, by='word')
dict_df[is.na(dict_df)] <- 0
head(dict_df)
dict_df$tot_occur <- dict_df$occur_news +dict_df$occur_blogs+dict_df$occur_tweets

dim(dict_df[dict_df$tot_occur>1,])
write.csv(dict_df, file = paste0(folder,"/dictionaries/en_US_clean_dict.csv"),row.names=FALSE)

# The same but this time including stop words

news_df <-getFreqTable( paste(folder, news, sep='/'), prefix = 'news', profane_list=profane_words, remove_sw = FALSE)
#head(news_df)

blogs_df<-getFreqTable(paste(folder, blogs, sep='/'), verbose = T, prefix = 'blogs', profane_list=profane_words, remove_sw = FALSE)
#head(blogs_df)

tweets_df<- getFreqTable(paste(folder, tweets, sep='/'), verbose = T, prefix = 'tweets', profane_list=profane_words, remove_sw = FALSE)
#head(tweets_df)

#joining the data in one table

dict_df <- news_df %>% full_join(blogs_df, by='word')%>% full_join(tweets_df, by='word')
dict_df[is.na(dict_df)] <- 0
head(dict_df)
dict_df$tot_occur <- dict_df$occur_news +dict_df$occur_blogs+dict_df$occur_tweets

dim(dict_df[dict_df$tot_occur>1,])
write.csv(dict_df, file = paste0(folder,"/dictionaries/en_US_raw_dict.csv"),row.names=FALSE)



###Sampling####################################################################################
ratios = seq(0,0.9, .05)
ratios[1] <- 0.01
for(i in 1:length(ratios)){
  file_ratios<- rep(ratios[i], length(files))
  dt_sample <- sampleFile(path = folder, files = files, ratios = file_ratios)
  saveSample(path = folder, dt_sample = dt_sample,separate = T, ratio =ratios[i])
}

dt_sample <- list()
# check the nullity of the vectors
# Prepare the statistics for the vocabulary coverage per each percentage of sampling
wordstat_r_df = sample_stats(path = folder, ratios=ratios , files = files, clean = F)
wordstat_c_df = sample_stats(path = folder, ratios=ratios , files = files, clean = T, profane_list = profane_words)
wordstat_r_df$category <-'Raw'
wordstat_c_df$category <-'Clean'
wd_stats_df<- rbind(wordstat_r_df,wordstat_c_df)

write.csv(wordstat_r_df, file = paste0(folder,"/stats/wordstat_r.csv"),row.names=FALSE)
write.csv(wordstat_c_df, file = paste0(folder,"/stats/wordstat_c.csv"),row.names=FALSE)


filestat_r_df <- getFileStats(F)
filestat_c_df <- getFileStats(T, profane_list = profane_words)
filestat_r_df$category <-'Raw'
filestat_c_df$category <-'Clean'
stats_df = rbind(wd_stats_df, filestat_r_df,filestat_c_df)

write.csv(stats_df, file = paste0(folder,"/stats/wordstat_all.csv"),row.names=FALSE)




############## Generate the ngrams from a sample of the data ##########

sample_10 <- paste0(folder, "/samples/sample_10/")
dir.create(sample_10, showWarnings = FALSE)
sampl_10_files <- list.files( paste0(folder, "/samples/"), pattern ="en_US[.][a-z]*_0[.]10_smpl[.]txt$")
file.copy(paste0(folder, "/samples/",sampl_10_files), sample_10, overwrite = T, copy.mode = TRUE, copy.date = FALSE)
docs <- Corpus(DirSource(sample_10))

start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)   
clean_doc<- cleanDocument(docs,  remove_sw = TRUE, profane_list =  profane_words)
end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
print(paste('Cleaning documents ', end_time - start_time))

class(clean_doc)

str<- concatenate(lapply(clean_doc,"[", 1))
head(str)

unigram_freq<- generateNgrams(text_content= str,n=1,verbose = TRUE, nfirst = 0)
write.csv(unigram_freq, file = paste0(sample_10,"/1_gram.csv"),row.names=FALSE)
bigram_freq<- generateNgrams(text_content= str,n=2,verbose = TRUE, nfirst = 0)
write.csv(bigram_freq, file = paste0(sample_10,"/2_gram.csv"),row.names=FALSE)
trigram_freq<- generateNgrams(text_content= str,n=3,verbose = TRUE, nfirst = 0)
write.csv(trigram_freq, file = paste0(sample_10,"/3_gram.csv"),row.names=FALSE)
fourgram_freq<- generateNgrams(text_content= str,n=4,verbose = TRUE, nfirst = 0)
write.csv(fourgram_freq, file = paste0(sample_10,"/4_gram.csv"),row.names=FALSE)


########## Test of sampling functionality ####
#samples_data <- sampleFile (paste(folder, "bckup", sep = "/"), c(news, blogs, tweets), ratios=c(0.01,0.01,0.01))
#saveSample (path=folder, samples_data, separate=T, ratio = 0.01)


