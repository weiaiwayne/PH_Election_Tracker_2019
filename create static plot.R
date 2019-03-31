library(data.table) 
library(dplyr)
library(RSQLite) 
library(quanteda)
library(ggplot2) 
library(igraph) 
library(stopwords)
library(lubridate)
library(scales)
library(ggthemes)
library(stringr)
library(wordcloud)
library(reshape2)
library(rCharts)
library(tidyverse)
library(tibbletime)
library(zoo)
library(textclean)
library(tm)
library(textnets)
library(tidytext)

db <- src_sqlite("phelectiontracker_accounts.sqlite", create = FALSE) #check if the sqlite database is up to date
df <- tbl(db, sql("SELECT * FROM tweets")) 
df <- collect(df) 
df <- df[!duplicated(df$content),] 

#filter users
df <- df[df$from_user_screen_name !="ThinkingPinoy",]
df <- df[df$from_user_screen_name !="PinoyAkoBlogger",]
df <- df[df$from_user_screen_name !="PinoyAkoBlogger",]
df <- df[df$from_user_screen_name !="SassSassot",]
df <- df[df$from_user_screen_name !="MochaUson",]
df <- df[df$from_user_screen_name !="Senyora",]
df <- df[df$from_user_screen_name !="AngTanongKoSayo",]
df <- df[df$from_user_screen_name !="MiriamLines",]
df <- df[df$from_user_screen_name !="BadtripKaDre",]
df <- df[df$from_user_screen_name !="IamNancyBinay",]

#convert strings
df$from_user_screen_name <- tolower(df$from_user_screen_name)
df$entities_mentions <- tolower(df$entities_mentions)
df$entities_hashtags <- tolower(df$entities_hashtags)

#tidy up timestamps
df$created_at <- ymd_hms(df$created_at) 
df$created_at <- with_tz(df$created_at,tz = "GMT") #,"Asia/Manila"
df$created_date <- as.Date(df$created_at)
df$created_date_label <- as.factor(df$created_date)
df$Month_Yr <- format(as.Date(df$created_date), "%Y-%m")
df$Month_Yr <- ymd(paste(df$Month_Yr, "15",sep="-"))
df$Month_Yr_label <- as.factor(df$Month_Yr)

#create a variable for time difference between posting and data collection
df$inserted_date <- ymd_hms(df$inserted_date)
df$inserted_date <- with_tz(df$inserted_date, tz = "GMT") #,"Asia/Manila"
df$inserted_date <- as.Date(df$inserted_date)
df$date_differ <- df$inserted_date - df$created_date
df$date_differ <- as.numeric(df$date_differ)
df$date_differ <- rescale(df$date_differ, to = c(1, 10)) #time difference is recaled to 1-10 pt scale

df$retweet_count_time <- df$retweet_count/df$date_differ
df$fav_count_time <- df$favorite_count/df$date_differ

#tidy up screen names
df <- df[!is.na(df$from_user_screen_name),]
df$from_user_screen_name <- as.factor(df$from_user_screen_name)

#convert some character columns to integers
df$entities_hashtags_count <- as.integer(df$entities_hashtags_count)
df$entities_media_count <- as.integer(df$entities_media_count)
df[is.na(df$entities_media_count),]$entities_media_count <- 0

df$entities_mentions_count <- as.integer(df$entities_mentions_count)


#get tweets since 10-01-2018
df <- df[df$created_date>="2018-10-01",]
df$rt_basecount <- NA
df[df$retweeted_status =="THIS IS A RETWEET",]$rt_basecount <- 1
df[df$retweeted_status !="THIS IS A RETWEET",]$rt_basecount <- 0
df$rt_percandidate <-ave(df$rt_basecount,df$from_user_screen_name,FUN=sum)

df<- df[df$retweeted_status !="THIS IS A RETWEET",] #focus on only RTs


#summarize data
bycandidate <- df %>% 
  group_by(from_user_screen_name) %>% 
  summarise(avg_rt = round(mean(retweet_count),digits=2),
            avg_fav = round(mean(favorite_count),digits=2),
            followercount = mean(from_user_followers_count),
            friendcount = mean(from_user_friends_count),
            totaltweetcount = mean(from_user_statuses_count),
            multimediacount = sum(entities_media_count),
            hashtagcount = sum(entities_hashtags_count),
            mentioncount = sum(entities_mentions_count),
            retweets_tcount = mean(rt_percandidate),
            tweet_count = length(unique(content))) 

#bycandidate$from_user_screen_name <- paste('@', bycandidate$from_user_screen_name, sep='')
#bycandidate$pc_multimedia <- bycandidate$multimediacount/bycandidate$tweet_count
#bycandidate$pc_hashtag <- bycandidate$hashtagcount/bycandidate$tweet_count
#bycandidate$pc_mention <- bycandidate$mentioncount/bycandidate$tweet_count
bycandidate$pc_rt <- bycandidate$retweets_tcount/(bycandidate$tweet_count+bycandidate$retweets_tcount)
bycandidate$pc_original <- 1-bycandidate$pc_rt
bycandidate$totaltweetscount_sinceoct2018 <- bycandidate$tweet_count+bycandidate$retweets_tcount

bycandidate$from_user_screen_name <- gsub("bamaquino", "Bam Aquino (LP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("sonnyangara", "Sonny Angara",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("senatorbinay", "Nancy Binay (UNA)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("piacayetano", "Pia Cayetano (NP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("officialgenbato", "Bato dela Rosa (PDPLBN)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("cheldiokno", "Chel Diokno (LP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("jvejercito", "JV Ejercito (NPC)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("sjpenrile", "Juan Ponce Enrile (PMP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("jinggoyestrada1", "Jinggoy Estrada (PMP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("sap_bong_go", "Bong Go (PDPLBN)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("samiragutoc", "Samira Gutoc (LP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("fthilbay", "Pilo Hilbay (AKSYON)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("jiggymanicad", "Jiggy Manicad (IND)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("manangimee", "Imee Marcos (NP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("sergeosmena", "Sergio Osmeña (IND)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("senkokopimentel", "Koko Pimentel(PDPLBN)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("sengracepoe", "Grace Poe (IND)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("bongrevillajrph", "Ramon Bong Revilla Jr. (LAKAS)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("maroxas", "Mar Roxas (LP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("erintanada", "Lorenzo Reyes Tañada (LP)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("tolentino2019", "Francis Tolentino (PDPLBN)",bycandidate$from_user_screen_name)
bycandidate$from_user_screen_name <- gsub("cynthia_villar", "Cynthia Villar (NP)",bycandidate$from_user_screen_name)

bycandidate_melt <- bycandidate %>% melt
bycandidate_pc <- bycandidate_melt[bycandidate_melt$variable=="tweet_count"|bycandidate_melt$variable=="totaltweetscount_sinceoct2018",]

save(bycandidate,bycandidate_pc, file = "/Users/wayne/Dropbox/Acer Laptop Sync/Data Science/PH_Election_Tracker/PH_tracker/plot.rda")


p1 <- ggplot(data = bycandidate_pc, aes(x = reorder(from_user_screen_name, value), y = value)) +
  geom_bar(aes(fill = variable), stat = "identity") + 
  #geom_text(aes(label=round(avg_rt, digits = 2)))+
  xlab("Candidates") + ylab("Tweet volume") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_fill_discrete(name = "Tweet type", labels=c("tweet_count"="original tweets", "totaltweetscount_sinceoct2018"="retweets of other users"))+
  #scale_color_fivethirtyeight("cyl") +
  theme_fivethirtyeight()+theme(legend.position = "bottom")+ggtitle("Tweet Volume")

p1

bycandidate_avgrt <- bycandidate_melt[bycandidate_melt$variable=="avg_rt",]

p2 <- ggplot(data = bycandidate, aes(x = reorder(from_user_screen_name, avg_rt), y = avg_rt)) +
  geom_bar(aes(fill = from_user_screen_name), stat = "identity") + 
  #geom_text(aes(label=round(avg_rt, digits = 2)))+
  xlab("Candidates") + ylab("Average Retweet Count") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight()+theme(legend.position = "none")+ggtitle("The Average Number of Times Candidates' Original Tweets Are Retweeted")

p2

p3 <- ggplot(data = bycandidate, aes(x = reorder(from_user_screen_name, avg_fav), y = avg_fav)) +
  geom_bar(aes(fill = from_user_screen_name), stat = "identity") + 
  #geom_text(aes(label=round(avg_rt, digits = 2)))+
  xlab("Candidates") + ylab("Average Favorite Count") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight()+theme(legend.position = "none")+ggtitle("The Average Number of Times Candidates' Original Tweets Are Favorited")

p3

Sys.Date()

p4 <- ggplot(data = bycandidate, aes(x = reorder(from_user_screen_name, followercount), y = followercount)) +
  geom_bar(aes(fill = from_user_screen_name), stat = "identity") + 
  #geom_text(aes(label=round(avg_rt, digits = 2)))+
  xlab("Candidates") + ylab("Average Follower Count") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight()+theme(legend.position = "none")+ggtitle("Follower Count")

p4

p5 <- ggplot(data = bycandidate, aes(x = reorder(from_user_screen_name, friendcount), y = friendcount)) +
  geom_bar(aes(fill = from_user_screen_name), stat = "identity") + 
  #geom_text(aes(label=round(avg_rt, digits = 2)))+
  xlab("Candidates") + ylab("Average Friend Count") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_fivethirtyeight()+theme(legend.position = "none")+ggtitle("Friend Count")

p5


