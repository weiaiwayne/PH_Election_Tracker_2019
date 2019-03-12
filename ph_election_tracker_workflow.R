library(data.table) 
library(dplyr)
library(RSQLite) 
library(quanteda)
library(ggplot2) 
library(igraph) 
library(stopwords)
library(lubridate)
library(scales)
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
df<- df[df$retweeted_status !="THIS IS A RETWEET",] #focus on only RTs

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

# OPTIONAL: create a data frame for account-level metadata
#accounts <- df[!duplicated(df$from_user_screen_name),]
#accounts$group <- NA
#unique(df$from_user_screen_name)

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
df$entities_mentions_count <- as.integer(df$entities_mentions_count)

#get tweets since 10-01-2018
df <- df[df$created_date>="2018-10-01",]


#summarize data
bycandidate_date <- df %>% 
  group_by(created_date_label, from_user_screen_name) %>% 
  summarise(avg_rt = round(mean(retweet_count_time),digits=2),
            #avg_rt = mean(retweet_count),
            avg_fav = round(mean(fav_count_time),digits=2),
            followercount = mean(from_user_followers_count),
            friendcount = mean(from_user_friends_count),
            totaltweetcount = mean(from_user_statuses_count),
            multimediacount = sum(entities_media_count),
            hashtagcount = sum(entities_hashtags_count),
            mentioncount = sum(entities_mentions_count),
            tweet_count = length(unique(content))) %>% melt

bycandidate_date[is.na(bycandidate_date$value),]$value <- 0
bycandidate_date$day_show <- as.character(bycandidate_date$created_date_label)

#ggplot(data = bycandidate_date[bycandidate_date$variable=="avg_rt",], aes(x = created_date_label, y = value, group = from_user_screen_name)) +
#  geom_line(size = 0.9, alpha = 0.7, aes(color = from_user_screen_name)) +
#  geom_point(size = 0) +
#  ylim(0, NA) +
#  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
#  ylab("Average retweet count") + 
#  ggtitle("Twitter engagement")

#h1 <- hPlot(x = "day_show", y = "value", data = bycandidate_date[bycandidate_date$variable=="avg_rt",], type = "line", group = "from_user_screen_name")
#h1$print("chart5",include_assets = TRUE)
#h1

#df_vis will be the file in the cloud server
df_vis <- df[,c("content","tweet_id","created_date","from_user_screen_name"),]

#REMOVE UNWANTED STRINGS
df_vis$content <-replace_url(df_vis$content, remove = TRUE)
#df_vis$content <- replace_non_ascii(df_vis$content,remove = TRUE)
df_vis$content <- make_plural(df_vis$content)
#df_vis$content <- replace_contraction(df_vis$content)
df_vis$content <- replace_date(df_vis$content,remove = TRUE)
df_vis$content <- replace_email(df_vis$content,remove = TRUE)
#df_vis$content <- replace_emoticon(df_vis$content,remove = TRUE)
#df_vis$content <- replace_white(df_vis$content,remove = TRUE)
df_vis$content <- replace_money(df_vis$content,remove = TRUE)
df_vis$content <- replace_number(df_vis$content,remove = TRUE)
df_vis$content <- replace_ordinal(df_vis$content,remove = TRUE)
#df_vis$content <- replace_symbol(df_vis$content,remove = TRUE)
#df_vis$content <- replace_tag(df_vis$content,remove = TRUE)
#df_vis$content <- replace_time(df_vis$content,remove = TRUE)

tlsw <- stopwords::stopwords('tl',source = "stopwords-iso") #remove stop words in Tagalog
df_vis$content = removeWords(df_vis$content, tlsw)

## Create textnets
bycandidate <- df_vis %>%
  group_by(from_user_screen_name) %>%
  summarise(content=paste(content,collapse='. '))

t1 <- PrepText(bycandidate, groupvar = "from_user_screen_name", textvar = "content", node_type = "groups", tokenizer = "tweets", pos = "all", remove_stop_words = TRUE, remove_numbers=FALSE, compound_nouns = FALSE)

t1 <- t1[t1$lemma !=".",]
t1 <- t1[t1$lemma !=" ",]
t1 <- t1[t1$lemma !="u.",]
t1 <- t1[t1$lemma !="'s",]
t1 <- t1[t1$lemma !="|",]
t1 <- t1[t1$lemma !="/",]
t1 <- t1[t1$lemma !="@",]
t1 <- t1[t1$lemma !="#",]

t1$lemma <- gsub(".*@.*", NA, t1$lemma)
t1 <- t1[!is.na(t1$lemma),]
t2 <- t1[grepl("#", t1$lemma),]

#remove words shorter than 2 characters
t1 <- t1[nchar(t1$lemma)>2,]
t1$from_user_screen_name <- trimws(t1$from_user_screen_name)
t1$lemma <- trimws(t1$lemma)

#n1 <- CreateTextnet(t1)
#n2 <- CreateTextnet(t2) #for a cultural network based on a common use of hashtags

#VisTextNet(n2, label_degree_cut = 0, betweenness=TRUE)
top_all_words_modularity_classes <- InterpretText(n1, t1)
text_centrality <- TextCentrality(n1)


#VisTextNet(n1, label_degree_cut = 0, betweenness=TRUE)
#write.graph(n1,"ph_candidates_timeline.graphml",format="graphml")

#save files
write.csv(bycandidate,"bycandidate.csv")
write.csv(df,"df_allsinceoct2018.csv")
write.csv(df_vis,"df.csv")
write.csv(bycandidate_date,"bycandidate_date.csv")
write.csv(t1, "t1.csv")
write.csv(t2, "t2.csv")

#remotes::install_github("JohnCoene/typed")
#remotes::install_github("JohnCoene/baffle")
#library(typed)

#typed(c("This is typed.", "This is an htmlwidget."), loop = TRUE, typeSpeed = 100)

