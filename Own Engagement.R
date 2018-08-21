#Loading Libraries
library(Rfacebook)
library(ggplot2)
library(scales)
library(dplyr)
library(magrittr)

#authentication
token = ""
man_united <- getPage(page='theweekendplan', n=100000, 
                      token=token,since='2014/01/01', 
                      until='2017/01/17')
# save data for later use
save(man_united, file='man_united.RData')
# load data for analysis
load('man_united.RData')
# combine data frames
colnames <- c('from_name', 'created_time', 'type', 
              'likes_count', 'comments_count', 'shares_count',
              'id', 'message', 'link')
page_data <- rbind(man_united[colnames])
names(page_data)[1] <- "Page"
# format post creation time
page_data$created_time <- as.POSIXct(page_data$created_time, 
                                     format = "%Y-%m-%dT%H:%M:%S+0000", 
                                     tz = "GMT")
# add new time based columns
page_data$month <- format(page_data$created_time, "%Y-%m")
page_data$year <- format(page_data$created_time, "%Y")
# total records
nrow(page_data)

# post counts per page
post_page_counts <- aggregate(page_data$Page, by=list(page_data$Page), length)
colnames(post_page_counts) <- c('Page', 'Count')
ggplot(post_page_counts, aes(x=Page, y=Count, fill=Page)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Count),  vjust=-0.3, position=position_dodge(.9), size=4) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()
#post counts by post type per page
post_type_counts <- aggregate(page_data$type, by=list(page_data$Page, page_data$type), length)
colnames(post_type_counts) <- c('Page', 'Type', 'Count')
ggplot(post_type_counts, aes(x=Page, y=Count, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()

# average likes per page by post type
likes_by_post_type <- aggregate(page_data$likes_count, 
                                by=list(page_data$Page, page_data$type), mean)
colnames(likes_by_post_type) <- c('Page', 'Type', 'AvgLikes')
ggplot(likes_by_post_type, aes(x=Page, y=AvgLikes, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()

# average shares per page by post type
shares_by_post_type <- aggregate(page_data$shares_count, 
                                 by=list(page_data$Page, page_data$type), mean)
colnames(shares_by_post_type) <- c('Page', 'Type', 'AvgShares')
ggplot(shares_by_post_type, aes(x=Page, y=AvgShares, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()


# page engagement over time
page_posts_df <- aggregate(page_data[['type']], by=list(page_data$month, page_data$Page), length)
colnames(page_posts_df) <- c('Month', 'Page', 'Count')
page_posts_df$Month <- as.Date(paste0(page_posts_df$Month, "-15"))
ggplot(page_posts_df, aes(x=Month, y=Count, group=Page)) + 
  geom_point(aes(shape=Page)) + 
  geom_line(aes(color=Page)) +
  theme_bw() + scale_x_date(date_breaks="3 month", date_labels='%m-%Y') +
  ggtitle("Page Engagement over time")


## user engagement with page over time
# create metric aggregation function
aggregate.metric <- function(metric, data) {
  m <- aggregate(data[[paste0(metric, "_count")]], list(month = data$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
mu_df <- subset(page_data, Page=="RainmakerEvents")
mu_stats_df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric, data=mu_df)
mu_stats_df <- do.call(rbind, mu_stats_df.list)
mu_stats_df <- mu_stats_df[order(mu_stats_df$month), ]

