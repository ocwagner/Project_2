library(tidyverse)
library(Hmisc)

spec_day="monday"
spec_phrase=paste0("weekday_is_",spec_day)

### read in file

raw_news_data<-select(read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\OnlineNewsPopularity\\OnlineNewsPopularity.csv"),-url)

cor_mat<-rcorr(as.matrix(raw_news_data))
data.frame(cor_mat$P[,"shares"])
data.frame(cor_mat$r[,"shares"])%>%
  mutate("ranked_dep"=rank(cor_mat$r[,"shares"]))

View(data.frame(cor_mat$P[,"shares"]))

data.frame(cor_mat$r[,"shares"])%>%
  rowid_to_column(var="variable")


colnames(raw_news_data)

attributes(raw_news_data)

raw_news_data$kw_max_avg
raw_news_data$weekday_is_monday

View(raw_news_data%>%
  subset(raw_news_data$)==1)
           
days_data<-raw_news_data%>%
  filter(raw_news_data$weekday_is_monday==1)

### selecting variables of interest
  
days_data%>%
  select(n_tokens_content,num)

