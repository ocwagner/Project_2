library(tidyverse)
library(Hmisc)
library(rmarkdown)

### read in file

raw_news_data<-select(read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\OnlineNewsPopularity\\OnlineNewsPopularity.csv"),-url)

### Create dataframe of parameters

day_of_week<-c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
spec_phrase<-c(paste0("weekday_is_",day_of_week))
day_param=lapply(day_of_week,FUN=function(x){list(day_of_week=x)})
spec_phrase_param=lapply(spec_phrase,FUN=function(x){list(spec_phrase=x)})

day_reports<-tibble(day_of_week,day_param,spec_phrase,spec_phrase_param)%>%
  mutate(output_file=paste0(day_of_week,".html"))

###

apply(day_reports, MARGIN = 1,
      FUN=function(x){
        render(input="C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\Project_2_Markdown.Rmd",
               output_file = x[[5]], params=x[[4]])})



### a few stats

cor_mat<-rcorr(as.matrix(raw_news_data))
data.frame(cor_mat$P[,"shares"])
data.frame(cor_mat$r[,"shares"])%>%
  mutate("ranked_dep"=rank(cor_mat$r[,"shares"]))

View(data.frame(cor_mat$P[,"shares"]))

data.frame(cor_mat$r[,"shares"])%>%
  rowid_to_column(var="variable")



