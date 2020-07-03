library(tidyverse)
library(Hmisc)
library(rmarkdown)
library(caret)
library(psych)
library(modelr)
library(knitr)



### Create dataframe of parameters

day_of_week<-c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
spec_phrase<-c(paste0("weekday_is_",day_of_week))
day_param=lapply(day_of_week,FUN=function(x){list(day_of_week=x)})
spec_phrase_param=lapply(spec_phrase,FUN=function(x){list(spec_phrase=x)})

day_reports<-tibble(day_of_week,day_param,spec_phrase,spec_phrase_param)%>%
  mutate(output_file=paste0(day_of_week,".md"))

###

apply(day_reports, MARGIN = 1,
      FUN=function(x){
        render(input="C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\Project_2_Markdown.Rmd",
               output_file = x[[5]], params=x[[4]])})

