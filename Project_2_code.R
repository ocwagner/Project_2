library(tidyverse)
library(Hmisc)
library(rmarkdown)
library(caret)
library(psych)
library(modelr)
library(knitr)


### read in file

raw_news_data<-select(read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\OnlineNewsPopularity\\OnlineNewsPopularity.csv"),-url,-timedelta)
View(raw_news_data)
attributes(raw_news_data)
summary(raw_news_data)
NROW(raw_news_data)

### factorize qualitative variables
raw_news_data$weekday_is_monday<-as.factor(raw_news_data$weekday_is_monday)
raw_news_data$weekday_is_tuesday<-as.factor(raw_news_data$weekday_is_tuesday)
raw_news_data$weekday_is_wednesday<-as.factor(raw_news_data$weekday_is_wednesday)
raw_news_data$weekday_is_thursday<-as.factor(raw_news_data$weekday_is_thursday)
raw_news_data$weekday_is_friday<-as.factor(raw_news_data$weekday_is_friday)
raw_news_data$weekday_is_saturday<-as.factor(raw_news_data$weekday_is_saturday)
raw_news_data$weekday_is_sunday<-as.factor(raw_news_data$weekday_is_sunday)
raw_news_data$is_weekend<-as.factor(raw_news_data$is_weekend)

raw_news_data$data_channel_is_lifestyle<-as.factor(raw_news_data$data_channel_is_lifestyle)
raw_news_data$data_channel_is_entertainment<-as.factor(raw_news_data$data_channel_is_entertainment)
raw_news_data$data_channel_is_bus<-as.factor(raw_news_data$data_channel_is_bus)
raw_news_data$data_channel_is_socmed<-as.factor(raw_news_data$data_channel_is_socmed)
raw_news_data$data_channel_is_tech<-as.factor(raw_news_data$data_channel_is_tech)
raw_news_data$data_channel_is_world<-as.factor(raw_news_data$data_channel_is_world)

### training and test sets
set.seed(1)
train <- sample(1:nrow(raw_news_data), size = nrow(raw_news_data)*0.8)
test <- dplyr::setdiff(1:nrow(raw_news_data), train)
raw_data_train <- raw_news_data[train, ]
raw_data_test <- raw_news_data[test, ]

### basic summary stats
data_for_summary<-raw_data_train%>%
  filter(raw_data_train$weekday_is_monday==1)
data.frame(summary(data_for_summary))
VARIABLE<-c(colnames(data_for_summary))
avg<-round(data.frame("AVG"=sapply(data_for_summary,mean,na.rm=FALSE)),2)
stan_dev<-data.frame("SD"=round(apply(data_for_summary,2,sd),2))
maximum<-data.frame("MAX"=round(as.numeric(apply(data_for_summary,2,max)),2))
minimum<-data.frame("MIN"=round(as.numeric(apply(data_for_summary,2,min)),2))
stat_data<-cbind(VARIABLES,avg,stan_dev,maximum,minimum)
rownames(stat_data)<-c()
stat_data

channel_dig<-data_for_summary%>%
  mutate("data_channel" = ifelse(data_channel_is_bus==1,"business",
                                 ifelse(data_channel_is_lifestyle==1,"lifestyle",
                                 ifelse(data_channel_is_entertainment==1,"entertainment",
                                 ifelse(data_channel_is_socmed==1,"social",
                                 ifelse(data_channel_is_tech==1,"tech","world"))))))


ggplot(channel_dig, aes(x=data_channel, y=shares, color=data_channel))+
  geom_boxplot()+
  geom_jitter(alpha=.15)+
  ylim(0,10000)+
  ylab("Shares")+
  xlab("Data Channel")+
  guides(color=guide_legend(title="Data Channel"))

channel_dig%>%
  group_by(data_channel)%>%
  dplyr::summarize(shares=median(shares))

cor_mat<-rcorr(as.matrix(raw_data_train))
data.frame(cor_mat$P[,"shares"])
cor_vals<-data.frame("correl"=cor_mat$r[,"shares"])%>%
  mutate("ranked_dep"=rank(cor_mat$r[,"shares"]))

cor_dig<-data.frame(VARIABLE,cor_vals)
cor_dig$VARIABLE<-factor(cor_dig$VARIABLE)

cor_dig%>%
  ggplot(aes(x=reorder(VARIABLE,correl),y=correl))+
  geom_col()+
  ylim(-0.15,0.15)+
  theme(axis.text.x = element_text(angle=90))+
  xlab("variable")+
  ylab("correlation with shares")


###

data.frame(apply(data_for_summary,2,min))
data.frame(apply(data_for_summary,2,max))
data.frame(apply(data_for_summary,2,mean,na.rm=TRUE))

###
help(scale)
scaled_train_data<-data.frame(scale(select(raw_data_train,-c(12:17,30:37)),center = TRUE, scale = TRUE))
scaled_train_data<-cbind(scaled_train_data,select(raw_data_train,c(12:17,30:37)))

scaled_test_data<-data.frame(scale(select(raw_data_test,-c(12:17,30:37)),center = TRUE, scale = TRUE))
scaled_test_data<-cbind(scaled_test_data,select(raw_data_test,c(12:17,30:37)))

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


### filter training data
day_data_train<-scaled_train_data%>%
  filter(scaled_train_data$weekday_is_monday==1)

### filter test data
day_data_test<-scaled_test_data%>%
  filter(scaled_test_data$weekday_is_monday==1)

simple<-raw_news_data%>%
lm(formula=shares~.)
summary(simple)

train(shares~., data=raw_news_data,method="lm",trControl=trainControl(method="repeatedcv"))

###
scaled_train_data<-data.frame(scale(select(raw_data_train,-"class"),center = TRUE, scale = TRUE))


### a few stats

cor_mat<-rcorr(as.matrix(raw_news_data))
data.frame(cor_mat$P[,"shares"])
cor_vals<-data.frame("correl"=cor_mat$r[,"shares"])%>%
  mutate("ranked_dep"=rank(cor_mat$r[,"shares"]))

cor_dig<-data.frame(VARIABLE,cor_vals)
cor_dig$VARIABLE<-factor(cor_dig$VARIABLE)

cor_dig%>%
  ggplot(aes(x=reorder(VARIABLE,correl),y=correl))+
  geom_col()+
  ylim(-0.15,0.15)+
  theme(axis.text.x = element_text(angle=90))+
  xlab("variable")+
  ylab("correlation with shares")

View(data.frame(cor_mat$P[,"shares"]))

data.frame(cor_mat$r[,"shares"])%>%
  rowid_to_column(var="variable")

### simple linear regression model

linear_all_model<-train(shares~., data=day_data_train,method="lm")

summary(linear_all_model)

linear_select_model<-train(shares~LDA_02+data_channel_is_world+avg_negative_polarity+
                           average_token_length+kw_avg_avg+
                           LDA_03+kw_max_avg+self_reference_avg_sharess+num_hrefs+num_imgs+
                           global_subjectivity, data=day_data_train,method="lm")

summary(linear_select_model)

### non linear models

bagged_tree<-train(shares~.,data=day_data_train,method="treebag")
summary(bagged_tree)

boosted_tree<-train(shares~.,data=day_data_train,method="gbm")

boosted_tree_select<-train(shares~LDA_02+data_channel_is_world+avg_negative_polarity+
                             average_token_length+kw_avg_avg+
                             LDA_03+kw_max_avg+self_reference_avg_sharess+num_hrefs+num_imgs+
                             global_subjectivity, data=day_data_train,method="gbm")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

bagged_tree_CV<-train(shares~.,data=day_data_train,method="treebag", trControl=fitControl)

### make predictions
lin_all_pred<-predict(linear_all_model,newdata=day_data_test)
lin_sel_pred<-predict(linear_select_model,newdata=day_data_test)
bag_tree_pred<-predict(bagged_tree,newdata=day_data_test)
bagged_tree_cv_pred<-predict(bagged_tree_CV,newdata=day_data_test)
boost_tree_pred<-predict(boosted_tree,newdata=day_data_test)
boost_tree_sel_pred<-predict(boosted_tree_select,newdata=day_data_test)


### compare predictiveness
summary(lin_all_pred)
RMSE(lin_sel_pred)
help(RMSE)
first<-RMSE(lin_all_pred,day_data_test$shares)
second<-RMSE(lin_sel_pred,day_data_test$shares)
third<-RMSE(bag_tree_pred,day_data_test$shares)
fourth<-RMSE(bag_tree_cv_pred,day_data_test$shares)
fifth<-RMSE(boost_tree_pred,day_data_test$shares)
sixth<-RMSE(boost_tree_sel_pred,day_data_test$shares)
`RMSE`<-c(first,second,third,fourth,fifth,sixth)
"model name"<-c("lin all","lin select","bag tree","bag tree cv","boost tree","boost tree select")
data.frame(`model name`,RMSE)
