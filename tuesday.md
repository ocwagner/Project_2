Project\_2\_Markdown
================
Owen Wagner
6/28/2020

# Introduction

In this project we are working with an online news popularity [data
set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
compiled by Fernandes, Vinagre, Cortez and Sernadela.

The dataset includes a total of 61 attributes, 58 of which are
predictive, and 39,644 observations. A description of the attributes is
provided below.

``` 
 0. url:                           URL of the article
 1. timedelta:                     Days between the article publication and
                                   the dataset acquisition
 2. n_tokens_title:                Number of words in the title
 3. n_tokens_content:              Number of words in the content
 4. n_unique_tokens:               Rate of unique words in the content
 5. n_non_stop_words:              Rate of non-stop words in the content
 6. n_non_stop_unique_tokens:      Rate of unique non-stop words in the
                                   content
 7. num_hrefs:                     Number of links
 8. num_self_hrefs:                Number of links to other articles
                                   published by Mashable
 9. num_imgs:                      Number of images
10. num_videos:                    Number of videos
11. average_token_length:          Average length of the words in the
                                   content
12. num_keywords:                  Number of keywords in the metadata
13. data_channel_is_lifestyle:     Is data channel 'Lifestyle'?
14. data_channel_is_entertainment: Is data channel 'Entertainment'?
15. data_channel_is_bus:           Is data channel 'Business'?
16. data_channel_is_socmed:        Is data channel 'Social Media'?
17. data_channel_is_tech:          Is data channel 'Tech'?
18. data_channel_is_world:         Is data channel 'World'?
19. kw_min_min:                    Worst keyword (min. shares)
20. kw_max_min:                    Worst keyword (max. shares)
21. kw_avg_min:                    Worst keyword (avg. shares)
22. kw_min_max:                    Best keyword (min. shares)
23. kw_max_max:                    Best keyword (max. shares)
24. kw_avg_max:                    Best keyword (avg. shares)
25. kw_min_avg:                    Avg. keyword (min. shares)
26. kw_max_avg:                    Avg. keyword (max. shares)
27. kw_avg_avg:                    Avg. keyword (avg. shares)
28. self_reference_min_shares:     Min. shares of referenced articles in
                                   Mashable
29. self_reference_max_shares:     Max. shares of referenced articles in
                                   Mashable
30. self_reference_avg_sharess:    Avg. shares of referenced articles in
                                   Mashable
31. weekday_is_monday:             Was the article published on a Monday?
32. weekday_is_tuesday:            Was the article published on a Tuesday?
33. weekday_is_wednesday:          Was the article published on a Wednesday?
34. weekday_is_thursday:           Was the article published on a Thursday?
35. weekday_is_friday:             Was the article published on a Friday?
36. weekday_is_saturday:           Was the article published on a Saturday?
37. weekday_is_sunday:             Was the article published on a Sunday?
38. is_weekend:                    Was the article published on the weekend?
39. LDA_00:                        Closeness to LDA topic 0
40. LDA_01:                        Closeness to LDA topic 1
41. LDA_02:                        Closeness to LDA topic 2
42. LDA_03:                        Closeness to LDA topic 3
43. LDA_04:                        Closeness to LDA topic 4
44. global_subjectivity:           Text subjectivity
45. global_sentiment_polarity:     Text sentiment polarity
46. global_rate_positive_words:    Rate of positive words in the content
47. global_rate_negative_words:    Rate of negative words in the content
48. rate_positive_words:           Rate of positive words among non-neutral
                                   tokens
49. rate_negative_words:           Rate of negative words among non-neutral
                                   tokens
50. avg_positive_polarity:         Avg. polarity of positive words
51. min_positive_polarity:         Min. polarity of positive words
52. max_positive_polarity:         Max. polarity of positive words
53. avg_negative_polarity:         Avg. polarity of negative  words
54. min_negative_polarity:         Min. polarity of negative  words
55. max_negative_polarity:         Max. polarity of negative  words
56. title_subjectivity:            Title subjectivity
57. title_sentiment_polarity:      Title polarity
58. abs_title_subjectivity:        Absolute subjectivity level
59. abs_title_sentiment_polarity:  Absolute polarity level
60. shares:                        Number of shares (target)
```

The goal of this analysis is to predict the number of times a news story
will be shared depending on predictive attributes within a given day of
the week. Without any background in journalism or social media, we will
begin our modelling by including all predictive variables and will then
take a blunt for approach to paring down variables by eliminating those
with little observed correlation with shares. We observe that for all
days of the week, this paring down improves the predictive capability of
at least the linear model.

We then evaluate two non-linear methods - bagged trees and boosted
trees. We apply the added distinction to the boosted tree model of using
all and just the select independent variables mentioned. We also look at
the bagged tree model with and without cross validation. For most days
of the week, there is little variation in predictiveness across models,
with the exception being Tuesday where the most basic linear model
performs markedly worse.

# Load in data

We begin by loading in the data and removing non-predictive variables.

``` r
knitr::opts_chunk$set(include = FALSE)
library(tidyverse)
library(rmarkdown)
library(knitr)

raw_news_data<-select(read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_2\\Project_2\\OnlineNewsPopularity\\OnlineNewsPopularity.csv"),-url, -timedelta)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

# Factorize qualitative data.

We then factorize the qualitative vraiables presented in the model as 0s
and 1s.

``` r
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
```

# Filter data by day

Next, we filter the data by day. In our console we have set up a
dataframe to set parameters, and file names for where discrete results
should be saved. We then walk RMarkdown through the iterations using an
apply function.

`day_of_week<-c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")`
`spec_phrase<-c(paste0("weekday_is_",day_of_week))`
`day_param=lapply(day_of_week,FUN=function(x){list(day_of_week=x)})`
`spec_phrase_param=lapply(spec_phrase,FUN=function(x){list(spec_phrase=x)})`

`day_reports<-tibble(day_of_week,day_param,spec_phrase,spec_phrase_param)%>%`
`mutate(output_file=paste0(day_of_week,".html"))`

`apply(day_reports, MARGIN = 1,` `FUN=function(x){`
`render(input="C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project``_2\\Project_2\\Project_2_Markdown.Rmd",`
`output_file = x[[5]], params=x[[4]])})`

``` r
day_raw_data<-raw_news_data%>%
  filter(raw_news_data[params$spec_phrase]==1)

day_raw_data[params$spec_phrase]
```

# Set up training and test sets

Training and test sets are established on the filtered, factorized data.

``` r
set.seed(1)
train <- sample(1:nrow(day_raw_data), size = nrow(day_raw_data)*0.7)
test <- dplyr::setdiff(1:nrow(day_raw_data), train)
raw_data_train <- day_raw_data[train, ]
raw_data_test <- day_raw_data[test, ]
```

# Basic summary stats

Below we generate a few summary statistics and data visualizations on
the data. To begin, we construct a table delineating standard deviation,
maximum, minimum and average for all variables. Next we build a boxplot
of shares by data channel. Generally speaking, the most widely shared
stories fall in the social and then lifestyle categories. Finally, we
illustrate the correlation of shares to other variables. A quick
inspection of these correlations suggest the polarity of the article,
the presence of certain keywords, the presence of images and shares of
referenced articles in Mashable have stronger correlations (pos. or
neg.) with shares.

``` r
VARIABLE<-c(colnames(raw_data_train))
avg<-round(data.frame("AVG"=sapply(raw_data_train,mean,na.rm=FALSE)),2)
stan_dev<-data.frame("SD"=round(apply(raw_data_train,2,sd),2))
maximum<-data.frame("MAX"=round(as.numeric(apply(raw_data_train,2,max)),2))
minimum<-data.frame("MIN"=round(as.numeric(apply(raw_data_train,2,min)),2))
stat_data<-cbind(VARIABLE,avg,stan_dev,maximum,minimum)
rownames(stat_data)<-c()
kable(stat_data)
```

| VARIABLE                         |       AVG |        SD |       MAX |      MIN |
| :------------------------------- | --------: | --------: | --------: | -------: |
| n\_tokens\_title                 |     10.46 |      2.10 |     19.00 |     4.00 |
| n\_tokens\_content               |    543.45 |    487.79 |   7081.00 |     0.00 |
| n\_unique\_tokens                |      0.67 |      9.74 |    701.00 |     0.00 |
| n\_non\_stop\_words              |      1.17 |     14.48 |   1042.00 |     0.00 |
| n\_non\_stop\_unique\_tokens     |      0.80 |      9.03 |    650.00 |     0.00 |
| num\_hrefs                       |     10.65 |     12.10 |    304.00 |     0.00 |
| num\_self\_hrefs                 |      3.32 |      3.76 |     62.00 |     0.00 |
| num\_imgs                        |      4.46 |      8.24 |    100.00 |     0.00 |
| num\_videos                      |      1.33 |      4.09 |     59.00 |     0.00 |
| average\_token\_length           |      4.54 |      0.85 |      7.97 |     0.00 |
| num\_keywords                    |      7.17 |      1.89 |     10.00 |     1.00 |
| data\_channel\_is\_lifestyle     |        NA |      0.21 |      1.00 |     0.00 |
| data\_channel\_is\_entertainment |        NA |      0.38 |      1.00 |     0.00 |
| data\_channel\_is\_bus           |        NA |      0.36 |      1.00 |     0.00 |
| data\_channel\_is\_socmed        |        NA |      0.25 |      1.00 |     0.00 |
| data\_channel\_is\_tech          |        NA |      0.40 |      1.00 |     0.00 |
| data\_channel\_is\_world         |        NA |      0.41 |      1.00 |     0.00 |
| kw\_min\_min                     |     25.02 |     68.30 |    217.00 |   \-1.00 |
| kw\_max\_min                     |   1131.27 |   3139.78 | 139600.00 |     0.00 |
| kw\_avg\_min                     |    307.70 |    485.28 |  15851.22 |   \-0.14 |
| kw\_min\_max                     |  13974.62 |  57967.08 | 843300.00 |     0.00 |
| kw\_max\_max                     | 755768.95 | 208211.19 | 843300.00 | 17100.00 |
| kw\_avg\_max                     | 262215.93 | 137696.27 | 843300.00 |  3616.67 |
| kw\_min\_avg                     |   1109.38 |   1148.74 |   3609.72 |     0.00 |
| kw\_max\_avg                     |   5619.23 |   6046.53 | 178675.00 |  2019.35 |
| kw\_avg\_avg                     |   3129.75 |   1319.11 |  29240.82 |   804.38 |
| self\_reference\_min\_shares     |   4196.15 |  21757.69 | 690400.00 |     0.00 |
| self\_reference\_max\_shares     |  10220.68 |  38784.23 | 843300.00 |     0.00 |
| self\_reference\_avg\_sharess    |   6450.22 |  25305.75 | 690400.00 |     0.00 |
| weekday\_is\_monday              |        NA |      0.00 |      0.00 |     0.00 |
| weekday\_is\_tuesday             |        NA |      0.00 |      1.00 |     1.00 |
| weekday\_is\_wednesday           |        NA |      0.00 |      0.00 |     0.00 |
| weekday\_is\_thursday            |        NA |      0.00 |      0.00 |     0.00 |
| weekday\_is\_friday              |        NA |      0.00 |      0.00 |     0.00 |
| weekday\_is\_saturday            |        NA |      0.00 |      0.00 |     0.00 |
| weekday\_is\_sunday              |        NA |      0.00 |      0.00 |     0.00 |
| is\_weekend                      |        NA |      0.00 |      0.00 |     0.00 |
| LDA\_00                          |      0.18 |      0.26 |      0.92 |     0.00 |
| LDA\_01                          |      0.13 |      0.21 |      0.92 |     0.00 |
| LDA\_02                          |      0.22 |      0.28 |      0.92 |     0.00 |
| LDA\_03                          |      0.22 |      0.30 |      0.92 |     0.00 |
| LDA\_04                          |      0.25 |      0.30 |      0.93 |     0.00 |
| global\_subjectivity             |      0.44 |      0.12 |      0.84 |     0.00 |
| global\_sentiment\_polarity      |      0.12 |      0.09 |      0.62 |     0.00 |
| global\_rate\_positive\_words    |      0.04 |      0.02 |      0.11 |     0.00 |
| global\_rate\_negative\_words    |      0.02 |      0.01 |      0.08 |     0.00 |
| rate\_positive\_words            |      0.68 |      0.19 |      1.00 |     0.00 |
| rate\_negative\_words            |      0.28 |      0.15 |      1.00 |     0.00 |
| avg\_positive\_polarity          |      0.35 |      0.10 |      0.83 |     0.00 |
| min\_positive\_polarity          |      0.09 |      0.07 |      0.70 |     0.00 |
| max\_positive\_polarity          |      0.75 |      0.25 |      1.00 |     0.00 |
| avg\_negative\_polarity          |    \-0.26 |      0.13 |      0.00 |   \-0.02 |
| min\_negative\_polarity          |    \-0.51 |      0.29 |      0.00 |   \-0.02 |
| max\_negative\_polarity          |    \-0.11 |      0.09 |      0.00 |   \-0.01 |
| title\_subjectivity              |      0.27 |      0.32 |      1.00 |     0.00 |
| title\_sentiment\_polarity       |      0.07 |      0.26 |      0.10 |   \-0.10 |
| abs\_title\_subjectivity         |      0.35 |      0.19 |      0.50 |     0.00 |
| abs\_title\_sentiment\_polarity  |      0.15 |      0.23 |      0.10 |     0.00 |
| shares                           |   3185.14 |  10021.60 | 441000.00 |    45.00 |

``` r
channel_dig<-raw_data_train%>%
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
```

![](tuesday_files/figure-gfm/summary%20stats-1.png)<!-- -->

``` r
cor_mat<-rcorr(as.matrix(raw_data_train))
data.frame(cor_mat$P[,"shares"])
```

    ##                               cor_mat.P....shares..
    ## n_tokens_title                         2.649944e-01
    ## n_tokens_content                       9.818871e-01
    ## n_unique_tokens                        7.911018e-01
    ## n_non_stop_words                       7.946811e-01
    ## n_non_stop_unique_tokens               8.066310e-01
    ## num_hrefs                              1.574285e-03
    ## num_self_hrefs                         5.088518e-01
    ## num_imgs                               3.297506e-05
    ## num_videos                             1.928429e-02
    ## average_token_length                   2.821602e-01
    ## num_keywords                           2.688467e-02
    ## data_channel_is_lifestyle              8.316665e-01
    ## data_channel_is_entertainment          2.156276e-01
    ## data_channel_is_bus                    8.529238e-01
    ## data_channel_is_socmed                 3.100332e-01
    ## data_channel_is_tech                   2.323270e-01
    ## data_channel_is_world                  1.877975e-04
    ## kw_min_min                             3.665943e-01
    ## kw_max_min                             8.573405e-01
    ## kw_avg_min                             8.783956e-01
    ## kw_min_max                             8.687033e-01
    ## kw_max_max                             2.038776e-01
    ## kw_avg_max                             1.696514e-05
    ## kw_min_avg                             4.373333e-04
    ## kw_max_avg                             4.858337e-05
    ## kw_avg_avg                             2.886580e-15
    ## self_reference_min_shares              1.031187e-04
    ## self_reference_max_shares              3.606291e-05
    ## self_reference_avg_sharess             2.003236e-06
    ## weekday_is_monday                               NaN
    ## weekday_is_tuesday                              NaN
    ## weekday_is_wednesday                            NaN
    ## weekday_is_thursday                             NaN
    ## weekday_is_friday                               NaN
    ## weekday_is_saturday                             NaN
    ## weekday_is_sunday                               NaN
    ## is_weekend                                      NaN
    ## LDA_00                                 5.715366e-01
    ## LDA_01                                 7.491814e-01
    ## LDA_02                                 8.000238e-06
    ## LDA_03                                 1.850895e-08
    ## LDA_04                                 5.084094e-01
    ## global_subjectivity                    6.212917e-03
    ## global_sentiment_polarity              8.082270e-01
    ## global_rate_positive_words             4.046485e-01
    ## global_rate_negative_words             2.219911e-01
    ## rate_positive_words                    1.298519e-01
    ## rate_negative_words                    3.860597e-01
    ## avg_positive_polarity                  1.706088e-01
    ## min_positive_polarity                  2.756667e-01
    ## max_positive_polarity                  1.201599e-02
    ## avg_negative_polarity                  1.364693e-01
    ## min_negative_polarity                  1.743212e-01
    ## max_negative_polarity                  7.952112e-01
    ## title_subjectivity                     3.824320e-01
    ## title_sentiment_polarity               2.598872e-01
    ## abs_title_subjectivity                 8.720498e-01
    ## abs_title_sentiment_polarity           3.763140e-01
    ## shares                                           NA

``` r
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
```

![](tuesday_files/figure-gfm/summary%20stats-2.png)<!-- -->

# Scale and center data

Before we begin training our models, the data, filtered by day, must be
scaled and centered to account for the large range in size across
independent variables as well as variation seen within these variables.

``` r
scaled_train_data<-data.frame(scale(select(raw_data_train,-c(12:17,30:37)),center = TRUE, scale = TRUE))
scaled_train_data<-cbind(scaled_train_data,select(raw_data_train,c(12:17,30:37)))

scaled_test_data<-data.frame(scale(select(raw_data_test,-c(12:17,30:37)),center = TRUE, scale = TRUE))
scaled_test_data<-cbind(scaled_test_data,select(raw_data_test,c(12:17,30:37)))
```

# Train linear models

We then train our linear models. As mentioned in the introduction, we
train one model to include all independent variables and one to include
a selection of those that are more correlated. Per 7/1/20 discussion
with Dr. Post this was deemed to be a reasonable approach for a dataset
where we lack any subject area expertise

``` r
linear_all_model<-train(shares~., data=scaled_train_data,method="lm")

summary(linear_all_model)

linear_select_model<-train(shares~LDA_02+data_channel_is_world+avg_negative_polarity+
                           average_token_length+kw_avg_avg+
                           LDA_03+kw_max_avg+self_reference_avg_sharess+num_hrefs+num_imgs+
                           global_subjectivity, data=scaled_train_data,method="lm")

summary(linear_select_model)
```

# Train non-linear models

We then train the model using bagged tree and boosted tree. In addition,
the bagged tree is cross validated and the boosted tree is trained on
the more highly correlated subset of independent variables.

``` r
bagged_tree<-train(shares~.,data=scaled_train_data,method="treebag")
summary(bagged_tree)

boosted_tree<-train(shares~.,data=scaled_train_data,method="gbm")

boosted_tree_select<-train(shares~LDA_02+data_channel_is_world+avg_negative_polarity+average_token_length+kw_avg_avg+LDA_03+kw_max_avg+self_reference_avg_sharess+num_hrefs+num_imgs+global_subjectivity, data=scaled_train_data,method="gbm")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

bagged_tree_CV<-train(shares~.,data=scaled_train_data,method="treebag", trControl=fitControl)
```

# Make predictions on test data

Predictions are made on the scaled, test data set.

``` r
lin_all_pred<-predict(linear_all_model,newdata=scaled_test_data)
```

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may be misleading

``` r
lin_sel_pred<-predict(linear_select_model,newdata=scaled_test_data)
bag_tree_pred<-predict(bagged_tree,newdata=scaled_test_data)
bagged_tree_cv_pred<-predict(bagged_tree_CV,newdata=scaled_test_data)
boost_tree_pred<-predict(boosted_tree,newdata=scaled_test_data)
boost_tree_sel_pred<-predict(boosted_tree_select,newdata=scaled_test_data)
```

## Compare predictiveness of models

The predictiveness of the models is evaluated using root mean square
error `RMSE`.

``` r
first<-RMSE(lin_all_pred,scaled_test_data$shares)
second<-RMSE(lin_sel_pred,scaled_test_data$shares)
third<-RMSE(bag_tree_pred,scaled_test_data$shares)
fourth<-RMSE(bagged_tree_cv_pred,scaled_test_data$shares)
fifth<-RMSE(boost_tree_pred,scaled_test_data$shares)
sixth<-RMSE(boost_tree_sel_pred,scaled_test_data$shares)
`RMSE`<-c(first,second,third,fourth,fifth,sixth)
"model name"<-c("lin all","lin select","bag tree","bag tree cv","boost tree","boost tree select")
kable(data.frame(`model name`,RMSE))
```

| model.name        |      RMSE |
| :---------------- | --------: |
| lin all           | 2.3393981 |
| lin select        | 0.9808803 |
| bag tree          | 0.9934652 |
| bag tree cv       | 1.0061847 |
| boost tree        | 0.9824935 |
| boost tree select | 0.9811629 |
