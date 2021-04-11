# CovidMonitor Italy
The **CovidMonitor Italy** dashboard was built using Shiny R framework. It was designed to obtain tweets from Twitter's API and perform a real time Sentiment Analysis on the COVID-19 related posts of users in Italy.

The number of tweets and key terms can be used to retrieve tweets with a range between 100 and 2000 tweets at a time due to API limitations. There are also a set of preferred key terms that can be used to guide the search. In the **sidebar** it could be selected the following parameters:

1.	**Select date**: there is a limit of past 8-9 days to retrieve information from Twitter's API.
2.	**Choose key terms**: group of key terms related to Covid-19 pandemic that are selected by default.
3.	**Add a particular key term**: user-defined term to include in the analysis.
4.	**Number of tweets for analysis**: a slider that controls how many tweets should be retrieved. It is noteworthy that the higher the number, the longer it may take for the app to load.
5.	**Get data**: submit button to load the data analysis in the whole app, except for the **Top 10 Trending Topics** tab because it contains dynamic charts that do not depend on the sidebar parameters.


## Components
1.	On the **Main metrics** tab, there is an overview of the most important metrics related to Twitter activity. This includes:
*	*Top 10 Trending Topics*: Twitter's hot topics in the last hour, for Italy and the world. There is no need to click on the *“Get data”* button to get this information. Italy Trendic Topics can not be retrieved in some occasions due to API problems❗
*	*Top users mentioning key terms*: a dynamic bar plot and table that shows the top tweeters according to the frequency with which they used the defined key terms.
*	*Frequency of tweets mentioning key terms*: a time series plot with the frequency of tweets mentioning defined key terms.


2.	On the **Sentiment Analysis** tab, there is the result of the text mining analysis of the tweets in Italy, related to COVID-19 pandemic. This includes:
* *Sentiment Score*: histograms of positive, negative and overall score for graphically analyzing the intensity of emotion in the tweets that contains the defined key terms. To compute the score, we use the polarity of Italian lemmas available in [Sentix](http://valeriobasile.github.io/twita/sentix.html) (Sentiment Italian Lexicon), a lexicon built by [TWITA](http://valeriobasile.github.io/twita/about.html) project investigation. Polarity ranges from -1 (totally negative) to 1 (totally positive) and measures the lexical sentiment. Thus, the Sentiment Score is calculated using simple arithmetic to understand the overall sentiment of Italian tweets related to COVID-19 pandemic. The complete list of positive and negative lemmas, built on Sentix polarity basis, can be found in the file `sentix.txt`. 
*	*Share of sentiment type*: a pie chart with the share (%) of positive and negative words (based on Sentiment Score), related to the retrieved tweets.
*	*Word cloud with key terms*: a word cloud plot that contains the most frequent words related to the retrieved tweets. It is a visual representation that gives greater prominence to words that appear more frequently, after removing stop words, whitespaces, punctuations, numbers, etc. from tweets. The slider controls the lowest word frequency parameter of the plot.
* *Top frequent words*: a bar plot with the most frequent words occurring in the retrieved tweets. The slider controls how many words should be plotted.


## Getting started
Once shiny package is installed, load the library and call the Shiny app:
    
    library(shiny)
    # Launch CovidMonitor Shiny app
    shiny::runGitHub("mathicard/CovidMonitor")



## Sources

* [ggplot2: Elegant Graphics for Data Analysis](https://ggplot2-book.org/index.html)
* [Mining Twitter with R](https://sites.google.com/site/miningtwitter/home)
* [Parallel Computation. R Programming for Data Science](https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html)
* [Quick Intro to Parallel Computing in R](https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)
* RStudio: IDE for R Programming
* [Sentix lexicon for Sentiment Analysis in Italy](http://valeriobasile.github.io/twita/sentix.html)
* [Shiny dashboards](https://rstudio.github.io/shinydashboard/index.html)
* [Shiny for R Studio](https://shiny.rstudio.com)
* [The Coronavirus in Italy from the Twitter's Point of View](http://tech.kode-datacenter.net:11000/covid19/articles/twitter-analysis-overview/)
* [Twitter's API](https://developer.twitter.com/en/docs/twitter-api)
* [Using R to extract interesting insights trending topics on Twitter](https://medium.com/@emmanuelsibanda/explaining-south-africas-sona-of-2019-through-a-twitter-keyword-analysis-f5306cad2d79)


## App development

This app was developed solely by Mathias Cardarello Fierro, based on the following previous works:
* [Corona-Virus Shiny APP](https://github.com/simmieyungie/Corona-Virus)
* [Twitter Sentiment Analysis using R & Shiny WebApp](https://github.com/ankit2web/Twitter-Sentiment-Analysis-using-R-Shiny-WebApp)
* [Twitter-sentiment-analysis-using-R-Shiny-App](https://github.com/vidhigandhi94/Twitter-sentiment-analysis-using-R-Shiny-App)
