# CovidMonitor
This CovidMonitor dashboard was built using Shiny R framework. It was designed to obtain tweets from twitter's API and perform a real time Sentiment Analysis on the COVID-19 related posts of users in Italy.

The number of tweets and key terms can be used to retrieve tweets with a range between 100 and 2000 tweets at a time due to API limitations. There are also a set of preferred key terms that can be used to guide the search. In the sidebar it could be selected the following parameters:

1.	*Select date*: there is a limit of past 8-9 days to retrieve information from Twitter's API.
2.	*Choose key terms*: group of key terms related to Covid-19 pandemic that are selected by default.
3.	*Add a particular key term*: user-defined term to include in the analysis.
4.	*Number of tweets for analysis*: a slider that controls how many tweets should be returned. It is noteworthy that the higher the number, the longer it may take for the app to load.
5.	*Get data*: submit button to load the data analysis in the whole app, except for the *Top 10 Trending Topics* tab because it is a dynamic chart that does not depend on the sidebar parameters.
