#######################
##### CovidMonitor ####
#######################

# This is a Shiny web application. 
# Created by Mathias Cardarello Fierro, 2021.


### 1. Setting up ###

## Firstly, we use the function p_load of 'pacman' to check if a package is installed, 
# if not it attempts to install it and then loads it

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny, shinydashboard, twitteR, rtweet, ROAuth, RCurl, stringr, ggplot2, glue, 
               httpuv, dplyr, purrr, reshape, tm, plotrix, plotly, parallel,
               RJSONIO, wordcloud, gridExtra, plyr, e1071, openssl, httpuv, base64enc)

# We use 'parallel' library to send tasks to each of the processing cores on our machine in parallel.
numCores <- detectCores() # get the number of cores available

## Then, we build the framework of the Shiny dashboard

# List of predefined key terms (related to COVID-19)
key_terms <- list("coronavirus","covid", "covid19", "covid-19", 
                  "iorestoacasa", "iostoacasa", "vaccino", "vaccini", "vaccinazioni")

# Components of the dashboard

header <- dashboardHeader(title = span(tagList(icon("twitter"), "CovidMonitor Italy")),
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "Welcome to CovidMonitor!",
                                         icon("hand-spock")),
                                         notificationItem(
                                           text = "To start click on 'Get data' button",
                                           icon("mouse-pointer"),
                                           status="warning")
                          ))


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main metrics", tabName = "overview", icon = icon("dashboard")),
    menuItem("Sentiment analysis", icon = icon("smile-beam"), tabName = "sentiment",
             badgeLabel = "Italy", badgeColor = "green"),
    dateRangeInput("date_picker", label = "Select dates:", start = Sys.Date() - 8, end = Sys.Date()),
    selectInput("key_terms_to_search", "Choose key terms:",
                key_terms,
                selected = key_terms,
                multiple = TRUE),
    textInput("user_term", "Add a particular key term:", 
              value = ""),
    
    sliderInput("maxTweets","Number of tweets for analysis:",min=100,max=2000,value=500), 
    actionButton("get_data", "Get data", class = "btn-success", style = "margin: 5px 5px 5px 75px; ")
  )
)

body <- dashboardBody(
  tabItems(
    # First tab
    tabItem(tabName = "overview",
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("fire-alt"), "Top 10 Trending Topics"),
                side = "right", height = "435px",
                tabPanel("World", "Global hot topics in the last hour", br(), br(), tableOutput("trendtable_world")),
                tabPanel("Italy", "Italy hot topics in the last hour", br(), br(), tableOutput("trendtable_it"))
              ),
              
              tabBox(
                title = tagList(shiny::icon("users"), "Top users mentioning key terms"),
                side = "right", height = "435px",
                tabPanel("Plot", plotlyOutput("tweetersplot", height = '100%')),
                tabPanel("Table", tableOutput("tweeterstable"))
                )
                
              ) , br()
            ,fluidRow(
              box(
                title = tagList(shiny::icon("chart-line"), "Frequency of tweets mentioning key terms"),
                status = "primary", solidHeader = TRUE,
                side = "right", width = 12, height = "320px",
                plotOutput("freq_plot", height = '100%')
              )
              
            )
            ),
              
    # Second tab
    tabItem(tabName = "sentiment",
            fluidRow(
            tabBox(
              title = tagList(shiny::icon("sort-numeric-up-alt"), "Sentiment score"),
              side = "right", height = "435px",
              tabPanel("Overall",
                       #,HTML
                   #  ("<div><h3> Histograms graphically depict the positivity or negativity of peoples' opinion about of the hashtag
                 #</h3></div>"), 
                     plotOutput("histScore", height = '100%')),
              tabPanel("Negative", plotOutput("histNeg", height = '100%')),
              tabPanel("Positive", plotOutput("histPos", height = '100%'))
            ),
            
            box(
              title = tagList(shiny::icon("chart-pie"), "Share of sentiment type"),
              status = "primary", solidHeader = TRUE,
              height = "435px",
              plotOutput("piechart", height = '100%')
                     
            )), br(),
            
            fluidRow(
              box(
              title = tagList(shiny::icon("cloud"), "Wordcloud with key terms"),
              status = "primary", solidHeader = TRUE,
              height = "435px",
              sliderInput("min_freq", "Lowest word frequency",
                           value = 1, min = 1, max = 5, width="40%"),
              plotOutput("wordcloud", height = '90%', width = '100%')),
              
              box(
                title = tagList(shiny::icon("star"), "Top frequent words"),
                status = "primary", solidHeader = TRUE,
                height = "435px",
                sliderInput("words", "Number of words shown",
                            value = 20, min = 1, max = 50, width="60%"),
                plotlyOutput("wordfreq", height = '100%')
                
              )
            )
            )
    
  ))



## Finally, we put them together into a dashboardPage (UI)

### 2. UI ###

ui <- dashboardPage(
  header,
  sidebar,
  body
)


### 3. SERVER ###

server <- function(input, output) {
  
  
  # Credentials for accessing the Twitter API
  ##### API source: https://github.com/ankit2web/Twitter-Sentiment-Analysis-using-R-Shiny-WebApp/blob/master/server.R
  
  api_key <- "LhLzIn0nbz5mORcE3wPdSmWjP"
  api_secret <- "xILbs2S5IbNiZyFwXU7VITcVCxxzf3SpA2Gbvn3qBNF8LY8woQ"
  access_token <- "110651492-aB9iL1exrmkb3Q2gmM2DEqCzz6eo0TQiqqjRRXec" 
  access_token_secret <- "8XZIQ6eVvOwAwhvGSSIL4SEitrttjAkf6SYAJHFXMFxz9"
  
  # Setting access using OAUTH protocol
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  cat("\014")
  origop <- options("httr_oauth_cache")
  options(httr_oauth_cache = TRUE)
  
  # Search for tweets and create a data.frame
  
  # Function to clean the tweets
  TweetFrame<-function(twtList)
  {
    df<- do.call("rbind", parallel::mclapply(twtList,as.data.frame,mc.cores = numCores))
    #Removal of emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  
  # List of italian words for sentimental analysis taken from Sentix (Sentiment Italian Lexicon)
  # We use it to identify if a sentence is positive or negative.
  
  df = read.delim('sentix.txt', header = FALSE, sep = "\t")
  pos.words = df[df$V6 > 0, 1]
  neg.words = df[df$V6 < 0, 1]
  
  # Function to create a data.frame with positive and negative words from tweets
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words)
    neg.words<<-c(neg.words)
  }
  
  # Process to compute Sentiment score
  
  # Starting with a function that includes text cleaning techniques
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=parallel::mclapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words, mc.cores = numCores)
    score_new=parallel::mclapply(list, `[[`, 1, mc.cores = numCores)
    pp1=score=parallel::mclapply(list, `[[`, 2, mc.cores = numCores)
    nn1=score=parallel::mclapply(list, `[[`, 3, mc.cores = numCores)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    # Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    # Creating three different data frames for Overall, Positive and Negative scores
    # Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    
    # Storing the first row (Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, var='Score')
    qq2=melt(q2, var='Positive')
    qq3=melt(q3, var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    
    # Creating data.frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    # Merging three data.frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    
    return(table_final)
  }
  
  # Function to obtain final Sentiment score
  percentage<-function(table_final)
  {
    # Positive Percentage
    
    # Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    # Adding column
    table_final$PosPercent = posSc / (posSc+negSc)
    
    # Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    # Negative Percentage
    
    # Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    # Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    return(table_final)
  }
  
  wordDatabase()
  
  
  # Looking for tweets with 'searchTwitter' function
  # For Italy, as few tweets have latitude and longitude embedded with them, we use language parameter to filter data
  
  twtList<-eventReactive(input$get_data, {twtList<-searchTwitter(paste(c(input$key_terms_to_search,input$user_term), 
                                                                       collapse = ' OR '), n=input$maxTweets, 
                                            since=as.character(input$date_picker[1]), 
                                            until=as.character(input$date_picker[2]), 
                                            resultType = "popular" , lang="it") })
  
  tweets<-eventReactive(input$get_data, {tweets<-TweetFrame(twtList() )})
  tweets_df<-eventReactive(input$get_data, {tweets_df<-twListToDF(twtList() )})
  
  
  result<-eventReactive(input$get_data, {result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-eventReactive(input$get_data, {table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-eventReactive(input$get_data, {table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  
  # Function to obtain top trending topics by location
  toptrends <- function(place)
  {
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name==place),3]
    trend = getTrends(woeid) #returns the top 30 trending topics for each day starting a week ago
    trends = trend[1:2]
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split=", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
    dat4 <- dat2[-dat3]
    return (dat4)
  }
  
  # Data of top 10 trending topics
  output$trendtable_it <- renderTable(toptrends("Italy")[1:10], colnames = FALSE)
  output$trendtable_world <- renderTable(toptrends("Worldwide")[1:10], colnames = FALSE)
  
 
  # Funtion to obtain top users mentioning key terms
  
  toptweeters<-function(tweetlist)
  {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    
    # Make a table for the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    d <- d[order(d$Freq, decreasing=T), ] #descending order of top charts according to frequency of tweets
    names(d) <- c("User","Tweets")
    d$User <- factor(d$User, levels = unique(d$User)[order(d$Tweets, decreasing = TRUE)])
    return (d)
  }
  

  # Barplot of the top users
  
  d<-eventReactive(input$get_data, {d<-toptweeters(  twtList() )})

  output$tweetersplot<-renderPlotly({
    plot_ly(
      y = d()$Tweets[1:20],
      x = d()$User[1:20],
      text = d()$Tweets[1:20],
      textposition='auto',
      type = 'bar') %>% layout(autosize = F, width = 500, height = 350)
  })
  
  output$tweeterstable<-renderTable(head(d(),10))

  
  # Tweets time series plot

    output$freq_plot<-renderPlot({
      ts_plot(tweets_df(), "days") +  geom_line(color="steelblue", size=1.5) + geom_point(size=2, color="darkgreen") + 
        theme(axis.title = element_text(size=12), axis.text = element_text(size = 12)) + xlab(NULL) + ylab("# tweets") }
      , height = 250, width = 1100)

    
  # Histograms of Sentiment score
  
  output$histPos<-renderPlot({
      ggplot(table_final(), aes(x=table_final()$Positive)) +
      geom_histogram(fill="springgreen4", color="#e9ecef", binwidth=0.5) +
      geom_vline(aes(xintercept=mean(table_final()$Positive)),
                 color="darkblue", linetype="dashed", size=0.8) +
      labs(x="Score", y = "Frequency", caption = paste("The positive mean score is", round(mean(table_final()$Positive),2))) +
      theme(axis.text = element_text(size = 12), plot.caption=element_text(size=12, hjust = 0, face="italic"))
}, height = 350, width = 500)

  
  output$histNeg<-renderPlot({
    ggplot(table_final(), aes(x=table_final()$Negative)) +
      geom_histogram(fill="firebrick", color="#e9ecef", binwidth=0.5) +
      geom_vline(aes(xintercept=mean(table_final()$Negative)),
                 color="darkblue", linetype="dashed", size=0.8) +
      labs(x="Score", y = "Frequency", caption = paste("The negative mean score is", round(mean(table_final()$Negative),2))) +
      theme(axis.text = element_text(size = 12), plot.caption=element_text(size=12, hjust = 0, face="italic"))
}, height = 350, width = 500)

  output$histScore<-renderPlot({
    ggplot(table_final(), aes(x=table_final()$Score, fill=..x..)) +
      geom_histogram(binwidth=0.5) +
      scale_fill_gradient(low='firebrick1', high="springgreen4") +
      geom_vline(aes(xintercept=mean(table_final()$Score)),
                 color="darkblue", linetype="dashed", size=0.8) +
      labs(x="Score", y = "Frequency", caption = paste("The mean Sentiment score is", round(mean(table_final()$Score),2))) +
      theme(axis.text = element_text(size = 12), legend.position="none", plot.caption=element_text(size=12, hjust = 0, face="italic"))
    }, height = 350, width = 500)


  # Pie of sentiments distribution
 
  data_pie <- eventReactive(input$get_data, {data_pie <- data.frame(
    labels = c("Positive", "Negative"),
    values = c(sum(table_final()$Positive), sum(table_final()$Negative))
  )})
  
  
  output$piechart <- renderPlot({ ggplot(data_pie(), aes(x="", y=data_pie()$values, fill=data_pie()$labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      theme(legend.text=element_text(size=12)) +
      labs(fill = "") +
      scale_fill_manual(values=c("firebrick", "springgreen4")) +
      geom_text(aes(label = scales::percent(data_pie()$values/sum(data_pie()$values), accuracy = .1)), color="white", position = position_stack(vjust = .5), size=5)
  #})
   }, height = 350, width = 500)
  
  
  # WordCloud
  
  # Function to clean the word cloud data
  clean_wordcloud_data<-function(text)
  {
    corpus_data <- VCorpus(VectorSource(text))
    
    # Cleaning data for word cloud by transforming the case, removing stop words, whitespaces, etc., and return the data
    word_cloud_data <- tm_map(corpus_data, removePunctuation)
    word_cloud_data <- tm_map(word_cloud_data, content_transformer(tolower))
    word_cloud_data <- tm_map(word_cloud_data, removeWords, stopwords("italian"))
    word_cloud_data <- tm_map(word_cloud_data, removeNumbers)
    word_cloud_data <- tm_map(word_cloud_data, stripWhitespace)
    return (word_cloud_data)
  }
  
  text_word <- eventReactive(input$get_data, {text_word<-clean_wordcloud_data(tweets())})
  output$wordcloud <- renderPlot({ wordcloud(text_word(), colors=brewer.pal(8, "Dark2"), 
                                             min.freq = input$min_freq, max.words=150, 
                                             rot.per=0.5, scale=c(1,1.5)) }, height = 280, width = 500)
  

  
  # Words frequency
  
  # Function to create a word frequency data.frame
  word_frequency <- function(data_words) 
  {
    dtm <- TermDocumentMatrix(data_words) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    df$word <- factor(df$word, levels = c(as.character(df$word)))
    return (df)
  }
  
  freq_table <- eventReactive(input$get_data, {freq_table<-word_frequency(text_word())})
  
  # Bar plot of words frequency
  
  output$wordfreq <- renderPlotly({
    plot_ly(
      y = freq_table()$freq[1:input$words],
      x = freq_table()$word[1:input$words],
      text = freq_table()$freq[1:input$words],
      textposition='auto',
      type = 'bar') %>% layout(autosize = F, width = 500, height = 250)
  })
  
  }

shinyApp(ui = ui, server = server)


