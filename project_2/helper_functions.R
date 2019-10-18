library(ggplot2)
library(tidyverse)

# Read and pre-process the data
mydata = read_tsv(url("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv"))
Date1 = as.Date(mydata$Date,format = "%m/%d/%Y")
mydata$Date = Date1
mydata_sort = mydata %>%
  arrange(Date) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" ))

# 1. Show the trend over time for different variables
trend = function(col){
  mydata1 = mydata_sort %>%  
    select(Date, matches(col)) %>%
    filter(is.na(get(col))==F & get(col) > 0) %>%
    group_by(Date) %>%
    summarise(amount = sum(get(col)))
  if(col == "Food Pounds" ){
    mydata1 %>%
      filter(amount < 2000) %>% # remove outliers
      ggplot(aes(x=Date, y=amount)) + 
      geom_point() + 
      geom_smooth() + 
      labs(x='Date', y=col, title = paste("The trend of '", col, "' over time"))
  }else if(col == "Clothing Items" || col == "Food Provided for"){
    mydata1 %>%
      filter(amount < 700) %>% # remove outliers
      ggplot(aes(x=Date, y=amount)) + 
      geom_point() + 
      geom_smooth() + 
      labs(x='Date', y=col, title = paste("The trend of '", col, "' over time"))
  }else{
    mydata1 %>%
      filter(amount != max(amount)) %>%
      ggplot(aes(x=Date, y=amount)) + 
      geom_point() + 
      labs(x='Date', y=col, title = paste("The trend of '", col, "' over time"))
  }
}

# 2. Show the potential relationships between different variables.
relation = function(y){
  mydata_sort %>%
    filter(`Food Provided for` > 0 & `Food Provided for` < 200) %>%
    filter(get(y) > 0 & get(y) < 1000) %>%
    filter(get(y) != max(get(y))) %>%
    ggplot(aes(x=`Food Provided for`,y=get(y))) +
    geom_point() + 
    labs(x='Number of people', y=y, 
         title = paste("The relationship between the number of people and '", y, "'"))
}

# 3. Show the average amount of food/clothes per person per visit getting.

average_plot = function(var){
  averg = mydata_sort %>%
    filter(`Food Provided for` > 0 ) %>%
    filter(get(var) > 0 ) %>%
    mutate(mean = get(var)/`Food Provided for`) %>%
    filter(mean < 100)
  ggplot(averg, aes(Date,mean)) +
    geom_point() + 
    geom_smooth() + 
    labs(x='Date', y=var, 
         title = paste("The change of '", var, "' per person per visit over time"))
}

average_num = function(var){
  averg = mydata_sort %>%
    select(Date, matches(var), `Food Provided for`) %>%
    filter(`Food Provided for` > 0 ) %>%
    filter(get(var) > 0 ) %>%
    mutate(mean = get(var)/`Food Provided for`) %>%
    filter(mean < 500)
  recent_avg = averg %>% 
    filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
  return(c(mean(averg$mean),mean(recent_avg$mean)))
}

# 4. Specific analysis for individuals.

# Summary of this client.
client_check = function(c){
  client_freq = mydata_sort %>%
    group_by(`Client File Number`) %>%
    summarise(n = n()) 
  if(sum(client_freq$`Client File Number` == c) == 0){
    cat("ERROR: There's no such a client!\n")
    exist <<- 0
  }else{
    cat(paste("Client", c, "has come", client_freq[which(client_freq$`Client File Number`==c),2],
                "times in total.", "\n"))
    exist <<- 1
    client = mydata_sort %>%
      filter(`Client File Number`==c) %>%
      arrange(Date) 
    client_interval = client$Date[-1] - client$Date[-length(client$Date)]
    cat(paste("The last date that Client", c, "came is", max(client$Date), 
              "and the average interval is", mean(client_interval), "days.",
              "Plots of coming time and intervals are shown to the right:"))
  }
}


# Plot the coming time of the client
client_plot1 = function(c){
    client = mydata_sort %>%
      filter(`Client File Number`==c) %>%
      arrange(Date) 
    ggplot(client, aes(x=Date,y=1)) + 
      geom_point() + 
      labs(x='Date', y='Visit or not', title = paste("Time of client",c, "came for help"))
}

# Plot the interval of the client coming time
client_plot2 = function(c){
    client = mydata_sort %>%
      filter(`Client File Number`==c) %>%
      arrange(Date) 
    client_interval = client$Date[-1] - client$Date[-length(client$Date)]
    plot(x=1:length(client_interval), y=client_interval, 
         xlab='Date', ylab='Visit or not', main=paste("Intervals of client",c,"came for help"))  
}





