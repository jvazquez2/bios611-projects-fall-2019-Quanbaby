library(tidyverse)
library(ggplot2)
mydata = read_tsv("/Users/apple/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_1/data/UMD_Services_Provided_20190719.tsv")
rawdata = mydata #back up the raw data

# 1. Show the change over time for
## arrange by Date
Date1 = as.Date(mydata$Date,format = "%m/%d/%Y")
mydata$Date = Date1
mydata_sort = mydata %>%
  arrange(Date)

#colplot = function(xx) {
#  mydata_sort %>%  
#    select(Date, {xx}) %>%
#    filter(is.na({xx})==F) %>%
#    group_by(Date) %>%
#    summarise(total = sum({xx})) %>%
#    ggplot(aes(x=Date, y=total)) + 
#    geom_point()
#}

#colplot("Diapers")
## the number of bus tickets:  
mydata_sort %>%  
  select(Date, `Bus Tickets (Number of)`) %>%
  filter(is.na(`Bus Tickets (Number of)`)==F) %>%
  group_by(Date) %>%
  summarise(day_tickets = sum(`Bus Tickets (Number of)`)) %>%
  ggplot(aes(x=Date, y=day_tickets)) + 
  geom_point()
  
## food pounds:
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point()
#### Found that there are some impossible date values and an outlier, remove those
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point()
#### Zoom in the plot of year 2005-2020
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point()
#### find the date of those outliers
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  filter(Food_total >= 2000)
#### remove thoes outliers, then fit a curve to show the tendency 
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  filter(Food_total < 2000) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point() +
  geom_smooth()

## clothing items:
mydata_sort %>%  
  select(Date, `Clothing Items`) %>%
  filter(is.na(`Clothing Items`)==F) %>%
  group_by(Date) %>%
  summarise(cloth_total = sum(`Clothing Items`)) %>%
  ggplot(aes(x=Date, y=cloth_total)) + 
  geom_point()
#### Zoom in the plot of year 2000-2019, remove the "future dates" and fit a curve
mydata_sort %>%  
  select(Date, `Clothing Items`) %>%
  filter(is.na(`Clothing Items`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(cloth_total = sum(`Clothing Items`)) %>%
  ggplot(aes(x=Date, y=cloth_total)) + 
  geom_point() + 
  geom_smooth()

## diapers:
mydata_sort %>%  
  select(Date, Diapers) %>%
  filter(is.na(Diapers)==F) %>%
  group_by(Date) %>%
  summarise(diapers_total = sum(Diapers)) %>%
  ggplot(aes(x=Date, y=diapers_total)) + 
  geom_point()
#### remove the outlier
mydata_sort %>%  
  select(Date, Diapers) %>%
  filter(is.na(Diapers)==F) %>%
  group_by(Date) %>%
  summarise(diapers_total = sum(Diapers)) %>%
  filter(diapers_total != max(diapers_total)) %>%
  ggplot(aes(x=Date, y=diapers_total)) + 
  geom_point()

## School Kits:
mydata_sort %>%  
  select(Date, `School Kits`) %>%
  filter(is.na(`School Kits`)==F) %>%
  group_by(Date) %>%
  summarise(school_total = sum(`School Kits`)) %>%
  ggplot(aes(x=Date, y=school_total)) + 
  geom_point()

## Hygiene Kits:
mydata_sort %>%  
  select(Date, `Hygiene Kits`) %>%
  filter(is.na(`Hygiene Kits`)==F) %>%
  group_by(Date) %>%
  summarise(hygiene_total = sum(`Hygiene Kits`)) %>%
  ggplot(aes(x=Date, y=hygiene_total)) + 
  geom_point()

## Financial Support:
mydata_sort %>%  
  select(Date, `Financial Support`) %>%
  filter(is.na(`Financial Support`)==F) %>%
  group_by(Date) %>%
  summarise(finance_total = sum(`Financial Support`)) %>%
  ggplot(aes(x=Date, y=finance_total)) + 
  geom_point()
#### Zoom in the plot of year 2000-2019, remove the "future dates" and plot non-zero points
mydata_sort %>%  
  select(Date, `Financial Support`) %>%
  filter(is.na(`Financial Support`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(finance_total = sum(`Financial Support`)) %>%
  filter(finance_total!=0) %>%
  ggplot(aes(x=Date, y=finance_total)) + 
  geom_point()

# 2. People (family) coming most frequently and two specific families.
freq = mydata_sort %>%
  group_by(`Client File Number`) %>%
  tally() %>%
  arrange(desc(n))

mydata_sort %>%
  filter(`Client File Number`==3502) %>%
  arrange(desc(Date))
mydata_sort %>%
  filter(`Client File Number`==805) %>%
  arrange(desc(Date))

# families coming recently
mydata_sort %>%
  arrange(desc(Date)) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" ))

mydata_sort %>%
  filter(`Client File Number`==814) %>%
  arrange(desc(Date))








  
  