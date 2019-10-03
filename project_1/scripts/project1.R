# load and back up the data
library(tidyverse)
library(ggplot2)
mydata = read_tsv("/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_1/data/UMD_Services_Provided_20190719.tsv")
rawdata = mydata #back up the raw data

# pre-processing
## arrange by Date
Date1 = as.Date(mydata$Date,format = "%m/%d/%Y")
mydata$Date = Date1
mydata_sort = mydata %>%
  arrange(Date)

# 1. Show the change over time for
## (1) the number of bus tickets:
mydata_sort %>%  
  select(Date, `Bus Tickets (Number of)`) %>%
  filter(is.na(`Bus Tickets (Number of)`)==F) %>%
  group_by(Date) %>%
  summarise(day_tickets = sum(`Bus Tickets (Number of)`)) %>%
  ggplot(aes(x=Date, y=day_tickets)) + 
  geom_point() + 
  labs(x='Date', y='number of tickets', title = "The trend of tickets over time")

## (2) people that UMD have helped:
mydata_people = mydata_sort %>%  
  select(Date, `Food Provided for`) %>%
  filter(is.na(`Food Provided for`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(people_total = sum(`Food Provided for`)) %>%
  filter(people_total < 500)  # remove outliers

ggplot(mydata_people, aes(x=Date, y=people_total)) + 
  geom_point() + 
  geom_smooth() +
  labs(x='Date', y='Number of people', title = "The trend of served people over time")

### show the number of people served per day
mean(mydata_people$people_total)
### show the number of people served recently per day
recent_people = mydata_people %>% 
  filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
mean(recent_people$people_total)

## (3) food pounds:
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point() + 
  labs(x='Date', y='food pounds', title = "The trend of food pounds over time")

### remove outliers
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  ggplot(aes(x=Date, y=Food_total)) + 
  geom_point() + 
  labs(x='Date', y='food pounds', title = "The trend of food pounds over time")

### list the date of outliers
mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  filter(Food_total >= 2000)

### fit the points with curve
mydata_food = mydata_sort %>%  
  select(Date, `Food Pounds`) %>%
  filter(is.na(`Food Pounds`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(Food_total = sum(`Food Pounds`)) %>%
  filter(Food_total != max(Food_total)) %>%
  filter(Food_total < 2000)
ggplot(mydata_food,aes(x=Date, y=Food_total)) + 
  geom_point() +
  geom_smooth() + 
  labs(x='Date', y='food pounds', title = "The trend of food pounds over time (without outliers)")

### show the number of food pound provided per day
mean(mydata_food$Food_total)
### show the number of food pound provided recently per day
recent_food = mydata_food %>% 
  filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
mean(recent_food$Food_total)

## (4) clothing items:
mydata_cloth = mydata_sort %>%  
  select(Date, `Clothing Items`) %>%
  filter(is.na(`Clothing Items`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(cloth_total = sum(`Clothing Items`))
ggplot(mydata_cloth, aes(x=Date, y=cloth_total)) + 
  geom_point() +
  geom_smooth() + 
  labs(x='Date', y='clothes', title = "The trend of clothes over time")

### show the number of clothes items provided per day
mean(mydata_cloth$cloth_total)
### show the number of clothes itmes provided recently per day
recent_cloth = mydata_cloth %>% 
  filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
mean(recent_cloth$cloth_total)

## (5) diapers:
mydata_sort %>%  
  select(Date, Diapers) %>%
  filter(is.na(Diapers)==F) %>%
  group_by(Date) %>%
  summarise(diapers_total = sum(Diapers)) %>%
  ggplot(aes(x=Date, y=diapers_total)) + 
  geom_point()
# remove outliers
mydata_sort %>%  
  select(Date, Diapers) %>%
  filter(is.na(Diapers)==F) %>%
  group_by(Date) %>%
  summarise(diapers_total = sum(Diapers)) %>%
  filter(diapers_total != max(diapers_total)) %>%
  ggplot(aes(x=Date, y=diapers_total)) + 
  geom_point() + 
  labs(x='Date', y='diapers', title = "The trend of diapers over time")

## (6) school Kits:
mydata_sort %>%  
  select(Date, `School Kits`) %>%
  filter(is.na(`School Kits`)==F) %>%
  group_by(Date) %>%
  summarise(school_total = sum(`School Kits`)) %>%
  ggplot(aes(x=Date, y=school_total)) + 
  geom_point() + 
  labs(x='Date', y='school kits', title = "The trend of school kits over time")


## (7) Hygiene Kits:
mydata_sort %>%  
  select(Date, `Hygiene Kits`) %>%
  filter(is.na(`Hygiene Kits`)==F) %>%
  group_by(Date) %>%
  summarise(hygiene_total = sum(`Hygiene Kits`)) %>%
  ggplot(aes(x=Date, y=hygiene_total)) + 
  geom_point() + 
  labs(x='Date', y='hygiene kits', title = "The trend of hygiene kits over time")

## (8) financial support:
mydata_sort %>%  
  select(Date, `Financial Support`) %>%
  filter(is.na(`Financial Support`)==F) %>%
  group_by(Date) %>%
  summarise(finance_total = sum(`Financial Support`)) %>%
  ggplot(aes(x=Date, y=finance_total)) + 
  geom_point() + 
  labs(x='Date', y='financial support', title = "The trend of financial support over time")

### zoom in the plot of year 2000-2019 abd remove the "future dates" and plot non-zero points
mydata_sort %>%  
  select(Date, `Financial Support`) %>%
  filter(is.na(`Financial Support`)==F) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" )) %>%
  filter(Date >= as.Date("1/1/2005",format = "%m/%d/%Y" )) %>%
  group_by(Date) %>%
  summarise(finance_total = sum(`Financial Support`)) %>%
  filter(finance_total!=0) %>%
  ggplot(aes(x=Date, y=finance_total)) + 
  geom_point() + 
  labs(x='Date', y='financial support', title = "The trend of financial support over time")

# 2. Find
## (1) whom have contributed to UMD financially:
mydata_sort %>%
  arrange(`Payer of Support`) %>%
  filter(is.na(`Payer of Support`)==F) %>%
  # case insensitive
  group_by(Category = stringi::stri_trans_totitle(`Payer of Support`)) %>%
  summarise(n = n())

## (2) what type of bills people have to pay:
mydata_sort %>%
  arrange(`Type of Bill Paid`) %>%
  filter(is.na(`Type of Bill Paid`)==F) %>%
  # case insensitive
  group_by(Category = stringi::stri_trans_totitle(`Type of Bill Paid`)) %>%
  summarise(n = n())

# 3. Show the potential relationships between different variables.
## Food pounds V.S. Food Provided for:
mydata_sort %>%
  filter(`Food Provided for` > 0) %>%
  filter(`Food Pounds` > 0 ) %>%
  filter(`Food Pounds` != max(`Food Pounds`)) %>%
  ggplot(aes(x=`Food Provided for`,y=`Food Pounds`)) +
  geom_point() + 
  labs(x='Number of people', y='Food pounds', 
       title = "The relationship between the number of people and the needs of food")

### remove all the single points which were likely to be typoes. 
mydata_sort %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 200) %>%
  filter(`Food Pounds` > 0 & `Food Pounds` < 1000) %>%
  filter(`Food Pounds` != max(`Food Pounds`)) %>%
  ggplot(aes(x=`Food Provided for`,y=`Food Pounds`)) +
  geom_point() + 
  labs(x='Number of people', y='Food pounds', 
       title = "The relationship between the number of people and the needs of food")

### average pounds of food per person per visit.
averg_food = mydata_sort %>%
  select(Date,`Food Pounds`,`Food Provided for`) %>%
  filter(`Food Provided for` > 0 ) %>%
  filter(`Food Pounds` > 0 ) %>%
  filter(`Food Pounds` != max(`Food Pounds`)) %>%
  mutate(mean_food = `Food Pounds`/`Food Provided for`) %>%
  filter(mean_food < 500) %>%
  filter(Date > as.Date("1/1/2000",format = "%m/%d/%Y" )) %>%
  filter(Date < as.Date("9/11/2019",format = "%m/%d/%Y" ))
ggplot(averg_food, aes(Date,mean_food,)) +
  geom_point() + 
  geom_smooth() + 
  labs(x='Date', y='Food pounds', title = "The change of the needs of food per person per visit over time")

#### show the average food pound needed per person per visit
mean(averg_food$mean_food)
#### show the average food pound needed recently per person per visit
recent_avgfood = averg_food %>% 
  filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
mean(recent_avgfood$mean_food)

## Clothing Items V.S. Food Provided for:
mydata_sort %>%
  filter(`Clothing Items` > 0) %>%
  filter(`Food Provided for` > 0 ) %>%
  ggplot(aes(x=`Food Provided for`,y=`Clothing Items`)) +
  geom_point() + 
  labs(x='Number of people', y='Clothes', 
       title = "The relationship between the number of people and the needs of clothes")
### remove outliers
mydata_sort %>%
  filter(`Clothing Items` > 0) %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 100) %>%
  ggplot(aes(x=`Food Provided for`,y=`Clothing Items`)) +
  geom_point() + 
  labs(x='Number of people', y='Clothes', 
       title = "The relationship between the number of people and the needs of clothes")
### average items of clothing per person per visit. 
averg_clothes = mydata_sort %>%
  select(Date,`Clothing Items`,`Food Provided for`) %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 100) %>%
  filter(`Clothing Items` > 0 ) %>%
  mutate(mean_clothes = `Clothing Items`/`Food Provided for`) %>%
  filter(Date > as.Date("1/1/2000",format = "%m/%d/%Y" )) %>%
  filter(Date < as.Date("9/11/2019",format = "%m/%d/%Y" ))
ggplot(averg_clothes, aes(Date,mean_clothes,)) +
  geom_point() + 
  geom_smooth() + 
  labs(x='Date', y='Clothes', 
       title = "The change of the needs of clothes per person per visit over time")

#### show the average clothes items needed per person per visit
mean(averg_clothes$mean_clothes)
#### show the average clothes items needed recently per person per visit
recent_avgclothes = averg_clothes %>% 
  filter(Date >= as.Date("1/1/2015",format = "%m/%d/%Y" ))
mean(recent_avgclothes$mean_clothes)

# 4. People (family) coming most frequently and examples of some specific families.
## who came most often.
mydata_sort %>%
  group_by(`Client File Number`) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
## take client 3502 and 805 as examples. 
client3502 = mydata_sort %>%
  filter(`Client File Number`==3502) %>%
  arrange(Date) 
ggplot(client3502, aes(x=Date,y=1)) + 
  geom_point() + 
  labs(x='Date', y='Visit or not', title = "Time of client 3502 came for help")

client3502_interval = client3502$Date[-1] - client3502$Date[-length(client3502$Date)]
plot(x=1:length(client3502_interval), y=client3502_interval, 
     xlab='Date', ylab='Visit or not', main="Intervals of client 3502 came for help")  
# remove the first point which was an outlier
plot(x=1:(length(client3502_interval)-1), y=client3502_interval[-1],
     xlab='Date', ylab='Visit or not', main="Intervals of client 3502 came for help")

mean(client3502_interval[-1])


client805 = mydata_sort %>%
  filter(`Client File Number`==805) %>%
  filter(Date >= as.Date("1/1/2000",format = "%m/%d/%Y" )) %>%
  arrange(Date)
ggplot(client805, aes(x=Date,y=1)) + 
  geom_point() + 
  labs(x='Date', y='Visit or not', title = "Time of client 805 came for help")

client805_interval = client805$Date[-1] - client805$Date[-length(client805$Date)]
plot(x=1:(length(client805_interval)), y=client805_interval, 
     xlab='Date', ylab='Interval days', main="Intervals of client 805 came for help")

mean(client805_interval)

## who came most recently.
mydata_sort %>%
  arrange(desc(Date)) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" ))
## take the most recent one which is client 814 for an example.
client814 = mydata_sort %>%
  filter(`Client File Number`==814) %>%
  arrange(Date)
ggplot(client814, aes(x=Date,y=1)) + 
  geom_point() + 
  labs(x='Date', y='Visit or not', title = "Time of client 814 came for help")

client814_interval = client814$Date[-1] - client814$Date[-length(client814$Date)]
plot(x=1:(length(client814_interval)), y=client814_interval, 
     xlab='Date', ylab='Interval days', main="Intervals of client 814 came for help")




