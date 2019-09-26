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

# 2. People (family) coming most frequently/recently and some specific families.

# Who came most often.
mydata_sort %>%
  group_by(`Client File Number`) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

client3502 = mydata_sort %>%
  filter(`Client File Number`==3502) %>%
  arrange(Date) 
ggplot(client3502, aes(x=Date,y=1)) + 
  geom_point()

client3502_interval = client3502$Date[-1] - client3502$Date[-length(client3502$Date)]
plot(x=1:length(client3502_interval), y=client3502_interval)
# plot(x=1:(length(client3502_interval)-1), y=client3502_interval[-1])
# remove the first point which was an outlier

client805 = mydata_sort %>%
  filter(`Client File Number`==805) %>%
  filter(Date >= as.Date("1/1/2000",format = "%m/%d/%Y" )) %>%
  arrange(Date)
ggplot(client805, aes(x=Date,y=1)) + 
  geom_point()

client805_interval = client805$Date[-1] - client805$Date[-length(client805$Date)]
plot(x=1:(length(client805_interval)), y=client805_interval)

# Who came most recently.
mydata_sort %>%
  arrange(desc(Date)) %>%
  filter(Date <= as.Date("09/11/2019",format = "%m/%d/%Y" ))

client814 = mydata_sort %>%
  filter(`Client File Number`==814) %>%
  arrange(Date)
ggplot(client814, aes(x=Date,y=1)) + 
  geom_point()

client814_interval = client814$Date[-1] - client814$Date[-length(client814$Date)]
plot(x=1:(length(client814_interval)), y=client814_interval)


## 3. Find
### (1) whom have contributed to UMD financially:

mydata_sort %>%
  arrange(`Payer of Support`) %>%
  filter(is.na(`Payer of Support`)==F) %>%
  group_by(`Payer of Support`) %>%
  summarise(n = n())


### (2) what type of bills people have to pay: 

mydata_sort %>%
  arrange(`Type of Bill Paid`) %>%
  filter(is.na(`Type of Bill Paid`)==F) %>%
  # case insensitive
  group_by(Category = stringi::stri_trans_totitle(`Type of Bill Paid`)) %>%
  summarise(n = n())

## 4. Show how the number of people that food provided for affects the needs of other help (If it does).

### Food pounds V.S. Food Provided for:
mydata_sort %>%
  filter(`Food Provided for` > 0) %>%
  filter(`Food Pounds` > 0 ) %>%
  filter(`Food Pounds` != max(`Food Pounds`)) %>%
  ggplot(aes(x=`Food Provided for`,y=`Food Pounds`)) +
  geom_point()

# remove the outliers
mydata_sort %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 200) %>%
  filter(`Food Pounds` > 0 & `Food Pounds` < 1000) %>%
  filter(`Food Pounds` != max(`Food Pounds`)) %>%
  ggplot(aes(x=`Food Provided for`,y=`Food Pounds`)) +
  geom_point() 

## Average pounds of food per person per visit. 
# The average pounds of food that one person needs at a time.
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
  geom_smooth()

mean(averg_food$mean_food)


### Clothing Items V.S. Food Provided for:
mydata_sort %>%
  filter(`Clothing Items` > 0) %>%
  filter(`Food Provided for` > 0 ) %>%
  ggplot(aes(x=`Food Provided for`,y=`Clothing Items`)) +
  geom_point()


mydata_sort %>%
  filter(`Clothing Items` > 0) %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 100) %>%
  ggplot(aes(x=`Food Provided for`,y=`Clothing Items`)) +
  geom_point()


## Average items of clothing per person per visit. 
averg_clothes = mydata_sort %>%
  select(Date,`Clothing Items`,`Food Provided for`) %>%
  filter(`Food Provided for` > 0 & `Food Provided for` < 100) %>%
  filter(`Clothing Items` > 0 ) %>%
  mutate(mean_clothes = `Clothing Items`/`Food Provided for`) %>%
  filter(Date > as.Date("1/1/2000",format = "%m/%d/%Y" )) %>%
  filter(Date < as.Date("9/11/2019",format = "%m/%d/%Y" ))

ggplot(averg_clothes, aes(Date,mean_clothes,)) +
  geom_point() + 
  geom_smooth()

mean(averg_clothes$mean_clothes)
















  
  