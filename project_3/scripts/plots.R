library(tidyverse)
library(ggplot2)
# Read data
client_final = read_tsv("../data/clients_final.tsv")

# Summary statistics:
## Density for client age at entry
ggplot(client_final, mapping = aes(`Client Age at Entry`)) + 
  geom_density() + 
  labs(title = "Density for client age at entry")
ggsave('../results/entry_age_density.png', height=4, width=6)

## Density for client spent days in shelter
ggplot(client_final, mapping = aes(`Time Spent (days)`)) + 
  geom_density() + 
  labs(title = "Density for days client spent in shelter")
ggsave('../results/days_spent_density.png', height=4, width=6)
### Zoom in <250 days:
ggplot(client_final %>% filter(`Time Spent (days)`<250), mapping = aes(`Time Spent (days)`)) + 
  geom_density() + 
  labs(title = "Density for days client spent in shelter (zoomed in)")
ggsave('../results/days_spent_density_zoomin.png', height=4, width=6)


## Gender of clients came to shelter
ggplot(client_final %>% select(`Client Gender`) %>% drop_na(), mapping = aes(`Client Gender`)) + 
  geom_bar() + 
  labs(title = "Barplot for clients gender")
ggsave('../results/gender_barplot.png', height=4, width=6)

## Veteran status of clients
ggplot(client_final %>% select(`Client Veteran Status`) %>% drop_na(), mapping = aes(`Client Veteran Status`)) + 
  geom_bar() + 
  labs(title = "Barplot for clients veteran status")
ggsave('../results/veteran_barplot.png', height=4, width=6)


# Relationship between Time spent (days) and:
## age and gender:
ggplot(client_final %>% filter(`Time Spent (days)`<750),  # remove one outlier 
       mapping = aes(x = `Client Age at Entry`, y = `Time Spent (days)`, 
                        group = `Client Gender`, color = `Client Gender`, legend = `Client Gender`  )) + 
  geom_violin() + 
  theme(legend.text = element_text(size = 8), legend.position = 'bottom') + 
  labs(title = "Violin plot for staying days and age/gender")
ggsave('../results/rela_age_gender.png', height=4, width=6)

## income at entry:
ggplot(client_final %>% filter(`Monthly Amount (Entry)` < 20000),
       mapping = aes(x = `Monthly Amount (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Point plot for staying days and income at entry")
ggsave('../results/rela_income.png', height=4, width=6)

## numbers of insurances at entry:
ggplot(client_final %>% filter(`Insurance (Entry)` < 30),
       mapping = aes(x = `Insurance (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Scatter plot for staying days and insurance at entry")
ggsave('../results/rela_insurance.png', height=4, width=6)

## non-cash services at entry:
ggplot(client_final,
       mapping = aes(x = `Noncash (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Scatter plot for staying days and noncash services at entry")
ggsave('../results/rela_noncash.png', height=4, width=6)



# Any changes at exit comparing at the entry
client_diff = read_tsv("../data/clients_diff.tsv")
ggplot(client_diff %>% drop_na(Score),
       mapping = aes(Score )) + 
  geom_density() + 
  labs(title = "Density for Score")
ggsave('../results/score.png', height=4, width=6)


# Generate HTML 
rmarkdown::render("../results/proj3_report.Rmd", "html_document")






