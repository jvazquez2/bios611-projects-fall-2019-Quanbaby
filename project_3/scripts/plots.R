library(tidyverse)
library(ggplot2)
# Read data
client_final = read_tsv("/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/data/clients_final.tsv")

# Summary statistics:
## Density for client age at entry
ggplot(client_final, mapping = aes(`Client Age at Entry`)) + 
  geom_density() + 
  labs(title = "Density for client age at entry")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/entry_age_density.png', height=4, width=6)

## Density for client spent days in shelter
ggplot(client_final, mapping = aes(`Time Spent (days)`)) + 
  geom_density() + 
  labs(title = "Density for days client spent in shelter")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/days_spent_density.png', height=4, width=6)

## Gender of clients came to shelter
ggplot(client_final %>% select(`Client Gender`) %>% drop_na(), mapping = aes(`Client Gender`)) + 
  geom_bar() + 
  labs(title = "Barplot for clients gender")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/gender_barplot.png', height=4, width=6)

## Veteran status of clients
ggplot(client_final %>% select(`Client Veteran Status`) %>% drop_na(), mapping = aes(`Client Veteran Status`)) + 
  geom_bar() + 
  labs(title = "Barplot for clients veteran status")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/veteran_barplot.png', height=4, width=6)


# Relationship between Time spent (days) and:
## age and gender:
ggplot(client_final %>% filter(`Time Spent (days)`<750),  # remove one outlier 
       mapping = aes(x = `Client Age at Entry`, y = `Time Spent (days)`, 
                        group = `Client Gender`, color = `Client Gender`, legend = `Client Gender`  )) + 
  geom_violin() + 
  theme(legend.text = element_text(size = 8), legend.position = 'bottom') + 
  labs(title = "Violin plot for staying days and age/gender")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/rela_age_gender.png', height=4, width=6)

## income at entry:
ggplot(client_final %>% filter(`Monthly Amount (Entry)` < 20000),
       mapping = aes(x = `Monthly Amount (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Point plot for staying days and income at entry")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/rela_income.png', height=4, width=6)

## numbers of insurances at entry:
ggplot(client_final %>% filter(`Insurance (Entry)` < 30),
       mapping = aes(x = `Insurance (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Point plot for staying days and insurance at entry")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/rela_insurance.png', height=4, width=6)

## non-cash services at entry:
ggplot(client_final,
       mapping = aes(x = `Noncash (Entry)`, y = `Time Spent (days)`)) + 
  geom_point() + 
  labs(title = "Point plot for staying days and noncash services at entry")
ggsave('/Users/quansun/Documents/GitHub/bios611-projects-fall-2019-Quanbaby/project_3/results/rela_noncash.png', height=4, width=6)


# Any trend about people came to/left from the shelter


# Any changes at exit comparing at the entry










