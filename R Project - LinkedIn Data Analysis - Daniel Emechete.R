# Load tidyverse
library(tidyverse) 

# Import data
postings = read_csv("C:/Users/Admin/Desktop/BDEEM/Sem1/Data Analysis - R/Data Analysis Project/LinkedIn Data Engineering Dataset/postings.csv")

#Rename variables
postings <- postings %>%
 rename(job_url = job_link, 
       job_level = `job level`)

#Bar plot showing the most common job titles
postings %>%
  group_by(job_title)%>%
  summarise(counts=n())%>%
  filter(counts>100) %>%
  ggplot(aes(x = reorder(job_title, counts),
             y=counts, fill = job_title)) +
  geom_bar(stat='identity') +
  labs(title = "Top Data Engineering Jobs",
       x = "Job Titles", y = "Number of Postings") +
  scale_x_discrete(guide = guide_axis(angle = 45))  
  #rotating the x-axis label
 

#Horizontal bar plots
#Showing countries with the most data engineering job postings
postings %>%
  group_by(search_country)%>%
  summarise(counts=n())%>%
  ggplot(aes(x = reorder(search_country, counts), y = counts)) +
  geom_bar(stat='identity', fill = "turquoise") +
  labs(title = "Geographical Distribution of Data Engineering Job Opportunities",
       x = "Countries", y = "Number of Postings") +
  coord_flip() 

# Explode data in the variable "job_skills"
skills <- postings %>%
  separate_rows(job_skills)

# Horizontal bar plots 
# Top skills required for data engineering jobs
skills %>%
  group_by(job_skills) %>%
  summarise(counts=n()) %>%
  filter(counts>1500) %>%
  ggplot(aes(x = reorder(job_skills, counts), y = counts)) +
  geom_bar(stat='identity', fill = "orange") +
  labs(title = "Most In-Demand Skills for Data Engineering Positions",
       x = "Skills", y = "Number of Times Listed") +
  coord_flip()
