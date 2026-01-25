install.packages('tidyverse')
library(tidyverse)
install.packages('readr')
library(readr)
install.packages('skimr')
library(skimr)
install.packages('dplyr')
library(dplyr)

imdb_data <- read_csv("Cleaned Data for imbd Top 1000 movies - imdb_dataset.csv")

imdb_clean <- imdb_data %>% 
  mutate(
    Runtime = as.numeric(str_remove(Runtime, " min")),
    
    Year_Release= as.numeric(Year_Release)
  ) %>% 
  drop_na(Gross)

str(imdb_clean)
glimpse(imdb_clean)
skim_without_charts(imdb_clean)


glimpse(imdb_data)
str(imdb_data)
class(imdb_data)
colnames(imdb_data)
skim_without_charts(imdb_data)



hist(imdb_clean$Gross)
hist(imdb_clean$IMDB_Rating)

imdb_clean %>% 
  arrange(desc(Gross)) %>% 
  select(Series_Title,Year_Release, IMDB_Rating, Gross) %>% 
  head(10) 

imdb_clean %>% 
  filter(Gross == max(Gross, na.rm=TRUE)) %>% 
  select(Series_Title ,Year_Release, IMDB_Rating, Gross)

cor(imdb_clean$IMDB_Rating, imdb_clean$Gross, use='complete.obs')


hist(imdb_clean$IMDB_Rating, 
     col='lightblue', 
     xlab='IMDB Ratings',
     border='black',
     main="IMDB Ratings Distribution"
     )

range(imdb_clean$IMDB_Rating)

summary(imdb_clean$IMDB_Rating)

imdb_clean %>% 
  arrange(IMDB_Rating, Gross) %>% 
  select(Series_Title, Year_Release, IMDB_Rating, Gross) %>% 
  head(10)

revenue_lane <- case_when(
  imdb_clean$Gross >250000000 ~ "More than 250M $",
  imdb_clean$Gross >100000000 ~ "More than 100M $ Club",
  imdb_clean$Gross > 50000000 ~ "Grossed over 50M$ ; great!",
  imdb_clean$Gross > 10000000 ~ "Grossed over 10 mil",
  imdb_clean$Gross > 1000000 ~ "Crossed over a milli (1M+ dollars)  ",
  TRUE ~ "Less than a milli ; still good needs work!"
)


imdb_rating_c <- case_when(
  imdb_clean$IMDB_Rating > 9.5 ~ " >9.5 -Goated",
  imdb_clean$IMDB_Rating > 9.0 ~ " >9.0 - Legendary Movies",
  imdb_clean$IMDB_Rating >8.5 ~ " >8.5 - Classics",
  imdb_clean$IMDB_Rating >8.0 ~ " >8.0 - Clean",
  TRUE ~ "<8.0 Rating - still need work"
  
)

imdb_clean %>% 
  summarize(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to='column names', values_to="missing_values_count") %>% 
  mutate(
    total_rows = nrow(imdb_data),
    missing_values_count_perc= round((missing_values_count/total_rows * 100), 2)) %>% 
  arrange(desc(missing_values_count))

skim_without_charts(imdb_clean)





# Most frequent stars
star_analysis <- imdb_clean %>%
  pivot_longer(cols = c(Star1, Star2, Star3, Star4), 
               names_to = "star_position", 
               values_to = "star_name") %>%
  group_by(star_name) %>%
  summarise(
    movie_count = n(),
    avg_rating = mean(IMDB_Rating, na.rm = TRUE),
    avg_revenue = mean(Gross, na.rm = TRUE),
    total_revenue = sum(Gross, na.rm = TRUE),
    avg_votes = mean(No_of_Votes, na.rm = TRUE)
  ) %>%
  arrange(desc(total_revenue))

# Top 20 stars by total revenue
star_analysis %>% head(20) %>% 
  ggplot(aes(x = reorder(star_name, total_revenue), y = total_revenue/1e9)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Stars by Total Box Office Revenue",
       x = "Actor/Actress",
       y = "Total Revenue (Billions $)") +
  theme_minimal()

# Star power vs revenue scatter
star_analysis %>%
  filter(movie_count >= 5) %>%  # Only stars with at least 5 movies
  ggplot(aes(x = movie_count, y = total_revenue/1e6)) +
  geom_point(aes(size = avg_rating, color = avg_rating), alpha = 0.7) +
  geom_smooth(method = "glm", se = FALSE, color = "red") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Star Popularity vs Revenue Generation",
       x = "Number of Top 1000 Movies",
       y = "Total Revenue (Millions $)",
       color = "Avg Rating",
       size = "Avg Rating") +
  theme_minimal()

star_analysis %>% 
  head(10) %>% 
  arrange(desc(total_revenue))






star_analysis_3 <- imdb_clean %>% 
      pivot_longer(cols= c(Star1, Star2, Star3, Star4),
                   names_to="Star_pos",
                   values_to="Actor_n_Actresses"
                   
                   ) %>% 
      group_by(Actor_n_Actresses) %>% 
    summarize(
      movie_count=n(),
      avg_revenues= mean(Gross, na.rm=TRUE), 
      total_revenues= sum(Gross, na.rm=TRUE), 
      avg_votes=mean(No_of_Votes, na.rm=TRUE), 
      avg_rating= mean(IMDB_Rating, na.rm=TRUE)
  
    ) %>% 
  arrange(desc(total_revenues))

View(star_analysis_3)


star_analysis_3 %>% head(20) %>% 
  ggplot(aes(x=reorder(Actor_n_Actresses, total_revenues), y= total_revenues/1e9))+
  geom_col(fill='steelblue')+
  coord_flip()+
  labs(title="Stars with Highest Movie Revenues ( Top 1000 movies - 1921-2019 )",
       x= "Actors and Actresses",
       y="Revenues in Billions ($B)"
       ) +
  theme_minimal()


star_analysis_3 %>%  
  filter(movie_count>=5) %>% 
  ggplot(aes(x= movie_count, y=total_revenues/1e6))+
  geom_point(aes(color=avg_rating, size=avg_rating), alpha=0.7)+
  scale_color_gradient(low="blue", high='red')+
  geom_smooth(method='lm', se=FALSE, color='red')+
  labs(title="Star Popularity vs Total Revenues Generated", 
       x= ' No of appearances in Top 1000 movies (All-time)',
       y= 'Total Revenues (Millions $) ',
       color="Avg_rating",
       size="Avg_rating"
       
       )+
  theme_minimal()









  
  

View(star_analysis_3)


























      
 