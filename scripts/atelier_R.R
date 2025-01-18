library(tidyverse)
library(ratdat)

### exploration de donnees
?complete_old
summary(complete_old)
head(complete_old)
str(complete_old)


### C'est le temps d'utiliser ggplot !
library(ggplot2)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

complete_old <- filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.1)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = year)) +
  geom_point(alpha = 0.1)


ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.1) +
  scale_color_viridis_d() +
  scale_x_log10()


ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) 


ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_violin(fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) 



plot_final <- ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  facet_wrap(vars(sex), ncol = 1) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Plot type", y = "Hindfoot length (mm)")

ggsave(filename = "figures/plot_final.png", 
       plot = plot_final, 
       height = 6, 
       width = 8)




###### tidyverse

surveys <- read_csv("data/raw/surveys_complete_77_89.csv")
View(surveys)

str(surveys)


#### select
select(surveys, plot_id, species_id)

select(surveys, c(3, 4))

select(surveys, -plot_id)

select(surveys, where(is.numeric))

select(surveys, where(anyNA))



#### filter 

filter(surveys, year == 1988)

filter(surveys, species_id %in% c("RM", "DO"))

filter(surveys, year == 1988 & species_id %in% c("RM", "DO"))


### select AND filter

#### 1ere façon

surveys_80_85 <- filter(surveys, year >= 1980 & year <= 1985)
select(surveys_80_85, year, month, species_id, plot_id)


select(filter(surveys, year >= 1980 & year <= 1985), year, month, species_id, plot_id)


surveys %>%
  filter(year == 1980:1985) %>% 
  select(year, month, species_id, plot_id)
  

surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)



surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000,
         weight_lbs = weight_kg * 2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg) 

library(lubridate)

surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  relocate(date, .after = year)

surveys %>% 
  group_by(sex, year) %>% 
  summarize(mean.weight = mean(weight, na.rm = TRUE),
            count = n())



