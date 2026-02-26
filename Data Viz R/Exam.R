# Using R
# Option 1: tidytuesdayR R package 
install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-04-01')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon_df <- tuesdata$pokemon_df

# Option 2: Read directly from GitHub

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

summary(pokemon_df)
pokemon_df
library(skimr)
library(tidyverse)
skim(pokemon_df)

rock_weight= pokemon_df |> 
  filter(type_1=="rock") |> 
  select(weight)


type_spec_attributes= pokemon_df |> 
  summarise(total_spec_att=sum(special_attack),
total_spec_def= sum(special_defense),
.by = type_1) |> 
  arrange(desc(total_spec_att))

type_spec_attributes

type_attributes= pokemon_df |> 
  summarise(total_att=sum(attack),
total_def= sum(defense),
.by = type_1) |> 
  arrange(desc(total_att))

type_attributes
# pokemon_df
library(ggplot2)
ggplot(type_spec_attributes, mapping=aes(x= total_spec_def, y= total_spec_att))+
  geom_abline(stat = type_attributes)

summary(pokemon_df)

ggplot(pokemon_df, mapping=aes(x= speed, y= special_attack))+
  geom_point(aes(col= type_1))
