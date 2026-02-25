library("tidyverse")
N=440
wholesale=read_csv("Data Viz R/Wholesale_customers_data.csv")#it creates a tibble, not a df. Tibble is easier
wholesale 

wholesale[,1]
library("skim")
skim("Desktop/Master_DataScience/Data Viz R/Wholesale_customers_data.csv")
summary(wholesale)
wholesale|>
  filter(Channel=="Retail") #select columns, usign a filter

wholesale|>
  filter(Channel=="Retail",Fresh>1000) #both condition are true

wholesale|>
  filter(Channel=="Retail"|Fresh>1000) #at least one condition must be true

wholesale|>
  select(Channel,Region, Grocery)#select columns

wholesale|>
  arrange(desc(Grocery)) #to sort in crescent or decrescent order

wholesale|>
  mutate(total_spend= Frozen+Grocery )#adding or transform columns

wholesale|>
  summarise(avg_fresh=mean(Frozen),
.by=Channel)#to compute summary statistics


####################EXERCISE #########
total_spend= wholesale|>
  mutate(total_spend=Fresh + Milk + Frozen+ Grocery+ Delicassen+Detergents_Paper) |> 
  mutate(share_grocery=Grocery/total_spend)

wholesale |> 
  filter(share_grocery>0.3)

wholesale |> 
  mutate(media=mean(Fresh), .by=Channel)

