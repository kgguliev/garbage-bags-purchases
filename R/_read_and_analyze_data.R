# Load libraries ----

library(tidyverse)
library(data.table)

# Load and join data ----

dem_data <- fread("./data/demography_data.csv")

purchase <- fread("./data/purchase_data.tsv")

joined_data <- purchase %>% 
  inner_join(dem_data, by = "hhkey")

# 1. Share of repeated purchases in the category ----
# Group by date to find out the number of buyers in a current period of time
# Also group by the buyer's cart ID which represents the purchase act

repeated_purchases <- joined_data %>% 
  group_by(movedate, hhkey) %>% 
  summarise(repeated = n_distinct(occaskey),
            share = repeated / sum(repeated)) %>% 
  arrange(desc(repeated)) %>% 
  ungroup()
