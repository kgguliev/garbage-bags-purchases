# Load libraries ----

library(tidyverse)
library(data.table)

# Load and join data ----

dem_data <- fread("./data/demography_data.csv")

purchase <- fread("./data/purchase_data.tsv")

joined_data <- purchase %>% 
  inner_join(dem_data, by = "hhkey") %>% 
  mutate(movedate = as.character(movedate))

# 1. Share of repeated purchases in the category ----
# Group by date to find out the number of buyers in a current period of time
# Also group by the buyer's ID and calculate the number of a distinct operation

repeated_purchases <- joined_data %>% 
  group_by(movedate, hhkey) %>% 
  summarise(repeated = n_distinct(occaskey)) %>% 
  arrange(desc(repeated)) %>% 
  ungroup() %>% 
  mutate(share = round((repeated / sum(repeated)) * 100, 2))

head(repeated_purchases, 5)

# 2. Divide buyers by tertiles into 3 groups - heavy, medium, light ----
# Compute tertiles among repeated buyers; find out the share of each group among the repeated buyers

grouped <- repeated_purchases %>% 
  filter(repeated >= 2) %>% 
  select(hhkey) %>%
  distinct() %>% 
  inner_join(joined_data, by = "hhkey") %>% 
  select(hhkey, occaskey, value, number) %>% 
  group_by(hhkey) %>% 
  summarise(cumulative_spending = sum(value)) %>% 
  ungroup()
  # mutate(tertile = ntile(cumulative_spending, 3),
  #        group = ifelse(tertile == 3, "heavy", ifelse(tertile == 2, "medium", "light")))

tertiles <- quantile(grouped$cumulative_spending, c(0:3 / 3))

grouped$group <- with(grouped,
                       cut(cumulative_spending,
                       tertiles,
                       include.lowest = T,
                       lables = c("heavy", "medium", "light")))

levels(grouped$group) <- c("light", "medium", "heavy")
