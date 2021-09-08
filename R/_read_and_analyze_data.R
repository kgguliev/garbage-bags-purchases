# Load libraries ----

library(tidyverse) # a basic library for manipulation and analysis
library(data.table) # just for reading files, best speed benchmarks
library(lubridate) # to manage time
library(zoo) # the same puprose

# Load and join data ----

dem_data <- fread("./data/demography_data.csv")

purchase <- fread("./data/purchase_data.tsv")

joined_data <- purchase %>% 
  inner_join(dem_data, by = "hhkey") %>% 
  mutate(movedate = as.yearmon(as.Date(ymd(movedate))))

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
# Division should correspond with cumulative spending 
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

arranged_grouped <- grouped %>%
  arrange(desc(cumulative_spending))

final_grouped <- grouped %>%
  group_by(group) %>% 
  summarise(n_per_group = n_distinct(hhkey)) %>% 
  ungroup() %>% 
  mutate(share_per_group = round((n_per_group / sum(n_per_group)) * 100, 2))

head(final_grouped)

# 3. Average cart volume by federal subjects for each of the group ----

cart_volume <- arranged_grouped %>% 
  inner_join(joined_data, by = "hhkey") %>% 
  select(hhkey, occaskey, value, number, federatio) %>% 
  group_by(federatio) %>% 
  summarise(mean_cart_volume = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(mean_cart_volume))

head(cart_volume, 9)

# 4. Plotting general and variate monthly spending ----
# Plotting monthly spending by regions 

plot_data <- joined_data %>% 
  select(hhkey, occaskey, movedate, value, channel, federatio) %>% 
  arrange(desc(movedate)) %>% 
  mutate(value = log(value))

ggplot(plot_data, aes(x = movedate, y = value, fill = federatio)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Central" = "green",
      "Far East" = "blue4",
      "Moscow" = "red",
      "North-West" = "deepskyblue",
      "Privolzhie" = "forestgreen",
      "Siberia" = "navy",
      "South" = "maroon1",
      "St.Petersburg" = "yellow",
      "Ural" = "orange"
    ),
    labels = c(
      "Central",
      "Far East",
      "Moscow",
      "North-West",
      "Privolzhie",
      "Siberia",
      "South",
      "St.Petersburg",
      "Ural"
    )
  ) +
  ggtitle(
    label = "Total spending by federal subjects",
    subtitle = "Dec 2014 - Nov 2015, Value logged"
  ) +
  ylab("Spending, rub") +
  xlab("Date")


ggplot(plot_data, aes(x = movedate, y = value)) +
  geom_col(fill = "powderblue") +
  scale_y_log10() +
  theme_minimal() +
  coord_flip() +
  ggtitle(
    label = "Total spending by channels",
    subtitle = "Dec 2014 - Nov 2015"
  ) +
  ylab("Spending, rub") +
  xlab("Date") +
  facet_wrap(~ channel, ncol = 1)

# 5. variant variable grouping ----

grouping_Variant <- joined_data %>%
  select(hhkey, occaskey, value, number, variant, federatio) %>%
  group_by(variant) %>%
  summarise(MEAN = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(MEAN)) %>%
  mutate(category = ntile(MEAN, 5),
         category = ifelse(category == 5, "MOST POPULAR", ifelse(category == 4, "POPULAR", 
                                                                 ifelse(category == 3, "INDIFFIRENT", 
                                                                        ifelse(category == 2, "LITTLE INTEREST", "NOT POPULAR AT ALL"))))) %>% 
select(variant, category)


plot_data2 <- grouping_Variant %>% 
  inner_join(joined_data, by = "variant") %>% 
  select(hhkey, occaskey, value, variant, category) %>% 
  mutate(value = log(value))

ggplot(plot_data2, aes(value)) +
  geom_density(fill = "navy", alpha = 0.4) +
  facet_wrap(~ category, nrow = 2) +
  theme_bw() +
  ggtitle(label = "Value density among variant's categories",
          subtitle = "Dec 2014 - Nov 2015, Value logged") +
  xlab("Value, rub") +
  ylab("")

