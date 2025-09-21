library(tidyverse); library(janitor)
df <- read_csv("most-of-the-data.csv") |> janitor::clean_names()


# perform transformation and field engineering.
df_processed <- df |> 
  pivot_longer(cols=c(everything(), -respondent), 
               names_to="category", values_to="response") |> 
  mutate(
    question = case_when(
                str_detect(category,"_1") ~ "1",
                str_detect(category,"_2") ~ "2",
                str_detect(category,"_3") ~ "3"),
    category = str_replace(category, "_\\d", ""),
    brand = case_when(
                str_detect(category, "target") ~ "Target",
                str_detect(category, "whole_foods") ~ "Whole Foods",
                str_detect(category, "costco") ~ "Costco",
                str_detect(category, "trader_joes") ~ "Trader Joe's",
    ),
    category = case_when(
                str_detect(category,"price") ~ "Price",
                str_detect(category,"quality") ~ "Quality",
                str_detect(category,"variety") ~ "Variety",
                .default=NA
    )
  ) |> 
  select(respondent,question,brand,category,response) |> 
  mutate(
    question = as_factor(question),
    brand = as_factor(brand)
  )


# question one 
df_processed |>
  filter(question == 1) |> 
  group_by(brand) |> 
  summarise(
    avg = mean(response, na.rm=TRUE),
    stderr = sd(response),
    min = min(response),
    max = max(response)
  )

df_processed |>
  filter(question == 1 & !is.na(response)) |> 
  ggplot(aes(x=brand, y=response, colour=brand)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, height=0.1, width=0.4) +
  scale_colour_brewer(palette="Dark2") +
  labs(
    y="Response",
    x="Brand",
    colour="Brand",
    title="Comparison of brand rankings",
    caption="Data points have been jittered both vertically and 
             horizontally to prevent overplotting."
  )

# basic heat map disply for question 2
df_processed |>
  filter(question == 2) |> 
  group_by(brand, category) |> 
  summarise(
    avg = mean(response),
    stderr = sd(response),
    min = min(response),
    max = max(response)
  ) |> ggplot(aes(brand, category, fill= avg)) + 
  geom_tile(colour="white") +
  scale_fill_viridis_b(direction=-1)


df_processed |>
  filter(question == 2) |> 
  group_by(brand, category) |> 
  summarise(
    avg = mean(response),
    stderr = sd(response),
    min = min(response),
    max = max(response)
  )

df_processed |>
  filter(question == 2) |> 
  group_by(brand, category, response) |> 
  summarise(
    count = n()
  ) |> table()

