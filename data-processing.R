library(tidyverse); library(janitor)
df <- read_csv("complete-data.csv") |> janitor::clean_names()

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

write_rds(df, "wide-data.rds")
write_rds(df_processed, "df_proc.rds")
