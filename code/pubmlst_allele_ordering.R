library(tidyverse)

camp <- read.table("raw/BIGSdb_250368_0481013786_82844.txt", sep="\t", header=TRUE, na="")

alleles <- camp |> filter(!is.na(CAMP0864)) |> select(id, source, CAMP0864) |>
  group_by(CAMP0864) |> count(source) |> ungroup()

alleles |> mutate(source = case_when(str_detect(source, "chick") ~ "Chicken",
                            str_detect(source, "beef") ~ "Cattle",
                            str_detect(source, "cattle") ~ "Cattle",
                            str_detect(source, "cow") ~ "Cattle",
                            str_detect(source, "lamb") ~ "Sheep",
                            str_detect(source, "sheep") ~ "Sheep",
                            str_detect(source, "human") ~ "Human",
                            str_detect(source, "water") ~ "Water",
                            TRUE ~ "Other")) |>
  group_by(CAMP0864, source) |>
  summarise(count = sum(n)) |>
  ungroup() |>
  mutate(Allele = as.numeric(CAMP0864)) |>
  group_by(Allele) |>
  filter(source != "Human") |>
  mutate(NumSource = n_distinct(source)) |>
  summarise(Source = if_else(first(NumSource) == 1, first(source), "Multiple")) |>
  filter(!Source %in% c("Other", "Multiple")) |>
  write_csv("data/camp0864.csv")
