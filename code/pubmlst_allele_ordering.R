library(tidyverse)

camp <- read.table("raw/BIGSdb_250368_0481013786_82844.txt", sep="\t", header=TRUE, na="")

alleles <- camp |> filter(!is.na(CAMP0864)) |> select(id, source, CAMP0864) |>
  group_by(CAMP0864) |> count(source) |> ungroup()

alleles |> mutate(case_when(source ))
