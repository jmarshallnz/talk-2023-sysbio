library(tidyverse)
library(vegan)
library(ecodist)

load("~/data/projects/wgs_attribution/dist_ranger/data/list_of_distance_matrices_j.RData")

d <- list_of_distance_matrices_j[["CAMP0001"]]

#mds <- metaMDS(d, k=2)

out <- nmds(as.dist(d))

plot(out)

iris.nmin <- min(out, dims=2)

plot(iris.nmin)

# Rotate this a bit for fun?!?
as.data.frame(iris.nmin) |>
  ggplot() +
  geom_point(aes(x=X1, y=X2)) +
  coord_equal()

# Hmm, this isn't right?!?
set.seed(3)
as.data.frame(iris.nmin) |>
  tibble::rowid_to_column() |>
  filter(X1 < 25) |>
  slice_sample(n=10) |>
  mutate(x1 = X1*cos(pi/4) - X2*sin(pi/4),
         x2 = X1*sin(pi/4) + X1*cos(pi/4)) |>
  select(rowid, x1, x2) |>
  left_join(data.frame(rowid=1:53, allele=rownames(d))) |>
  select(allele, x1, x2) |>
  write_csv("data/forest/pco_coords.csv")

dat <- read_csv("data/forest/pco_coords.csv")

d |> as.data.frame() |> rownames_to_column("allele") |> pivot_longer(-allele, names_to="allele2", values_to="distance") |>
  semi_join(dat) |>
  semi_join(dat, by = c('allele2' = 'allele')) |>
  write_csv("data/forest/pco_dist.csv")

edge_dat <- dat |> cross_join(dat |> select(allele2=allele, y1=x1,y2=x2)) |>
  filter(x1 < y1) |>
  left_join(read_csv("data/forest/pco_dist.csv"))

ggplot(dat) +
  geom_point(aes(x=x1, y=x2)) +
  geom_text(aes(x=x1, y=x2, label=allele), nudge_x=1, nudge_y=1) +
  geom_segment(data=edge_dat, aes(x=x1, y=x2, xend=y1, yend=y2), alpha=0.1) +
  coord_equal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggplot(dat) +
  geom_point(aes(x=x1, y=x2)) +
  geom_segment(data=edge_dat, aes(x=x1, y=x2, xend=y1, yend=y2), alpha=0.1) +
  coord_equal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


