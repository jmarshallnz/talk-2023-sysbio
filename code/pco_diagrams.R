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
as.data.frame(iris.nmin) |>
  mutate(x1 = X1*cos(pi/3) - X2*sin(pi/3),
         x2 = X1*sin(pi/3) + X1*cos(pi/3)) |>
  ggplot() +
  geom_point(aes(x=x1, y=x2)) +
  coord_equal()

