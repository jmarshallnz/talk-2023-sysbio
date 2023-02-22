library(tidyverse)
library(gganimate)
library(transformr)

set.seed(5)
attr_data <- read.csv("data/dirichlet_island/attribution_data.csv") %>%
  filter(Source != "Human" | Year >= 2008)
sts = attr_data %>%
  group_by(ST) %>% count(Source) %>% spread(Source, n, fill=0) %>%
  ungroup()

top50 <- sts %>% mutate(ST = fct_lump(factor(ST), n=49, w=Human)) %>%
  gather(Source, Count, -ST) %>% 
  group_by(ST, Source) %>% summarise(Count = sum(Count)) %>%
  ungroup() %>% spread(Source, Count) %>%
  mutate(ST = fct_reorder(ST, Human, .fun = identity, .desc=TRUE),
         ST = fct_relevel(ST, "Other", after = 50)) %>%
  select(ST, Poultry) %>% uncount(Poultry) %>%
  sample_n(100) %>% count(ST, name = "Poultry", .drop=FALSE) %>%
  mutate(Prior = 1) %>%
  gather(Source, Count, -ST) %>% mutate(Proportion = Count/sum(Count))

alpha <- top50$Count
random <- as.data.frame(t(rdirichlet(20, alpha)))
#random <- bind_cols(random, V21=random[,1])

uncertainty <- bind_cols(top50, random) %>%
  select(-Count, -Proportion) %>% gather(Iteration, Proportion, V1:V20) %>%
  extract(Iteration, into="Iteration", regex="([0-9]+)", convert=TRUE)

# plot
anim <- ggplot(uncertainty, aes(x=ST, y=Proportion, fill=Source)) + geom_col() +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=1, suffix=""), expand=c(0,0)) +
  scale_fill_manual(values=c(Poultry = "steelblue", Prior = "grey50")) +
  guides(fill='none') +
  coord_cartesian(ylim = c(0,25*scale_fact)) +
  theme_minimal(base_size = 18) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust=0.5)) +
  transition_states(Iteration, transition_length = 1, state_length = 0.1) +
  ease_aes('cubic-in-out')

animate(anim, renderer = gifski_renderer(file='dirichlet_uncertainty.gif', loop=FALSE),
        width = 10*96, height = 3.5*96, units = "px", duration=11, fps=30)
