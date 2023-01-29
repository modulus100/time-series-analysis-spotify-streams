library(tidyverse)
library(lubridate)
library(pwt9)
library(ggplot2)
library(ggthemes)

theme_blog <- function(){
  theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
          plot.background = element_rect("#272b30"),
          plot.title = element_text(colour = "white"),
          panel.grid = element_line(colour = "#425d65"),
          axis.text = element_text(colour = "white"),
          axis.title = element_text(colour = "white"),
          strip.text = element_text(colour = "white"))
}


pwt <- pwt9.0 %>%
  select(country, year, avh)

# head(pwt)

pwt_wide <- pwt %>%
  pivot_wider(names_from = year, values_from = avh)  %>%
  filter(!is.na(`1950`)) %>%
  mutate_at(vars(-country), as.numeric)


wss <- map_dbl(1:5, ~{kmeans(select(pwt_wide, -country), ., nstart=50,iter.max = 15 )$tot.withinss})

n_clust <- 1:5

elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))


ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
  theme_blog()
