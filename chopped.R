tuesdata <- tidytuesdayR::tt_load('2020-08-25')
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped

library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(tidyverse)

chopped_text <- chopped %>% 
  pivot_longer(cols=c(appetizer, entree, dessert),
               names_to="course", values_to="ingredient")  %>%
  select(episode_name, course, ingredient)

chopped_words <- chopped_text %>% unnest_tokens(word, ingredient) %>%
  anti_join(stop_words)

chopped_pairs <- chopped_words %>% 
  pairwise_count(word, episode_name, sort = TRUE, upper = FALSE)

set.seed(1234)
chopped_pairs %>%
  filter(n >=12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n, edge_colour=course), edge_colour = "chocolate2") +
  geom_node_point(size = 5, alpha=0.5) +
  geom_node_text(aes(label = name), family="Avenir", repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  labs(title="Chalk(olate) & cheese", subtitle="Word connections for course ingredients in Chopped",
       caption="Source: Wikipedia. Network analysis based on example in Text Mining with R by Julia Silge and David Robinson") +
  theme(plot.title=element_text(size=40, family="Baskerville"),
        plot.subtitle=element_text(size=15, family="Avenir"),
        plot.margin = unit(c(1,1,1,1), "cm")) 

  ggsave("chopped.png", dpi="retina", width=15)






