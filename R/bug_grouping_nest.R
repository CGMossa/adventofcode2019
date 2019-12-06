
library(dplyr)
library(tidyr)

iris_with_windowed_grouping <- iris %>%
  mutate(grouping_id = rep(seq_len(n()/2), each = 2))

#' We are just grouping consequitive columns together
iris_with_windowed_grouping %>%
  head()

iris_with_windowed_grouping %>%
  group_by(grouping_id) %>% nest() #works fine

iris_with_windowed_grouping %>%
  group_by(grouping_id) %>% nest(data = c(Species, Petal.Width))
