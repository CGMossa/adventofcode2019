source("R/eda_startup.r")

bm_press <- bench::press(
  n = 2^(8:14),
  {
    bench::mark(
      rep_init = rep(0, n),
      vector_init = vector("numeric", n)
    )
  }
)

bm_press %>%
  autoplot()

bm_press %>%
  summary()

bm_press_df <- bm_press %>%
  mutate(expression = expression %>% as.character) %>%
  # select(expression, n, n_itr, n_gc, time)
  select(expression, n, time) %>%
  group_by(n) %>%
  nest(data = c(expression,time)) %>%
  ungroup()




bm_press_df %>% slice(3) %$% {
  A <- data
}



bm_press_df$time %>%
  map_dbl(length)

  # unnest(time %>% unclass())
  # mutate(time = as.list(time))
  # group_by(n) %>%
  # nest(data = c(time))

