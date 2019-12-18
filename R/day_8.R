
source("R/eda_startup.r")

image_pixels <- read_lines("day_8_input.txt") %>%
  strsplit(character(1)) %>%
  unlist() %>%
  as.numeric()



"123456789012" %>%
  strsplit(character(0)) %>%
  unlist() %>% as.numeric() %>%
  split(rep(seq_len(length(.) %/% (2 * 3)), each = length(.) / length(.) %/% (2 * 3))) %>%
  lapply(. %>% matrix(nrow = 2, ncol = 3, byrow = TRUE))


image_pixels %>%
  split(rep(seq_len(length(.) %/% (6 * 25)), each = length(.) / length(.) %/% (6 * 25))) %>%
  lapply(. %>% matrix(nrow = 6, ncol = 25, byrow = TRUE)) %>%

  lapply(function(x) {
    # browser()
    table(x) %>% as.list()
  }) %>%
  transpose() %>%
  simplify_all() %>%
  as_tibble() %>%
  rowid_to_column("layer") %>%
  arrange(`0`) %>%
  filter(layer == 19)


  # pivot_longer(
  #   -layer,
  #   names_to = "num", values_to = "freq")


