example_input <-
".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##"

example_input %>%
  strsplit("\n") %>%
  lapply(function(x) strsplit(x, "")) %>%
  unlist(., recursive = FALSE) %>% {
    # Matrix::Matrix(unlist(.),
    matrix(unlist(.),
           ncol = length(.[[1]]),
           nrow = length(.),
           byrow = TRUE)
  } %>%

  # as_tibble(.name_repair = janitor::make_clean_names) %>%
  as_tibble() %>%
  rowid_to_column() %>% mutate(rowid=rowid-1) %>%
  pivot_longer(-rowid, names_to = c(NULL, "col"), names_pattern = "(\\d+)", names_ptypes = list(col = integer())) %>%
  mutate(col = col - 1) %>%
  rename(row = rowid) %>%

  mutate(value = case_when(value == "." ~ NA_real_,
                           value == "#" ~ 1,
                           value == "X" ~ 0)) %>%
  na.omit
  identity()


# {rbind(unlist(., recursive = FALSE))}
# dist()
