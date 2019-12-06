
# library(tidyverse)
source("R/eda_startup.r")


input <- read_lines("day_3_input.txt")
input

wire1 <- input[1] %>% strsplit(",") %>% unlist()
wire2 <- input[2] %>% strsplit(",") %>% unlist()

follow_the_wire <- function(wire) {

  # results <- vector("list", length(wire) + 1)
  results <- list()
  x <- 0
  y <- 0
  x_sign <- list(R = +1, L = -1, U =  0, D =  0)
  y_sign <- list(R =  0, L =  0, U = +1, D = -1)
  direction <- substr(wire[1], 1, 1)
  distance  <- as.integer(substring(wire[1], 2, nchar(wire[1])))
  for (dest in wire) {
    results <- append(results, list(list(x = x, y = y, direction = direction, distance = distance)))
    # results <- append(results, list(c(x = x, y = y)))
    direction <- substr(dest, 1, 1)
    distance  <- as.integer(substring(dest, 2, nchar(dest)))

    # switch(direction,
    #        R = {seq.int(from = x, to = x + distance, by = +1)browser()},
    #        L = {seq.int(from = x, to = x + distance, by = +1)browser()},
    #        U = {seq.int(from = x, to = x + distance, by = +1)browser()},
    #        D = {seq.int(from = x, to = x + distance, by = +1)browser()})

    x <- x + x_sign[[direction]] * distance
    y <- y + y_sign[[direction]] * distance
  }
  results <- append(results, list(list(x = x, y = y, direction = NA_character_, distance = NA_integer_)))
  results
}

wire1_results <- follow_the_wire(wire1)
wire1_results
wire2_results <- follow_the_wire(wire2)
wire2_results

wire_list_to_df <- . %>%
  transpose() %>%
  simplify_all() %>%
  as_tibble()
wire1_df <- wire1_results %>%
  wire_list_to_df()
wire2_df <- wire2_results %>%
  wire_list_to_df()

wire_df <- bind_rows(wire1_df, wire2_df, .id = "wire_id")

wire_df

plot_wire_df <- . %>% {
  ggplot(., aes(x, y, group = wire_id)) +

    geom_path(aes(color = wire_id)) +
    geom_point(size = 1) +
    geom_text(data = . %>%
                # rownames_to_column("id") %>%
                group_by(wire_id) %>%
                mutate(id = 1:n()) %>%
                # slice(c(1:4, sample.int(n() - 4, min(10, n())) + 4)) %>%
                ungroup() %>%
                mutate(label = glue("{id}:{direction}({distance})")),
              mapping = aes(label = label),
              # position = position_jitterdodge(10,10, 3),
              size = 2) +
    scale_colour_manual(values = c("1" = "green", "2" = "red", "cross_point" = "violet")) +
    coord_fixed()
} %T>%
  plotly::ggplotly() %>%
  identity()

plot_wire_df(wire_df)

input_example_1_wire_1 <- "R8,U5,L5,D3" %>% strsplit(",") %>% unlist()
input_example_1_wire_2 <- "U7,R6,D4,L4" %>% strsplit(",") %>% unlist()
input_example_1_wire_1_df <- follow_the_wire(input_example_1_wire_1) %>% wire_list_to_df()
input_example_1_wire_2_df <- follow_the_wire(input_example_1_wire_2) %>% wire_list_to_df()
input_example_1_df <-
  bind_rows(follow_the_wire(input_example_1_wire_1) %>% wire_list_to_df(),
            follow_the_wire(input_example_1_wire_2) %>% wire_list_to_df(),
            .id = "wire_id")

input_example_1_df %>%
  plot_wire_df()

combined_wires_df_to_segments <- . %>%
  group_by(wire_id) %>%
  mutate(wire_point_id = 1:n()) %>%
  select(contains("id"), everything()) %>%  #unnecessary
  slice(1, rep(2:(n() - 1), each = 2), n()) %>%
  ungroup() %>%
  mutate(segment_id = seq.int(1, n() / 2) %>% rep(each = 2)) %>%
  select(contains("id"), everything()) %>%  #unnecessary
  group_by(segment_id) %>% select(x, y) %>% nest() %>% ungroup() %>%
  identity()
segments_df <- combined_wires_df_to_segments(input_example_1_df)
segments_df

pair_segments_df <- combn(segments_df %>% nrow(), 2, function(x) {
  segments_df %>%
    slice(x) %>%
    mutate(label = c("from", "to"))
}, simplify = FALSE) %>%
  bind_rows(.id = "segment_pairs") %>%
  pivot_wider(names_from = label, values_from = c(data, segment_id))

intersecting_segments <-
  combn(segments_df %>% nrow(), 2, function(x) {
    s1 <- segments_df$data[[x[[1]]]] %>% as.matrix()
    s2 <- segments_df$data[[x[[2]]]] %>% as.matrix()
    if (pracma::segm_intersect(s1, s2)) {
      # s1 <- segments_df$data[[x[[1]]]] %>% as.matrix()
      # s2 <- segments_df$data[[x[[2]]]] %>% as.matrix()
      # browser()
      if (all(s1 %*% s2 == 0)) {
        return()
      }

      p1 <- s1[1, ]; p2 <- s1[2, ]; p3 <- s2[1, ]; p4 <- s2[2, ]
      A <- cbind(p2 - p1, p4 - p3)
      b <- (p3 - p1)
      a <- solve(A, b)

      if (any(a == 0)) {
        return()
      }

      list(s1 = s1,
           s2 = s2,
           points = (p1 + a[1]*(p2-p1)))
    }
  }, simplify = FALSE)

cross_points <- intersecting_segments %>%
  compact() %>%
  map("points") %>%
  enframe() %>% unnest_auto(value) %>%
  mutate(label = "cross_point") %>%
  identity()

input_example_1_df %>%
  plot_wire_df() %>% {
    . + geom_point(
      data = cross_points,
      mapping = aes(x, y, color = label),
      size = 10,
      shape = 4, inherit.aes = FALSE)
    }


cross_points
distances_cross <- dist(rbind(c(0,0),cross_points[,c("x", "y")]), method = "manhattan")
distances_cross
as.matrix(distances_cross)[,1] %>% sort() %>% head(4)










find_intersections <- function(segments) {

  intersecting_segments <- segments %>%
    combn(2, function(x) {
      if (pracma::segm_intersect(x[[1]], x[[2]])) {
        s1 <- x[[1]]
        s2 <- x[[2]]

        if (all(s1 %*% s2 == 0)) {
          return()
        }

        p1 <- s1[1, ]; p2 <- s1[2, ]; p3 <- s2[1, ]; p4 <- s2[2, ]
        A <- cbind(p2 - p1, p4 - p3)
        b <- (p3 - p1)
        a <- solve(A, b)

        list(s1 = s1,
             s2 = s2,
             points = (p1 + a[1]*(p2-p1)))
      }
    }, simplify = FALSE)

  intersecting_segments %>%
    compact()
}

# wire_intersections <- find_intersections(input_example_1_wire_1_df,
#                                          input_example_1_wire_2_df)
#
# corner_points <- wire_intersections %>%
#   map("points") %>%
#   prepend(list(c(x = 1, y = 1))) %>%
#   transpose() %>%
#   simplify2array()
#
# corner_points %>% head()
#
# distances_cross <- dist(corner_points, method = "manhattan")
# as.matrix(distances_cross)[,1] %>% sort() %>% head(4)

stop("end of script")

# wire1_results %>%
#   append(wire2_results)


wire_intersections <- find_intersections(wire1, wire2)

corner_points <- wire_intersections %>%
  map("points") %>%
  prepend(list(c(x = 1, y = 1))) %>%
  transpose() %>%
  simplify2array()

corner_points %>% head()

distances_cross <- dist(corner_points, method = "manhattan")
as.matrix(distances_cross)[,1] %>% sort() %>% head(4)

# bench::mark(
#   dt = map2(results %>% head(-1), results[-1], append) %>%
#     transpose() %>% simplify2array(),
#   purrr = map2(results %>% head(-1), results[-1], append) %>%
#     data.table::rbindlist(),
#   check = FALSE
# )

