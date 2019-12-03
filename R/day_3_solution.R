
library(tidyverse)


input <- read_lines("day_3_input.txt")
input

wire1 <- input[1] %>% strsplit(",") %>% unlist()
wire2 <- input[2] %>% strsplit(",") %>% unlist()

follow_the_wire <- function(wire) {

  # results <- vector("list", length(wire))
  results <- list()
  x <- 0
  y <- 0
  x_sign <- list(R = +1, L = -1, U =  0, D =  0)
  y_sign <- list(R =  0, L =  0, U = +1, D = -1)
  for (dest in wire) {
    # results <- append(results, list(list(x = x, y = y)))
    results <- append(results, list(c(x = x, y = y)))
    direction <- substr(dest, 1, 1)
    distance  <- as.integer(substring(dest, 2, nchar(dest)))

    switch(direction,
           R = {seq.int(from = x, to = x + distance, by = +1)browser()},
           L = {seq.int(from = x, to = x + distance, by = +1)browser()},
           U = {seq.int(from = x, to = x + distance, by = +1)browser()},
           D = {seq.int(from = x, to = x + distance, by = +1)browser()})

    x <- x + x_sign[[direction]] * distance
    y <- y + y_sign[[direction]] * distance
  }
  results
}

wire1_results <- follow_the_wire(wire1)
wire1_results
wire2_results <- follow_the_wire(wire2)
wire2_results

wire1_results %>%
  append(wire2_results)

find_intersections <- function(wire1, wire2) {
  # results[1:3]
  # results[1:3] %>% head(-1) #w/o last
  # results[1:3] %>% tail(-1) #w/o first
  results <- follow_the_wire(wire1) %>%
    append(follow_the_wire(wire2))
  segments <- map2(results %>% head(-1), results[-1], rbind)

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

