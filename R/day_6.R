source("R/eda_startup.r")

example <- "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"


ex_map <- example %>%
  strsplit('\n') %>%
  unlist() %>%
  lapply(. %>% strsplit(")")) %>%
  unlist(recursive = FALSE) %>%
  transpose() %>%
  simplify_all() %>%
  set_names("value", "key") %$% {
    print(key)
    print(value)
    hashmap::hashmap(key, value)
  }

input_to_hashmap <- . %>%
  strsplit('\n') %>%
  unlist() %>%
  lapply(. %>% strsplit(")")) %>%
  unlist(recursive = FALSE) %>%
  transpose() %>%
  simplify_all() %>%
  set_names("value", "key") %$% {
    print(key)
    print(value)
    hashmap::hashmap(key, value)
  }


ex_map$keys()

ex_map$find("D") #C
ex_map$find("C") #B
ex_map$find("B") #COM
ex_map$find("COM") # NA

library(foreach)

foreach(object = ex_map$keys()) %do% {
  orbit_count <- 0
  repeat {
    orbits = ex_map$find(object)
    if (is.na(orbits)) {
      break
    } else {
      object <- orbits
      orbit_count <- orbit_count + 1
    }
  }
  c(count = orbit_count)
} %>%
  unlist() %>% sum()

problem_input <- read_lines("day_6_input.txt")
ex_prob <- problem_input %>%
  lapply(. %>% strsplit(")")) %>%
  unlist(recursive = FALSE) %>%
  transpose() %>%
  simplify_all() %>%
  set_names("value", "key") %$% {
    # print(key)
    # print(value)
    hashmap::hashmap(key, value)
  }

foreach(object = ex_prob$keys()) %do% {
  orbit_count <- 0
  repeat {
    orbits = ex_prob$find(object)
    if (is.na(orbits)) {
      break
    } else {
      object <- orbits
      orbit_count <- orbit_count + 1
    }
  }
  c(count = orbit_count)
} %>%
  unlist() %>% sum()
#'
#' Answer: 322508
#'


# PART TWO ----------------------------------------------------------------

find_common_node <- function(ex_prob) {
  traj_list <- foreach(object = list("YOU", "SAN")) %do% {
    trajectory <- list(object)
    repeat {
      orbits = ex_prob$find(object)
      if (is.na(orbits)) {
        break
      } else {
        object <- orbits
        trajectory <- append(trajectory, object)
      }
    }
    trajectory
  }

  traj_list <- traj_list %>%
    set_names(c("YOU","SAN")) %>%
    simplify_all() %>%
    identity()

  list(
    traj_list = traj_list,
    common_node =
      outer(traj_list$YOU, traj_list$SAN, `==`) %>%
      which(arr.ind = TRUE) %>%
      as_tibble() %>%
      # arrange(row, col) %>%
      # arrange(col, row) %>%
      mutate(row_value = traj_list$YOU[row]) %>%
      mutate(col_value = traj_list$SAN[col]) %>%
      slice(1)
  )
}

ex_part2 <- "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"

ex_part2 <- input_to_hashmap(ex_part2)

find_common_node(ex_part2)

find_common_node(ex_prob)

# row - 2 + col - 2.
# ANSWER: 496
#


