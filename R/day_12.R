
source("R/eda_startup.r")

# EXPERIMENT
# init_positions <- "<x=-1, y=0, z=2>
# <x=2, y=-10, z=-7>
# <x=4, y=-8, z=8>
# <x=3, y=5, z=-1>" %>%
#   strsplit("\n") %>%
#   unlist(recursive = FALSE) %>%
#
#   str_match("<x=([+-]?\\d+), y=([+-]?\\d+), z=([+-]?\\d+)>")


# init_pos <- init_positions %>%
#   set_colnames(c("raw", "pos_x", "pos_y", "pos_z")) %>%
#   simplify2array() %>%
#   # `[`(., -1, drop=FALSE) %>%
#   `[`(,2:4) %>%
#   apply(2, as.numeric) %>%
#   identity()

day_12 <- function(input_string, max_steps) {

  init_positions <- input_string %>%
    strsplit("\n") %>%
    unlist(recursive = FALSE) %>%

    str_match("<x=([+-]?\\d+), y=([+-]?\\d+), z=([+-]?\\d+)>")


  init_pos <- init_positions %>%
    set_colnames(c("raw", "pos_x", "pos_y", "pos_z")) %>%
    simplify2array() %>%
    # `[`(., -1, drop=FALSE) %>%
    `[`(,2:4) %>%
    apply(2, as.numeric) %>%
    identity()

  init_state <- cbind(init_pos, vel_x = 0, vel_y = 0, vel_z = 0)

  # max_steps <- 10
  steps <- vector("list", max_steps + 1)
  steps[[1]] <- list(step = 0, state = init_state)
  state <- init_state
  for (current_step in seq_len(max_steps)) {
    #gravity
    combn(nrow(state), 2, FUN = function(pair_idx) {
      diff_pos <-  diff(state[pair_idx, 1:3], lag = 1, differences = 1)
      state[pair_idx, 4:6] <<- state[pair_idx, 4:6] +
        rbind(sign(diff_pos), (-1) * sign(diff_pos))
    }, simplify = FALSE)
    init_state
    state

    state[, 1:3] <- state[, 1:3] + state[, 4:6]
    # state[,4:6] <- state[, 4:6]
    state

    total_energy <- state %>% abs() %>%
      cbind(pot = rowSums(.[,1:3]),
            kin = rowSums(.[,4:6])) %>% {
              sum(.[,"pot"] * .[, "kin"])
            }
    total_energy

    steps[[current_step + 1]] <-
      list(step         = current_step,
           total_energy = total_energy,
           state        = state)
  }
  steps
}

first_example <- day_12("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>", max_steps = 10)

first_example %>% tail(1)

second_example <- day_12("<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>", max_steps = 100)

second_example[[101]]

problem <- day_12("<x=13, y=9, z=5>
<x=8, y=14, z=-2>
<x=-5, y=4, z=11>
<x=2, y=-6, z=1>", max_steps = 1000)


problem %>%
  tail(1)
#' ANSWER: 6490
