
#' Attempting to answer [SO question](https://stackoverflow.com/questions/59248550/create-new-vectors-with-no-element-being-in-the-same-position-as-the-original-ve)

V1 <- 1:10
no_of_resampled_V1 <- 3

# FAULTY APPROACH: memory intensive
# sample_space <- gtools::permutations(n = length(V1), r = length(V1), v = V1, set = FALSE)
# ALTERNATIGVE TO THE ABOVE
# RcppAlgos::permuteSample(v = V1, m = length(V1), repetition = FALSE, n = no_of_resampled_V1)
#' does not work as the above



# SO Question -------------------------------------------------------------

#' [source](https://stackoverflow.com/questions/59246882/syntax-to-remove-interaction-terms-of-one-variable-with-all-other-variables-in-t)
source("R/eda_startup.r")
ex_data <- rerun(7, rnorm(100)) %>%
  set_names(c("y", paste0("x",1:6))) %>%
  as_tibble()

ex_data %>%
  lm(y ~ . + (. - x1) ^ 2, data = .) %>%
  coef() %>%
  names() %>%
  glue_collapse(sep = '\n')


# SO Question -------------------------------------------------------------


source("R/eda_startup.r")

g1 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(cyl, disp))
g2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_line(aes(cyl, 500 - disp))

g1_table <- ggplot2::ggplotGrob(g1)
g2_table <- ggplot2::ggplotGrob(g2)

g_all <- gtable::gtable_add_grob(
  gtable::gtable_filter(g1_table, "panel", invert = TRUE),
  gtable::gtable_filter(g2_table, "panel", invert = FALSE), 7, 5,
  z = 1, name = "panel")
plot(g_all)
