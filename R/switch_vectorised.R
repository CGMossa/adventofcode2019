#' Is the `switch`-statement vectorised?
#'

some_vector <- sample(letters[1:4], replace=TRUE, size = 10)
some_vector

# switched_some_vector <- switch(some_vector,
#        a = 1,
#        b = 2,
#        c = 3,
#        d = 42)
#' Unfortunately, switch has to be of a certain size

# switch("a",
switch("d",
       )

vec_switch <- function(vector_exprs, cases) {
  # browser()
  switch(vector_exprs,
         # cases)
         unlist(force(cases)))
}
vec_switch <- Vectorize(vec_switch, vectorize.args = "vector_exprs")

vec_switch(
  some_vector,list(a=1, b=2, c=3,d=42)
)
