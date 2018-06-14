require(magrittr)
require(tictoc)

tictocify <- function(f) {
  int_f <- function(...) {
    tic()
    toc()
    return(x)
  }
  formals(int_f) <- formals(f)

  body(int_f) <- body(int_f) %>%
    as.list() %>%
    append(quote(x <- do.call(f, map(names(formals(f)), as.name))),
           after = 2) %>%
    as.call()

  return(int_f)
}

source('R/is.output.same.R')
set.seed(1)
stuff <- rnorm(n = 100000)
map_new <- tictocify(map_lgl)
is.output.same(map_new(stuff, is_greater_than, 0), map_lgl)
