random <- function (x, ...)   UseMethod("random")

# adapted from utils:::head.data.frame
random.data.frame <- function (x, n = 6L, ...) 
{
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) 
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  ii <- sample(seq_len(x), n)
  x[ii, , drop = FALSE]
}

