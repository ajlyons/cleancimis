#' Compute chill portions
#'
#' Compute chill portions
#'
#' @param temp_ts_numeric numeric vector of temperatures
#' @param delta_t_h number
#'
#' @details
#' This computes chill portions
#' Works only for temperatures every hour or 30 min.
#'
#' @importFrom purrr pluck
#' @importFrom dplyr tibble mutate
#'
#' @export

# Function to calculate chill portions
# Works only for temperatures every hour or 30 min.
cc_chill_portion <- function(temp_ts_numeric, delta_t_h = 1) {
  if (!is.numeric(temp_ts_numeric) | sum(is.na(temp_ts_numeric)) > 0) {
    stop("temp_ts_numeric must be a numeric vector without NA's")
  }

  hourtemps <- tibble(Temp = temp_ts_numeric)

  e0 <- 4153.5
  e1 <- 12888.8
  a0 <- 139500
  a1 <- 2567000000000000000
  slp <- 1.6
  tetmlt <- 277
  aa <- a0 / a1
  ee <- e1 - e0

  hourtemps <- hourtemps |>
    mutate(
      TK = Temp + 273,
      ftmprt = slp * tetmlt * (TK - tetmlt) / TK,
      sr = exp(ftmprt),
      xi = sr / (1 + sr),
      xs = aa * exp(ee / TK),
      ak1 = a1 * exp(-e1 / TK),
      interE = c(0, rep(NA, length(TK) - 1))
    )

  xs <- hourtemps |> pluck("xs")
  xi <- hourtemps |> pluck("xi")
  ak1 <- hourtemps |> pluck("ak1")
  S <- ak1
  S[1] <- 0
  E <- S

  for (i in 2:nrow(hourtemps)) {
    if (E[i - 1] < 1) {
      S[i] <- E[i - 1]
      E[i] <- xs[i] - (xs[i] - S[i]) * exp(-ak1[i])
    } else {
      S[i] <- E[i - 1] - E[i - 1] * xi[i - 1]
      E[i] <- xs[i] - (xs[i] - S[i]) * exp(-ak1[i])
    }
  }

  hourtemps <- hourtemps |>
    mutate(
      interE = E,
      delt = ifelse(
        interE < 1,
        0,
        interE * xi * delta_t_h
      )
    )

  return(hourtemps$delt)
}
