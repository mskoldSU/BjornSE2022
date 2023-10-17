#' Prepares data to be used with MARK software through RMark, uses calendar week as sampling instances
#'
#' @param data Data with rows corresponding to observations and columns id, date, sex
#' @param model RMark model type
#'
#' @return A list suitable for use with RMark
#'
#' @export
data2mark <- function(data, model){
  data |> dplyr::mutate(id = id,
                        sex = as.factor(sex),
                        week = lubridate::isoweek(date),
                        n = 1,
                        .keep = "none") |>
    dplyr::distinct() |>
    dplyr::arrange(week) |>
    tidyr::pivot_wider(id_cols = c("id", "sex"),
                       names_from = week, values_from = n,
                       values_fill = c(n = 0)) |>
    tidyr::unite("ch", -c("id", "sex"), sep = "") |>
    RMark::process.data(model = model, groups = "sex")
}

#' Fits a range of models using RMark
#'
#' @param data Data with rows corresponding to observations and columns id, date, sex
#'
#' @return A table of fitted models and summaries
#'
#' @export
fit_mark <- function(data){
  # Define model types
  p.time.mixture <- list(formula = ~time + mixture, share = TRUE)
  p.time <- list(formula = ~time, share = TRUE)
  p.time.sex <- list(formula = ~time + sex, share = TRUE)
  pi.1 <- list(formula = ~1)
  pi.sex <- list(formula = ~sex)
  f0.sex <-  list(formula = ~sex)
  # Fit models
  fit1 <- data2mark(data, "FullHet") |>
    RMark::mark(model = "HetClosed", model.parameters = list(pi = pi.1, p = p.time.mixture, f0 = f0.sex),
                output = FALSE, silent = TRUE, delete = TRUE)
  fit2 <- data2mark(data, "FullHet") |>
    RMark::mark(model = "HetClosed", model.parameters = list(pi = pi.sex, p = p.time.mixture, f0 = f0.sex),
                output = FALSE, silent = TRUE, delete = TRUE)
  fit3 <- data2mark(data, "Closed") |>
    RMark::mark(model = "Closed", model.parameters = list(p = p.time.sex, f0 = f0.sex),
                output = FALSE, silent = TRUE, delete = TRUE)
  fit4 <- data2mark(data, "Closed") |>
    RMark::mark(model = "Closed", model.parameters = list(p = p.time, f0 = f0.sex),
                output = FALSE, silent = TRUE, delete = TRUE)

  # Join and extract model summaries
  all_fit <- tibble::tibble(fit = list(fit1, fit2, fit3, fit4))
  table <- all_fit |> dplyr::rowwise() |>
    dplyr::mutate(
      nm = fit[["results"]][["derived"]][["N Population Size"]][["estimate"]][1] |> round(),
      nf = fit[["results"]][["derived"]][["N Population Size"]][["estimate"]][2] |> round(),
      nfm = nm + nf,
      nm_l = fit[["results"]][["derived"]][["N Population Size"]][["lcl"]][1],
      nm_u = fit[["results"]][["derived"]][["N Population Size"]][["ucl"]][1],
      nf_l = fit[["results"]][["derived"]][["N Population Size"]][["lcl"]][2],
      nf_u = fit[["results"]][["derived"]][["N Population Size"]][["ucl"]][2],
      tot_var = fit[["results"]][["derived.vcv"]][["N Population Size"]] |> sum(),
      nfm_l = nfm / exp(1.96 * sqrt(log(1 + tot_var / nfm^2))),
      nfm_u = nfm * exp(1.96 * sqrt(log(1 + tot_var / nfm^2))),
      AICc = fit[["results"]][["AICc"]],
      model = fit[["model.name"]] |> stringr::str_remove_all("~|c\\(\\)")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dAICc = round(AICc - min(AICc), 1))
  table
}
#' Simulates from a fitted object
#'
#' @param fit Fitted models
#'
#' @return A table simulated population sizes
#'
#' @export
sim_pop <- function(fit){
  beta1 <- fit[["results"]][["beta"]]["f0:(Intercept)", "estimate"]
  beta2 <- fit[["results"]][["beta"]]["f0:sexHona", "estimate"]
  vcv_full <- fit[["results"]][["beta.vcv"]]
  d <- nrow(vcv_full)
  vcv_beta <- vcv_full[(d - 1):d,(d - 1):d]
  betasim <- MASS::mvrnorm(1, c(beta1, beta2), vcv_beta)
  tot_est_m <- fit[["results"]][["derived"]][[1]][1, "estimate"]
  tot_est_f <- fit[["results"]][["derived"]][[1]][2, "estimate"]
  found_m <- tot_est_m - exp(beta1)
  found_f <- tot_est_f - exp(beta1 + beta2)

  tibble::tibble(pop_m = found_m + exp(betasim[1]),
                 pop_f = found_f + exp(betasim[1] + betasim[2]),
                 pop = pop_m + pop_f)
}



recurse <- function(pop, rate, hunted){
  n <- length(pop)
  if (n == 1)
    return(pop)
  for (i in 2:n){
    pop[i] <- pop[i-1] * rate - hunted[i]
  }
  pop
}

#' Simulates a population forwards in time
#'
#' @param model_fit Fitted models
#' @param rate Natural growth rate
#' @param hunted Yearly and regional hunting data
#'
#' @return A table simulated population sizes
#'
#' @export
forward <- function(model_fits, rate, hunted){
  dplyr::bind_rows(
    tibble::tibble(region = "Region A", year = c(2021, 2022, 2023)),
    tibble::tibble(region = "Region B", year = c(2019, 2020, 2021, 2022, 2023)),
    tibble::tibble(region = "Region C", year = c(2020, 2021, 2022, 2023)),
    tibble::tibble(region = "Region D", year = c(2022, 2023))
  ) |>
    dplyr::left_join(hunted, by = c("region", "year")) |>
    dplyr::left_join(model_fits |> dplyr::mutate(region = stringr::str_sub(survey, 1, 8)), by = c("region")) |>
    rowwise() |>
    dplyr::mutate(pop = sim_pop(fit)$pop) |>
    dplyr::ungroup() |>
    dplyr::mutate(pop = recurse(pop, rate, hunted), .by = "region") |>
    dplyr::select(region, year, hunted, pop)
}
