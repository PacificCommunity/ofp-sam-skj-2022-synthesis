#' Change seasonal model to seasons as years
change_seasons_as_years <- function(dir,
                                    natural_mortality_at_age,
                                    maturity_at_age,
                                    n_seasons = 4,
                                    max_period = 12) {
  # Supporting information
  input <- r4ss::SS_read(dir = dir)
  data <- input[["dat"]]
  control <- input[["ctl"]]
  n_years <- 1 + data[["endyr"]] - data[["styr"]] 
  n_times <- n_years * n_seasons
  lookup_table <- tibble::tibble(
    year = data[["endyr"]]:data[["styr"]]
  ) %>%
    tidyr::crossing(
      seas = 1:4
    ) %>%
    dplyr::mutate(new = 1:n())

  # Checks
  stopifnot("does not work with age data" = is.null(data[["agecomp"]]))
  
  # Changes to data file
  data[["styr"]]  <- 1
  data[["endyr"]] <- max(lookup_table[["new"]])
  data[["nseas"]] <- 1
  data[["months_per_seas"]] <- 12 / n_seasons
  # TODO: determine if this needs changed
  data[["Nsubseasons"]]
  data[["spawn_month"]] <- 1
  data[["Nages"]] <- data[["Nages"]] * n_seasons
  data[["catch"]] <- dplyr::left_join(
    x = data[["catch"]],
    y = lookup_table,
    by = c("year", "seas")
  ) %>%
    dplyr::select(-year) %>%
    dplyr::mutate(seas = 1) %>%
    dplyr::rename(year = new) %>%
    dplyr::relocate(year) %>%
    as.data.frame()
  data[["CPUE"]] <- dplyr::left_join(
    x = data[["CPUE"]] %>%
      dplyr::mutate(
        seas = rep(1:n_seasons, each = 12/n_seasons)[seas]
      ),
    y = lookup_table,
    by = c("year", "seas")
  ) %>%
    dplyr::select(-year) %>%
    dplyr::mutate(seas = 1) %>%
    dplyr::rename(year = new) %>%
    dplyr::relocate(year) %>%
    as.data.frame()
  data[["lencomp"]] <- dplyr::left_join(
    x = data[["lencomp"]] %>%
      dplyr::mutate(
        Seas = rep(1:n_seasons,each = 12/n_seasons)[Seas]
      ),
    y = lookup_table,
    by = c("Yr" = "year", "Seas" = "seas")
  ) %>%
    dplyr::select(-Yr) %>%
    dplyr::mutate(Seas = 1) %>%
    dplyr::rename(Yr = new) %>%
    dplyr::relocate(Yr) %>%
    as.data.frame()
  data[["max_periods"]] <- max_period
  data[["tag_releases"]] <- dplyr::left_join(
    x = data[["tag_releases"]],
    y = lookup_table,
    by = c("Yr" = "year", "Season" = "seas")
  ) %>%
    dplyr::select(-Yr) %>%
    dplyr::mutate(Season = 1, Age = Age * n_seasons) %>%
    dplyr::rename(Yr = new) %>%
    dplyr::relocate(Yr, .after = "Area") %>%
    as.data.frame()
  data[["tag_recaps"]] <- dplyr::left_join(
    x = data[["tag_recaps"]],
    y = lookup_table,
    by = c("Yr" = "year", "Season" = "seas")
  ) %>%
    dplyr::select(-Yr) %>%
    dplyr::mutate(Season = 1) %>%
    dplyr::rename(Yr = new) %>%
    dplyr::relocate(Yr, .after = "TG") %>%
    as.data.frame()
  data[["spawn_seas"]] <- 1
  # TODO: decide if this should be 1
  data[["spawn_month"]] <- 1
  
  # Changes to control
  control[["Nages"]] <- data[["Nages"]]
  # TODO: write check for type of age and maturity parameters
  # stopifnot(control[[""]])
  control[["natM"]] <- natural_mortality_at_age
  control[["Age_Maturity"]] <- maturity_at_age
  control[["N_tag_groups"]]
  control[["firstAgeMove"]] <- control[["firstAgeMove"]] * n_seasons
  control[["recr_dist_pattern"]][, "age"] <-
    control[["recr_dist_pattern"]][, "age"] * n_seasons
  # TODO: fix age2 in file to 10 not 16
  control[["moveDef"]][, c("age", "age2")] <-
    control[["moveDef"]][, c("age", "age2")] * n_seasons
  control[["moveDef"]] <- control[["moveDef"]] %>%
    dplyr::filter(seas == 1)
  control[["N_moveDef"]] <- NROW(control[["moveDef"]])
  # TODO: fix vector of M
  control[["natM"]]
  control[["MG_parms"]] <- control[["MG_parms"]][
    !grepl("_seas_[2-9]|_seas_1[1-9]+",row.names(control[["MG_parms"]]), perl = TRUE),
  ]
  control[["Growth_Age_for_L1"]] <-
    control[["Growth_Age_for_L1"]] * n_seasons
  # TODO: fix max age here
  control[["Growth_Age_for_L2"]] <-
    control[["Growth_Age_for_L2"]] * n_seasons
  stopifnot(control[["maturity_option"]] == 3)
  # TODO: fix vector of age-maturity
  control[["Age_Maturity"]]
  # TODO: check
  control[["MGparm_seas_effects"]]
  control[["MainRdevYrFirst"]] <- lookup_table[
    match(control[["MainRdevYrFirst"]], lookup_table[["year"]]),
    "new"
  ] %>% as.numeric()
  control[["MainRdevYrLast"]] <- lookup_table[
    match(control[["MainRdevYrLast"]], lookup_table[["year"]]),
    "new"
  ] %>% as.numeric()
  control[c(
    "recdev_early_start",
    "last_early_yr_nobias_adj",
    "first_yr_fullbias_adj",
    "last_yr_fullbias_adj",
    "first_recent_yr_nobias_adj"
  )] <- c(0, 1, n_seasons, max(lookup_table[["new"]]), max(lookup_table[["new"]]) + 1)
  control[["F_ballpark_year"]] <- -1
  stopifnot(all(control[["age_selex_types"]][, "Pattern"] == 0))
  # TODO: check for time-varying selectivity and tag
  # loss/chronic/overdispersion/report/decay

  # Changes to starter file
  input[["start"]][["minyr_sdreport"]] <- -1
  input[["start"]][["maxyr_sdreport"]] <- -1
  input[["start"]][["N_STD_yrs"]] <- 0
  input[["start"]][["min_age_summary_bio"]] <-
    input[["start"]][["min_age_summary_bio"]] * n_seasons
  input[["start"]][["F_age_range"]] <-
    input[["start"]][["F_age_range"]] * n_seasons

  input[["dat"]] <- data
  input[["ctl"]] <- control

  r4ss::SS_write(input, dir = dir, overwrite = TRUE)
}


age <- matrix(c(
  0.6729,
  0.6729,
  0.5948,
  0.4446,
  0.3165,
  0.2414,
  0.2118,
  0.2095,
  0.2264,
  0.2593,
  0.3062,
  0.3637,
  0.4246,
  0.4764,
  0.5126,
  0.5328,
  0.5388
), nrow = 1) %>% as.data.frame()
maturity <- matrix(c(
  0,
  0.0196337,
  0.107812,
  0.332058,
  0.605065,
  0.794749,
  0.894301,
  0.942967,
  0.967477,
  0.98062,
  0.988155,
  0.992744,
  0.995687,
  0.997658,
  0.999024,
  1,
  1
), nrow = 1) %>% as.data.frame()
ignore <- r4ss::copy_SS_inputs(
  dir.old = "C:/git/PacificCommunity/ofp-sam/ofp-sam-skj-2022-synthesis/model_input/season",
  dir.new = "C:/git/PacificCommunity/ofp-sam/ofp-sam-skj-2022-synthesis/model_input/byseason",
  verbose = FALSE,
  overwrite = TRUE
)
change_seasons_as_years(
  dir = "C:/git/PacificCommunity/ofp-sam/ofp-sam-skj-2022-synthesis/model_input/byseason",
  natural_mortality_at_age = age,
  maturity_at_age = maturity
  )
