library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)
# devtools::install_github("coolbutuseless/devout")
# devtools::install_github("coolbutuseless/devoutaudio")
library(devoutaudio)

ipf_lifts <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv"
)

ipf_to_use <- ipf_lifts %>%
  dplyr::filter(
    event != "SB",
    !is.na(age_class),
    place == 1,
    !is.na(bodyweight_kg),
    !is.na(best3bench_kg)
  ) %>%
  dplyr::select(
    sex,
    age_class,
    bodyweight_kg,
    best3bench_kg,
    date
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    year = lubridate::year(date)
  ) %>%
  dplyr::group_by(year, sex) %>%
  dplyr::mutate(
    highest_bench = max(best3bench_kg)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(best3bench_kg == highest_bench) %>%
  dplyr::select(-best3bench_kg, -date)

dplyr::distinct(ipf_to_use, age_class)
dplyr::distinct(ipf_to_use, year)
dplyr::glimpse(ipf_to_use)

# General plan:
# Men will be low notes, women high notes.
# Each year = 0.5 seconds. That took too long, playing with that in year_pause.
# Pitch is based on age_class that "won" for that sex.
# Higher of the two = full duration, lower = lower/higher*full?
year_pause <- 0.2
sex_age_map <- ipf_to_use %>%
  dplyr::distinct(sex, age_class) %>%
  dplyr::arrange(sex, age_class) %>%
  dplyr::mutate(
    note = c(
      6:9/10,
      1:4/10
    )
  )

ipf_to_use %>%
  dplyr::select(-bodyweight_kg) %>%
  tidyr::pivot_wider(
    names_from = sex,
    values_from = c(age_class, highest_bench),
    values_fn = list(age_class = list, highest_bench = unique)
  ) %>%
  dplyr::select(-year) %>%
  purrr::pwalk(function(...) {
    # Sometimes there's no F in the list, but there's always M. F is never
    # higher than M.
    M_note <- sex_age_map %>%
      dplyr::filter(sex == "M", age_class %in% ..1) %>%
      dplyr::pull(note)
    purrr::walk(M_note, function(this_note) {
      devoutaudio::play_ping(
        lr_fraction = 0.5,
        y_fraction = this_note,
        duration = year_pause
      )
    })

    if (!is.na(..4)) {
      F_note <- sex_age_map %>%
        dplyr::filter(sex == "F", age_class %in% ..2) %>%
        dplyr::pull(note)
      purrr::walk(F_note, function(this_note) {
        devoutaudio::play_ping(
          lr_fraction = 0.5,
          y_fraction = this_note,
          duration = year_pause*(..4/..3)
        )
      })
    }
    Sys.sleep(year_pause)
  })
