library(vital)
library(tsibble)
library(dplyr)
library(ggplot2)

# HMD Norwegian data as downloaded from mortality.org on 21 April 2024
nor <- read_hmd_files(c("Population.txt", "Mx_1x1.txt")) |>
  mutate(
    Mortality = if_else(is.na(Mortality) & Population == 0, 0, Mortality)
  ) |>
  filter(Sex != "Total") |>
  collapse_ages(max_age = 100)
nor

index_var(nor)
key_vars(nor)
vital_vars(nor)

nor |>
  autoplot(Mortality) +
  scale_y_log10()

nor |>
  mutate(Population = if_else(Sex == "Female", -Population, Population)) |>
  autoplot(Population) +
  coord_flip() +
  facet_grid( . ~ Sex, scales = "free_x")


# Life tables for males and females in Norway
life_table(nor)


## Life expectancy
nor |>
  life_expectancy() |>
  ggplot(aes(x = Year, y = ex, color = Sex)) +
  geom_line()

# Smoothed data
sm_nor <- nor |>
  smooth_mortality(Mortality)
sm_nor |>
  filter(Year == 1967) |>
  autoplot(Mortality) +
  geom_line(aes(y = .smooth), col = "blue") +
  ylab("Mortality rate") +
  scale_y_log10()


# Lee-Carter model
lc <- nor |>
  model(lee_carter = LC(log(Mortality)))
lc
lc |>
  filter(Sex == "Female") |>
  report()

autoplot(lc)
age_components(lc)
time_components(lc)
lc |> forecast(h = 20)

# FDM model
fdm <- nor |>
  smooth_mortality(Mortality) |>
  model(hu = FDM(log(.smooth)))
fc_fdm <- fdm |>
  forecast(h = 20)
autoplot(fc_fdm) +
  scale_y_log10()

fdm |>
  autoplot(show_order = 3)

age_components(fdm)
time_components(fdm)


## Coherent model

fdm_coherent <- nor |>
  smooth_mortality(Mortality) |>
  make_pr(.smooth) |>
  model(hby = FDM(log(.smooth), coherent = TRUE))
fc_coherent <- fdm_coherent |>
  forecast(h = 20) |>
  undo_pr(.smooth)
fc_coherent
autoplot(fc_coherent) + scale_y_log10()

