library(dplyr)
library(palmerpenguins)

# Prepare penguin data
penguins_clean <- palmerpenguins::penguins |>
  filter(
    !is.na(body_mass_g),
    !is.na(flipper_length_mm),
    !is.na(bill_length_mm)
  ) |>
  select(
    species,
    island,
    bill_length_mm,
    bill_depth_mm,
    flipper_length_mm,
    body_mass_g
  )

# Save the prepared dataset to the package
usethis::use_data(
  penguins_clean,
  overwrite = TRUE
)