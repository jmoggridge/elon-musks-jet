library(tidyverse)
library(rtoot)
library(scales)
rtoot::auth_setup(instance = 'vis.social', type = 'user')

# get all statuses from @elonjet
elonjet_toots <-
  search_accounts('mastodon.social/@elonjet') |>
  slice(1) |>
  pluck('id') |>
  rtoot::get_account_statuses(id = _, limit = 9999999)

# parse numbers from flight data statuses
flights <- elonjet_toots |>
  select(id, uri, created_at, content) |>
  filter(str_detect(tolower(content), 'fuel|litres|emissions')) |>
  arrange(created_at) |>
  mutate(
    txt = content |> tolower() |> str_remove_all('<.*?>|~|,'),
    weight = str_extract(txt, '[0-9]+(?= kg)') |> parse_number(),
    volume = str_extract(txt, '[0-9]+(?= liters)') |> parse_number(),
    cost = str_extract(txt, '\\$[0-9]+') |> parse_number(),
    emissions = str_extract(txt, '[0-9]+(?= tons of)') |> parse_number()
  ) |>
  mutate(across(where(is.numeric),
                .f = list('cumsum' = cumsum),
                .names = '{.col}_{.fn}'))

flights$created_at |> min() |> as_date()

# summarise flights for written data
flight_summary <- flights |>
  slice(nrow(flights)) |>
  as.list() |>
  append(lst(
      n_flights = nrow(flights),
      first = min(flights$created_at) |> as_date(),
      days_ago = as.character(Sys.Date() - first) |> parse_number(),
      daily_emissions = max(flights$emissions_cumsum) / days_ago,
      daily_volume = max(flights$volume_cumsum) / days_ago,
    ))

summary1 <- flight_summary |>
  glue::glue_data(
    "Since {first} ({days_ago} days ago), Elon's jet flown at least {n_flights} times, using a total of ~{comma(volume_cumsum)} L ({comma(weight_cumsum)}) of fuel, costing ~${comma(cost_cumsum)}, and creating ~{comma(emissions_cumsum)} tons of CO2 emissions. On a daily basis, Elon's jet uses an average of {comma(daily_volume)} L of fuel and generates {round(daily_emissions, 1)} tons of CO2. For reference, the average American family creates 7.5 tons of CO2 emissions per year, which is only {signif(100* (7.5/365) / daily_emissions, 2)}% of the emissions of Elon's private jet. Elon Musk's flying emits roughly the same amount of CO2 as {round(daily_emissions / (7.5/365))} average American households combined."
  )

print(summary1)
