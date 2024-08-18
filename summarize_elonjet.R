library(tidyverse)
library(rtoot)

rtoot::auth_setup()

elonjet_acct <-
  search_accounts('mastodon.social/@elonjet') |>
  slice(1) |>
  as.list()

elonjet_acct$id

statuses <-
  rtoot::get_account_statuses(id = elonjet_acct$id, max_id = NULL, limit = 99999) |>
  glimpse()

flights <-
  statuses |>
  select(id, uri, created_at, content) |>
  filter(str_detect(tolower(content), 'fuel|litres|emissions'))

flight_data <-
  flights |>
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
                .names = '{.col}_{.fn}')) |>
  glimpse()

