# app/logic/importing_data.R

box::use(
  dplyr[filter, summarise, mutate, group_by,
        pull, distinct, arrange],
  tidyr[replace_na]
)

grocery_table <-  readRDS("app/data/grocery_table.rds") |>
  mutate(product_discount = product_discount |>
                            replace_na(0) ) |>
  mutate(value_total = product_quantity*(product_value - product_discount))
product_table <- readRDS("app/data/product_table.rds")

#' @export
grocery_by_day_super <- grocery_table |>
  group_by(grocery_day, supermarket_name) |>
  summarise(value_grocery = value_total |> sum())

#' @export
grocery_by_day <- grocery_by_day_super |>
  group_by(grocery_day) |>
  summarise(value_grocery = value_grocery |> sum())

#' @export
total_groceries <-  grocery_by_day_super |> nrow()

#' @export
latest_grocery <- grocery_by_day |>
                    summarise(max_date = grocery_day |> max()) |>
                    pull(max_date)
#' @export
value_lg <-  grocery_by_day |>
  filter(grocery_day == max(grocery_day)) |>
  pull(value_grocery)

#' @export
filters_choices <- list(
  product_category = product_table |>
                      distinct(product_category, product_subcategory) |>
                      arrange(product_category),
  date_range = grocery_table |>
                      summarise(max_date = grocery_day |> max(),
                                min_date = grocery_day |> min()),
  supermarkets = grocery_table |>
                      distinct(supermarket_name) |>
                      arrange() |>
                      pull()
)
