# app/logic/importing_data.R

box::use(
  dplyr[filter, summarise, mutate, group_by,
        pull, distinct, arrange],
  tidyr[replace_na],
  lubridate[day, year, month]
)

#' @export
#'  Raw grocery table
grocery_table <-  readRDS("app/data/grocery_table.rds") |>
  mutate(product_discount = product_discount |>
                            replace_na(0) ) |>
  mutate(value_total = product_quantity*(product_value - product_discount))

#' @export
#' Raw product table
product_table <- readRDS("app/data/product_table.rds")

#' @export
#' Table with grocery grouped by day and supermarket
grocery_by_day_super <- grocery_table |>
  group_by(grocery_day, supermarket_name) |>
  summarise(value_grocery = value_total |> sum()) |>
  mutate(Month = grocery_day |> month(),
         Year = grocery_day |> year(),
         Week = ceiling(day(grocery_day) / 7)
         )

#' @export
#' Table with grocery grouped by day
grocery_by_day <- grocery_by_day_super |>
  group_by(grocery_day) |>
  summarise(value_grocery = value_grocery |> sum())

#' @export
#' Number total of groceries
total_groceries <-  grocery_by_day_super |> nrow()

#' @export
#' Latest grocery date
latest_grocery <- grocery_by_day |>
                    summarise(max_date = grocery_day |> max()) |>
                    pull(max_date)

#' @export
#' Value of the amount spent on the latest grocery
value_lg <-  grocery_by_day |>
  filter(grocery_day == max(grocery_day)) |>
  pull(value_grocery)

#' @export
#' List with the choices of the filters to be used on Analytics page
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

#' @export
#' Value of the average total amount spent on all groceries
