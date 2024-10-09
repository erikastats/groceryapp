# app/main.R

box::use(
  bslib[page_navbar, nav_panel, nav_menu, bs_theme],
  shiny[NS, tagList, moduleServer, br, icon ]
)

box::use(
  app/view/overview,
  app/logic/importing_data,
  app/logic/constant[grocery_app_theme],
  app/view/grocery_page,
  app/view/product_page
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
      id = ns("navbar_id"),
      title = "Grocery analytics",

      theme = grocery_app_theme,
      nav_panel("Overview",
                icon = icon("magnifying-glass-dollar"),
                overview$ui(ns("overview_page"),
                            importing_data$total_groceries,
                            importing_data$latest_grocery,
                            importing_data$value_lg
                            )
                ),
      nav_panel("Groceries Analytics",
                icon = icon("chart-line"),
                grocery_page$ui(ns("analytics_page"))
                ),
      nav_panel("Product Analytics",
                icon = icon("carrot"),
                product_page$ui(ns("product_page")))

  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {



# modules -----------------------------------------------------------------
    overview$server(id = "overview_page",
                    groc_data = importing_data$grocery_by_day_super,
                    calendar_data = importing_data$grocery_by_day
                    )
    grocery_page$server(id = "analytics_page")
    product_page$server(id = "product_page")

  })
}
