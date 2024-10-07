# app/view/overview.R

box:: use(
  shiny[NS, moduleServer, tagList, icon],
  bslib[card, card_header, card_body, layout_columns, value_box],
  reactable[reactable, renderReactable, reactableOutput]
)

#' Overview page (UI)
#'
#' A Shiny module containing overview of registered groceries
#'
#' @param id id
#' @param total_groceries total number of registered groceries
#' @param latest_grocery date of the latest grocery made
#' @param value_lg  total value spent on the latest grocery
#' @return ui for Overview page with total groceries, latest grocery, value of the latest grocery,
#' table with groceries details and graph with frequency of groceries
#' @export

ui <- function(id, total_groceries, latest_grocery, value_lg){
  ns <- NS(id)
  tagList(
    layout_columns(
      value_box(
        title = "Total number of registered groceries",
        value = total_groceries,
        showcase = icon("basket-shopping")
      ),
      value_box(
        title = "Latest grocery",
        value = latest_grocery,
        showcase = icon("cart-shopping")
      ),
      value_box(
        title = "Last total spent",
        value = value_lg,
        showcase = icon("money-check-dollar")
      )
  ),
  layout_columns(
    card(
      card_header("General groceries"),
      reactableOutput(ns("groc_table"))
    ),
    card(
      card_header("Grocery frequency")
      # Add googleVis plot
    )

  )
  )
}

#' Athletes page (server)
#'
#' @param id id
#' @param groc_data groceries overview data
#' @return server for overview page
#' @export
#'

server <- function(id, groc_data){
  moduleServer(id, function(input, output, session){


# output ------------------------------------------------------------------

       output$groc_table <- renderReactable({
         groc_data |>
           reactable()
       })
  })
}
