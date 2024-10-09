# app/view/valuebox_analytics

box::use(
  shiny[NS, moduleServer, tagList, icon, uiOutput, renderUI],
  bslib[layout_columns, value_box]
)

#' Value box module (UI)
#'
#' A Shiny module containing three value boxes for the Analytics page
#'
#' @param id id
#' @return ui for value box module
#' @export

ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_columns(
      uiOutput(ns("valuebox1")),
      uiOutput(ns("valuebox2")),
      uiOutput(ns("valuebox3"))
    )
  )
}

#' Value box module  (server)
#'
#' @param id id
#' @param groc_data groceries overview data
#' @return server for value box module
#' @export

server <- function(id,
                   avg_grocery,
                   grocery_trips,
                   avg_grocery_period,
                   period_f){
  moduleServer(id, function(input, output, session){


  # output ------------------------------------------------------------------
  output$valuebox1 <- renderUI({
    value_box(
      title = "Avg. Grocery Spend",
      value = avg_grocery(),
      showcase = icon("money-check-dollar")
    )
  })

  output$valuebox2 <- renderUI({
    value_box(
      title = paste0("Avg. Grocery Trips ", period_f(),"ly") ,
      value = grocery_trips(),
      showcase = icon("clock-rotate-left")
    )
  })

  output$valuebox3 <- renderUI({
    value_box(
      title = paste0("Avg. Grocery Spend ", period_f(),"ly"),
      value = avg_grocery_period(),
      showcase = icon("money-bill-wave")
    )
  })

  })
}
