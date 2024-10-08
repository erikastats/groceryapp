# app/view/analytics.R

box:: use(
  shiny[NS, moduleServer, tagList, icon, uiOutput, renderUI, br, h6],
  shinyWidgets[airDatepickerInput, pickerInput, checkboxGroupButtons,
               switchInput],
  bslib[page_sidebar, sidebar, layout_columns, value_box,
        card, card_header, card_body],
  dplyr[filter]

)

box::use(
  app/logic/importing_data[filters_choices]
)

#' Analytics page (UI)
#'
#' A Shiny module containing analysis of grocery data
#'
#' @param id id
#' @return ui for Analytics groceries page with several
#'  insights about grocery data
#' @export

ui <- function(id){
  ns <- NS(id)
  tagList(
    page_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        airDatepickerInput(
          inputId = ns("date_filter"),
          label = "Select range of dates:",
          range = TRUE,
          inline = TRUE,
          value = c(filters_choices$date_range$min_date,
                    Sys.Date())
        ),
        pickerInput(
          inputId = ns("supermarket_filter"),
          label = "Select a supermarket",
          choices = filters_choices$supermarkets,
          options = list(
            title = "Nothing selected",
            size = 5),
          multiple = TRUE
        ),
        pickerInput(
          inputId = ns("category_filter"),
          label = "Select a category",
          choices = unique(filters_choices$product_category$product_category),
          options = list(
            title = "Nothing selected",
            size = 5),
          multiple = TRUE
        ),
        uiOutput( ns("subcategory_filter")),
        br(),
        h6("Select favorite products:"),
        switchInput(
          inputId = ns("favorite_filter"),
          label = icon("star")
        )
      ),
      layout_columns(

      )
    )
  )
}

#' Analytics page (server)
#'
#' @param id id
#' @param groc_data groceries overview data
#' @return server for Analytics page
#' @export
server <-  function(id){
  moduleServer(id, function(input, output, session){

    ns <- NS(id)

  # output ------------------------------------------------------------------
  output$subcategory_filter <- renderUI({

    data = filters_choices$product_category

    if (is.null(input$category_filter)){
      data = data |>
        filter(product_category %in% input$category_filter)
    }
    subcategory_choices = data$product_subcategory |> unique()

    pickerInput(
      inputId = ns("subcategory_filter2"),
      label = "Select a subcategory",
      choices = subcategory_choices,
      options = list(
        title = "Nothing selected",
        size = 5),
      multiple = TRUE
    )

  })



  })
}
