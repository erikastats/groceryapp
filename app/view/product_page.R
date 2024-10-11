# app/view/product_page

box::use(
  shiny[NS, moduleServer, tagList, icon, uiOutput, renderUI,
        reactive],
  shinyWidgets[airDatepickerInput, pickerInput],
  bslib[layout_columns, value_box, card, card_header,
        page_sidebar, sidebar],
  lubridate[floor_date],
  dplyr[filter, arrange, desc, summarise, group_by, mutate, ungroup],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  utils[head, tail]
)

box::use(
  app/logic/importing_data[filters_choices, grocery_table]
)

#' Product page (UI)
#'
#' A Shiny module containing analysis of products an their
#' categories from the grocery data#'
#' @param id id
#' @return ui for product page
#' @export

ui <-  function(id){
  ns <- NS(id)
  tagList(
    page_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        airDatepickerInput(
          inputId = ns("datestart_filter"),
          label = "Select start date:",
          value = c(floor_date(filters_choices$date_range$max_date, unit = "year"))
        ),
        airDatepickerInput(
          inputId = ns("dateend_filter"),
          label = "Select end date:",
          value = c(filters_choices$date_range$max_date)
        ),
        pickerInput(
          inputId = ns("supermarket_filter"),
          label = "Select a supermarket",
          choices = filters_choices$supermarkets,
          # selected = c("Tesco", "Lidl"),
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
        uiOutput( ns("subcategory_filter"))
      ),
      layout_columns(
        card(
        card_header("Top 15 most consumed products"),
        plotlyOutput(ns("topproducts"))
      ),
      card(
        card_header("Top 10 most expensive products"),
        plotlyOutput(ns("topexpensive"))
      ),
      card(
        card_header("Top 10 most affortable products"),
        plotlyOutput(ns("topaffortable"))
      )
      )

    )
  )
}



#' Product page (server)
#'
#' @param id id
#' @return server for Product page
#' @export

server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- NS(id)


  # data --------------------------------------------------------------------
  grocery_reactive <- reactive({
    #filtering date
    data = grocery_table |>
      filter(grocery_day >= input$datestart_filter &
               grocery_day <= input$dateend_filter)
    #filtering supermarket
    if (!is.null(input$supermarket_filter)){
      data = data |>
        filter(supermarket_name %in% input$supermarket_filter)
    }
    #filtering product category
    if (!is.null(input$category_filter)){
      data = data |>
        filter(product_category %in% input$category_filter)
    }
    #filtering product subcategory
    if (!is.null(input$subcategory_filter2)){
      data = data |>
        filter(product_subcategory %in% input$subcategory_filter2)
    }

    data
  })



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

    output$topproducts <- renderPlotly({

      data = grocery_reactive() |>
        group_by(product_name) |>
        summarise(prod_quant_total = sum(product_quantity, na.rm = TRUE),
                  prod_value_total = sum(value_total)) |>
        arrange(prod_quant_total) |>
        ungroup() |>
        mutate(product_name = factor(product_name, levels = product_name))

      data |>
        tail(15) |>
        plot_ly(x = ~prod_quant_total, y = ~product_name,
                type = "bar", orientation = "h") |>
        layout( xaxis = list(title = "Total Quantity"),
                yaxis = list(title = "Products"))

    })

    output$topexpensive <- renderPlotly({

      data = grocery_reactive() |>
        group_by(product_name) |>
        summarise(
                  prod_value_mean = mean(product_value, na.rm = TRUE)) |>
        arrange(prod_value_mean) |>
        ungroup() |>
        tail(10) |>
        mutate(product_name = factor(product_name, levels = product_name))

      data |>
        plot_ly(x = ~prod_value_mean, y = ~product_name,
                type = "bar", orientation = "h") |>
        layout( xaxis = list(title = "Average Value"),
                yaxis = list(title = "Products"))
    })

    output$topaffortable <- renderPlotly({
      data = grocery_reactive() |>
        group_by(product_name) |>
        summarise(
          prod_value_mean = mean(product_value, na.rm = TRUE)) |>
        arrange(prod_value_mean) |>
        ungroup() |>
        head(10) |>
        mutate(product_name = factor(product_name, levels = product_name))

      data |>
        plot_ly(x = ~prod_value_mean, y = ~product_name,
                type = "bar", orientation = "h") |>
        layout( xaxis = list(title = "Average Value"),
                yaxis = list(title = "Products"))
    })

  })
}

