#app/logic/constant.R

box::use(
  bslib[bs_theme, font_google]
)

#' @export
# A bs_theme with the colors

grocery_app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  bg = "#f8f9fa",                # Light background color
  fg = "#333333",                # Darker foreground for text
  primary = "#28a745",           # Green primary color
  secondary = "#f3969a",         # Amber secondary color
  success = "#198754",           # Darker green for success messages
  base_font = font_google("Roboto"),  # Google font for base text
  code_font = font_google("Fira Code")
)
