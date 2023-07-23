# run.R

my_packages <- c("data.table", "shiny", "shinyWidgets", "tools")
install_if_missing <- function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))

library(shiny)
port <- Sys.getenv("PORT")
shiny::runApp(
  appDir = getwd(),
  host = "0.0.0.0",
  port = as.numeric(port)
)
