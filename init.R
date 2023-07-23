# init.R

helpers.installPackages(
  "tools", "shiny", "data.table", "BH"
)
install.packages("anytime_0.3.9.tar.gz", repos = NULL, type = "source")
helpers.installPackages(
  "shinyWidgets"
)
