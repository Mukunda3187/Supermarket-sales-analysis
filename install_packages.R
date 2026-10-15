# Run this file ONCE to install every package the app needs.
# In RStudio: open this file and click "Source" (top-right of the editor).

packages <- c("shiny", "bslib", "ggplot2", "dplyr", "DT",
              "rpart", "rpart.plot", "readr", "scales")

missing <- packages[!packages %in% rownames(installed.packages())]

if (length(missing) == 0) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
message("Done. You can now run the app with  shiny::runApp()")
