# Starts the app. Open this file in RStudio and click "Source",
# or type  source("run_app.R")  in the Console.
# Make sure your working directory is the project folder (see README.md, Step 5).

if (!file.exists("app.R")) {
  stop("app.R not found. In RStudio use Session > Set Working Directory > To Source File Location, ",
       "or open the .Rproj file / the project folder first.")
}
shiny::runApp(".", launch.browser = TRUE)
