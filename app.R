# ==============================================================================
#  SUPERMARKET SALES DATA ANALYSIS AND CUSTOMER INSIGHTS USING R (SHINY)
#  --------------------------------------------------------------------------
#  Run this app by opening this file in RStudio and clicking "Run App",
#  or by typing  shiny::runApp()  in the R console (see README.md).
#
#  File layout of this script (search for the "SECTION" headings):
#    SECTION 1  Packages, constants, colours
#    SECTION 2  Data loading, validation and cleaning
#    SECTION 3  Helper functions (formatting, summaries, chart helpers)
#    SECTION 4  User interface (sidebar + 7 pages)
#    SECTION 5  Server logic (data, filters, pages, models, downloads)
#    SECTION 6  Start the app
# ==============================================================================


# ==============================================================================
# SECTION 1 - PACKAGES, CONSTANTS, COLOURS
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(DT)
  library(rpart)
  library(scales)
})

# rpart.plot draws the decision tree. If it is missing, the app still starts and
# falls back to a basic tree drawing (see the Decision Tree page).
HAS_RPART_PLOT <- requireNamespace("rpart.plot", quietly = TRUE)

options(shiny.maxRequestSize = 30 * 1024^2)   # allow uploads up to 30 MB

# The four theme colours requested for the project
COL <- list(
  green  = "#4F7D32",   # primary green
  gold   = "#FFD16A",   # light golden yellow
  yellow = "#F5B52E",   # bright yellow
  red    = "#D9361E",   # red-orange
  ink    = "#26331F",
  muted  = "#5F6B58"
)
# Palette for charts that need several categories (tints/shades of the theme)
CAT_PAL <- c("#4F7D32", "#F5B52E", "#8DB36B", "#D9361E", "#FFD16A", "#A8461F", "#2F4D1C", "#C9A227")

# Where to look for the default dataset (relative paths only)
DEFAULT_FILES <- c(
  file.path("data", "SuperMarket Analysis.csv"),
  file.path("data", "SuperMarket Analysis(1).csv"),
  file.path("data", "SuperMarket_Analysis.csv")
)

# Columns the app understands. Each entry: cleaned name, accepted spellings of
# the original header (lower-case, letters/digits only), type, and whether the
# column is required.  "Sales" also accepts "Total" (some versions of this
# dataset call it Total).
COLUMN_SPEC <- list(
  Invoice_ID       = list(keys = c("invoiceid", "invoice", "invoiceno"),            type = "text",    required = FALSE),
  Branch           = list(keys = c("branch"),                                        type = "text",    required = TRUE),
  City             = list(keys = c("city"),                                          type = "text",    required = TRUE),
  Customer_type    = list(keys = c("customertype", "customer"),                      type = "text",    required = TRUE),
  Gender           = list(keys = c("gender", "sex"),                                 type = "text",    required = TRUE),
  Product_line     = list(keys = c("productline", "product"),                        type = "text",    required = TRUE),
  Unit_price       = list(keys = c("unitprice", "price"),                            type = "numeric", required = TRUE),
  Quantity         = list(keys = c("quantity", "qty"),                               type = "numeric", required = TRUE),
  Tax_5pct         = list(keys = c("tax5", "tax", "tax5pct"),                        type = "numeric", required = FALSE),
  Sales            = list(keys = c("sales", "total"),                                type = "numeric", required = TRUE),
  Date             = list(keys = c("date"),                                          type = "date",    required = TRUE),
  Time             = list(keys = c("time"),                                          type = "text",    required = FALSE),
  Payment          = list(keys = c("payment", "paymentmethod"),                      type = "text",    required = TRUE),
  COGS             = list(keys = c("cogs"),                                          type = "numeric", required = FALSE),
  Gross_margin_pct = list(keys = c("grossmarginpercentage", "grossmargin"),          type = "numeric", required = FALSE),
  Gross_income     = list(keys = c("grossincome"),                                   type = "numeric", required = TRUE),
  Rating           = list(keys = c("rating"),                                        type = "numeric", required = TRUE)
)

# Friendly names for the filter / grouping choices
GROUP_CHOICES <- c(
  "Branch" = "Branch", "City" = "City", "Product line" = "Product_line",
  "Payment method" = "Payment", "Customer type" = "Customer_type",
  "Gender" = "Gender", "Month" = "Month"
)

`%||%` <- function(a, b) if (is.null(a)) b else a


# ==============================================================================
# SECTION 2 - DATA LOADING, VALIDATION AND CLEANING
# ==============================================================================

# Turn a column header into a simple key: "Tax 5%" -> "tax5", "Unit price" -> "unitprice"
header_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Try several common date formats and keep the one that parses the most values.
parse_dates <- function(x) {
  formats <- c("%m/%d/%Y", "%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y",
               "%Y/%m/%d", "%d.%m.%Y", "%m/%d/%y", "%d/%m/%y")
  best <- rep(as.Date(NA), length(x)); best_n <- -1; best_fmt <- NA_character_
  for (f in formats) {
    p <- suppressWarnings(as.Date(x, format = f))
    n <- sum(!is.na(p))
    if (n > best_n) { best <- p; best_n <- n; best_fmt <- f }
  }
  list(date = best, format = best_fmt, parsed = best_n)
}

# Read, validate and clean a CSV file. Never stops with an R error: it returns
# either list(error = "message for the user") or the cleaned data and a report.
load_dataset <- function(path, label = basename(path)) {

  # ---- 1. file exists? ----
  if (is.null(path) || length(path) == 0 || !file.exists(path)) {
    return(list(error = paste0(
      "Dataset file not found. Put your CSV in the 'data' folder of the project ",
      "(for example data/SuperMarket Analysis.csv), or upload a CSV on the Dataset Explorer page.")))
  }

  # ---- 2. read everything as text first so nothing is lost silently ----
  raw <- tryCatch(
    readr::read_csv(path,
                    col_types   = readr::cols(.default = readr::col_character()),
                    na          = c("", "NA", "N/A", "n/a", "NULL", "null"),
                    show_col_types = FALSE, progress = FALSE, name_repair = "minimal"),
    error = function(e) e)
  if (inherits(raw, "error")) {
    return(list(error = paste0("The file '", label, "' could not be read as a CSV file. Details: ",
                               conditionMessage(raw))))
  }
  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(raw) == 0 || ncol(raw) < 3) {
    return(list(error = paste0("The file '", label, "' looks empty or is not a comma-separated table ",
                               "(found ", nrow(raw), " rows and ", ncol(raw), " columns).")))
  }
  names(raw) <- trimws(gsub("﻿", "", names(raw), fixed = TRUE))
  # trim spaces and treat blank text as missing
  raw[] <- lapply(raw, function(x) { x <- trimws(x); x[!is.na(x) & x == ""] <- NA; x })
  n_original <- nrow(raw)

  # ---- 3. map the real column names to the names used in this app ----
  keys <- header_key(names(raw))
  used <- rep(FALSE, length(keys))
  map  <- setNames(rep(NA_integer_, length(COLUMN_SPEC)), names(COLUMN_SPEC))
  for (nm in names(COLUMN_SPEC)) {
    hit <- which(keys %in% COLUMN_SPEC[[nm]]$keys & !used)
    if (length(hit) > 0) {
      # prefer the spelling listed first (e.g. "Sales" before "Total")
      hit <- hit[order(match(keys[hit], COLUMN_SPEC[[nm]]$keys))][1]
      map[nm] <- hit; used[hit] <- TRUE
    }
  }
  required <- names(COLUMN_SPEC)[vapply(COLUMN_SPEC, function(s) s$required, logical(1))]
  missing_req <- required[is.na(map[required])]
  if (length(missing_req) > 0) {
    return(list(error = paste0(
      "The file '", label, "' is missing required column(s): ",
      paste(gsub("_", " ", missing_req), collapse = ", "), ". ",
      "Columns found in the file: ", paste(names(raw), collapse = ", "), ".")))
  }

  # ---- 4. build the working data frame (cleaned column names) ----
  present <- names(map)[!is.na(map)]
  df <- raw[, map[present], drop = FALSE]
  names(df) <- present
  # keep any extra, unrecognised columns too (nothing is thrown away)
  extra_idx <- which(!used)
  if (length(extra_idx) > 0) {
    extra <- raw[, extra_idx, drop = FALSE]
    names(extra) <- make.unique(make.names(names(extra)), sep = "_")
    names(extra) <- ifelse(names(extra) %in% names(df), paste0(names(extra), "_extra"), names(extra))
    df <- cbind(df, extra)
  }

  # ---- 5. cleaning steps, each one counted ----
  steps <- data.frame(Step = "Original records read from the file",
                      Rows_removed = 0L, Rows_remaining = n_original, stringsAsFactors = FALSE)
  add_step <- function(label, before, after) {
    steps[nrow(steps) + 1, ] <<- list(label, as.integer(before - after), as.integer(after))
  }

  # (a) exact duplicate rows
  dup <- duplicated(df)
  n_dup <- sum(dup)
  df <- df[!dup, , drop = FALSE]
  add_step("Exact duplicate rows removed", n_original, nrow(df))

  # (b) convert types; remember how many cells could not be converted
  invalid_cells <- data.frame(Column = character(), Invalid_values = integer(), stringsAsFactors = FALSE)
  date_format <- NA_character_
  for (nm in present) {
    sp <- COLUMN_SPEC[[nm]]
    if (sp$type == "numeric") {
      before_na <- is.na(df[[nm]])
      conv <- suppressWarnings(as.numeric(gsub(",", "", df[[nm]], fixed = TRUE)))
      conv[is.infinite(conv)] <- NA
      bad <- sum(is.na(conv) & !before_na)
      if (bad > 0) invalid_cells[nrow(invalid_cells) + 1, ] <- list(nm, bad)
      df[[nm]] <- conv
    } else if (sp$type == "date") {
      before_na <- is.na(df[[nm]])
      pd <- parse_dates(df[[nm]])
      bad <- sum(is.na(pd$date) & !before_na)
      if (bad > 0) invalid_cells[nrow(invalid_cells) + 1, ] <- list(nm, bad)
      df[[nm]] <- pd$date
      date_format <- pd$format
    }
  }
  # tidy the capitalisation of two categorical columns
  fix_case <- function(x) ifelse(is.na(x), NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
  df$Customer_type <- fix_case(df$Customer_type)
  df$Gender        <- fix_case(df$Gender)

  # (c) rows missing a required value (includes values that could not be converted)
  n_before <- nrow(df)
  bad_rows <- rowSums(is.na(df[, required, drop = FALSE])) > 0
  df <- df[!bad_rows, , drop = FALSE]
  add_step("Rows with a missing or unreadable required value removed", n_before, nrow(df))

  # (d) impossible values
  n_before <- nrow(df)
  ok <- df$Quantity > 0 & df$Unit_price > 0 & df$Sales >= 0 & df$Rating >= 0
  df <- df[ok, , drop = FALSE]
  add_step("Rows with non-positive quantity/price or negative sales/rating removed", n_before, nrow(df))

  if (nrow(df) == 0) {
    return(list(error = paste0("After cleaning, no valid rows were left in '", label,
                               "'. Check that the numeric columns and the Date column contain valid values.")))
  }
  rownames(df) <- NULL

  # ---- 6. summaries for the Dataset Explorer page ----
  n_missing_raw <- vapply(raw, function(x) sum(is.na(x)), integer(1))
  missing_tbl <- data.frame(Column = names(raw), Missing_values = as.integer(n_missing_raw),
                            Percent_missing = round(100 * n_missing_raw / n_original, 2),
                            stringsAsFactors = FALSE, row.names = NULL)

  type_of <- function(x) if (inherits(x, "Date")) "Date" else if (is.numeric(x)) "Number" else "Text"
  mapping_tbl <- data.frame(
    Original_column = names(raw),
    Used_as = vapply(seq_along(raw), function(i) {
      m <- names(map)[which(map == i)]
      if (length(m) == 1) m else "(kept as extra column)"
    }, character(1)),
    stringsAsFactors = FALSE)
  mapping_tbl$Type_after_cleaning <- vapply(mapping_tbl$Used_as, function(u) {
    if (u %in% names(df)) type_of(df[[u]]) else "Text"
  }, character(1))
  mapping_tbl$Required <- ifelse(mapping_tbl$Used_as %in% required, "Yes", "No")
  mapping_tbl$Missing_values_in_file <- as.integer(n_missing_raw)

  notes <- character()
  removed_total <- n_original - nrow(df)
  if (removed_total > 0.05 * n_original) {
    notes <- c(notes, sprintf("%.1f%% of the records (%d of %d) were removed during cleaning. Please check the data.",
                              100 * removed_total / n_original, removed_total, n_original))
  }
  if ("Invoice_ID" %in% names(df) && anyDuplicated(df$Invoice_ID) > 0) {
    notes <- c(notes, sprintf("%d Invoice ID values appear more than once (kept as they are).",
                              sum(duplicated(df$Invoice_ID))))
  }
  if (nrow(df) < 30) notes <- c(notes, "Fewer than 30 records remain; charts and models may not be meaningful.")
  sales_src <- names(raw)[map["Sales"]]

  list(error = NULL, data = df, label = label,
       n_original = n_original, n_clean = nrow(df), n_duplicates = n_dup,
       steps = steps, invalid_cells = invalid_cells, missing = missing_tbl,
       mapping = mapping_tbl, date_format = date_format, notes = notes,
       sales_source = sales_src)
}


# ==============================================================================
# SECTION 3 - HELPER FUNCTIONS
# ==============================================================================

num  <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
safe_div <- function(a, b) if (is.na(b) || b == 0) NA_real_ else a / b
fmt_or_na <- function(x, f = function(v) sprintf("%.3f", v)) if (is.na(x)) "N/A" else f(x)

# Summary table by one grouping column (used on several pages)
summarise_by <- function(df, by) {
  if (by == "Month") df$Month <- format(df$Date, "%Y-%m")
  df %>%
    group_by(across(all_of(by))) %>%
    summarise(Transactions = n(),
              Quantity     = sum(Quantity),
              Avg_sale     = mean(Sales),      # must come BEFORE Sales is replaced by its sum
              Avg_rating   = mean(Rating),
              Sales        = sum(Sales),
              Gross_income = sum(Gross_income),
              .groups = "drop") %>%
    select(all_of(by), Transactions, Quantity, Sales, Gross_income, Avg_sale, Avg_rating) %>%
    arrange(desc(Sales)) %>%
    as.data.frame()
}

# Interactive table helper
make_dt <- function(df, round_cols = NULL, digits = 2, page_length = 10, simple = FALSE) {
  opts <- list(pageLength = page_length, scrollX = TRUE,
               dom = if (simple) "t" else "frtip",
               language = list(emptyTable = "No rows to show"))
  w <- DT::datatable(df, rownames = FALSE, class = "stripe hover compact", options = opts)
  round_cols <- intersect(round_cols, names(df))
  if (length(round_cols) > 0) w <- DT::formatRound(w, round_cols, digits)
  w
}

# ---- ggplot helpers ----
theme_app <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title        = element_text(face = "bold", colour = COL$green),
          panel.grid.minor  = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title        = element_text(colour = COL$muted),
          axis.text         = element_text(colour = COL$ink),
          legend.position   = "bottom",
          legend.title      = element_blank(),
          plot.background   = element_rect(fill = "white", colour = NA))
}

# One-series bar chart; the tallest bar is highlighted in bright yellow
bar_plot <- function(d, x, y, ylab, horizontal = FALSE, digits = 0) {
  d <- as.data.frame(d)
  d <- d[order(d[[y]], decreasing = TRUE), , drop = FALSE]
  d$.hi  <- d[[y]] == max(d[[y]])
  d$.lab <- num(d[[y]], digits)
  d[[x]] <- factor(d[[x]], levels = if (horizontal) rev(unique(d[[x]])) else unique(d[[x]]))
  p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]], fill = .hi)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = c(`TRUE` = COL$yellow, `FALSE` = COL$green), guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = label_comma()) +
    labs(x = NULL, y = ylab) + theme_app()
  if (horizontal) {
    p + geom_text(aes(label = .lab), hjust = -0.1, size = 3.6, colour = COL$ink) + coord_flip()
  } else {
    p + geom_text(aes(label = .lab), vjust = -0.45, size = 3.6, colour = COL$ink)
  }
}

# Stacked bars: total per category, split by a second variable
stack_plot <- function(f, cat, fill, horizontal = FALSE) {
  d <- f %>% group_by(.data[[cat]], .data[[fill]]) %>% summarise(Sales = sum(Sales), .groups = "drop")
  tot <- d %>% group_by(.data[[cat]]) %>% summarise(Sales = sum(Sales), .groups = "drop") %>% arrange(desc(Sales))
  lv <- if (horizontal) rev(tot[[cat]]) else tot[[cat]]
  d[[cat]] <- factor(d[[cat]], levels = lv); tot[[cat]] <- factor(tot[[cat]], levels = lv)
  fl <- sort(unique(d[[fill]]))
  p <- ggplot(d, aes(x = .data[[cat]], y = Sales, fill = .data[[fill]])) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = setNames(CAT_PAL[seq_along(fl)], fl)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = label_comma()) +
    labs(x = NULL, y = "Total sales") + theme_app()
  if (horizontal) {
    p + geom_text(data = tot, aes(label = num(Sales), fill = NULL), hjust = -0.1, size = 3.6, colour = COL$ink) + coord_flip()
  } else {
    p + geom_text(data = tot, aes(label = num(Sales), fill = NULL), vjust = -0.45, size = 3.6, colour = COL$ink)
  }
}

# ---- small UI helpers ----
kpi_card <- function(label, value, hint = NULL, cls = "") {
  div(class = paste("kpi", cls),
      div(class = "kpi-label", label),
      div(class = "kpi-value", value),
      if (!is.null(hint)) div(class = "kpi-hint", hint))
}
dash_card <- function(title, ..., sub = NULL) {
  div(class = "dash-card",
      div(class = "dash-card-head",
          span(class = "dash-card-title", title),
          if (!is.null(sub)) span(class = "dash-card-sub", sub)),
      div(class = "dash-card-body", ...))
}
page_head <- function(title, subtitle) {
  div(class = "page-head", h2(title), p(subtitle))
}
page_panel <- function(id, ...) {
  conditionalPanel(condition = sprintf("input.page == '%s'", id), ...)
}
dl_btn <- function(id, label) {
  downloadButton(id, label, class = "btn-outline-primary btn-sm")
}

# Stratified train/test split: returns the row numbers for the TEST set
strat_split <- function(y, test_frac, seed) {
  set.seed(seed)
  unlist(lapply(split(seq_along(y), y), function(i) {
    n_test <- max(1, round(length(i) * test_frac))
    if (length(i) <= 1) return(i)
    i[sample.int(length(i), min(n_test, length(i) - 1))]
  }), use.names = FALSE)
}

csv_out <- function(x, file) {
  utils::write.csv(x, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}


# ==============================================================================
# SECTION 4 - USER INTERFACE
# ==============================================================================

NAV <- list(
  overview  = list("Overview Dashboard",  "chart-pie"),
  sales     = list("Sales Analysis",      "chart-line"),
  customers = list("Customer Insights",   "users"),
  logistic  = list("Logistic Regression", "percent"),
  kmeans    = list("K-Means Clustering",  "bullseye"),
  tree      = list("Decision Tree",       "sitemap"),
  explorer  = list("Dataset Explorer",    "table")
)

app_theme <- bs_theme(
  version = 5,
  primary = COL$green, secondary = COL$yellow, warning = COL$yellow, danger = COL$red,
  success = COL$green, info = COL$gold,
  bg = "#FBF9F1", fg = COL$ink,
  base_font = font_collection("Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
)

sidebar_ui <- sidebar(
  width = 300, bg = COL$green, fg = "#FFFFFF", open = "desktop",
  div(class = "brand",
      h1("Supermarket Sales Analysis"),
      p("Customer insights from transaction data, built with R Shiny")),
  div(class = "side-heading", "Pages"),
  div(class = "nav-radio",
      radioButtons("page", NULL, selected = "overview",
                   choiceValues = as.list(names(NAV)),
                   choiceNames  = unname(lapply(NAV, function(n) tagList(icon(n[[2]]), n[[1]]))))),
  div(class = "side-heading", "Filters"),
  div(class = "record-count", textOutput("n_selected", inline = TRUE)),
  uiOutput("filters_ui"),
  actionButton("reset_filters", "Reset filters", icon = icon("rotate-left"), class = "btn-gold")
)

ui <- page_sidebar(
  title = NULL, theme = app_theme, fillable = FALSE, sidebar = sidebar_ui,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("Supermarket Sales Analysis")
  ),
  uiOutput("data_status"),

  # ---------------------------------------------------------------- Page 1
  page_panel("overview",
    page_head("Overview Dashboard", "Key figures for the transactions selected by the filters."),
    uiOutput("ov_kpis"),
    uiOutput("ov_insights"),
    div(class = "grid-2",
        dash_card("Sales by branch", plotOutput("ov_branch", height = 290)),
        dash_card("Sales by product line", plotOutput("ov_product", height = 290))),
    div(class = "grid-2",
        dash_card("Weekly sales", sub = "Weeks start on Monday; first and last weeks may be partial",
                  plotOutput("ov_time", height = 290)),
        dash_card("Sales by payment method", plotOutput("ov_payment", height = 290)))
  ),

  # ---------------------------------------------------------------- Page 2
  page_panel("sales",
    page_head("Sales Analysis", "Compare sales performance across branches, cities, product lines and days."),
    div(class = "grid-2",
        dash_card("Sales by city (split by branch)", plotOutput("sa_city", height = 300),
                  sub = "Each bar shows total sales; colours show the branch"),
        dash_card("Sales by product line (split by branch)", plotOutput("sa_product", height = 300))),
    div(class = "grid-2",
        dash_card("Quantity sold by product line", plotOutput("sa_qty", height = 300)),
        dash_card("Daily sales trend", sub = "Thin line = each day, bold line = 7-day moving average",
                  plotOutput("sa_daily", height = 300))),
    dash_card("Revenue and gross income summary",
      div(class = "control-row",
          selectInput("sa_group", "Summarise by", choices = GROUP_CHOICES, selected = "Branch")),
      DTOutput("sa_table"),
      div(class = "dl-row", dl_btn("dl_sales_summary", "Download this summary (CSV)")))
  ),

  # ---------------------------------------------------------------- Page 3
  page_panel("customers",
    page_head("Customer Insights", "How Member and Normal transactions compare, by gender, rating and basket size."),
    div(class = "note-box",
        p(strong("Please note: "), "each row in the data is one transaction (invoice), not one person. The dataset has no customer ID, ",
          "so the figures below describe transactions made by Member / Normal customers, not individual customers' histories.")),
    uiOutput("ci_findings"),
    div(class = "grid-2",
        dash_card("Sales: Member vs Normal", plotOutput("ci_type", height = 280)),
        dash_card("Sales by gender and customer type", plotOutput("ci_gender", height = 280))),
    div(class = "grid-2",
        dash_card("Customer rating distribution", sub = "Dashed line = average rating", plotOutput("ci_rating", height = 280)),
        dash_card("Average rating by branch", plotOutput("ci_rating_branch", height = 280))),
    dash_card("Purchasing quantity distribution", sub = "Number of transactions for each quantity bought",
              plotOutput("ci_qty", height = 260)),
    dash_card("Customer type comparison",
      DTOutput("ci_table"),
      br(),
      p(class = "dash-card-sub", "By customer type and gender"),
      DTOutput("ci_table2"),
      div(class = "dl-row", dl_btn("dl_customer_summary", "Download customer comparison (CSV)")))
  ),

  # ---------------------------------------------------------------- Page 4
  page_panel("logistic",
    page_head("Logistic Regression", "Classify a transaction's customer type (Member = 1, Normal = 0) from basket and store details."),
    div(class = "note-box",
        p(strong("What this model does: "), "it uses ", code("glm(..., family = binomial())"),
          " to estimate the probability that a transaction was made by a Member, using Quantity, Unit price, Product line, ",
          "Branch, Gender and Payment method. It classifies existing transactions in this dataset; it does not predict future customer behaviour.")),
    dash_card("Model settings",
      div(class = "control-row",
          sliderInput("lr_thr", "Probability threshold (Member if probability ≥ threshold)", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
          sliderInput("lr_test", "Share of data used for testing (%)", min = 10, max = 40, value = 25, step = 5),
          numericInput("lr_seed", "Random seed", value = 123, min = 1, step = 1),
          checkboxInput("lr_use_filters", "Use only the transactions selected by the sidebar filters", value = FALSE))),
    uiOutput("lr_banner"),
    uiOutput("lr_kpis"),
    div(class = "grid-2",
        dash_card("Confusion matrix (test set)", plotOutput("lr_cm_plot", height = 300)),
        dash_card("Predicted probability of Member (test set)", plotOutput("lr_prob_plot", height = 300))),
    dash_card("Model coefficients", sub = "Fitted on the training set only. Odds ratio > 1 means higher odds of Member.",
              DTOutput("lr_coef")),
    dash_card("Test-set predictions",
      DTOutput("lr_pred"),
      div(class = "dl-row", dl_btn("dl_lr", "Download test predictions (CSV)")))
  ),

  # ---------------------------------------------------------------- Page 5
  page_panel("kmeans",
    page_head("K-Means Clustering", "Group transactions with similar numeric characteristics."),
    div(class = "note-box",
        p(strong("Transaction segmentation, not customer segmentation: "),
          "each cluster is a group of similar transactions. The same person could appear in different clusters, and the dataset cannot tell us who they are. ",
          "Features are standardised (mean 0, standard deviation 1) before clustering. Clusters are numbered from the lowest to the highest average sales.")),
    dash_card("Clustering settings",
      div(class = "control-row",
          sliderInput("km_k", "Number of clusters (k)", min = 2, max = 8, value = 3, step = 1),
          numericInput("km_seed", "Random seed", value = 123, min = 1, step = 1),
          checkboxGroupInput("km_feats", "Features used",
                             choices = c("Unit price" = "Unit_price", "Quantity" = "Quantity", "Sales" = "Sales",
                                         "Gross income" = "Gross_income", "Rating" = "Rating"),
                             selected = c("Unit_price", "Quantity", "Sales", "Gross_income", "Rating"), inline = TRUE),
          checkboxInput("km_use_filters", "Cluster only the transactions selected by the sidebar filters", value = TRUE))),
    uiOutput("km_banner"),
    div(class = "grid-2",
        dash_card("Cluster sizes", DTOutput("km_sizes")),
        dash_card("Average characteristics per cluster", DTOutput("km_means"))),
    div(class = "grid-2",
        dash_card("Quantity vs sales, coloured by cluster", sub = "Quantity is jittered slightly so points do not overlap",
                  plotOutput("km_scatter", height = 320)),
        dash_card("Rating by cluster", sub = "Box = middle 50% of ratings, diamond = average", plotOutput("km_rating", height = 320))),
    uiOutput("km_explain"),
    dash_card("Cluster assignment for each transaction",
      DTOutput("km_assign"),
      div(class = "dl-row", dl_btn("dl_km", "Download cluster assignments (CSV)")))
  ),

  # ---------------------------------------------------------------- Page 6
  page_panel("tree",
    page_head("Decision Tree", "Regression tree (rpart) that predicts the sales amount of a transaction."),
    uiOutput("dt_formula_note"),
    dash_card("Model settings",
      div(class = "control-row",
          sliderInput("dt_depth", "Maximum tree depth", min = 2, max = 8, value = 4, step = 1),
          numericInput("dt_cp", "Complexity parameter (cp, smaller = bigger tree)", value = 0.01, min = 0, max = 0.5, step = 0.005),
          sliderInput("dt_test", "Share of data used for testing (%)", min = 10, max = 40, value = 25, step = 5),
          numericInput("dt_seed", "Random seed", value = 123, min = 1, step = 1),
          checkboxInput("dt_use_filters", "Use only the transactions selected by the sidebar filters", value = FALSE))),
    uiOutput("dt_banner"),
    uiOutput("dt_kpis"),
    dash_card("Decision tree (trained on the training set)", plotOutput("dt_tree", height = 480)),
    div(class = "grid-2",
        dash_card("Actual vs predicted sales (test set)", sub = "Dashed line = perfect prediction", plotOutput("dt_scatter", height = 320)),
        dash_card("Variable importance", plotOutput("dt_imp", height = 320))),
    dash_card("Test-set predictions",
      DTOutput("dt_pred"),
      div(class = "dl-row", dl_btn("dl_dt", "Download test predictions (CSV)")))
  ),

  # ---------------------------------------------------------------- Page 7
  page_panel("explorer",
    page_head("Dataset Explorer", "Inspect, filter, validate and download the data."),
    uiOutput("ex_kpis"),
    uiOutput("ex_notes"),
    dash_card("Use another dataset (optional)",
      p("The project's own CSV loads automatically. To try another compatible supermarket CSV, upload it here. ",
        "It is validated and cleaned in the same way."),
      div(class = "control-row",
          uiOutput("upload_ui"),
          div(actionButton("use_default", "Go back to the default dataset", icon = icon("rotate-left"),
                           class = "btn-outline-primary btn-sm")))),
    dash_card("Data table (filtered)", sub = "Use the search box, click column headings to sort, and use the page buttons",
      DTOutput("ex_table"),
      div(class = "dl-row",
          dl_btn("dl_filtered", "Download filtered dataset (CSV)"),
          dl_btn("dl_clean", "Download cleaned dataset (CSV)"))),
    div(class = "grid-2",
        dash_card("Cleaning report", DTOutput("ex_steps")),
        dash_card("Missing values in the original file", DTOutput("ex_missing"))),
    dash_card("Column names and data types", sub = "How each original column is used in this app", DTOutput("ex_columns"))
  )
)


# ==============================================================================
# SECTION 5 - SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------- DATA
  rv <- reactiveValues(use_default = TRUE, nonce = 0)

  observeEvent(input$upload, { rv$use_default <- FALSE })
  observeEvent(input$use_default, { rv$use_default <- TRUE; rv$nonce <- rv$nonce + 1 })

  output$upload_ui <- renderUI({
    rv$nonce
    fileInput("upload", "Upload a CSV file", accept = c(".csv", "text/csv"))
  })

  default_path <- function() {
    hit <- DEFAULT_FILES[file.exists(DEFAULT_FILES)]
    if (length(hit) > 0) return(hit[1])
    any_csv <- list.files("data", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
    if (length(any_csv) > 0) any_csv[1] else DEFAULT_FILES[1]
  }

  loaded <- reactive({
    if (!isTRUE(rv$use_default) && !is.null(input$upload)) {
      load_dataset(input$upload$datapath, label = input$upload$name)
    } else {
      p <- default_path()
      load_dataset(p, label = basename(p))
    }
  })

  # The cleaned data. If loading failed, every output shows the error message.
  data <- reactive({
    r <- loaded()
    validate(need(is.null(r$error), r$error))
    r$data
  })

  output$data_status <- renderUI({
    r <- loaded()
    if (!is.null(r$error)) return(div(class = "warn-box", p(strong("Dataset problem: "), r$error)))
    NULL
  })

  # ---------------------------------------------------------------- FILTERS
  output$filters_ui <- renderUI({
    d <- data()
    input$reset_filters          # re-draw with default selections when Reset is clicked
    mk <- function(id, label, col) {
      ch <- sort(unique(d[[col]]))
      tags$details(class = "filter-group", open = if (length(ch) <= 3) NA else NULL,
                   tags$summary(label),
                   checkboxGroupInput(id, NULL, choices = ch, selected = ch))
    }
    tagList(
      mk("f_branch", "Branch", "Branch"),
      mk("f_city", "City", "City"),
      mk("f_product", "Product line", "Product_line"),
      mk("f_cust", "Customer type", "Customer_type"),
      mk("f_gender", "Gender", "Gender"),
      mk("f_pay", "Payment method", "Payment"),
      dateRangeInput("f_date", "Date range", start = min(d$Date), end = max(d$Date),
                     min = min(d$Date), max = max(d$Date))
    )
  })

  # Filtered data; may have zero rows (used by downloads)
  filtered_raw <- reactive({
    d <- data()
    dr <- input$f_date
    keep <- d$Branch %in% (input$f_branch %||% character(0)) &
      d$City %in% (input$f_city %||% character(0)) &
      d$Product_line %in% (input$f_product %||% character(0)) &
      d$Customer_type %in% (input$f_cust %||% character(0)) &
      d$Gender %in% (input$f_gender %||% character(0)) &
      d$Payment %in% (input$f_pay %||% character(0))
    if (length(dr) == 2 && !anyNA(dr)) keep <- keep & d$Date >= dr[1] & d$Date <= dr[2]
    d[keep, , drop = FALSE]
  })

  # Filtered data for charts: shows a clear message when nothing is selected
  filtered <- reactive({
    f <- filtered_raw()
    validate(need(nrow(f) > 0,
                  "No transactions match the current filters. Tick more options in the sidebar or click 'Reset filters'."))
    f
  })

  output$n_selected <- renderText({
    r <- loaded()
    if (!is.null(r$error)) return("No data loaded")
    sprintf("%s of %s transactions selected", num(nrow(filtered_raw())), num(nrow(r$data)))
  })

  # ---------------------------------------------------------------- PAGE 1: OVERVIEW
  output$ov_kpis <- renderUI({
    f <- filtered()
    div(class = "kpi-grid",
        kpi_card("Total sales revenue", num(sum(f$Sales)), "Sum of Sales", "kpi-gold"),
        kpi_card("Total gross income", num(sum(f$Gross_income), 2), "Sum of Gross income", "kpi-gold"),
        kpi_card("Transactions", num(nrow(f)), "Rows (invoices), not people"),
        kpi_card("Total quantity sold", num(sum(f$Quantity)), "Units"),
        kpi_card("Average transaction value", num(mean(f$Sales), 2), "Mean sales per transaction", "kpi-yellow"),
        kpi_card("Average customer rating", num(mean(f$Rating), 2), "Out of 10 (as recorded)"))
  })

  output$ov_insights <- renderUI({
    f <- filtered()
    top <- function(col) {
      f %>% group_by(.data[[col]]) %>% summarise(s = sum(Sales), n = n(), .groups = "drop") %>% arrange(desc(s))
    }
    b <- top("Branch"); p <- top("Product_line"); pm <- top("Payment")
    most_used <- pm %>% arrange(desc(n)) %>% slice(1)
    pay_text <- if (pm[[1]][1] == most_used[[1]][1]) {
      sprintf("<b>%s</b> is both the highest-sales and the most-used payment method (%s sales, %s transactions).",
              pm[[1]][1], num(pm$s[1]), num(pm$n[1]))
    } else {
      sprintf("<b>%s</b> has the highest sales (%s); <b>%s</b> is used most often (%s transactions).",
              pm[[1]][1], num(pm$s[1]), most_used[[1]][1], num(most_used$n))
    }
    div(class = "dash-card",
        div(class = "dash-card-head", span(class = "dash-card-title", "Quick insights"),
            span(class = "dash-card-sub", "Calculated from the filtered data")),
        div(class = "dash-card-body", tags$ul(class = "insight-list",
          tags$li(HTML(sprintf("Highest-sales branch: <b>%s</b> with %s (%s of sales).",
                               b[[1]][1], num(b$s[1]), pct(b$s[1] / sum(b$s))))),
          tags$li(HTML(sprintf("Highest-sales product line: <b>%s</b> with %s (%s of sales).",
                               p[[1]][1], num(p$s[1]), pct(p$s[1] / sum(p$s))))),
          tags$li(HTML(pay_text)))))
  })

  output$ov_branch <- renderPlot({
    d <- filtered() %>% group_by(Branch) %>% summarise(Sales = sum(Sales), .groups = "drop")
    bar_plot(d, "Branch", "Sales", "Total sales")
  }, res = 96)

  output$ov_product <- renderPlot({
    d <- filtered() %>% group_by(Product_line) %>% summarise(Sales = sum(Sales), .groups = "drop")
    bar_plot(d, "Product_line", "Sales", "Total sales", horizontal = TRUE)
  }, res = 96)

  output$ov_time <- renderPlot({
    f <- filtered()
    f$Week <- f$Date - (as.integer(format(f$Date, "%u")) - 1L)
    d <- f %>% group_by(Week) %>% summarise(Sales = sum(Sales), .groups = "drop")
    top_pt <- d[which.max(d$Sales), ]
    ggplot(d, aes(Week, Sales)) +
      geom_line(colour = COL$green, linewidth = 1) +
      geom_point(colour = COL$green, size = 2) +
      geom_point(data = top_pt, colour = COL$yellow, size = 4.5) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0.05, 0.15))) +
      scale_x_date(date_labels = "%d %b") +
      labs(x = "Week starting", y = "Total sales") + theme_app()
  }, res = 96)

  output$ov_payment <- renderPlot({
    d <- filtered() %>% group_by(Payment) %>% summarise(Sales = sum(Sales), .groups = "drop")
    bar_plot(d, "Payment", "Sales", "Total sales")
  }, res = 96)

  # ---------------------------------------------------------------- PAGE 2: SALES ANALYSIS
  output$sa_city <- renderPlot({ stack_plot(filtered(), "City", "Branch") }, res = 96)
  output$sa_product <- renderPlot({ stack_plot(filtered(), "Product_line", "Branch", horizontal = TRUE) }, res = 96)

  output$sa_qty <- renderPlot({
    d <- filtered() %>% group_by(Product_line) %>% summarise(Quantity = sum(Quantity), .groups = "drop")
    bar_plot(d, "Product_line", "Quantity", "Units sold", horizontal = TRUE)
  }, res = 96)

  output$sa_daily <- renderPlot({
    f <- filtered()
    days <- data.frame(Date = seq(min(f$Date), max(f$Date), by = "day"))
    d <- merge(days, f %>% group_by(Date) %>% summarise(Sales = sum(Sales), .groups = "drop"), all.x = TRUE)
    d$Sales[is.na(d$Sales)] <- 0
    d$Rolling <- if (nrow(d) >= 7) as.numeric(stats::filter(d$Sales, rep(1 / 7, 7), sides = 1)) else NA_real_
    ggplot(d, aes(Date)) +
      geom_line(aes(y = Sales, colour = "Daily sales"), linewidth = 0.6, alpha = 0.7) +
      geom_line(aes(y = Rolling, colour = "7-day average"), linewidth = 1.3, na.rm = TRUE) +
      scale_colour_manual(values = c("Daily sales" = COL$green, "7-day average" = COL$red)) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0.02, 0.1))) +
      scale_x_date(date_labels = "%d %b") +
      labs(x = NULL, y = "Total sales") + theme_app()
  }, res = 96)

  sa_summary <- reactive({
    s <- summarise_by(filtered(), input$sa_group %||% "Branch")
    names(s)[1] <- names(GROUP_CHOICES)[match(names(s)[1], GROUP_CHOICES)]
    s
  })
  output$sa_table <- renderDT({
    make_dt(sa_summary(), round_cols = c("Sales", "Gross_income", "Avg_sale", "Avg_rating"), simple = TRUE)
  })
  output$dl_sales_summary <- downloadHandler(
    filename = function() paste0("sales_summary_", tolower(input$sa_group %||% "branch"), ".csv"),
    content  = function(file) csv_out({ s <- sa_summary(); s[-1] <- lapply(s[-1], round, 4); s }, file))

  # ---------------------------------------------------------------- PAGE 3: CUSTOMER INSIGHTS
  type_colours <- function(f) {
    lv <- sort(unique(f$Customer_type)); setNames(CAT_PAL[seq_along(lv)], lv)
  }

  output$ci_type <- renderPlot({
    d <- filtered() %>% group_by(Customer_type) %>% summarise(Sales = sum(Sales), .groups = "drop")
    bar_plot(d, "Customer_type", "Sales", "Total sales")
  }, res = 96)

  output$ci_gender <- renderPlot({
    f <- filtered()
    d <- f %>% group_by(Gender, Customer_type) %>% summarise(Sales = sum(Sales), .groups = "drop")
    ggplot(d, aes(Gender, Sales, fill = Customer_type)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = num(Sales)), position = position_dodge(width = 0.8), vjust = -0.45, size = 3.4, colour = COL$ink) +
      scale_fill_manual(values = type_colours(f)) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.18))) +
      labs(x = NULL, y = "Total sales") + theme_app()
  }, res = 96)

  output$ci_rating <- renderPlot({
    f <- filtered()
    ggplot(f, aes(Rating)) +
      geom_histogram(binwidth = 0.5, boundary = 0, fill = COL$green, colour = "white") +
      geom_vline(xintercept = mean(f$Rating), colour = COL$red, linetype = "dashed", linewidth = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Rating", y = "Number of transactions") + theme_app()
  }, res = 96)

  output$ci_rating_branch <- renderPlot({
    d <- filtered() %>% group_by(Branch) %>% summarise(Avg_rating = mean(Rating), .groups = "drop")
    bar_plot(d, "Branch", "Avg_rating", "Average rating", digits = 2)
  }, res = 96)

  output$ci_qty <- renderPlot({
    d <- filtered() %>% count(Quantity)
    d$Quantity <- factor(d$Quantity)
    ggplot(d, aes(Quantity, n)) +
      geom_col(fill = COL$green, width = 0.7) +
      geom_text(aes(label = n), vjust = -0.45, size = 3.4, colour = COL$ink) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Quantity bought in one transaction", y = "Number of transactions") + theme_app()
  }, res = 96)

  ci_by_type <- reactive({
    filtered() %>% group_by(Customer_type) %>%
      summarise(Transactions = n(), Total_sales = sum(Sales), Gross_income = sum(Gross_income),
                Avg_sale = mean(Sales), Avg_quantity = mean(Quantity), Avg_unit_price = mean(Unit_price),
                Avg_rating = mean(Rating), .groups = "drop") %>% as.data.frame()
  })
  ci_by_type_gender <- reactive({
    filtered() %>% group_by(Customer_type, Gender) %>%
      summarise(Transactions = n(), Total_sales = sum(Sales), Avg_sale = mean(Sales),
                Avg_quantity = mean(Quantity), Avg_rating = mean(Rating), .groups = "drop") %>% as.data.frame()
  })
  output$ci_table  <- renderDT(make_dt(ci_by_type(), setdiff(names(ci_by_type()), c("Customer_type", "Transactions")), simple = TRUE))
  output$ci_table2 <- renderDT(make_dt(ci_by_type_gender(), c("Total_sales", "Avg_sale", "Avg_quantity", "Avg_rating"), simple = TRUE))
  output$dl_customer_summary <- downloadHandler(
    filename = function() "customer_type_comparison.csv",
    content = function(file) {
      a <- ci_by_type(); a$Gender <- "All"
      b <- ci_by_type_gender()
      cols <- c("Customer_type", "Gender", "Transactions", "Total_sales", "Avg_sale", "Avg_quantity", "Avg_rating")
      out <- rbind(a[, cols], b[, cols]); out[-(1:2)] <- lapply(out[-(1:2)], round, 4)
      csv_out(out, file)
    })

  output$ci_findings <- renderUI({
    f <- filtered(); items <- list()
    ct <- ci_by_type()
    tot_sales <- sum(ct$Total_sales)
    for (i in seq_len(nrow(ct))) {
      items[[length(items) + 1]] <- tags$li(HTML(sprintf(
        "<b>%s</b> transactions: %s (%s of transactions) worth %s in sales (%s of sales); average sale %s, average rating %s.",
        ct$Customer_type[i], num(ct$Transactions[i]), pct(ct$Transactions[i] / sum(ct$Transactions)),
        num(ct$Total_sales[i]), pct(ct$Total_sales[i] / tot_sales), num(ct$Avg_sale[i], 2), num(ct$Avg_rating[i], 2))))
    }
    if (nrow(ct) == 2) {
      hi <- which.max(ct$Avg_sale); lo <- 3 - hi
      rel <- (ct$Avg_sale[hi] - ct$Avg_sale[lo]) / ct$Avg_sale[lo]
      items[[length(items) + 1]] <- tags$li(sprintf(
        "%s transactions have a %s average sale than %s transactions (%s difference). That gap is %s.",
        ct$Customer_type[hi], "higher", ct$Customer_type[lo], pct(rel),
        if (rel < 0.05) "small, so it should not be treated as a meaningful difference without a statistical test" else "noticeable, but it describes these transactions only and does not show cause"))
    }
    g <- f %>% group_by(Gender) %>% summarise(s = sum(Sales), .groups = "drop") %>% arrange(desc(s))
    if (nrow(g) > 1) items[[length(items) + 1]] <- tags$li(HTML(sprintf(
      "<b>%s</b> transactions account for the larger share of sales (%s vs %s for %s).",
      g$Gender[1], pct(g$s[1] / sum(g$s)), pct(g$s[2] / sum(g$s)), g$Gender[2])))
    rb <- f %>% group_by(Branch) %>% summarise(r = mean(Rating), .groups = "drop") %>% arrange(desc(r))
    if (nrow(rb) > 1) items[[length(items) + 1]] <- tags$li(HTML(sprintf(
      "Average rating is highest for <b>%s</b> (%s) and lowest for <b>%s</b> (%s); the overall average is %s.",
      rb$Branch[1], num(rb$r[1], 2), rb$Branch[nrow(rb)], num(rb$r[nrow(rb)], 2), num(mean(f$Rating), 2))))
    qt <- as.integer(names(sort(table(f$Quantity), decreasing = TRUE))[1])
    items[[length(items) + 1]] <- tags$li(sprintf(
      "A typical transaction contains %s units on average; the most common basket size is %d units.",
      num(mean(f$Quantity), 1), qt))
    dash_card("What the data shows", tags$ul(class = "insight-list", items),
              sub = "Written automatically from the filtered data")
  })

  # ---------------------------------------------------------------- PAGE 4: LOGISTIC REGRESSION
  lr_base <- reactive({
    b <- if (isTRUE(input$lr_use_filters)) filtered() else data()
    b[b$Customer_type %in% c("Member", "Normal"), , drop = FALSE]
  })

  # The model is re-fitted only when the data, split or seed change - NOT when the threshold moves.
  lr_fit <- reactive({
    df <- lr_base()
    seed <- input$lr_seed; frac <- (input$lr_test %||% 25) / 100
    validate(need(!is.null(seed) && is.finite(seed), "Enter a whole number for the random seed."))
    validate(need(nrow(df) >= 60, sprintf(
      "Only %d Member/Normal transactions are available. At least 60 are needed to train and test the model. Widen the filters or untick the filter option.", nrow(df))))
    validate(need(length(unique(df$Customer_type)) == 2,
                  "Only one customer type (Member or Normal) is present in this selection, so a classification model cannot be trained. Widen the filters or untick the filter option."))
    df$y <- as.integer(df$Customer_type == "Member")
    test_idx <- strat_split(df$y, frac, seed)
    train <- df[-test_idx, , drop = FALSE]; test <- df[test_idx, , drop = FALSE]
    validate(need(all(table(factor(train$y, 0:1)) >= 5) && all(table(factor(test$y, 0:1)) >= 2),
                  "The training or testing split contains too few transactions of one customer type. Use more data or a different test share."))

    cat_vars <- intersect(c("Product_line", "Branch", "Gender", "Payment"), names(train))
    num_vars <- intersect(c("Quantity", "Unit_price"), names(train))
    for (v in cat_vars) train[[v]] <- droplevels(factor(train[[v]]))
    keep_cat <- cat_vars[vapply(train[cat_vars], nlevels, integer(1)) >= 2]
    dropped  <- setdiff(cat_vars, keep_cat)       # a variable with one level carries no information
    for (v in keep_cat) test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
    n_test_before <- nrow(test)
    if (length(keep_cat) > 0) test <- test[complete.cases(test[, keep_cat, drop = FALSE]), , drop = FALSE]
    validate(need(nrow(test) >= 5, "Too few test transactions remain after removing categories unseen in training."))

    preds <- c(num_vars, keep_cat)
    fml <- reformulate(preds, response = "y")
    warns <- character()
    fit <- tryCatch(
      withCallingHandlers(glm(fml, data = train, family = binomial()),
                          warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) e)
    if (inherits(fit, "error")) validate(paste("The logistic regression could not be fitted:", conditionMessage(fit)))
    prob <- suppressWarnings(as.numeric(predict(fit, newdata = test, type = "response")))
    ok <- is.finite(prob)
    test <- test[ok, , drop = FALSE]; prob <- prob[ok]
    validate(need(length(prob) >= 5, "The model could not produce probabilities for the test data."))

    notes <- character()
    if (length(dropped) > 0) notes <- c(notes, paste("Left out because they have only one value in this data:", paste(dropped, collapse = ", ")))
    if (n_test_before > nrow(test)) notes <- c(notes, sprintf("%d test rows were skipped because they contained a category not seen in training.", n_test_before - nrow(test)))
    if (anyNA(coef(fit))) notes <- c(notes, "Some predictors are perfectly redundant with others, so their coefficients could not be estimated.")
    notes <- c(notes, unique(warns))
    minority <- min(table(train$y)) / nrow(train)
    if (minority < 0.2) notes <- c(notes, sprintf("Class imbalance: the smaller customer type is only %s of the training data. Accuracy alone can look good while the small class is predicted poorly; check recall and F1.", pct(minority)))

    list(fit = fit, train = train, test = test, prob = prob, notes = notes, seed = seed, preds = preds)
  })

  lr_eval <- reactive({
    m <- lr_fit(); thr <- input$lr_thr %||% 0.5
    actual <- factor(ifelse(m$test$y == 1, "Member", "Normal"), levels = c("Normal", "Member"))
    pred   <- factor(ifelse(m$prob >= thr, "Member", "Normal"), levels = c("Normal", "Member"))
    cm <- table(Actual = actual, Predicted = pred)
    TP <- cm["Member", "Member"]; TN <- cm["Normal", "Normal"]; FP <- cm["Normal", "Member"]; FN <- cm["Member", "Normal"]
    precision <- safe_div(TP, TP + FP); recall <- safe_div(TP, TP + FN)
    f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
    majority <- names(which.max(table(m$train$y)))
    baseline <- mean(m$test$y == as.integer(majority))
    list(actual = actual, pred = pred, cm = cm, accuracy = (TP + TN) / sum(cm),
         precision = precision, recall = recall, f1 = f1, baseline = baseline,
         majority_label = ifelse(majority == "1", "Member", "Normal"), thr = thr)
  })

  output$lr_banner <- renderUI({
    m <- lr_fit(); e <- lr_eval()
    scope <- if (isTRUE(input$lr_use_filters)) "the transactions selected by the sidebar filters" else "the full cleaned dataset"
    tagList(
      div(class = "ok-box", p(strong("Data used: "), sprintf(
        "%s Member/Normal transactions from %s. A stratified random split (seed %s) gives %s training rows and %s test rows. The model is fitted on the training rows only; every metric below is calculated on the test rows only.",
        num(nrow(m$train) + nrow(m$test)), scope, m$seed, num(nrow(m$train)), num(nrow(m$test))))),
      if (length(m$notes) > 0) div(class = "warn-box", tags$ul(class = "insight-list", lapply(m$notes, tags$li))))
  })

  output$lr_kpis <- renderUI({
    e <- lr_eval()
    div(class = "kpi-grid",
        kpi_card("Test accuracy", pct(e$accuracy), "Correct classifications / all test rows", "kpi-gold"),
        kpi_card("Precision", fmt_or_na(e$precision), "Predicted Member that were Member"),
        kpi_card("Recall", fmt_or_na(e$recall), "Actual Member that were found"),
        kpi_card("F1-score", fmt_or_na(e$f1), "Balance of precision and recall", "kpi-yellow"),
        kpi_card("Baseline accuracy", pct(e$baseline), paste0("Always guessing '", e$majority_label, "' (most common in training)"), "kpi-red"))
  })

  output$lr_cm_plot <- renderPlot({
    e <- lr_eval()
    d <- as.data.frame(e$cm); names(d) <- c("Actual", "Predicted", "n")
    ggplot(d, aes(Predicted, Actual, fill = n)) +
      geom_tile(colour = "white", linewidth = 1.5) +
      geom_text(aes(label = n), size = 7, fontface = "bold", colour = COL$ink) +
      scale_fill_gradient(low = "#FFF6DD", high = COL$yellow, guide = "none") +
      labs(x = "Predicted customer type", y = "Actual customer type") +
      theme_app() + theme(panel.grid = element_blank())
  }, res = 96)

  output$lr_prob_plot <- renderPlot({
    m <- lr_fit(); e <- lr_eval()
    d <- data.frame(Prob = m$prob, Actual = e$actual)
    ggplot(d, aes(Prob, fill = Actual)) +
      geom_histogram(binwidth = 0.05, boundary = 0, position = "identity", alpha = 0.7, colour = "white") +
      geom_vline(xintercept = e$thr, colour = COL$red, linetype = "dashed", linewidth = 1) +
      scale_fill_manual(values = c(Normal = COL$yellow, Member = COL$green)) +
      scale_x_continuous(limits = c(0, 1), labels = label_number(accuracy = 0.1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Predicted probability of Member (red line = threshold)", y = "Test transactions") + theme_app()
  }, res = 96)

  output$lr_coef <- renderDT({
    s <- summary(lr_fit()$fit)$coefficients
    d <- data.frame(Term = rownames(s), Estimate = s[, 1], Odds_ratio = exp(s[, 1]),
                    Std_error = s[, 2], p_value = s[, 4], row.names = NULL)
    make_dt(d, c("Estimate", "Odds_ratio", "Std_error", "p_value"), digits = 4, simple = TRUE)
  })

  lr_pred_tbl <- reactive({
    m <- lr_fit(); e <- lr_eval()
    out <- data.frame(Invoice_ID = if ("Invoice_ID" %in% names(m$test)) m$test$Invoice_ID else seq_len(nrow(m$test)),
                      Actual_customer_type = as.character(e$actual),
                      Predicted_customer_type = as.character(e$pred),
                      Probability_Member = round(m$prob, 4),
                      Threshold_used = e$thr, stringsAsFactors = FALSE)
    out$Correct <- ifelse(out$Actual_customer_type == out$Predicted_customer_type, "Yes", "No")
    out
  })
  output$lr_pred <- renderDT(make_dt(lr_pred_tbl(), "Probability_Member", digits = 4))
  output$dl_lr <- downloadHandler(filename = function() "logistic_regression_test_predictions.csv",
                                  content = function(file) csv_out(lr_pred_tbl(), file))

  # ---------------------------------------------------------------- PAGE 5: K-MEANS
  km_fit <- reactive({
    df <- if (isTRUE(input$km_use_filters)) filtered() else data()
    feats <- input$km_feats; k <- input$km_k; seed <- input$km_seed
    validate(need(length(feats) >= 2, "Select at least two features to cluster on."))
    validate(need(!is.null(seed) && is.finite(seed), "Enter a whole number for the random seed."))
    validate(need(!is.null(k) && k >= 2, "Choose at least 2 clusters."))
    validate(need(nrow(df) >= max(10, 3 * k), sprintf(
      "Only %d transactions are selected, which is too few for %d clusters. Widen the filters or choose fewer clusters.", nrow(df), k)))
    X <- df[, feats, drop = FALSE]
    validate(need(all(vapply(X, function(v) all(is.finite(v)), logical(1))),
                  "The selected data contains missing or invalid numeric values, so clustering was not run."))
    sds <- vapply(X, stats::sd, numeric(1))
    constant <- feats[is.na(sds) | sds == 0]
    keep <- setdiff(feats, constant)
    validate(need(length(keep) >= 2, "Fewer than two selected features vary in this selection (the others are constant), so clustering was not run."))
    X <- X[, keep, drop = FALSE]
    validate(need(nrow(unique(X)) >= k, "There are fewer distinct transactions than clusters. Choose fewer clusters."))
    Z <- scale(X)
    set.seed(seed)
    fit <- tryCatch(kmeans(Z, centers = k, nstart = 25, iter.max = 100), error = function(e) e)
    if (inherits(fit, "error")) validate(paste("K-Means could not be run:", conditionMessage(fit)))
    # number clusters from lowest to highest average sales so labels are stable and readable
    ord <- order(tapply(df$Sales, fit$cluster, mean))
    df$Cluster <- match(fit$cluster, ord)
    notes <- character()
    if (length(constant) > 0) notes <- c(notes, paste("Not used because they are constant in this selection:", paste(constant, collapse = ", ")))
    if (isTRUE(fit$ifault == 4)) notes <- c(notes, "The algorithm did not fully converge; try a different seed.")
    if (all(c("Sales", "Gross_income", "Unit_price", "Quantity") %in% keep))
      notes <- c(notes, "Sales and Gross income are both calculated from Unit price × Quantity, so these features overlap and give that information extra weight. You can untick some of them above.")
    list(df = df, k = k, features = keep, seed = seed, ratio = fit$betweenss / fit$totss, notes = notes)
  })

  num_cols <- c("Unit_price", "Quantity", "Sales", "Gross_income", "Rating")

  output$km_banner <- renderUI({
    m <- km_fit()
    scope <- if (isTRUE(input$km_use_filters)) "the transactions selected by the sidebar filters" else "the full cleaned dataset"
    tagList(
      div(class = "ok-box", p(strong("Data used: "), sprintf(
        "%s transactions from %s, grouped into %d clusters using %s (seed %s). Clustering is unsupervised, so there is no training/test split. Between-cluster variation explains %s of the total variation in the standardised features.",
        num(nrow(m$df)), scope, m$k, paste(gsub("_", " ", m$features), collapse = ", "), m$seed, pct(m$ratio)))),
      if (length(m$notes) > 0) div(class = "warn-box", tags$ul(class = "insight-list", lapply(m$notes, tags$li))))
  })

  km_sizes_tbl <- reactive({
    d <- km_fit()$df
    s <- d %>% count(Cluster) %>% mutate(Percent = round(100 * n / sum(n), 1))
    names(s) <- c("Cluster", "Transactions", "Percent_of_transactions"); as.data.frame(s)
  })
  km_means_tbl <- reactive({
    d <- km_fit()$df
    m <- d %>% group_by(Cluster) %>% summarise(across(all_of(num_cols), mean), .groups = "drop") %>% as.data.frame()
    overall <- data.frame(Cluster = "All", t(colMeans(d[, num_cols])), check.names = FALSE)
    m$Cluster <- as.character(m$Cluster)
    rbind(m, overall)
  })
  output$km_sizes <- renderDT(make_dt(km_sizes_tbl(), "Percent_of_transactions", 1, simple = TRUE))
  output$km_means <- renderDT(make_dt(km_means_tbl(), num_cols, 2, simple = TRUE))

  cluster_colours <- function(k) setNames(CAT_PAL[seq_len(k)], as.character(seq_len(k)))

  output$km_scatter <- renderPlot({
    m <- km_fit(); d <- m$df; d$Cluster <- factor(d$Cluster)
    ggplot(d, aes(Quantity, Sales, colour = Cluster)) +
      geom_jitter(width = 0.15, height = 0, size = 2.2, alpha = 0.8) +
      scale_colour_manual(values = cluster_colours(m$k)) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(labels = label_comma()) +
      labs(x = "Quantity", y = "Sales") + theme_app() +
      theme(panel.grid.major.x = element_line(colour = "grey92"))
  }, res = 96)

  output$km_rating <- renderPlot({
    m <- km_fit(); d <- m$df; d$Cluster <- factor(d$Cluster)
    ggplot(d, aes(Cluster, Rating, fill = Cluster)) +
      geom_boxplot(alpha = 0.85, outlier.colour = COL$red, width = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white", colour = COL$ink) +
      scale_fill_manual(values = cluster_colours(m$k), guide = "none") +
      labs(x = "Cluster", y = "Rating") + theme_app()
  }, res = 96)

  output$km_explain <- renderUI({
    d <- km_fit()$df
    overall <- colMeans(d[, num_cols])
    items <- lapply(sort(unique(d$Cluster)), function(cl) {
      sub <- d[d$Cluster == cl, ]
      cm <- colMeans(sub[, num_cols])
      diff <- (cm - overall) / overall
      big <- names(sort(abs(diff[abs(diff) >= 0.15]), decreasing = TRUE))
      big <- head(big, 3)
      txt <- if (length(big) == 0) {
        "all five measures are within 15% of the overall average, so this cluster is not distinctive on these variables."
      } else {
        paste0(paste(vapply(big, function(b) sprintf("%s is %s than average (%s vs %s overall, %s%s)",
                gsub("_", " ", tolower(b)), if (diff[b] > 0) "higher" else "lower",
                num(cm[b], 2), num(overall[b], 2), if (diff[b] > 0) "+" else "−", pct(abs(diff[b]), 0)), character(1)),
                collapse = "; "), ".")
      }
      tags$li(HTML(sprintf("<b>Cluster %d</b> (%s transactions, %s): %s", cl, num(nrow(sub)), pct(nrow(sub) / nrow(d)), txt)))
    })
    dash_card("How to read the clusters", tags$ul(class = "insight-list", items),
              sub = "Differences of at least 15% from the overall average are listed. These describe transactions only; no customer labels are assigned.")
  })

  km_assign_tbl <- reactive({
    m <- km_fit(); d <- m$df
    cols <- intersect(c("Invoice_ID", "Branch", "Product_line", "Customer_type", num_cols), names(d))
    out <- d[, cols, drop = FALSE]
    out <- cbind(Cluster = d$Cluster, out); out
  })
  output$km_assign <- renderDT(make_dt(km_assign_tbl(), c("Unit_price", "Sales", "Gross_income"), 2))
  output$dl_km <- downloadHandler(filename = function() "kmeans_cluster_assignments.csv",
                                  content = function(file) csv_out(km_assign_tbl(), file))

  # ---------------------------------------------------------------- PAGE 6: DECISION TREE
  output$dt_formula_note <- renderUI({
    d <- tryCatch(data(), error = function(e) NULL)
    ratio_txt <- NULL
    if (!is.null(d)) {
      r <- d$Sales / (d$Unit_price * d$Quantity)
      if (all(is.finite(r)) && stats::sd(r) < 1e-6 * mean(r)) {
        ratio_txt <- sprintf("In the loaded data, Sales is exactly %s × Unit price × Quantity for every row (this is calculated from the data). ", num(mean(r), 4))
      }
    }
    div(class = "note-box", p(strong("How to interpret the results: "), ratio_txt,
        "Quantity and unit price are therefore very closely tied to a transaction's sales, so low errors do not mean the model would generalise to a different store or a new situation. ",
        "A tree can only approximate the multiplication with a limited number of splits, so its predictions come in steps. The test rows are never used to build the tree."))
  })

  dt_fit <- reactive({
    df <- if (isTRUE(input$dt_use_filters)) filtered() else data()
    seed <- input$dt_seed; frac <- (input$dt_test %||% 25) / 100
    cp <- input$dt_cp; depth <- input$dt_depth
    validate(need(!is.null(seed) && is.finite(seed), "Enter a whole number for the random seed."))
    validate(need(!is.null(cp) && is.finite(cp) && cp >= 0, "Enter a complexity parameter of 0 or more."))
    validate(need(nrow(df) >= 60, sprintf("Only %d transactions are selected; at least 60 are needed to train and test the tree.", nrow(df))))
    set.seed(seed)
    test_idx <- sample.int(nrow(df), round(nrow(df) * frac))
    train <- df[-test_idx, , drop = FALSE]; test <- df[test_idx, , drop = FALSE]
    cat_vars <- intersect(c("Product_line", "Branch", "Customer_type", "Gender", "Payment"), names(train))
    for (v in cat_vars) train[[v]] <- droplevels(factor(train[[v]]))
    keep_cat <- cat_vars[vapply(train[cat_vars], nlevels, integer(1)) >= 2]
    for (v in keep_cat) test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
    preds <- c(intersect(c("Quantity", "Unit_price"), names(train)), keep_cat)
    fml <- reformulate(preds, response = "Sales")
    fit <- tryCatch(rpart(fml, data = train, method = "anova",
                          control = rpart.control(cp = cp, maxdepth = depth, minsplit = 20, minbucket = 7)),
                    error = function(e) e)
    if (inherits(fit, "error")) validate(paste("The decision tree could not be trained:", conditionMessage(fit)))
    pred <- as.numeric(predict(fit, newdata = test))
    ok <- is.finite(pred)
    test <- test[ok, , drop = FALSE]; pred <- pred[ok]
    validate(need(length(pred) >= 5, "The tree could not produce predictions for the test data."))
    err <- test$Sales - pred
    rmse <- sqrt(mean(err^2)); mae <- mean(abs(err))
    base_rmse <- sqrt(mean((test$Sales - mean(train$Sales))^2))
    r2 <- 1 - sum(err^2) / sum((test$Sales - mean(test$Sales))^2)
    list(fit = fit, train = train, test = test, pred = pred, rmse = rmse, mae = mae,
         base_rmse = base_rmse, r2 = r2, seed = seed, preds = preds, depth = depth)
  })

  output$dt_banner <- renderUI({
    m <- dt_fit()
    scope <- if (isTRUE(input$dt_use_filters)) "the transactions selected by the sidebar filters" else "the full cleaned dataset"
    div(class = "ok-box", p(strong("Data used: "), sprintf(
      "%s transactions from %s, split at random (seed %s) into %s training rows and %s test rows. The tree is trained on the training rows only; RMSE, MAE and the scatter plot use the test rows only. Predictors: %s.",
      num(nrow(m$train) + nrow(m$test)), scope, m$seed, num(nrow(m$train)), num(nrow(m$test)),
      paste(gsub("_", " ", m$preds), collapse = ", "))))
  })

  output$dt_kpis <- renderUI({
    m <- dt_fit()
    div(class = "kpi-grid",
        kpi_card("Test RMSE", num(m$rmse, 2), "Typical size of the error (in sales units)", "kpi-gold"),
        kpi_card("Test MAE", num(m$mae, 2), "Average absolute error", "kpi-gold"),
        kpi_card("Test R²", sprintf("%.3f", m$r2), "Share of sales variation explained", "kpi-yellow"),
        kpi_card("Baseline RMSE", num(m$base_rmse, 2), "If we always predicted the training average", "kpi-red"),
        kpi_card("Tree leaves", num(sum(m$fit$frame$var == "<leaf>")), paste("Maximum depth allowed:", m$depth)))
  })

  output$dt_tree <- renderPlot({
    m <- dt_fit()
    validate(need(nrow(m$fit$frame) > 1, "The tree has no splits with the current settings. Lower the complexity parameter (cp) to grow a tree."))
    drawn <- FALSE
    if (HAS_RPART_PLOT) {
      drawn <- tryCatch({
        rpart.plot::rpart.plot(m$fit, type = 4, extra = 101, fallen.leaves = TRUE,
                               box.palette = "Greens", shadow.col = "gray85", branch.lty = 1, cex = NULL, roundint = FALSE)
        TRUE
      }, error = function(e) FALSE)
    }
    if (!drawn) {   # basic fallback if rpart.plot is not installed
      plot(m$fit, uniform = TRUE, margin = 0.1)
      text(m$fit, use.n = TRUE, cex = 0.8)
    }
  }, res = 96)

  output$dt_scatter <- renderPlot({
    m <- dt_fit(); d <- data.frame(Actual = m$test$Sales, Predicted = m$pred)
    ggplot(d, aes(Actual, Predicted)) +
      geom_abline(slope = 1, intercept = 0, colour = COL$red, linetype = "dashed", linewidth = 1) +
      geom_point(colour = COL$green, size = 2.2, alpha = 0.75) +
      scale_x_continuous(labels = label_comma()) + scale_y_continuous(labels = label_comma()) +
      labs(x = "Actual sales", y = "Predicted sales") + theme_app() +
      theme(panel.grid.major.x = element_line(colour = "grey92"))
  }, res = 96)

  output$dt_imp <- renderPlot({
    m <- dt_fit(); vi <- m$fit$variable.importance
    validate(need(length(vi) > 0, "The tree has no splits, so there is no variable importance to show."))
    d <- data.frame(Variable = gsub("_", " ", names(vi)), Importance = as.numeric(vi) / sum(vi) * 100)
    bar_plot(d, "Variable", "Importance", "Importance (% of total)", horizontal = TRUE, digits = 1)
  }, res = 96)

  dt_pred_tbl <- reactive({
    m <- dt_fit()
    data.frame(Invoice_ID = if ("Invoice_ID" %in% names(m$test)) m$test$Invoice_ID else seq_len(nrow(m$test)),
               Actual_sales = round(m$test$Sales, 4), Predicted_sales = round(m$pred, 4),
               Error = round(m$test$Sales - m$pred, 4), stringsAsFactors = FALSE)
  })
  output$dt_pred <- renderDT(make_dt(dt_pred_tbl(), c("Actual_sales", "Predicted_sales", "Error"), 2))
  output$dl_dt <- downloadHandler(filename = function() "decision_tree_test_predictions.csv",
                                  content = function(file) csv_out(dt_pred_tbl(), file))

  # ---------------------------------------------------------------- PAGE 7: DATASET EXPLORER
  output$ex_kpis <- renderUI({
    r <- loaded(); validate(need(is.null(r$error), r$error))
    div(class = "kpi-grid",
        kpi_card("Original records", num(r$n_original), r$label, "kpi-gold"),
        kpi_card("Cleaned records", num(r$n_clean), "Used by every page"),
        kpi_card("Duplicate rows found", num(r$n_duplicates), "Exact copies, removed"),
        kpi_card("Records removed in total", num(r$n_original - r$n_clean),
                 pct((r$n_original - r$n_clean) / r$n_original), if (r$n_original - r$n_clean > 0) "kpi-red" else ""),
        kpi_card("Currently filtered", num(nrow(filtered_raw())), "After the sidebar filters", "kpi-yellow"))
  })

  output$ex_notes <- renderUI({
    r <- loaded(); validate(need(is.null(r$error), r$error))
    sales_note <- if (!identical(tolower(r$sales_source), "sales"))
      sprintf("The column '%s' in the file is used as Sales. ", r$sales_source) else NULL
    tagList(
      div(class = "ok-box", p(strong("Source: "), r$label, ". ", sales_note,
                              sprintf("Dates were read using the format %s. ", gsub("%", "", r$date_format, fixed = TRUE)),
                              "Each row is a transaction; there is no customer ID.")),
      if (length(r$notes) > 0) div(class = "warn-box", tags$ul(class = "insight-list", lapply(r$notes, tags$li))))
  })

  output$ex_table <- renderDT({
    f <- filtered_raw()
    validate(need(nrow(f) > 0, "No transactions match the current filters."))
    make_dt(f, c("Unit_price", "Tax_5pct", "Sales", "COGS", "Gross_income", "Gross_margin_pct"), 2)
  })
  output$ex_steps <- renderDT({
    r <- loaded(); validate(need(is.null(r$error), r$error))
    s <- r$steps
    if (nrow(r$invalid_cells) > 0) {
      iv <- data.frame(Step = paste0("Note: unreadable values in column ", r$invalid_cells$Column),
                       Rows_removed = r$invalid_cells$Invalid_values, Rows_remaining = NA_integer_)
      s <- rbind(s, iv)
    }
    make_dt(s, simple = TRUE)
  })
  output$ex_missing <- renderDT({
    r <- loaded(); validate(need(is.null(r$error), r$error)); make_dt(r$missing, simple = FALSE, page_length = 8)
  })
  output$ex_columns <- renderDT({
    r <- loaded(); validate(need(is.null(r$error), r$error)); make_dt(r$mapping, simple = TRUE)
  })

  output$dl_filtered <- downloadHandler(
    filename = function() paste0("filtered_supermarket_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) csv_out(filtered_raw(), file))
  output$dl_clean <- downloadHandler(
    filename = function() paste0("cleaned_supermarket_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) csv_out(data(), file))
}


# ==============================================================================
# SECTION 6 - START THE APP
# ==============================================================================
shinyApp(ui, server)
