# Supermarket Sales Data Analysis and Customer Insights Using R

An interactive R Shiny dashboard for the supermarket case study. It reads **your own CSV file**
(`data/SuperMarket Analysis.csv`), cleans it, and provides 7 pages: an overview dashboard, sales
analysis, customer insights, three machine-learning models (Logistic Regression, K-Means, Decision
Tree) and a dataset explorer. Everything runs **locally**. No internet services, API keys or paid tools are used.
Every number on screen is calculated from the data when the app runs.

---

## 1. How the application is organised (architecture)

```
        CSV file  ->  load_dataset()  ->  data()  ->  filtered()  ->  charts, KPI cards, tables
   (data folder)     read, validate,     cleaned     sidebar          |
                     clean, report       data        filters          +-> Logistic Regression (glm)
                                                                      +-> K-Means (kmeans)
                                                                      +-> Decision Tree (rpart)
```

* `app.R` has three parts: **UI** (what you see), **server** (the calculations), and helper functions.
  Sections are labelled `SECTION 1` ... `SECTION 6` so you can find things.
* A **reactive expression** (`reactive({...})`) is an R calculation that re-runs automatically only when the inputs
  it uses change. `data()` -> `filtered()` -> every chart follows this chain. The models use reactives too, so a model is
  re-fitted only when its own settings change (moving the probability threshold does **not** retrain the model).
* Columns are renamed internally (e.g. `Customer type` -> `Customer_type`, `Unit price` -> `Unit_price`).
  If your file has `Total` instead of `Sales`, it is detected and used as Sales. The original names are shown
  on the Dataset Explorer page.

## 2. Folder structure

```
supermarket-shiny/
├── app.R                      <- the whole application
├── install_packages.R         <- run once to install packages
├── run_app.R                  <- optional: starts the app
├── README.md                  <- this file
├── data/
│   └── SuperMarket Analysis.csv   <- YOUR dataset (already included)
└── www/
    └── styles.css             <- local colours and layout (exact theme colours)
```

## 3. Install and run on Windows (step by step)

**Step 1 - Install R.** Go to <https://cran.r-project.org/bin/windows/base/>, click *Download R for Windows*, run the installer
and accept the defaults.

**Step 2 - Install RStudio Desktop.** Go to <https://posit.co/download/rstudio-desktop/>, download the free *RStudio Desktop*
installer and run it. (Install R first.)

**Step 3 - Create the project folder.** Unzip `supermarket-shiny.zip` somewhere simple, for example
`C:\Users\<you>\Documents\supermarket-shiny`. The folder must look like the structure in section 2.
If you unzipped it yourself, make sure `app.R` is directly inside the folder (not inside a second nested folder).

**Step 4 - Put the CSV in the right place.** The file must be inside the `data` folder. The app finds
`data/SuperMarket Analysis.csv`, `data/SuperMarket Analysis(1).csv` or `data/SuperMarket_Analysis.csv`
(or, failing that, the first `.csv` in `data`). It is already there in this download.

**Step 5 - Open the project in RStudio.** Start RStudio, then *File -> Open File...* and choose `app.R`.
Then click *Session -> Set Working Directory -> To Source File Location*. This tells R where the project is,
so the relative path `data/...` works. (You can confirm with `getwd()` in the Console.)

**Step 6 - Install the packages (only once).** Paste this into the RStudio **Console** (bottom-left panel) and press Enter:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "DT", "rpart", "rpart.plot", "readr", "scales"))
```

If RStudio asks to restart R or install from source, choose **Yes** / binaries. This can take a few minutes.
(Alternative: open `install_packages.R` and click *Source*.)

**Step 7 - Run the app.** Either click the green **Run App** button at the top of the `app.R` editor, or paste this into the Console:

```r
shiny::runApp()
```

**Step 8 - Open it in the browser.** A window or your browser opens automatically. If not, look in the Console for
`Listening on http://127.0.0.1:XXXX` and paste that address into Chrome/Edge. It only works on your own computer.

**Step 9 - Stop the app.** Click the red **Stop** sign in the RStudio Console, or press `Esc` in the Console.
Then close the browser tab.

## 4. The pages and how to test them

| Page | What to try |
|---|---|
| **Overview Dashboard** | Six KPI cards, quick insights, four charts. Untick a branch in the sidebar and watch every number change. |
| **Sales Analysis** | Sales by city and product line (split by branch), quantity by product line, daily trend with a 7-day average. Change "Summarise by" and download the summary CSV. |
| **Customer Insights** | Member vs Normal, gender, rating and quantity charts, plus automatically written findings. Remember: one row = one transaction, not one person. |
| **Logistic Regression** | Move the threshold slider: the confusion matrix, precision, recall and F1 update instantly (the model is *not* re-fitted). Change the seed or test share to re-split. Compare test accuracy with the **baseline accuracy** card. Download test predictions. |
| **K-Means Clustering** | Change k (2-8) or the features. Read the "How to read the clusters" text. Download assignments. |
| **Decision Tree** | See the tree, test RMSE/MAE, actual-vs-predicted plot. Lower *cp* for a bigger tree. Download predictions. |
| **Dataset Explorer** | Search/sort/page the table, see original vs cleaned counts, missing values and column types. Download the filtered or cleaned data. Upload another CSV to test validation. |

**Filters** (left sidebar) apply to all dashboard pages. For the supervised models (Logistic Regression and Decision Tree) the
tick-box "Use only the transactions selected by the sidebar filters" is **off by default**, so they train on the full cleaned data.
K-Means uses the filtered data by default. Each model page states exactly how many rows were used for training and testing.
All reported model metrics are calculated on the **test set only**.

### Important notes for your report (honest interpretation)
* **Logistic Regression** classifies the customer type of a transaction from basket and store details. It does not predict future
  behaviour. Compare accuracy with the baseline (always guessing the most common class). If the model is not better than the
  baseline, then these predictors do not carry much information about customer type. That is a legitimate finding.
* **K-Means** groups *transactions*, not people. Sales and Gross income are both computed from Unit price x Quantity, so they overlap.
* **Decision Tree**: in this dataset Sales equals Unit price x Quantity x 1.05 exactly (the app checks this at runtime), so good accuracy is expected
  and does not mean the model would work in a different situation.

## 5. Troubleshooting

| Problem | Fix |
|---|---|
| **"Dataset file not found"** | The CSV must be in the `data` folder, and R's working directory must be the project folder. Run `getwd()` and `list.files("data")` in the Console. Fix with *Session -> Set Working Directory -> To Source File Location*. |
| **"missing required column(s)"** | The message lists what is missing and which columns were found. Required: Branch, City, Customer type, Gender, Product line, Unit price, Quantity, Sales (or Total), Date, Payment, gross income, Rating. Fix the header names in the CSV. |
| **"there is no package called ..."** | Run the install command from Step 6 again. For one package: `install.packages("DT")`. |
| **Date conversion problems** | The app tries `m/d/Y`, `d/m/Y`, `Y-m-d` and others and keeps the best match. Rows whose date cannot be read are counted in the *Cleaning report* on the Dataset Explorer page. Check that the Date column has one consistent format. |
| **"Port already in use" / address in use** | Stop any other running app (red Stop sign), or choose another port: `shiny::runApp(port = 4545)`. |
| **Model training errors** | The page explains why (too few rows, only one customer type after filtering, too few of one class in a split, k larger than the number of distinct rows). Reset the filters, untick "Use only the transactions selected by the sidebar filters", use a different seed, or lower k. |
| **Empty plots / "No transactions match the current filters"** | Too many filters are unticked. Click **Reset filters**. |
| **The decision tree looks basic (no colours)** | `rpart.plot` is not installed; the app falls back to a plain tree. Run `install.packages("rpart.plot")`. |
| **Text looks garbled when CSV is opened in Excel** | Open Excel -> *Data -> From Text/CSV* and choose encoding *UTF-8*. |
| **Upload fails** | Only comma-separated `.csv` files up to 30 MB are accepted; the message on screen says what is wrong. Click "Go back to the default dataset" to return. |

## 6. Optional: deploying later (not needed for your submission)

Running locally is enough for the case study. If you later want it online: (a) free hosting at **shinyapps.io**
(`install.packages("rsconnect")`, create a free account, then `rsconnect::deployApp()`), or (b) a **Posit Connect / Shiny Server**
installation at your institution. Deployment uses the same files; keep `data/` and `www/` with `app.R`.
