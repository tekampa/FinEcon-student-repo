# ============================================================================
# bond_project_simple.R -- Bond Project, example code for ALL QUESTIONS in the bond_project.pdf assignment
#
# WHAT THIS SCRIPT DOES
#   Q1  Reads daily bond PRICES for five bonds, and draws a line graph.
#   Q2  Same for the YIELD TO MATURITY (YTM) of each bond.
#   Q3  Computes a few numbers that help interpret the graphs: how prices and
#       yields move together, and the largest one-day yield moves.
#   Q4  Prices the United Airlines bond from its yield alone, by discounting
#       its cash flows, and compares the answer to FactSet's quoted price.
#   Q5  Builds a summary table of all five bonds on the last date, sorted by
#       YTM, and a dot plot of the yields.
#   Q6  Finds the two bonds whose maturity dates are closest together.
#
# HOW TO USE IT
#   This code uses the instructor's five bonds. To use your own bonds:
#     1. Put your FactSet exports in bond-assignment/data/ with the same names.
#     2. Edit the table `my_bonds` in Section 1 (CUSIP, ticker, company name).
#     3. Run the script top to bottom.
#   You can ask an AI tool to help adapt the code, but you are responsible for
#   understanding it. If a line is unclear, ask AI to explain that line.
#
#   Lines that start with assert_that() are CODE CHECKS. If something looks
#   wrong (for example, a price of 0.98 instead of 98), the script stops with
#   a message telling you what to look at. Keep these checks in your own code
#   and create more as you deem necessary.
#
# INPUT (in bond-assignment/data/)
#   BondPrices.xlsx  Date + one column per bond, price as PERCENT OF FACE
#   BondYields.xlsx  Date + one column per bond, yield to maturity in PERCENT
#   Each column name looks like
#       United Airlines, Inc. 4.625% 15-APR-2029 (90932LAH0)
#   so the coupon rate, the maturity date and the CUSIP (the bond's ID code)
#   are read from the column name instead of being typed in by hand.
#   Download with as many decimals as FactSet allows: Question 4 needs them.
#
# OUTPUT (written to bond-assignment/output/)
#   fig_*.png                   figures for your slides
#   tab_*.tex and tab_*.md      tables
#   bond_prices_dollars.csv     prices converted to dollars per bond
# ============================================================================


# ---- 0. Packages -----------------------------------------------------------
# A package is a bundle of extra R commands. Install them ONCE on your
# computer (remove the # at the start of the next line, run it, put the # back):
# install.packages(c("tidyverse", "readxl", "scales", "assertthat", "here", "knitr"))

# Load the packages every time you run the script.
library(tidyverse)    # data handling (dplyr, tidyr) and graphs (ggplot2)
library(readxl)       # reads Excel files
library(scales)       # formats axis labels (%, $)
library(assertthat)   # assert_that() sanity checks
library(here)         # builds file paths relative to the project folder
library(knitr)        # kable() turns a table into LaTeX or Markdown


# ---- 1. Settings: the only part you should need to edit --------------------

# Tell R where this script lives, so it can find the data and output folders
# no matter which folder you run it from. If you rename the script, update
# the file name here too.
here::i_am("bond-assignment/scripts/bond_project_student.R")
data_dir   <- here("bond-assignment", "data")
output_dir <- here("bond-assignment", "output")
dir.create(output_dir, showWarnings = FALSE)   # create the folder if missing

FACE_VALUE <- 1000   # face value of one bond, in $. Check this for your bonds.
FREQ       <- 2      # coupon payments per year (2 = every six months)
FOCUS      <- "UAL"  # the bond used when a question asks about one bond

# Your bonds: one row per bond. The CUSIP must match the code in brackets at
# the end of the FactSet column name. The order of the rows is the order in
# which companies appear in tables and legends.
my_bonds <- tribble(
  ~cusip,      ~ticker, ~company,
  "90932LAH0", "UAL",   "United Airlines",
  "747525BS1", "QCOM",  "Qualcomm",
  "74460WAG2", "PSA",   "Public Storage",
  "609207BE4", "MDLZ",  "Mondelez",
  "79466LAJ3", "CRM",   "Salesforce"
)

# One color per company (a colorblind-friendly palette).
firm_palette <- c("United Airlines" = "#0072B2", "Qualcomm"       = "#E69F00",
                  "Public Storage"  = "#009E73", "Mondelez"       = "#CC79A7",
                  "Salesforce"      = "#D55E00")

# A common look for all graphs: no gridlines, legend at the bottom, and a
# white background (so the figure is readable on any slide template).
theme_bond <- theme_minimal(base_size = 12) +
  theme(panel.grid      = element_blank(),
        axis.line       = element_line(colour = "grey30"),
        legend.title    = element_blank(),
        legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.caption    = element_text(colour = "grey35", hjust = 0),
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA))


# ---- 2. Read the data ------------------------------------------------------
# FactSet files have a title in row 1 and a blank row 2, so we skip 2 rows and
# the column names come from row 3.
# The data arrive "wide" (one column per bond). pivot_longer() turns them
# "long": one row per date and bond, which is the shape ggplot2 needs.
prices_long <- read_excel(file.path(data_dir, "BondPrices.xlsx"), skip = 2) |>
  mutate(date = as.Date(Date)) |>
  select(-Date) |>
  pivot_longer(-date, names_to = "header", values_to = "price")

yields_long <- read_excel(file.path(data_dir, "BondYields.xlsx"), skip = 2) |>
  mutate(date = as.Date(Date)) |>
  select(-Date) |>
  pivot_longer(-date, names_to = "header", values_to = "ytm")

# Put prices and yields side by side, then read the bond's details out of the
# column name. str_match() pulls out the piece of text that fits a pattern:
#   "([0-9.]+)%"          the coupon rate, e.g. 4.625%
#   "([0-9]{2}-[A-Z]{3}-[0-9]{4})"  the maturity date, e.g. 15-APR-2029
#   "\\(([A-Z0-9]{9})\\)"  the 9-character CUSIP inside brackets
bond_panel <- inner_join(prices_long, yields_long, by = c("date", "header")) |>
  drop_na() |>                                   # drop days with no quote
  arrange(date) |>
  mutate(coupon_rate = as.numeric(str_match(header, "([0-9.]+)%")[, 2]) / 100,
         maturity    = as.Date(str_match(header, "([0-9]{2}-[A-Z]{3}-[0-9]{4})")[, 2],
                               format = "%d-%b-%Y"),
         cusip       = str_match(header, "\\(([A-Z0-9]{9})\\)")[, 2],
         # dollar coupon paid every six months on one bond
         coupon_semi = coupon_rate / FREQ * FACE_VALUE) |>
  left_join(my_bonds, by = "cusip") |>           # add ticker and company name
  # Store company as a "factor" so tables and legends follow the my_bonds order.
  mutate(company = factor(company, levels = my_bonds$company))

# One row per bond with its fixed characteristics.
bonds <- bond_panel |>
  distinct(ticker, company, cusip, coupon_rate, maturity, coupon_semi)


# ---- 3. Code checks: check the data before you trust it ------------------
assert_that(!any(is.na(bonds$ticker)),
            msg = "A CUSIP in the FactSet file is missing from my_bonds")
assert_that(!any(is.na(bonds$coupon_rate)), !any(is.na(bonds$maturity)),
            msg = "Could not read a coupon rate or maturity from a column name")
assert_that(nrow(bonds) == 5, msg = "Expected exactly five bonds")

n_dates <- n_distinct(bond_panel$date)
assert_that(all(table(bond_panel$ticker) == n_dates),
            msg = "Some bond is missing dates that others have")
# A price in percent of face is near 100. Near 1,000 means it is already in
# dollars; near 1 means it is a decimal.
assert_that(all(between(bond_panel$price, 50, 150)),
            msg = "A price is outside 50-150: check percent-of-face units")
# Create more code checks as you deem needed here or future sections.
# For you to take ownership of the code with AI,
# it's good to lean on too many code checks and visualizations to
# ensure data and output correctness.

first_date <- min(bond_panel$date)
last_date  <- max(bond_panel$date)
message(sprintf("Loaded %d bonds x %d dates, %s to %s",
                nrow(bonds), n_dates, first_date, last_date))

# ---- 4. Question 1: prices -------------------------------------------------

## 4a. Price table, one column per company, sorted by date.
## pivot_wider() is the reverse of pivot_longer(): long back to wide.
prices_wide <- bond_panel |>
  select(date, company, price) |>
  pivot_wider(names_from = company, values_from = price, names_sort = TRUE) |>
  arrange(date)

## 4c. From percent of face to dollars, for the United bond on the first date.
## A quote of 98.409 means 98.409% of the $1,000 face value.
ual     <- filter(bonds, ticker == FOCUS)
ual_row <- filter(bond_panel, ticker == FOCUS, date == first_date)
ual_dollar <- ual_row$price / 100 * FACE_VALUE

cat(sprintf(
  "\nQ1(c) %s (%s) on %s: quoted %.3f%% of face\n      => %.3f/100 x $%s = $%.2f per bond\n",
  ual$company, ual$cusip, format(first_date, "%d %b %Y"),
  ual_row$price, ual_row$price, format(FACE_VALUE, big.mark = ","), ual_dollar))

# Same conversion for every bond and every date, saved to a CSV file.
bond_panel <- mutate(bond_panel, price_dollar = price / 100 * FACE_VALUE)
assert_that(all(abs(bond_panel$price_dollar - bond_panel$price * 10) < 1e-9),
            msg = "Dollar price should be 10 x the percent quote at $1,000 face")

write_csv(select(bond_panel, date, ticker, company, price, price_dollar),
          file.path(output_dir, "bond_prices_dollars.csv"))

## 4d. Line graph of prices: date on the x-axis, price on the y-axis,
## one colored line per company.
source_caption <- paste0("Source: FactSet, ", format(first_date, "%d %b %Y"),
                         " to ", format(last_date, "%d %b %Y"), ".")

p_prices <- ggplot(bond_panel, aes(date, price, colour = company)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = firm_palette) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Daily price, percent of face value; one bond per issuer",
       subtitle = "",
       x = NULL, y = "Price (% of face value)",
       caption = source_caption) +
  theme_bond
ggsave(file.path(output_dir, "fig_q1_bond_prices.png"), p_prices, bg = "white",
       width = 9, height = 5.5, dpi = 300)


# ---- 5. Question 2: yields -------------------------------------------------
# Same steps as Question 1, with the yield instead of the price.
yields_wide <- bond_panel |>
  select(date, company, ytm) |>
  pivot_wider(names_from = company, values_from = ytm, names_sort = TRUE) |>
  arrange(date)

p_yields <- ggplot(bond_panel, aes(date, ytm, colour = company)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = firm_palette) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "Daily yield to maturity, percentage points",
       subtitle = "",
       x = NULL, y = "Yield to maturity (%)",
       caption = source_caption) +
  theme_bond
ggsave(file.path(output_dir, "fig_q2_bond_yields.png"), p_yields, bg = "white",
       width = 9, height = 5.5, dpi = 300)
