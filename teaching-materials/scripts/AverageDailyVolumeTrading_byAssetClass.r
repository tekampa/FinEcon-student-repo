# =============================================================================
# Average Daily Trading Volume by Asset Class
# -----------------------------------------------------------------------------
# Recreates the "Bond Trading Volume" figure (Chapter 2: Bonds) in R.
#
# Takeaway the figure supports:
#   U.S. Treasury debt is the most actively traded asset class
#
# DATA SOURCE: SIFMA Research -- https://www.sifma.org/research/statistics
#  as part of the Capital Markets Fact Book and data under the XLS data tables.
#   Data is updated in late July usually
#
#   NOTE ON UPDATING:
#     1. Go to the pages above, download excel file. In contents find
#     2. Read the annual ADV table and append the new year(s) as rows to
#        data/BondTradingVolume_SIFMA.csv (keep the column names). SIFMA updates 
#        retroactively, so you may need to update multiple years at once.
#     3. Re-run this script. Everything below adapts to whatever years are present.
#   The CSV in data/ is the reproducible fallback so this script runs offline.
#
# OUTPUT: output/Bonds/AverageDailyVolume_byAssetClass.png

# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("tidyverse", "here", "scales", "assertthat"))
library(tidyverse)   # dplyr + ggplot2 + tidyr + readr
library(here)        # build paths from the project root, not the working dir
library(scales)      # nice axis formatting (dollars, commas)
library(assertthat)  # programmatic sanity checks (catch data problems early)

# ---- 1. Paths (edit only if the directory layout changes) ------------------------
# Anchor all paths to the repo root. here::i_am() declares where THIS script
# lives relative to the repo root; here then resolves every path from that root,
# no matter your working directory (RStudio, VS Code, or `Rscript`). If you see
# an error here, you are running from outside the FinEcon-student-repo folder --
# open the repo (or its .Rproj) first.
here::i_am("teaching-materials/scripts/AverageDailyVolumeTrading_byAssetClass.r")

data_file <- here("teaching-materials", "data", "BondTradingVolume_SIFMA.csv")
out_dir   <- here("teaching-materials", "output", "Bonds")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Read the data -------------------------------------------------------
# Wide format: one row per Year, one column per asset class (USD billions).
adv_wide <- read_csv(data_file, show_col_types = FALSE)

# --- Sanity checks: fail loudly if the data isn't what we expect -------------
assert_that(nrow(adv_wide) >= 2, msg = "Need at least two years of data to draw lines.")
assert_that(!any(is.na(adv_wide$Year)), msg = "Some Year values are missing.")
# Volumes are dollar amounts -> must be non-negative:
assert_that(all(adv_wide %>% select(-Year) %>% unlist() >= 0, na.rm = TRUE),
            msg = "Found a negative trading volume -- check the data.")

# ---- 3. Reshape to long format for ggplot --------------------------
# ggplot wants one row per (Year, asset class, value).
asset_levels <- c("UST", "Equity", "MBS", "Corporate", "Munis")  # rough size order
asset_labels <- c(UST = "U.S. Treasuries",
                   Equity = "Equity",
                   MBS = "Agency MBS",
                   Corporate = "Corporate bonds",
                   Munis = "Municipals")

adv_long <- adv_wide %>%
  pivot_longer(cols = -Year, names_to = "asset", values_to = "adv") %>%
  filter(!is.na(adv)) %>%
  mutate(asset = factor(asset, levels = asset_levels))

# ---- 4. Direct labels (Healy best practice: label lines directly, drop legend) --------
last_year <- max(adv_long$Year)
end_labels <- adv_long %>%
  filter(Year == last_year) %>%
  mutate(label = asset_labels[as.character(asset)])

# ---- 5. Colour palette (Okabe-Ito, colorblind-safe) ------------------------
# Treasuries in a strong blue; everything else muted so UST reads as the story.
pal <- c(UST       = "#0072B2",  # blue  (highlight)
         Equity    = "#E69F00",  # orange
         MBS       = "#009E73",  # green
         Corporate = "#CC79A7",  # pink
         Munis     = "#999999")  # grey

# ---- 6. Plot ----------------------------------------------------------------
p <- ggplot(adv_long, aes(x = Year, y = adv, colour = asset)) +
  geom_line(aes(linewidth = asset == "UST")) +          # thicker line for UST
  geom_point(size = 1.4) +
  # Direct end-of-line labels instead of a legend:
  geom_text(data = end_labels,
            aes(label = label),
            hjust = 0, nudge_x = 0.15, size = 3.4, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.7), guide = "none") +
  scale_x_continuous(breaks = seq(min(adv_long$Year), last_year, by = 2),
                     # room on the right for the labels:
                     expand = expansion(mult = c(0.02, 0.28))) +
  scale_y_continuous(labels = scales::label_dollar(suffix = "B"),
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, NA)) +
  labs(
    title    = "Average daily trading volume by asset class (USD billions)",
    subtitle = "",
    x = NULL, y = "Average daily volume",
    caption  = paste0("Source: SIFMA Research (www.sifma.org/research/statistics). ",
                      "Fixed-income and equity average daily trading volume, ",
                      min(adv_long$Year), "-", last_year, ".")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey30"),
    plot.caption  = element_text(colour = "grey45", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ---- 7. Save ----------------------------------------------------------------
png_path <- file.path(out_dir, "AverageDailyVolume_byAssetClass.png")
ggsave(png_path, p, width = 9, height = 5.5, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
