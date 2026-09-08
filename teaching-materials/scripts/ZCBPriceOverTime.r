# =============================================================================
# Zero-Coupon Bond Price Over Time
# -----------------------------------------------------------------------------
# Recreates the price-path figure (Chapter 2: Bonds) in R, using the numbers
# from the worked example taught alongside it.
#
# The bond in the slide:
#   Zero-coupon bond, par (face) value $1,000, 10 years to maturity at
#   origination, priced at a 5% yield to maturity with ANNUAL compounding.
#
# Takeaway the figure supports:
#   Holding the yield fixed, a zero's price RISES over time and converges to
#   par at maturity -- the "pull to par." The zero pays no coupons, so the
#   entire 5% return arrives as price appreciation. The path is convex: the
#   dollar gain per year grows as maturity approaches, because each year's
#   gain is 5% of a larger and larger price.
#
# Benchmark values from the worked example:
#   P(10) = 1000 / 1.05^10 = 613.91
#   P(9)  = 1000 / 1.05^9  = 644.61
#   P(8)  = 1000 / 1.05^8  = 676.84
#   P(1)  = 1000 / 1.05^1  = 952.38
#   (P(T) indexes YEARS REMAINING, so P(10) is the price at origination.)
#
# NO EXTERNAL DATA: the path is computed directly from the zero-coupon pricing
#   formula, so this script runs offline from a clean session.
#
# OUTPUT: output/Bonds/ZCBPriceOverTime.png
#
# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("tidyverse", "here", "scales", "assertthat"))
library(tidyverse)   # dplyr + ggplot2 + tidyr
library(here)        # build paths from the project root, not the working dir
library(scales)      # nice axis formatting (dollars, percents)
library(assertthat)  # programmatic sanity checks (catch mistakes early)

# ---- 1. Paths (edit only if the directory layout changes) ------------------
# Anchor all paths to the repo root. here::i_am() declares where THIS script
# lives relative to the repo root; here then resolves every path from that root,
# no matter your working directory (RStudio, VS Code, or `Rscript`). If you see
# an error here, you are running from outside the FinEcon-student-repo folder --
# open the repo (or its .Rproj) first.
here::i_am("teaching-materials/scripts/ZCBPriceOverTime.r")

out_dir <- here("teaching-materials", "output", "Bonds")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Bond parameters (change these to re-price a different zero) ---------
face              <- 1000   # par / face value ($)
ytm               <- 0.05   # annual yield to maturity (5%), held FIXED
original_maturity <- 10     # years to maturity at origination
freq              <- 1      # compounding periods per year (1 = annual)

# ---- 3. Pricing function ----------------------------------------------------
# Price a zero-coupon bond given an ANNUAL yield `y` and the time remaining.
# There are no coupons, so the price is just the discounted face value:
#   P = F / (1 + i)^n
# where i = y / freq is the per-period yield and n = years_remaining * freq.
# This is the same TI BA II Plus / HP 12C setup as a coupon bond with PMT = 0
# (N = periods, I/Y = per-period rate, FV = face).
zcb_price <- function(y, years_remaining) {
  i <- y / freq
  n <- years_remaining * freq
  face / (1 + i)^n
}

# ---- 4. Build the price path ------------------------------------------------
# Walk forward one year at a time. Calendar time rises, time remaining falls.
zcb <- tibble(
  years_since_issue = 0:original_maturity,
  years_to_maturity = original_maturity - years_since_issue
) %>%
  mutate(price = zcb_price(ytm, years_to_maturity))

# --- Sanity checks: fail loudly if the numbers aren't sensible ---------------
# (1) Pull to par: with the yield fixed, price must strictly rise over time.
assert_that(all(diff(zcb$price) > 0),
            msg = "ZCB price should rise over time when the yield is constant.")
# (2) Convexity: the annual price gain should itself grow as maturity nears.
assert_that(all(diff(diff(zcb$price)) > 0),
            msg = "Annual price gains should grow as maturity approaches.")
# (3) Terminal condition: at maturity the bond pays exactly face value.
assert_that(abs(zcb$price[zcb$years_to_maturity == 0] - face) < 1e-6,
            msg = "At maturity, ZCB price must equal par value.")
# (4) Benchmarks: reproduce the four prices quoted in the worked example.
benchmarks <- tibble(
  years_to_maturity = c(10, 9, 8, 1),
  target_price      = c(613.91, 644.61, 676.84, 952.38)
) %>%
  left_join(zcb, by = "years_to_maturity")
assert_that(all(abs(benchmarks$price - benchmarks$target_price) < 0.01),
            msg = "Computed prices do not match the worked-example values.")

# ---- 5. Reference points: the prices quoted in the example ------------------
# Label the same five prices the slide walks through. Placement is set per
# point (hjust/nudge) so no label sits on top of the price path: the four
# early labels hang BELOW the line, the last one sits ABOVE it, and each is
# anchored on the side with empty space.
mark_points <- zcb %>%
  filter(years_to_maturity %in% c(10, 9, 8, 1, 0)) %>%
  mutate(
    label = case_when(
      years_to_maturity == 10 ~ "P(10) = $613.91",
      years_to_maturity == 9  ~ "P(9) = $644.61",
      years_to_maturity == 8  ~ "P(8) = $676.84",
      years_to_maturity == 1  ~ "P(1) = $952.38",
      years_to_maturity == 0  ~ "P(0) = $1,000"
    ),
    # hjust 0 = text starts at the point; hjust 1 = text ends at the point.
    lab_hjust = if_else(years_to_maturity == 0, 1, 0),
    lab_x     = years_since_issue + if_else(years_to_maturity == 0, -0.25, 0.25),
    lab_y     = price + if_else(years_to_maturity == 0, 30, -26)
  )

# ---- 6. Color palette (Okabe-Ito, colorblind-safe) ------------------------
line_col <- "#0072B2"  # blue        for the price path (the story)
mark_col <- "#D55E00"  # vermillion  for the quoted reference prices

# ---- 7. Plot ----------------------------------------------------------------
p <- ggplot(zcb, aes(x = years_since_issue, y = price)) +
  # Par reference: the level the price is being pulled toward.
  geom_hline(yintercept = face, linetype = "dashed",
             colour = "grey55", linewidth = 0.5) +
  annotate("text", x = 0, y = face, label = "Par = $1,000",
           hjust = 0, vjust = -0.6, size = 4.6, colour = "grey35") +
  geom_line(linewidth = 1.8, colour = line_col) +
  geom_point(data = mark_points, colour = mark_col, size = 3.4) +
  # Direct labels on the quoted prices (Healy best practice: label the data).
  geom_text(data = mark_points,
            aes(x = lab_x, y = lab_y, label = label, hjust = lab_hjust),
            size = 4.8, colour = "grey20", lineheight = 0.95) +
  scale_x_continuous(breaks = 0:original_maturity,
                     # extra room on the right for the direct labels
                     expand = expansion(mult = c(0.02, 0.20))) +
  scale_y_continuous(labels = scales::label_dollar(),
                     breaks = seq(600, 1000, by = 50),
                     limits = c(570, 1050),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    # Kept short so nothing runs past the figure edge at projector font sizes.
    title    = "ZCB's price is pulled to par",
    subtitle = "10-year zero, $1,000 par, fixed 5% YTM, annual compounding",
    x = "Years since origination",
    y = "Bond price",
    caption  = "P = $1,000 / 1.05^T, where T is the years remaining."
  ) +
  # Larger base_size so axis text/titles stay legible when projected in class.
  theme_minimal(base_size = 18) +
  theme(
    # Align title/subtitle/caption to the whole figure, not the panel, so they
    # start at the left margin and have the full width to run in.
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30"),
    plot.caption     = element_text(colour = "grey45", hjust = 0, size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- 8. Save ----------------------------------------------------------------
png_path <- file.path(out_dir, "ZCBPriceOverTime.png")
# Squarer aspect ratio (vs a wide 9x5.5): when the slide sets width=\linewidth,
# a taller figure fills more of the available slide height, so it reads bigger.
ggsave(png_path, p, width = 8, height = 6, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
