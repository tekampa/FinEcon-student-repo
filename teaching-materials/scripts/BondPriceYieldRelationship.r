# =============================================================================
# Bond Price-Yield Relationship
# -----------------------------------------------------------------------------
# Recreates the price-yield curve (Chapter 2: Bonds) in R.
#
# The bonds in the slide:
#   7% coupon, ANNUAL coupons, par (face) value $1,000, at two maturities:
#   a 5-year bond and a 30-year bond.
#
# Takeaway the figure supports:
#   Bond price is a downward-sloping, CONVEX function of the interest rate
#   (yield to maturity). Price falls as yields rise, the curve flattens at
#   higher yields, and price = par exactly when YTM = coupon rate (7%).
#   Comparing the two maturities shows the LONGER bond's price is far more
#   sensitive to yield changes -- its curve is much steeper.
#
# NO EXTERNAL DATA: the curve is computed directly from the bond-pricing
#   formula, so this script runs offline from a clean session.
#
# OUTPUT: output/Bonds/BondPriceYieldRelationship.png
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
here::i_am("teaching-materials/scripts/BondPriceYieldRelationship.r")

out_dir <- here("teaching-materials", "output", "Bonds")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Bond parameters (change these to re-price a different bond) ---------
face        <- 1000        # par / face value ($)
coupon_rate <- 0.07        # annual coupon rate (7%)
maturities  <- c(5, 30)    # years to maturity -- one curve per maturity
freq        <- 1           # coupon payments per year (1 = annual compounding)

# ---- 3. Pricing function ----------------------------------------------------
# Price a level-coupon bond given an ANNUAL yield to maturity `y` and a
# `maturity` in years. We discount at the per-period yield (y / freq), matching
# how the TI BA II Plus / HP 12C are set up (N = periods, I/Y = per-period rate,
# PMT = coupon per period).
#   P = C * [1 - (1+i)^-n] / i   +   F / (1+i)^n
# where i = y / freq is the per-period yield and n = maturity * freq.
bond_price <- function(y, maturity) {
  i   <- y / freq
  n   <- maturity * freq            # total coupon periods
  cpn <- face * coupon_rate / freq  # cash coupon per period
  # Annuity value of the coupons + present value of the face repayment.
  # The ifelse guards the i = 0 knife-edge (price is just the undiscounted sum).
  pv_coupons <- ifelse(i == 0,
                       cpn * n,
                       cpn * (1 - (1 + i)^(-n)) / i)
  pv_face    <- face / (1 + i)^n
  pv_coupons + pv_face
}

# ---- 4. Build the price-yield curves ----------------------------------------
# Sweep annual YTM from 0% to 20% and price EACH maturity at each yield.
py <- expand_grid(maturity = maturities,
                  ytm      = seq(0, 0.20, by = 0.0005)) %>%
  mutate(price     = bond_price(ytm, maturity),
         mat       = factor(maturity, levels = sort(maturities)),
         mat_label = paste0(maturity, "-year"))

# --- Sanity checks: fail loudly if the numbers aren't sensible ---------------
# Check the shape of every maturity's curve independently.
for (m in maturities) {
  price_m <- py %>% filter(maturity == m) %>% arrange(ytm) %>% pull(price)
  # (1) Inverse relationship: price must strictly fall as yield rises.
  assert_that(all(diff(price_m) < 0),
              msg = paste0(m, "-year: price should fall as yield rises."))
  # (2) Convexity: the price curve should be convex (second difference > 0).
  assert_that(all(diff(diff(price_m)) > 0),
              msg = paste0(m, "-year: price-yield curve should be convex."))
  # (3) Par condition: when YTM = coupon rate, price = face value.
  assert_that(abs(bond_price(coupon_rate, m) - face) < 1e-6,
              msg = paste0(m, "-year: at YTM = coupon, price must equal par."))
}

# ---- 5. Reference point: par (YTM = coupon rate) ----------------------------
# Highlight the point where the bond sells at par -- the pivot of the discount
# vs premium intuition taught alongside this figure.
par_point <- tibble(
  ytm   = coupon_rate,
  price = face,
  label = "YTM = coupon = 7%\nPrice = par = $1,000"
)

# ---- 6. Color palette (Okabe-Ito, colorblind-safe) ------------------------
# One colour per maturity; longest maturity in the strong blue (the story).
pal      <- c("5" = "#009E73",   # green   (short bond, flat curve)
              "30" = "#0072B2")  # blue    (long bond, steep curve)
mark_col <- "#D55E00"            # vermillion for the shared par reference point

# Direct end-of-line labels (Healy best practice: label lines, drop legend).
# Anchor at the right edge of each curve, where the two are well separated.
end_labels <- py %>%
  filter(ytm == max(ytm)) %>%
  mutate(label = mat_label)

# ---- 7. Plot ----------------------------------------------------------------
p <- ggplot(py, aes(x = ytm, y = price, colour = mat)) +
  geom_line(linewidth = 1.6) +
  # Dashed guides dropping from the shared par point to each axis:
  geom_segment(data = par_point, inherit.aes = FALSE,
               aes(x = ytm, xend = ytm, y = -Inf, yend = price),
               linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_segment(data = par_point, inherit.aes = FALSE,
               aes(x = -Inf, xend = ytm, y = price, yend = price),
               linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_point(data = par_point, inherit.aes = FALSE,
             aes(x = ytm, y = price), colour = mark_col, size = 3.6) +
  # Par-point label, up and to the right of the point, clear of the curves.
  geom_text(data = par_point, inherit.aes = FALSE,
            aes(x = ytm, y = price, label = label),
            hjust = 0, vjust = 0, nudge_x = 0.003, nudge_y = 100,
            size = 5, colour = "grey20", lineheight = 0.95) +
  # Direct labels at the end of each curve:
  geom_text(data = end_labels, aes(label = label),
            hjust = 0, nudge_x = 0.002, size = 5.4, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  # A 5% tick grid plus the coupon rate (7%). Coarser than every-2% so the
  # large projector fonts don't collide near the coupon tick.
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = sort(c(seq(0, 0.20, by = 0.05), coupon_rate)),
                     # extra room on the right for the direct curve labels
                     expand = expansion(mult = c(0.01, 0.16))) +
  scale_y_continuous(labels = scales::label_dollar(),
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, NA)) +
  labs(
    title    = "Longer maturities are more yield-sensitive",
    subtitle = "7% coupon, annual coupons, $1,000 par value",
    x = "Yield to maturity",
    y = "Bond price",
    caption  = "Price computed from the coupon-bond present-value formula."
  ) +
  # Larger base_size so axis text/titles stay legible when projected in class.
  theme_minimal(base_size = 18) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30"),
    plot.caption     = element_text(colour = "grey45", hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- 8. Save ----------------------------------------------------------------
png_path <- file.path(out_dir, "BondPriceYieldRelationship.png")
# Squarer aspect ratio (vs a wide 9x5.5): when the slide sets width=\linewidth,
# a taller figure fills more of the available slide height, so it reads bigger.
ggsave(png_path, p, width = 7.5, height = 6, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
