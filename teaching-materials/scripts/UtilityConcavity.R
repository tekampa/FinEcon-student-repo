# =============================================================================
# Concavity of u(.)
# -----------------------------------------------------------------------------
#
# This is a CONCEPT diagram, not a data plot -- like the source slide, it is
# drawn schematically (no axis numbers), so it intentionally departs from the
# usual "label every axis with units" chart practice.
#
# Utility function: CRRA, u(x) = x^(1-gamma) / (1-gamma).
# gamma = 0.5
#
# OUTPUT: output/RiskAversion/UtilityConcavity.png
#
# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("tidyverse", "here", "assertthat"))
library(tidyverse)   # dplyr + ggplot2 + tidyr
library(here)        # build paths from the project root, not the working dir
library(assertthat)  # programmatic sanity checks (catch mistakes early)

# ---- 1. Paths ----------------------------------------------------------------
here::i_am("teaching-materials/scripts/UtilityConcavity.R")

out_dir <- here("teaching-materials", "output", "RiskAversion")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Utility function -----------------------------------------------------
gamma <- 0.5  # CRRA risk-aversion coefficient (chosen for a visible, gentle bend)

crra_utility <- function(x, gamma) {
  if (gamma == 1) log(x) else x^(1 - gamma) / (1 - gamma)
}

x_max <- 10
curve_df <- tibble(x = seq(0, x_max, length.out = 400)) %>%
  mutate(u = crra_utility(x, gamma))

# --- Sanity checks: fail loudly if the curve isn't actually concave ----------
assert_that(all(diff(curve_df$u) > 0),
            msg = "u(x) should be strictly increasing in wealth.")
assert_that(all(diff(diff(curve_df$u)) < 0),
            msg = "u(x) should be strictly concave (diminishing marginal utility).")

# ---- 3. Plot ------------------------------------------------------------------
# Schematic diagram: no gridlines, no axis numbers, just the curve, the origin
# dot, and the two labels the source sketch carries ("u" and "x").
axis_col <- "grey20"

p <- ggplot(curve_df, aes(x = x, y = u)) +
  geom_line(linewidth = 1.3, colour = "#D55E00") +
  # Axes drawn as plain arrows, since this is a chalkboard-style sketch, not a
  # data chart -- there are no meaningful tick numbers to report.
  annotate("segment", x = 0, xend = x_max * 1.04, y = 0, yend = 0,
           colour = axis_col, linewidth = 0.6,
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  annotate("segment", x = 0, xend = 0,
           y = 0, yend = max(curve_df$u) * 1.12,
           colour = axis_col, linewidth = 0.6,
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  geom_point(data = tibble(x = 0, u = 0), size = 2.6, colour = "black") +
  annotate("text", x = x_max * 1.02, y = 0, label = "W",
           vjust = 1.6, hjust = 1, size = 6, fontface = "italic") +
  annotate("text", x = x_max, y = crra_utility(x_max, gamma),
           label = "u(W)", colour = "#D55E00",
           vjust = -0.8, hjust = 0, size = 6.5, fontface = "italic") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 8, r = 20, b = 20, l = 8))

# ---- 4. Save ------------------------------------------------------------------
png_path <- file.path(out_dir, "UtilityConcavity.png")
ggsave(png_path, p, width = 7, height = 5, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
