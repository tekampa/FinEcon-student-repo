# =============================================================================
# Jensen's Inequality for a Concave Utility Function
# -----------------------------------------------------------------------------
#
#   u(px + (1-p)y) >= p u(x) + (1-p) u(y)     for a concave u(.)
#
# i.e. the utility OF the expected outcome exceeds the EXPECTED utility of the
# gamble -- the curve sits above its own chord.
#
#
# Utility function: CRRA, u(x) = x^(1-gamma) / (1-gamma), gamma = 0.5 (same
# choice as UtilityConcavity.R, so both figures show the same curve).
#
# OUTPUT: output/RiskAversion/UtilityConcavity_Gamble.png
#
# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("tidyverse", "here", "assertthat"))
library(tidyverse)
library(here)
library(assertthat)

# ---- 1. Paths ----------------------------------------------------------------
here::i_am("teaching-materials/scripts/UtilityConcavity_Gamble.R")

out_dir <- here("teaching-materials", "output", "RiskAversion")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Utility function and gamble outcomes ---------------------------------
gamma <- 0.5  # CRRA risk-aversion coefficient (matches UtilityConcavity.R)

crra_utility <- function(x, gamma) {
  if (gamma == 1) log(x) else x^(1 - gamma) / (1 - gamma)
}

x_max <- 10
x_out <- 2    # low outcome  x
y_out <- 8    # high outcome y
p     <- 0.6  # weight on the LOW outcome x, i.e. the mixture is p*x + (1-p)*y

assert_that(x_out < y_out, msg = "x must be the low outcome and y the high outcome.")
assert_that(p > 0 && p < 1, msg = "p must be an interior probability/weight.")

mix_x   <- p * x_out + (1 - p) * y_out          # px + (1-p)y, on the x-axis
u_x     <- crra_utility(x_out, gamma)
u_y     <- crra_utility(y_out, gamma)
u_mix   <- crra_utility(mix_x, gamma)           # u(px + (1-p)y): the curve
chord_u <- p * u_x + (1 - p) * u_y              # pu(x) + (1-p)u(y): the chord

# --- Sanity check: Jensen's inequality must hold in the direction we're
# illustrating (curve above chord) -- if it doesn't, gamma isn't in the
# concave range and the diagram would show the wrong pattern.
assert_that(u_mix > chord_u,
            msg = "u(px+(1-p)y) should exceed pu(x)+(1-p)u(y) for a concave u(.).")

curve_df <- tibble(x = seq(0, x_max, length.out = 400)) %>%
  mutate(u = crra_utility(x, gamma))

# ---- 3. Plot ------------------------------------------------------------------
axis_col  <- "grey20"
guide_col <- "grey55"
curve_col <- "#D55E00"
chord_col <- "#0072B2"

y_top <- max(curve_df$u) * 1.12

p_plot <- ggplot(curve_df, aes(x = x, y = u)) +
  # Axes as plain arrows (schematic sketch, not a data chart).
  annotate("segment", x = 0, xend = x_max * 1.05, y = 0, yend = 0,
           colour = axis_col, linewidth = 0.6,
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = y_top,
           colour = axis_col, linewidth = 0.6,
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  # Dashed guide lines: vertical at x, mix_x, y; horizontal at each utility level.
  annotate("segment", x = x_out, xend = x_out, y = 0, yend = u_x,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = mix_x, xend = mix_x, y = 0, yend = u_mix,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = y_out, xend = y_out, y = 0, yend = u_y,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = 0, xend = x_out, y = u_x, yend = u_x,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = 0, xend = mix_x, y = chord_u, yend = chord_u,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = 0, xend = mix_x, y = u_mix, yend = u_mix,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = 0, xend = y_out, y = u_y, yend = u_y,
           linetype = "dashed", colour = guide_col) +
  # The chord from (x, u(x)) to (y, u(y)).
  annotate("segment", x = x_out, xend = y_out, y = u_x, yend = u_y,
           colour = chord_col, linewidth = 0.9) +
  # The curve itself.
  geom_line(linewidth = 1.3, colour = curve_col) +
  # Marked points.
  annotate("point", x = x_out, y = u_x, size = 2.6) +
  annotate("point", x = y_out, y = u_y, size = 2.6) +
  annotate("point", x = mix_x, y = chord_u, size = 2.6, colour = chord_col) +
  annotate("point", x = mix_x, y = u_mix, size = 2.6, colour = curve_col) +
  # x-axis labels. Sizes bumped up (~1.4x) so the diagram reads from the back
  # of a lecture hall off a projector; margins below are widened to match,
  # since the longer y-axis labels (hjust > 1) now need more room to the left.
  annotate("text", x = x_out, y = 0, label = "x",
           vjust = 1.8, size = 7.5, fontface = "italic") +
  annotate("text", x = mix_x, y = 0, label = "px+(1-p)y",
           vjust = 1.8, size = 6.4) +
  annotate("text", x = y_out, y = 0, label = "y",
           vjust = 1.8, size = 7.5, fontface = "italic") +
  # y-axis labels. chord_u and u_mix sit only ~0.2 apart on this axis (gamma
  # is gentle here, unlike the wealth-example script), so pinning both labels
  # to the y-axis at x=0 would collide. Instead of nudging one away from its
  # true height (which detaches it from its own dashed guide line), the mix
  # label is placed next to its own point on the curve at x = mix_x, where
  # there's open space to the right before the "y" cluster at x = y_out.
  annotate("text", x = 0, y = u_x, label = "u(x)",
           hjust = 1.15, size = 6.6) +
  annotate("text", x = 0, y = chord_u, label = "pu(x)+(1-p)u(y)",
           hjust = 1.05, size = 6.4) +
  annotate("text", x = mix_x, y = u_mix, label = "u(px+(1-p)y)",
           hjust = 1, vjust = -1, size = 6.4) +
  annotate("text", x = 0, y = u_y, label = "u(y)",
           hjust = 1.15, size = 6.6) +
  annotate("text", x = x_max * 1.03, y = 0, label = "x",
           vjust = 1.8, hjust = 1, size = 8.2, fontface = "italic") +
  annotate("text", x = x_max, y = crra_utility(x_max, gamma), label = "u",
           colour = curve_col, vjust = -0.8, hjust = 0, size = 9,
           fontface = "italic") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(14, 70, 14, 145))

# ---- 4. Save ------------------------------------------------------------------
png_path <- file.path(out_dir, "UtilityConcavity_Gamble.png")
ggsave(png_path, p_plot, width = 10, height = 7, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
