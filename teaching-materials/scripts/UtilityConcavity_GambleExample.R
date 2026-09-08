# =============================================================================
# Jensen's Inequality for a Concave Utility Function -- Wealth-Gamble Example
# -----------------------------------------------------------------------------
# Same diagram as UtilityConcavity_Gamble.R, but plotted using
# the course's own worked example (Chapter 4, "Expected Utility Theory at
# Work"): a person with wealth W = $100,000 faces a gamble that pays a gain of
# $2,000 with probability p, or a loss of $1,600 with probability (1-p):
#
#   x = W - 1,600 = $98,400   (bad outcome)
#   y = W + 2,000 = $102,000  (good outcome)
#
# The slide leaves p general but here use p = 0.5 to illustrate the gamble.
# here purely to place a mixture point on the diagram -- change `p_gain` below
# to match whatever value you want to illustrate.
#
# RISK-AVERSION PARAMETER IS VERY HIGH FOR ILLUSTRATION (gamma = 150):
# A $3,600 gamble is small next to $100,000 of wealth (3.6%). With any
# economically reasonable CRRA risk aversion (estimates in the literature run
# roughly 1-10), the curvature of u(.) between $98,400 and $102,000 is
# imperceptible on a plot
# gamma = 150 is chosen ONLY to
# make the bend visible in this figure -- it is not a claim about anyone's
# actual risk aversion, and the script says so again next to where gamma is
# set.
#
# WHY WE DIVIDE WEALTH BY $100,000 INSIDE u(): with gamma = 150, raw dollar
# amounts raised to the power (1-gamma) = -149 underflow to exactly 0 in
# double-precision floating point (100000^-149 is far below the smallest
# representable positive double). Computing u on wealth/$100,000 instead keeps
# the arithmetic in range. This rescaling multiplies u(.) everywhere by the
# same positive constant (100000^(gamma-1)), which is a valid positive-scalar
# transformation of a CRRA utility function -- it changes no expected-utility
# comparison, so the diagram is still a faithful (if rescaled) picture of the
# true u(wealth), not a schematic stand-in.
#
# OUTPUT: output/RiskAversion/UtilityConcavity_GambleExample.png
#
# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("tidyverse", "here", "scales", "assertthat"))
library(tidyverse)
library(here)
library(scales)      # dollar-formatted axis labels
library(assertthat)

# ---- 1. Paths ----------------------------------------------------------------
here::i_am("teaching-materials/scripts/UtilityConcavity_GambleExample.R")

out_dir <- here("teaching-materials", "output", "RiskAversion")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. The wealth gamble (Chapter 4, "Expected Utility Theory at Work") ----
wealth <- 100000
gain   <- 2000
loss   <- 1600
# Illustrative only -- the slide leaves p general.
p_gain <- 0.5

# With gain and loss this close in size, p = 0.5 puts E[wealth] just $200 from
# W, and puts u(reject)/u(E[wealth])/u(y) within a tight vertical band (the
# curve has already nearly saturated by then). Rather than moving p to force
# more spacing, the plot below places every label by hand, right next to its
# own point, instead of stacking them all on a shared axis -- see section 4.

x_dollar   <- wealth - loss                                # $98,400, bad outcome
y_dollar   <- wealth + gain                                 # $102,000, good outcome
mix_dollar <- (1 - p_gain) * x_dollar + p_gain * y_dollar   # E[wealth | gamble]

assert_that(x_dollar == 98400,  msg = "Bad outcome should be W - $1,600 = $98,400.")
assert_that(y_dollar == 102000, msg = "Good outcome should be W + $2,000 = $102,000.")
assert_that(p_gain > 0 && p_gain < 1, msg = "p_gain must be an interior probability.")

# ---- 3. CRRA utility, to scale -----------------------------------------------
# See header: gamma is set absurdly high on purpose, purely so the Jensen gap
# is visible at this dollar scale. A realistic gamma (1-10) would draw as a
# straight line here.
gamma      <- 150
scale_unit <- 100000  # avoids floating-point underflow at this gamma; see header

crra_utility <- function(w_dollar, gamma) {
  w <- w_dollar / scale_unit
  if (gamma == 1) log(w) else w^(1 - gamma) / (1 - gamma)
}

u_x      <- crra_utility(x_dollar, gamma)
u_y      <- crra_utility(y_dollar, gamma)
u_mix    <- crra_utility(mix_dollar, gamma)                       # u(E[wealth])
u_reject <- crra_utility(wealth, gamma)                           # u(W): utility of NOT gambling
chord_u  <- (1 - p_gain) * u_x + p_gain * u_y                     # E[u(wealth)]: EU(accept)

# --- Sanity checks: fail loudly if the numbers aren't what the diagram claims -
pad <- 200  # dollars of padding shown on each side of the two outcomes
dom_lo <- x_dollar - pad
dom_hi <- y_dollar + pad
curve_df <- tibble(w = seq(dom_lo, dom_hi, length.out = 400)) %>%
  mutate(u = crra_utility(w, gamma))

assert_that(all(diff(curve_df$u) > 0),
            msg = "u(.) should be strictly increasing in wealth.")
assert_that(all(diff(diff(curve_df$u)) < 0),
            msg = "u(.) should be strictly concave (diminishing marginal utility).")
assert_that(u_mix > chord_u,
            msg = "u(E[wealth]) should exceed E[u(wealth)] for a concave u(.).")

# Whether an EU-maximizer accepts the gamble depends on EU(accept) = chord_u vs.
# u(reject) = u_reject -- NOT on the (unrelated) Jensen comparison above. Build
# the caption sentence from whichever way it actually comes out, so this stays
# correct if wealth/gain/loss/p_gain/gamma are edited later.
accept_conclusion <- if (chord_u > u_reject) {
  "At this level of risk aversion, EU(accept) > EU(reject): an EU-maximizer still takes the gamble."
} else {
  "At this level of risk aversion, EU(accept) < EU(reject): a risk-averse EU-maximizer declines this positive-EMV gamble."
}

# ---- 4. Plot ------------------------------------------------------------------
guide_col <- "grey55"
curve_col <- "#D55E00"
chord_col <- "#0072B2"
dollar_lab <- function(x) scales::label_dollar()(x)


# Label placement: u(reject), u(E[wealth]) and u(y) all fall within a narrow
# vertical band once gamma is this large (the curve has already flattened),
# and wealth ($100,000) and E[wealth] ($100,200) sit only $200 apart on the
# x-axis. A shared axis of tick labels would collide for both reasons, so
# every point gets its own label placed by hand right next to it, using
# whichever direction (up/down/left/right) is actually open around that
# point on the curve -- the standard fix for crowded direct labels.
lab_size <- 3.9

# Two staggered rows for the x-axis labels: wealth ($100,000) and E[wealth]
# ($100,200) are only $200 apart, so their text would collide on one row.
# Both rows sit in the blank strip below u_x -- the curve is monotonic, so
# nothing is plotted below its own left endpoint across the WHOLE domain.
u_range  <- max(curve_df$u) - min(curve_df$u)
y_row1   <- min(curve_df$u) - 0.06 * u_range
y_row2   <- min(curve_df$u) - 0.12 * u_range

p_plot <- ggplot(curve_df, aes(x = w, y = u)) +
  annotate("segment", x = x_dollar, xend = x_dollar, y = -Inf, yend = u_x,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = wealth, xend = wealth, y = -Inf, yend = u_reject,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = mix_dollar, xend = mix_dollar, y = -Inf, yend = u_mix,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = y_dollar, xend = y_dollar, y = -Inf, yend = u_y,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = x_dollar, xend = y_dollar, y = u_x, yend = u_y,
           colour = chord_col, linewidth = 0.9) +
  geom_line(linewidth = 1.3, colour = curve_col) +
  annotate("point", x = x_dollar, y = u_x, size = 2.6) +
  annotate("point", x = y_dollar, y = u_y, size = 2.6) +
  annotate("point", x = wealth, y = u_reject, size = 2.6) +
  annotate("point", x = mix_dollar, y = chord_u, size = 2.6, colour = chord_col) +
  annotate("point", x = mix_dollar, y = u_mix, size = 2.6, colour = curve_col) +
  # -- direct labels, one per point, each nudged into open space -----------
  # u_x is the curve's global minimum, so the strip directly BELOW it is
  # blank across the entire domain -- safe to center a label there without
  # running off the left edge (which hjust > 1 was doing before).
  annotate("text", x = x_dollar, y = u_x, label = paste0("u(", dollar_lab(x_dollar), ")"),
           hjust = -0.1, vjust = 1.6, size = lab_size) +
  # reject and mix are only $200 apart, so their labels are pushed in
  # OPPOSITE directions along the (nearly blank) space above the flattening
  # curve: reject's text ends at its point (grows leftward), mix's text
  # starts at its point (grows rightward) -- so they grow apart, not together.
  annotate("text", x = wealth, y = u_reject,
           label = paste0("u(", dollar_lab(wealth), ") = EU(reject)"),
           hjust = 1.05, vjust = -0.5, size = lab_size) +
  annotate("text", x = mix_dollar, y = chord_u,
           label = paste0("EU(accept) = ", scales::percent(p_gain), " x ", "u(",dollar_lab(y_dollar), ")",
                       " +\n", scales::percent(1 - p_gain), " x ",  "u(",dollar_lab(x_dollar), ")"),
           hjust = -0.1, vjust = 1, size = lab_size, lineheight = 0.9) +
  annotate("text", x = mix_dollar, y = u_mix, label = paste0("u(E[gamble]) = ", "u(", dollar_lab(mix_dollar), ")"),
           hjust = 0, vjust = -1.6, size = lab_size) +
  annotate("text", x = y_dollar, y = u_y, label = paste0("u(", dollar_lab(y_dollar), ")"),
           hjust = 0.7, vjust = -1.0, size = lab_size) +
  annotate("text", x = dom_hi, y = crra_utility(dom_hi, gamma), label = "u",
           colour = curve_col, vjust = -0.9, hjust = 0.5, size = 6.5,
           fontface = "italic") +
  # -- x-axis labels, staggered into two rows (see note above `y_row1`) ----
  annotate("text", x = x_dollar, y = y_row1, label = dollar_lab(x_dollar), size = lab_size) +
  annotate("text", x = wealth,   y = y_row1, label = dollar_lab(wealth),   size = lab_size) +
  annotate("text", x = mix_dollar, y = y_row2, label = dollar_lab(mix_dollar), size = lab_size) +
  annotate("text", x = y_dollar, y = y_row1, label = dollar_lab(y_dollar), size = lab_size) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(breaks = NULL,
                      expand = expansion(mult = c(0.20, 0.14))) +
  labs(
    title    = "A concave u(.) makes the sure outcome worth more than the gamble",
    subtitle = paste0("W = ", dollar_lab(wealth), ", gain ", dollar_lab(gain),
                       " vs. loss ", dollar_lab(loss),
                       "\nE[gamble] = ", scales::percent(p_gain), " x ", dollar_lab(y_dollar),
                       " + ", scales::percent(1 - p_gain), " x ", dollar_lab(x_dollar),
                       " = ", dollar_lab(mix_dollar)),
    caption = accept_conclusion,
    x = "Wealth", y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(colour = "grey30", size = 11),
    plot.caption     = element_text(colour = "grey35", size = 10.5, hjust = 0,
                                     margin = margin(t = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank()
  )

# ---- 5. Save ------------------------------------------------------------------
png_path <- file.path(out_dir, "UtilityConcavity_GambleExample.png")
ggsave(png_path, p_plot, width = 8.5, height = 6.3, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
