# =============================================================================
# Why a CARA Investor Rejects a Fair Gamble
# -----------------------------------------------------------------------------
# Companion figure for the Chapter 4 slide "CARA Utility Function":
#
#   u(W) = -exp(-a W) / a,  a = 0.0001,  W = $20,000
#   gamble: +$10,000 with prob 0.5, -$10,000 with prob 0.5  (fair, E[g] = 0)
#
# The investor compares
#   EU(reject) = u($20,000)
#   EU(accept) = 0.5 u($30,000) + 0.5 u($10,000)
# and rejects because u(.) is concave, so the chord joining u($10,000) to
# u($30,000) lies BELOW the curve. The mechanism is visible directly in the
# two vertical arrows: the $10,000 gain buys 855.5 of utility while the
# $10,000 loss costs 2,325.4, so a coin flip between them loses on average.
#
# Read along the x-axis, the same picture answers the following slide ("how
# much would this investor pay to avoid the gamble?"). The certainty
# equivalent CE solves u(CE) = EU(accept), which for CARA inverts in closed
# form,
#
#   CE = -log(-a * EU(accept)) / a = $15,662.19
#
# so the risk premium is E[wealth] - CE = $4,337.81. NOTE that the course's
# (1/2) a sigma^2 approximation gives $5,000 here, about $662 too high: it is
# a second-order approximation, accurate only when a * sigma is small, and
# a * sigma = 1 for this gamble. (For CARA the approximation always overstates
# the true premium, since log(cosh(x)) <= x^2 / 2.) A $1,000 version of the
# same gamble, a * sigma = 0.1, would match the approximation to within a dime
# -- worth saying out loud in class, since the slides use both.
#
# OUTPUT: output/RiskAversion/CARAUtility_Gamble.png
#
# Run top-to-bottom from a clean R session.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
# One-time install (uncomment if needed):
# install.packages(c("ggplot2", "dplyr", "tibble", "here", "scales",
#                    "assertthat"))
library(ggplot2)
library(dplyr)
library(tibble)
library(here)
library(scales)      # dollar-formatted labels
library(assertthat)

# ---- 1. Paths ---------------------------------------------------------------
here::i_am("teaching-materials/scripts/CARAUtility_Gamble.R")

out_dir <- here("teaching-materials", "output", "RiskAversion")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 2. Parameters from the slide -------------------------------------------
# Every number in the figure is derived from these four, so re-running after
# editing them produces a correct (if differently spaced) figure. The label
# placement below assumes a fair gamble, p_gain = 0.5 with equal gain and
# loss, which is what makes W and E[wealth] the same point on the x-axis.
a      <- 0.0001   # absolute risk aversion; for CARA, A(W) = a at every W
wealth <- 20000    # initial wealth W
h      <- 10000    # the gamble pays +h or -h
p_gain <- 0.5      # fair bet

cara_utility <- function(w, a) -exp(-a * w) / a

w_bad  <- wealth - h   # $10,000
w_good <- wealth + h   # $30,000

u_bad     <- cara_utility(w_bad, a)
u_good    <- cara_utility(w_good, a)
u_reject  <- cara_utility(wealth, a)                   # EU(reject) = u(W)
eu_accept <- p_gain * u_good + (1 - p_gain) * u_bad    # EU(accept) = E[u(W+g)]

# The two utility "steps" that make the rejection intuitive: equal dollar
# moves up and down, unequal utility consequences.
step_up   <- u_good - u_reject   # utility gained if the coin comes up heads
step_down <- u_reject - u_bad    # utility lost  if it comes up tails

# Certainty equivalent: u(CE) = EU(accept), inverted exactly for CARA.
ce           <- -log(-a * eu_accept) / a
risk_premium <- wealth - ce      # W = E[wealth] for a fair gamble
sigma        <- h                # sd of the gamble when p = 0.5 and payoff +/-h
rp_approx    <- 0.5 * a * sigma^2   # course approximation, (1/2) a sigma^2

# ---- 3. Sanity checks -------------------------------------------------------
# Fail loudly if the arithmetic stops matching what the figure asserts.
dom_lo <- w_bad - 1000
dom_hi <- w_good + 1000

curve_df <- tibble(w = seq(dom_lo, dom_hi, length.out = 600)) |>
  mutate(u = cara_utility(w, a))

assert_that(all(diff(curve_df$u) > 0),
            msg = "u(.) should be strictly increasing in wealth.")
assert_that(all(diff(diff(curve_df$u)) < 0),
            msg = "u(.) should be strictly concave.")
assert_that(eu_accept < u_reject,
            msg = "Concave u(.) implies EU(accept) < u(W) for a fair gamble.")
assert_that(step_down > step_up,
            msg = "Utility lost from -h should exceed utility gained from +h.")
assert_that(abs(cara_utility(ce, a) - eu_accept) < 1e-8,
            msg = "u(CE) should equal EU(accept) by construction.")
# Second, independent route to the same CE for a symmetric CARA gamble:
# 0.5(exp(-a(W-h)) + exp(-a(W+h))) = exp(-aW) cosh(a h).
assert_that(abs(ce - (wealth - log(cosh(a * h)) / a)) < 1e-6,
            msg = "CE should match the closed form W - log(cosh(a*h))/a.")
assert_that(ce < wealth && ce > w_bad,
            msg = "The certainty equivalent should lie between W - h and W.")
# (1/2) a sigma^2 is an upper bound on the CARA risk premium, tight only for
# small a * sigma. See the header note.
assert_that(risk_premium < rp_approx,
            msg = "(1/2) a sigma^2 should overstate the true CARA premium.")

# ---- 4. Formatting helpers --------------------------------------------------
guide_col <- "grey55"
curve_col <- "#D55E00"   # Okabe-Ito orange: the utility function
chord_col <- "#0072B2"   # Okabe-Ito blue:   the gamble (the chord)
note_col  <- "grey30"

dollar_lab <- function(x) label_dollar()(x)
cent_lab   <- function(x) label_dollar(accuracy = 0.01)(x)
util_lab   <- function(x) label_comma(accuracy = 0.1)(x)
# format(scientific = FALSE) keeps a = 0.0001 from printing as "1e-04".
a_lab      <- format(a, scientific = FALSE)

u_span <- max(curve_df$u) - min(curve_df$u)   # vertical reference unit

# Utility is ordinal, so the level of u(.) carries no meaning on its own --
# only comparisons do. Tick labels are suppressed on the y-axis and the few
# numbers that matter are printed in the annotations instead.

# The chord, as a function of wealth: the expected utility of a gamble over
# $10,000 and $30,000 whose mean wealth is w.
chord_at <- function(w) {
  u_bad + (w - w_bad) * (u_good - u_bad) / (w_good - w_bad)
}

# ---- 5. Rows of annotation below the curve ----------------------------------
# u(.) is increasing, so nothing is plotted below u($10,000) anywhere in the
# domain: that strip holds the risk-premium arrow and the x-axis labels.
arrow_row <- u_bad - 0.05 * u_span   # horizontal risk-premium arrow
drop_stop <- u_bad - 0.09 * u_span   # where the dashed drops end
tick_row  <- u_bad - 0.13 * u_span   # x-axis labels

# ---- 6. Plot ----------------------------------------------------------------
plot_cara <- ggplot(curve_df, aes(x = w, y = u)) +
  # Dashed drops at the gamble outcomes, at the certainty equivalent, and at
  # initial wealth (which is also expected wealth, since the bet is fair).
  annotate("segment", x = w_bad, xend = w_bad, y = drop_stop, yend = u_bad,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = ce, xend = ce, y = drop_stop, yend = eu_accept,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = wealth, xend = wealth, y = drop_stop, yend = u_reject,
           linetype = "dashed", colour = guide_col) +
  annotate("segment", x = w_good, xend = w_good, y = drop_stop, yend = u_good,
           linetype = "dashed", colour = guide_col) +
  # Level line for EU(accept), from the curve at CE across to the chord at W.
  annotate("segment", x = ce, xend = wealth + 0.04 * (w_good - w_bad),
           y = eu_accept, yend = eu_accept,
           linetype = "dashed", colour = guide_col) +
  # The chord: expected utility of the gamble.
  annotate("segment", x = w_bad, xend = w_good, y = u_bad, yend = u_good,
           colour = chord_col, linewidth = 0.9) +
  geom_line(linewidth = 1.3, colour = curve_col) +
  # The two utility steps: the same $10,000 either way, unequal utility.
  annotate("segment", x = w_bad, xend = w_good, y = u_reject, yend = u_reject,
           linetype = "dotted", colour = guide_col) +
  annotate("segment", x = w_good, xend = w_good, y = u_reject, yend = u_good,
           colour = note_col, linewidth = 0.7,
           arrow = arrow(length = unit(2, "mm"), ends = "both",
                         type = "closed")) +
  annotate("segment", x = w_bad, xend = w_bad, y = u_bad, yend = u_reject,
           colour = note_col, linewidth = 0.7,
           arrow = arrow(length = unit(2, "mm"), ends = "both",
                         type = "closed")) +
  # The two step arrows are left unlabelled on the plot -- their unequal
  # lengths are the point, and the numbers are in the caption.
  # The comparison that decides the question, at W: u(W) against EU(gamble).
  annotate("segment", x = wealth, xend = wealth, y = eu_accept, yend = u_reject,
           colour = note_col, linewidth = 0.7,
           arrow = arrow(length = unit(2, "mm"), ends = "both",
                         type = "closed")) +
  annotate("point", x = w_bad,  y = u_bad,  size = 2.8) +
  annotate("point", x = w_good, y = u_good, size = 2.8) +
  annotate("point", x = wealth, y = u_reject,  size = 2.8, colour = curve_col) +
  annotate("point", x = wealth, y = eu_accept, size = 2.8, colour = chord_col) +
  annotate("point", x = ce,     y = eu_accept, size = 2.8, colour = curve_col) +
  # Labels for the two utility levels: u(W) above the dotted W-level line and
  # to its left, EU(gamble) at the right end of its own dashed level line,
  # where the space below the chord is empty.
  annotate("text", x = wealth - 0.01 * (w_good - w_bad), y = u_reject,
           label = paste0("u(", dollar_lab(wealth), ") = EU(reject)"),
           hjust = 1, vjust = -0.7, size = 4.2, colour = curve_col) +
  annotate("text", x = wealth + 0.05 * (w_good - w_bad), y = eu_accept,
           label = "EU(gamble) = EU(accept)",
           hjust = 0, vjust = 0.4, size = 4.2, colour = chord_col) +
  # Direct labels instead of a legend.
  annotate("text", x = dom_hi, y = cara_utility(dom_hi, a),
           label = "u(W)", colour = curve_col, hjust = 1, vjust = -1,
           size = 5.2, fontface = "italic") +
  # The chord carries no label of its own: the blue "EU(gamble)" label at the
  # chord's midpoint already identifies it, and a second one crowded the
  # $30,000 arrow.
  # The verdict, in the open space above the curve on the left.
  annotate("text", x = dom_lo + 0.03 * (dom_hi - dom_lo),
           y = max(curve_df$u) - 0.03 * u_span,
           label = paste0("Reject: the gamble is worth only\nCE = ",
                          cent_lab(ce), " of certain wealth,\n",
                          cent_lab(risk_premium), " less than the ",
                          dollar_lab(wealth), " in hand."),
           hjust = 0, vjust = 1, size = 4.6, lineheight = 1.05,
           colour = note_col) +
  # The risk premium, read along the x-axis rather than the y-axis.
  annotate("segment", x = ce, xend = wealth, y = arrow_row, yend = arrow_row,
           colour = note_col, linewidth = 0.7,
           arrow = arrow(length = unit(2, "mm"), ends = "both",
                         type = "closed")) +
  annotate("text", x = wealth + 0.02 * (w_good - w_bad), y = arrow_row,
           label = paste0("risk premium = ", cent_lab(risk_premium)),
           hjust = 0, vjust = 0.4, size = 4.2, colour = note_col) +
  # x-axis labels, placed by hand (see section 5).
  annotate("text", x = w_bad, y = tick_row,
           label = dollar_lab(w_bad), size = 4.4) +
  annotate("text", x = ce, y = tick_row,
           label = paste0("CE = ", cent_lab(ce)), size = 4.4) +
  annotate("text", x = wealth, y = tick_row,
           label = paste0(dollar_lab(wealth), "\n= W = E[wealth]"),
           size = 4.4, vjust = 0.7, lineheight = 0.95) +
  annotate("text", x = w_good, y = tick_row,
           label = dollar_lab(w_good), size = 4.4) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.06, 0.06))) +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0.09, 0.05))) +
  labs(
    title    = paste0("A CARA investor turns down a fair ", dollar_lab(h),
                      " coin flip"),
    subtitle = paste0("u(W) = -exp(-aW)/a with a = ", a_lab, ", W = ",
                      dollar_lab(wealth), ";  the gamble pays +",
                      dollar_lab(h), " or -", dollar_lab(h),
                      " with probability ", p_gain, " each"),
    caption  = paste0(
      "The ", dollar_lab(h), " gain buys ", util_lab(step_up),
      " of utility but the ", dollar_lab(h), " loss costs ",
      util_lab(step_down), ", so EU(gamble) = ", util_lab(eu_accept),
      " falls short of\nu(", dollar_lab(wealth), ") = ", util_lab(u_reject),
      ": reject. The investor would pay up to ", cent_lab(risk_premium),
      " to avoid the gamble. The (1/2)aσ² rule gives ",
      cent_lab(rp_approx), ", which\noverstates the premium here because ",
      "aσ = ", a * sigma, " is not small; on a ", dollar_lab(1000),
      " version of the same gamble the two agree to the dime."
    ),
    x = "Wealth W", y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(colour = "grey30", size = 11),
    plot.caption     = element_text(colour = "grey35", size = 10.5, hjust = 0,
                                    margin = margin(t = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title.x     = element_text(margin = margin(t = 2)),
    plot.margin      = margin(14, 26, 10, 26)
  )

# ---- 7. Save ----------------------------------------------------------------
png_path <- file.path(out_dir, "CARAUtility_Gamble.png")
ggsave(png_path, plot_cara, width = 10, height = 7, dpi = 300, bg = "white")

message("Saved figure to:\n  ", png_path)
message("EU(reject) = u(W) = ", util_lab(u_reject))
message("EU(accept)         = ", util_lab(eu_accept))
message("Certainty equivalent = ", cent_lab(ce),
        ";  risk premium = ", cent_lab(risk_premium),
        "  (0.5*a*sigma^2 = ", cent_lab(rp_approx), ")")
