######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Figure with observations per week

############ Load data ----
#load("iNat.RData") # Created using 1_Data.R

############ Loading libraries ----
source("0_Libraries.R")

############ Loading iNat data ----
source("1_Data.R")

# DPI and font variables
dpi_resolution <- 600
font_name <- "Arial"

start_date     <- as.Date("2016-01-01")

# ── PREP ──────────────────────────────────────────────────────────────────────
df <-inat
df <- df %>%
  mutate(observed_on = as.Date(observed_on)) %>%
  filter(observed_on >= start_date)

# ── DAILY → WEEKLY aggregation ────────────────────────────────────────────────
occurrences_per_week <- df %>%
  mutate(week_start = floor_date(observed_on, unit = "week", week_start = 1)) %>%
  count(week_start, name = "occurrences") %>%
  complete(
    week_start = seq.Date(floor_date(start_date, "week", week_start = 1),
                          as.Date("2024-12-31"), by = "week"),
    fill = list(occurrences = 0)
  ) %>%
  filter(week_start <= max(week_start[occurrences > 0]))  # trim trailing zeros

# ── WEEKDAY distribution ──────────────────────────────────────────────────────
weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

weekday_counts <- df %>%
  mutate(weekday = factor(substr(weekdays(observed_on), 1, 3), levels = weekday_levels)) %>%
  count(weekday, name = "n", .drop = FALSE) %>%
  mutate(pct = n / sum(n) * 100)

# ── INSET plot ────────────────────────────────────────────────────────────────
p_inset <- ggplot(weekday_counts, aes(x = weekday, y = pct)) +
  geom_col(fill = "darkgreen") +
  scale_y_continuous(limits = c(0, 25)) +
  labs(title = "Weekday Distribution (%)", x = NULL, y = NULL) +
  theme_bw(base_family = font_name) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white", color = "black", linewidth = 0.3),
    plot.title       = element_text(size = 11, hjust = 0.5),
    axis.text        = element_text(size = 11),
    panel.grid       = element_blank()                          # <-- grid removed
  )

# ── MAIN plot ─────────────────────────────────────────────────────────────────
year_starts <- seq.Date(as.Date("2016-01-01"), as.Date("2025-09-30"), by = "year")
p_main <- ggplot(occurrences_per_week, aes(x = week_start, y = occurrences)) +
  geom_line(color = "darkgreen", linewidth = 0.6) +
  geom_area(fill = "darkgreen", alpha = 0.3) +
  annotation_custom(
    grob = ggplotGrob(p_inset),
    xmin = as.numeric(as.Date("2016-05-01")),                  # <-- much narrower
    xmax = as.numeric(as.Date("2018-09-30")),                  # <-- (was 2016)
    ymin = 6000,
    ymax = 10000
  ) +
  scale_x_date(
    limits = c(min(occurrences_per_week$week_start), as.Date("2025-12-31")),
    breaks = year_starts,
    labels = format(year_starts, "%Y"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 10000)) +
  labs(x = "Year", y = "Occurrences") +
  theme_bw(base_family = font_name) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(size = 19),
    axis.text.x      = element_text(size = 14, hjust = 0),
    axis.text.y      = element_text(size = 14, angle = 90, vjust = 0.5),
    panel.grid       = element_blank()                          # <-- grid removed
  )

print(p_main)

# ── SAVE ──────────────────────────────────────────────────────────────────────
ggsave("obsperweek_plot.png", plot = p_main,
       width = 15, height = 10, dpi = dpi_resolution, bg = "white")


