################################################################################
user_circular <- user_char[,c("mean_month_numeric", "rho", "early_cat")]
user_circular_plot <- user_circular %>%
  filter(!is.na(mean_month_numeric), !is.na(rho)) %>%
  mutate(mean_month_rad = (mean_month_numeric-1) * 2 * pi / 12)
###########################
ggplot(user_circular_plot,
       aes(x = mean_month_rad, y = rho, color = early_cat)) +
  geom_point(alpha = 0.7, size = 2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 11 * 2 * pi / 12, length.out = 12),
    labels = month.abb,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  labs(
    x = NULL,
    y = "ρ (circular concentration)",
    title = "Seasonality Strength and Mean Month (Polar View)",
    color = "Group"
  ) +
  theme_minimal()

################################################################################
user_circular <- user_char[user_char$RorV=="Resident",c("mean_month_numeric", "rho", "early_cat")]
user_circular_plot <- user_circular %>%
  filter(!is.na(mean_month_numeric), !is.na(rho)) %>%
  mutate(mean_month_rad = (mean_month_numeric-1) * 2 * pi / 12)
###########################
ggplot(user_circular_plot,
       aes(x = mean_month_rad, y = rho, color = early_cat)) +
  geom_point(alpha = 0.7, size = 2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 11 * 2 * pi / 12, length.out = 12),
    labels = month.abb,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  labs(
    x = NULL,
    y = "ρ (circular concentration)",
    title = "Seasonality (Residents)",
    color = "Group"
  ) +
  theme_minimal()

################################################################################
user_circular <- user_char[user_char$RorV=="Visitor",c("mean_month_numeric", "rho", "early_cat")]
user_circular_plot <- user_circular %>%
  filter(!is.na(mean_month_numeric), !is.na(rho)) %>%
  mutate(mean_month_rad = (mean_month_numeric-1) * 2 * pi / 12)
###########################
ggplot(user_circular_plot,
       aes(x = mean_month_rad, y = rho, color = early_cat)) +
  geom_point(alpha = 0.7, size = 2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 11 * 2 * pi / 12, length.out = 12),
    labels = month.abb,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  labs(
    x = NULL,
    y = "ρ (circular concentration)",
    title = "Seasonality (Visitors)",
    color = "Group"
  ) +
  theme_minimal()