### TESTING FUNCTIONS
franv <- all[all$user.id=="305",]

###### Plotting check
ggplot(user_circular_plot,
       aes(x = mean_month_rad, y = rho)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
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
    title = "Seasonality Strength and Mean Month (Polar View)"
  ) +
  theme_minimal()

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