pacman::p_load(
  ggplot2,
  gganimate,
  dplyr,
  tidyr,
  gifski,
  systemfonts,
  ragg,
  ggtext
)

# 1. Define Waypoints
waypoints <- tribble(
  ~point_id , ~alpha , ~beta ,
          1 , 0.5    , 0.5   ,
          2 , 1.0    , 1.0   ,
          3 , 2.0    , 2.0   ,
          4 , 9      , 9     ,
          5 , 9      , 1     ,
          6 , 1      , 9     ,
          7 , 0.2    , 0.2
)

# 2. Interpolate and Calculate Parametrizations
n_steps <- 60

plot_data <- waypoints |>
  mutate(next_alpha = lead(alpha), next_beta = lead(beta)) |>
  filter(!is.na(next_alpha)) |>
  group_by(point_id) |>
  reframe(
    step = 1:n_steps,
    # Interpolate Alpha/Beta first
    curr_alpha = alpha + (next_alpha - alpha) * (step / n_steps),
    curr_beta = beta + (next_beta - beta) * (step / n_steps),

    # Calculate Mu and Phi based on the CURRENT interpolated values
    curr_mu = curr_alpha / (curr_alpha + curr_beta),
    curr_phi = curr_alpha + curr_beta,

    # Pre-format the label string to ensure fixed width (prevents text jitter)
    label_text = sprintf(
      "&alpha;: %5.1f   &beta;: %5.1f <br> &mu;:    %5.1f   &phi;:  %5.1f",
      curr_alpha,
      curr_beta,
      curr_mu,
      curr_phi
    )
  ) |>
  mutate(frame_id = row_number())

# 3. Generate Density Curves
expanded_data <- plot_data |>
  cross_join(data.frame(x = seq(0, 1, length.out = 300))) |>
  mutate(y = dbeta(x, curr_alpha, curr_beta))
# 4. Plot
p <- ggplot(expanded_data, aes(x = x, y = y)) +
  geom_area(fill = "black", alpha = 0.9) +
  geom_line(color = "black", linewidth = 1.2) +
  ggtext::geom_richtext(
    mapping = aes(label = label_text),
    x = 0.5,
    y = 8.5,
    label.color = NA,
    fill = NA,
    size = 6.5,
    family = "Optima",
    color = "black"
  ) +
  labs(
    title = "Beta Distribution",
    y = "Density",
    x = "P(<theta;<sub>i</sub>)"
  ) +
  coord_cartesian(ylim = c(0, 10), expand = FALSE) +
  theme_light(base_family = "Optima") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 12),
    axis.title.x = element_markdown(),
    element_text(family = "Optima"),
    # Remove standard subtitle space since we are using geom_text
    plot.subtitle = element_blank()
  ) +
  transition_time(frame_id) +
  view_follow(fixed_y = TRUE)

# 5. Render
final_animation <- animate(
  p,
  nframes = nrow(plot_data),
  fps = 30,
  width = 5,
  device = "ragg_png",
  height = 5,
  units = "in",
  res = 300,
  renderer = gifski_renderer()
)

anim_save("img/beta_distribution_tour.gif")
