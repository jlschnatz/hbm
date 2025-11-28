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
# We only need to vary 'p' (probability of black marble).
# N is fixed at 100.
waypoints <- tribble(
  ~point_id , ~prob ,
          1 , 0.05  , # Start with low probability
          2 , 0.20  ,
          3 , 0.50  , # Symmetric
          4 , 0.80  ,
          5 , 0.95  , # High probability
          6 , 0.05 # Return to start
)

# 2. Interpolate and Calculate Parametrizations
n_steps <- 30
fixed_N <- 100

plot_data <- waypoints |>
  mutate(next_prob = lead(prob)) |>
  filter(!is.na(next_prob)) |>
  group_by(point_id) |>
  reframe(
    step = 1:n_steps,
    # Interpolate p (probability)
    curr_prob = prob + (next_prob - prob) * (step / n_steps),

    # Calculate Expected Value (Mean) and Std Dev for the label
    curr_mu = fixed_N * curr_prob,
    curr_sigma = sqrt(fixed_N * curr_prob * (1 - curr_prob)),

    # Pre-format the label string
    # We use 'p' for probability and 'E[x]' for expected count
    label_text = paste0("&theta;<sub>i</sub> = ", round(curr_prob, 2))
  ) |>
  mutate(frame_id = row_number())

# 3. Generate Probability Mass (PMF)
# Note: x is integers 0 to 100, not a sequence of decimals
expanded_data <- plot_data |>
  cross_join(data.frame(x = 0:fixed_N)) |>
  mutate(y = dbinom(x, size = fixed_N, prob = curr_prob))

# 4. Plot
p <- ggplot(expanded_data, aes(x = x, y = y)) +
  # CRITICAL CHANGE: discrete distributions require bars (geom_col), not area/lines
  geom_col(fill = "black", width = 0.8) +

  ggtext::geom_richtext(
    mapping = aes(label = label_text),
    x = 50, # Center the text
    y = 0.18, # Pin text near top of the new y-scale
    label.color = NA,
    fill = NA,
    size = 6.5,
    family = "Optima",
    color = "black"
  ) +
  labs(
    title = "<b>Binomial Distribution</b> (n<sub>i</sub> = 100)",
    y = "Probability Mass",
    x = "Number of Black Marbles (y<sub>i</sub>)"
  ) +
  # Set limits appropriate for probabilities (0 to 1), not density
  coord_cartesian(ylim = c(0, 0.2), expand = FALSE) +
  theme_light(base_family = "Optima") +
  theme(
    plot.title = element_markdown(size = 18),
    axis.title = element_text(size = 12),
    plot.subtitle = element_blank(),
    axis.title.x = element_markdown()
  ) +
  transition_time(frame_id)

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

anim_save("img/binomial_distribution_tour.gif")
