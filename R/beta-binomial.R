# Bottle Flip Simulation
# Bayesian beta-binomial model for black/white marbles
# This script simulates data for a hierarchical binomial process (like 20 different
# bottles, each flipped 100 times) and then uses Stan to fit a beta-binomial
# model to recover the parameters.

# Load required packages
set.seed(25) # Set a random seed for reproducible results

# Check if 'pacman' (a package manager) is installed, and install it if not
if (!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}

# Use pacman to load all required packages. It will install them if they are missing.
pacman::p_load(
  rstan, # For interfacing with Stan
  bayesplot, # For plotting MCMC results
  ggplot2, # For general data visualization
  #sysfonts, # For font management
  systemfonts,
  #showtext, # For using custom fonts in plots
  ragg,
  dplyr, # For data manipulation
  distributional, # For working with probability distribution objects
  ggtext, # For enhanced text rendering in ggplot2
  tidyr, # For data pivoting
  patchwork, # For combining ggplot2 plots
  ggdist, # For visualizing distributions
  scico,
  latex2exp, # For converting LaTeX to expressions
  ggh4x,
  fs
)

dir_delete("figures")
dir_create("figures/single")

# Load a specific Google Font ("Source Serif 4") for use in plots
#font_add_google("Source Serif 4", "font")
# Tell R to automatically use the 'showtext' package for rendering text in plots
#showtext_auto()
# Set the default dots-per-inch (DPI) for 'showtext' to ensure high-quality text
dpi <- 500
#showtext_opts(dpi = dpi)
# Set a default theme for all 'bayesplot' graphs, using a theme from 'sjPlot'
# and specifying our custom font
bayesplot_theme_set(sjPlot::theme_sjplot(base_family = "Optima"))
color_scheme_set("gray")

fit_mod <- stan_model(
  "stan/beta-binomial.stan",
  model_name = "beta-binomial",
  verbose = TRUE,
  save_dso = TRUE
)

col_vals <- rev(scico(n = 3, palette = "grayC", begin = 0.2, end = 1))

# Functions
sim_betabinom <- function(k, mu, phi, n_trials, seed) {
  withr::with_seed(seed, {
    n <- rep(n_trials, k)
    alpha <- mu * phi
    beta <- (1 - mu) * phi
    theta <- rbeta(k, alpha, beta)
    y <- rbinom(k, size = n, prob = theta)
    n <- c(n, 1)
    y <- c(y, 1)
    k <- k + 1
    list(
      stan_data = list(
        N = k,
        n = n,
        y = y
      ),
      table_data = data.frame(
        id = 1:k,
        n = n,
        y = y
      )
    )
  })
}

plot_slab <- function(x, ..., title = NULL, log = FALSE, log_phi = FALSE) {
  x <- x |>
    tidybayes::spread_draws(...) |>
    tidyr::pivot_longer(
      cols = -c(.chain, .iteration, .draw),
      names_to = "parameter",
      values_to = "value"
    )

  if (log_phi) {
    x <- x |>
      dplyr::mutate(
        value = ifelse(
          parameter == "phi",
          log(value),
          value
        )
      )
  }

  if (log) {
    p <- ggplot2::ggplot(x, ggplot2::aes(x = log(value), y = parameter))
  } else {
    p <- ggplot2::ggplot(x, ggplot2::aes(x = value, y = parameter))
  }
  p +
    ggdist::stat_slab(
      mapping = ggplot2::aes(fill = ggplot2::after_stat(level)),
      .width = seq(0, 1, 0.2),
      normalize = "xy",
      show.legend = FALSE
    ) +
    #ggdist::stat_spike(
    #  mapping = ggplot2::aes(linetype = ggplot2::after_stat(at)),
    #  at = c("Median" = median, "Mode" = ggdist::Mode),
    #  normalize = "xy",
    #  size = 1.5
    #) +
    ggplot2::facet_wrap(~parameter, scales = "free", ncol = 1) +
    #ggplot2::scale_linetype(
    #  name = "Point Estimates",
    #  labels = c("dotted", "solid")
    #) +
    scico::scale_fill_scico_d(
      name = "Percentiles",
      palette = "lipari",
      labels = c(
        "0-10:90-100",
        "10-20:80-90",
        "20-30:70-80",
        "30-40:60-70",
        "40-50:50-60"
      ),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(override.aes = list(size = 0.5))
    ) +
    ggplot2::labs(x = "Parameter Value", y = NULL) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_light(base_family = "Optima") +
    ggplot2::theme(
      #plot.margin = ggplot2::margin(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(),
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = "bold"),
      strip.background = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 5),
        size = 11
      )
    )
}


#--- Plot the Priors ---
# This plot visualizes the prior distributions *used in the Stan model*
# (Uniform for mu, Exponential for phi) to show what assumptions
# the model will start with.
p_priors <- data.frame(
  name = c("&phi; ~ exp(1)", "&mu; ~ unif(0, 1)"),
  # Create distribution objects using the 'distributional' package
  xdist = c(dist_exponential(rate = 1), dist_uniform(0, 1))
) |>
  ggplot(aes(xdist = xdist)) + # Map the distribution objects to the aesthetic
  facet_wrap(~name, scales = "free", nrow = 1) + # Create separate plots for each prior
  stat_slab(normalize = "panels", expand = TRUE, scale = 1, fill = "black") +
  xlab(NULL) +
  scale_y_continuous(
    name = "Density",
    expand = expansion(),
    limits = c(0, 1.5)
  ) +
  theme_linedraw(base_family = "Optima") +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    strip.text = element_markdown(color = "white", size = 11),
    strip.background = element_rect(fill = "black"),
    axis.text.y.left = element_text(margin = margin(r = 10))
  )

# Save the prior visualization plot to a PNG file
dpi <- 300
ggsave(
  "figures/priors.png",
  plot = p_priors,
  width = 8,
  height = 6,
  dpi = dpi,
  bg = "white"
)

# ------------------------------------------------------
# RUN TWO SIMULATIONS WITH DIFFERENT PHI VALUES
# φ = 0.7 and φ = 10
# ------------------------------------------------------

phi_values <- c(0.8, 20)
mu_values <- c(0.5, 0.2)

sim_values <- data.frame(mu = mu_values, phi = phi_values)


for (i in seq_len(nrow(sim_values))) {
  cli::cli_h1(
    "\nSimulation for φ = {sim_values$phi[i]} and μ = {sim_values$mu[i]}"
  )

  # --- Simulate data ---
  sim <- sim_betabinom(
    k = 7,
    mu = sim_values$mu[i],
    phi = sim_values$phi[i],
    n_trials = 100,
    seed = 24
  )

  data_stan <- sim$stan_data
  table_data <- sim$table_data

  # --- Create marble dotplot ---
  marbles <- table_data |>
    rename(successes = y) |>
    mutate(failures = n - successes) |>
    rowwise() |>
    mutate(type = list(c(rep("black", successes), rep("white", failures)))) |>
    unnest(type) |>
    group_by(id) |>
    mutate(
      index = row_number() - 1,
      x = index %% 10,
      y = index %/% 10
    ) |>
    ungroup() |>
    mutate(
      id = factor(
        paste("Bag", id),
        levels = paste("Bag", seq_len(nrow(table_data)))
      )
    )

  p_marbles <- ggplot(marbles, aes(x = x, y = y, fill = type)) +
    geom_point(shape = 21, size = 3, color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("black" = "black", "white" = "white")) +
    facet_wrap(~id, ncol = 4) +
    coord_equal() +
    labs(title = paste0("Marble Composition per Bag")) +
    theme_minimal(base_family = "Optima") +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.margin = margin(5, 5, 5, 5),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  ggsave(
    paste0("figures/dotplot_marbles_phi_", sim_values$phi[i], ".png"),
    plot = p_marbles,
    width = 7,
    height = 5,
    dpi = dpi,
    bg = "white"
  )

  for (j in as.character(unique(marbles$id))) {
    marbles |>
      mutate(id = as.character(id)) |>
      filter(id == j) |>
      distinct() |>
      ggplot(aes(x = x, y = y, fill = type)) +
      geom_point(shape = 21, size = 5, color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("black" = "black", "white" = "white")) +
      coord_equal() +
      labs(title = j) +
      theme_minimal(base_family = "Optima") +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        #plot.margin = margin(5, 5, 5, 5),
        axis.text = element_blank(),
        axis.title = element_blank()
      ) -> p_single_bag

    ggsave(
      paste0(
        "figures/single/marbles_",
        sub("Bag ", "", j),
        "_phi_",
        sim_values$phi[i],
        ".png"
      ),
      plot = p_single_bag,
      width = 2.5,
      height = 2.5,
      dpi = dpi,
      bg = "transparent"
    )
  }

  # --- Fit the model ---
  samples_mod <- sampling(
    object = fit_mod,
    data = data_stan,
    iter = 1e4,
    chains = 4,
    algorithm = "NUTS",
    seed = 42,
    open_progress = TRUE,
    cores = 8,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )

  summary(samples_mod)$summary[, c(
    "mean",
    "sd",
    "2.5%",
    "25%",
    "50%",
    "75%",
    "97.5%",
    "n_eff",
    "Rhat"
  )] |>
    round(1) |>
    print()

  tidybayes::spread_draws(samples_mod, theta[i]) |>
    ungroup() |>
    mutate(
      group = factor(
        paste("Bag", i),
        levels = paste("Bag", seq_len(nrow(table_data)))
      )
    ) |>
    ggplot(aes(y = group, x = theta)) +
    stat_slab(
      mapping = aes(fill = after_stat(level)),
      .width = seq(0, 1, 0.2),
      normalize = "xy",
      show.legend = FALSE
    ) +
    #stat_spike(
    #  mapping = aes(linetype = after_stat(at)),
    #  at = c("Median" = median, "Mode" = ggdist::Mode),
    #  normalize = "xy",
    #  size = 1.5
    #) +
    scale_fill_scico_d(
      name = "Percentiles",
      palette = "lipari",
      labels = c(
        "0-10:90-100",
        "10-20:80-90",
        "20-30:70-80",
        "30-40:60-70",
        "40-50:50-60"
      ),
      guide = guide_legend(reverse = TRUE)
    ) +
    #scale_linetype(
    #  name = "Point Estimates",
    #  labels = c("dotted", "solid")
    #) +
    scale_x_continuous(
      name = "p(&theta;<sub>i</sub> | data)",
      limits = c(0, 1),
      expand = expansion()
    ) +
    scale_y_discrete(
      name = NULL,
      labels = paste0("Bag ", seq_len(nrow(table_data)))
    ) +
    labs(
      title = paste0("Posterior Distributions of Bag Thetas"),
      subtitle = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_light(base_family = "Optima") +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = ggtext::element_markdown()
    ) -> p_single_bag_thetas

  ggsave(
    paste0("figures/mcmc_areas_thetas_phi_", sim_values$phi[i], ".png"),
    plot = p_single_bag_thetas,
    width = 7,
    height = 5,
    dpi = dpi,
    bg = "white"
  )

  # --- Posterior plots for hyperparameters ---

  p1_hyper <- plot_slab(
    samples_mod,
    mu,
    phi,
    title = paste0("Posterior of Hyperparameters"),
    log_phi = TRUE
  ) +
    facetted_pos_scales(
      x = list(
        mu = scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2),
          expand = expansion()
        ),
        phi = scale_x_continuous(
          limits = c(-3, 3),
          breaks = scales::breaks_pretty(n = 6),
          expand = expansion(),
        )
      ),
      y = list(
        mu = scale_y_discrete(
          labels = c(TeX("$\\frac{\\alpha}{\\alpha + \\beta}$"))
        ),
        phi = scale_y_discrete(
          labels = c(TeX("$\\ln(\\alpha + \\beta)$"))
        )
      )
    )

  p2_hyper <- plot_slab(
    samples_mod,
    alpha,
    beta,
    log = TRUE,
    title = paste0("Posterior of Hyperparameters")
  ) +
    facetted_pos_scales(
      x = list(
        alpha = scale_x_continuous(
          #limits = c(0, 6),
          breaks = scales::breaks_pretty(n = 6),
          expand = expansion(),
          #transform = "log"
        ),
        beta = scale_x_continuous(
          #limits = c(0, 6),
          breaks = scales::breaks_pretty(n = 6),
          expand = expansion(),
          #transform = "log"
        )
      ),
      y = list(
        alpha = scale_y_discrete(
          labels = c(TeX("log(\\alpha = \\mu \\phi"))
        ),
        beta = scale_y_discrete(
          labels = c(TeX("\\beta = (1 - \\mu) \\phi"))
        )
      )
    )

  p_hyper <- p1_hyper +
    p2_hyper +
    plot_layout(ncol = 2, guides = "collect") &
    theme(
      #legend.position = "bottom",
    )

  ggsave(
    paste0("figures/mcmc_areas_hyperparams_phi_", sim_values$phi[i], ".png"),
    plot = p1_hyper,
    width = 7,
    height = 5,
    dpi = dpi,
    bg = "white"
  )

  p_comb <- p_marbles +
    p_single_bag_thetas +
    p1_hyper +
    plot_layout(
      nrow = 1,
      widths = c(1, 1, 1),
      guides = "collect"
    ) &
    theme(
      legend.position = "bottom"
    )

  ggsave(
    paste0("figures/combined_phi_", sim_values$phi[i], ".svg"),
    plot = p_comb,
    width = 18,
    height = 6,
    dpi = dpi,
    bg = "transparent"
  )
}
