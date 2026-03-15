plot_hyper <- function(
  x,
  ...,
  title = NULL,
  log = FALSE,
  log_phi = FALSE,
  filename
) {
  nms <- c("&mu;", "&kappa;")
  names(nms) <- c("mu", "phi")
  x <- x |>
    tidybayes::spread_draws(...) |>
    tidyr::pivot_longer(
      cols = -c(.chain, .iteration, .draw),
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(stat = mean(value), .by = c(parameter))

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
  plt <- p +
    ggdist::stat_slab(
      normalize = "xy",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(aes(xintercept = stat)) +
    ggplot2::facet_wrap(
      ~parameter,
      scales = "free",
      nrow = 1,
      labeller = ggplot2::labeller(parameter = nms)
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::scale_y_discrete(
      name = NULL,
      labels = NULL,
      expand = ggplot2::expansion()
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        mu = ggplot2::scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.25),
          expand = expansion()
        ),
        phi = ggplot2::scale_x_continuous(
          limits = c(0, 15),
          breaks = seq(0, 15, 5),
          expand = expansion(),
        )
      )
    ) +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella", base_size = 11) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(),
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "transparent"),
      strip.text = ggtext::element_markdown(color = "black", size = 11),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 5),
        size = 11
      ),
      panel.spacing.x = ggplot2::unit(0.5, "cm"),
      plot.margin = ggplot2::margin(l = 3, r = 3)
    )

  ggplot2::ggsave(filename, plot = plt, width = 3, height = 1.5)
  return(filename)
}

plot_marbles <- function(marble_data, file, arrow = FALSE) {
  if (arrow) {
    marble_data$id <- with(marble_data, paste0("↑\n", id))
  }
  plt <- ggplot2::ggplot(
    dplyr::slice_sample(marble_data, n = nrow(marble_data)),
    ggplot2::aes(x = x, y = y, fill = type)
  ) +
    ggplot2::geom_point(
      shape = 21,
      size = 2,
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("black" = "black", "white" = "white")
    ) +
    ggplot2::facet_wrap(~id, nrow = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_family = "TeX Gyre Pagella") +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "transparent"),
      strip.text = ggplot2::element_text(
        size = 11,
        margin = ggplot2::margin(b = 5)
      ),
      plot.margin = ggplot2::margin(l = 1, r = 1, t = 0, b = 3),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.25, "cm")
    )

  ggplot2::ggsave(
    file,
    plt,
    width = 8,
    height = 1.3,
    create.dir = TRUE
  )
}


plot_marbles_single <- function(marble_data, base_file) {
  files_created <- character() # Initialize an empty vector to store file paths

  for (j in as.character(unique(marble_data$id))) {
    filename <- paste0(base_file, "_", sub("Bag ", "", j), ".svg")
    plt <- marble_data |>
      dplyr::mutate(id = as.character(id)) |>
      dplyr::filter(id == j) |>
      dplyr::distinct() |>
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = type)) +
      ggplot2::geom_point(
        shape = 21,
        size = 5,
        color = "black",
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c("black" = "black", "white" = "white")
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = j) +
      ggplot2::theme_minimal(base_family = "TeX Gyre Pagella") +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 16,
          hjust = 0.5,
          face = "bold"
        ),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )

    ggplot2::ggsave(
      filename = filename,
      plot = plt,
      width = 2.5,
      height = 2.5,
      create.dir = TRUE
    )

    files_created <- c(files_created, filename)
  }

  return(files_created)
}


plot_slab <- function(posterior_samples, table_data, filename) {
  plt <- tidybayes::spread_draws(posterior_samples, theta[i]) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      group = factor(
        paste("Bag", i),
        levels = paste("Bag", seq_len(nrow(table_data)))
      )
    ) |>
    dplyr::mutate(stat_theta = mean(theta), .by = group) |>
    ggplot2::ggplot(ggplot2::aes(y = group, x = theta)) +
    ggplot2::facet_wrap(
      ~group,
      nrow = 1,
      scales = "free_y"
    ) +
    ggdist::stat_slab(
      height = 0.9,
      normalize = "xy",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(aes(xintercept = stat_theta)) +
    ggplot2::scale_x_continuous(
      name = "P(&theta;<sub>i</sub> | y<sub>1</sub>, ..., y<sub>n</sub>)",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = c("0", "", "0.5", "", "1"),
      expand = expansion()
    ) +
    ggplot2::scale_y_discrete(
      name = NULL,
      labels = NULL,
      expand = expansion()
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella", base_size = 11) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(l = 3, r = 3, b = 0, 5),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(size = 11),
      strip.background = ggplot2::element_rect(fill = "transparent"),
      strip.text = ggplot2::element_text(color = "black", size = 11),
      panel.spacing.x = ggplot2::unit(0.25, "cm")
    )

  ggplot2::ggsave(filename, plt, width = 8, height = 1.5)
  return(filename)
}

plot_beta_static <- function(filename, width = 8, height = 2) {
  # parameters
  alpha <- c(1, 10, 2, 8, 0.25)
  beta <- c(1, 10, 8, 2, 0.25)

  # reparametrization
  mu <- alpha / (alpha + beta)
  kappa <- alpha + beta

  plot_data <- data.frame(alpha, beta, mu, kappa)
  plot_data$dist <- distributional::dist_beta(alpha, beta)
  plot_data$id <- seq_len(nrow(plot_data))

  plot_data$label <- with(
    plot_data,
    sprintf(
      "&alpha; = %s, &beta; = %s<br>&mu; = %s, &kappa; = %s",
      alpha,
      beta,
      mu,
      kappa
    )
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(xdist = dist)) +
    ggplot2::facet_grid(cols = ggplot2::vars(label)) +
    ggdist::stat_slab(normalize = "groups", scale = 1) +
    ggplot2::geom_vline(aes(xintercept = mu)) +
    ggplot2::labs(y = "Density", x = NULL) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1, .2),
      expand = ggplot2::expansion()
    ) +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella") +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0.5, "cm"),
      strip.text = ggtext::element_markdown(
        color = "black",
        size = 11,
        hjust = 0.5
      ),
      strip.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(r = 7.5, l = 2.5)
    ) -> plt

  ggplot2::ggsave(filename, plt, width = width, height = height)
}


plot_shapebias <- function(fit, filename) {
  draws_choice_prob <- fit$draws("choice_prob", format = "df")

  # Define the empirical data from Smith et al. (2002)
  # The trained 19-month-olds chose Shape 70% of the time.
  # The remaining 30% is distributed to Texture and Color (15% each).
  empirical_data <- data.frame(
    parameter = c("choice_prob[1]", "choice_prob[2]", "choice_prob[3]"),
    empirical_value = c(0.70, 0.15, 0.15)
  )

  # 3. Generate the overlay plot
  plt <- draws_choice_prob |>
    tidyr::pivot_longer(
      dplyr::starts_with("choice_prob"),
      names_to = "parameter"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = parameter, y = value)) +
    ggdist::stat_slab(alpha = 0.3, scale = 0.8) +
    geomtextpath::geom_texthline(
      yintercept = 1 / 3,
      label = "Random Guessing",
      family = "TeX Gyre Pagella",
      size = 3.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = variable, y = median, color = "Model Prediction"),
      data = posterior::summarise_draws(draws_choice_prob),
      inherit.aes = FALSE,
      size = 0.75,
      shape = 21,
      stroke = 1.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = parameter,
        y = empirical_value,
        color = "Empirical Data (19-months-olds)"
      ),
      data = empirical_data,
      inherit.aes = FALSE,
      size = 0.75,
      shape = 21,
      stroke = 1.5
    ) +
    ggplot2::scale_y_continuous(
      name = "Relative Choice Probability",
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_x_discrete(
      name = NULL,
      labels = c("Shape Match", "Texture Match", "Color Match")
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "Model Prediction" = "black", #"#013B4F",
        "Empirical Data (19-months-olds)" = "darkgray" #"#AD0000"
      )
    ) +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5),
        size = 10,
        color = "black"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line(linejoin = "mitre"),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 7.5),
        size = 10
      ),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "top",
      legend.text = ggplot2::element_text(size = 10),
      legend.margin = ggplot2::margin()
    )

  ggplot2::ggsave(filename, plt, create.dir = TRUE, width = 6, height = 2.5)
  return(filename)
}


###

plot_combined <- function(
  marble_data,
  posterior_samples,
  table_data,
  file,
  arrow = FALSE
) {
  if (arrow) {
    marble_data$id <- with(marble_data, paste0("<b>&#x2191;</b><br>", id))
  }
  plt1 <- ggplot2::ggplot(
    dplyr::slice_sample(marble_data, n = nrow(marble_data)),
    ggplot2::aes(x = x, y = y, fill = type)
  ) +
    ggplot2::geom_point(
      shape = 21,
      size = 2,
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("black" = "black", "white" = "white")
    ) +
    ggplot2::facet_wrap(~id, nrow = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_family = "TeX Gyre Pagella") +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "transparent"),
      strip.text = ggtext::element_markdown(
        size = 11,
        margin = ggplot2::margin(b = 5)
      ),
      plot.margin = ggplot2::margin(l = 1, r = 1, t = 0, b = 3),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.25, "cm")
    )

  plt2 <- tidybayes::spread_draws(posterior_samples, theta[i]) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      group = factor(
        sprintf("&theta;<sub>%s</sub><br>Bag %s", i, i),
        levels = sprintf(
          "&theta;<sub>%s</sub><br>Bag %s",
          seq_len(nrow(table_data)),
          seq_len(nrow(table_data))
        )
      )
    ) |>
    dplyr::mutate(stat_theta = mean(theta), .by = group) |>
    ggplot2::ggplot(ggplot2::aes(y = group, x = theta)) +
    ggplot2::facet_wrap(
      ~group,
      nrow = 1,
      scales = "free_y"
    ) +
    ggdist::stat_slab(
      height = 0.9,
      normalize = "xy",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(aes(xintercept = stat_theta)) +
    ggplot2::scale_x_continuous(
      name = NULL,
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = c("0", "", "0.5", "", "1"),
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_y_discrete(
      name = NULL,
      labels = NULL,
      expand = expansion()
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella", base_size = 11) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(l = 3, r = 3, b = 0, 5),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(size = 11),
      strip.background = ggplot2::element_rect(fill = "transparent"),
      strip.text = ggtext::element_markdown(color = "black", size = 11),
      panel.spacing.x = ggplot2::unit(0.25, "cm")
    )

  plt3 <- data.frame(
    xend = 1:8,
    x = rep(mean(1:8), 8),
    y = 1,
    yend = 0
  ) |>
    ggplot2::ggplot() +
    ggarchery::geom_arrowsegment(
      mapping = ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      color = "black",
      linewidth = 0.3,
      arrows = ggplot2::arrow(
        type = "closed",
        length = ggplot2::unit(0.2, "cm"),
        angle = 20
      )
    ) +
    ggplot2::theme_void()

  nms <- c("Mean &mu;", "Precision &kappa;")
  names(nms) <- c("mu", "phi")

  plt4 <- posterior_samples |>
    tidybayes::spread_draws(mu, phi) |>
    tidyr::pivot_longer(
      cols = -c(.chain, .iteration, .draw),
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(stat = mean(value), .by = c(parameter)) |>
    ggplot2::ggplot(ggplot2::aes(x = value, y = parameter)) +
    ggdist::stat_slab(
      normalize = "xy",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stat)) +
    ggplot2::facet_wrap(
      ~parameter,
      scales = "free",
      nrow = 1,
      labeller = ggplot2::labeller(parameter = nms)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_discrete(
      name = NULL,
      labels = NULL,
      expand = ggplot2::expansion()
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        mu = ggplot2::scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2),
          expand = ggplot2::expansion()
        ),
        phi = ggplot2::scale_x_continuous(
          limits = c(0, 15),
          breaks = seq(0, 15, 5),
          expand = ggplot2::expansion(),
        )
      )
    ) +
    ggplot2::theme_light(base_family = "TeX Gyre Pagella", base_size = 11) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(),
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "transparent"),
      strip.text = ggtext::element_markdown(color = "black", size = 11),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 5),
        size = 11
      ),
      panel.spacing.x = ggplot2::unit(0.5, "cm"),
      plot.margin = ggplot2::margin(l = 3, r = 3)
    )

  layout <- "
  ##AAAA##
  BBBBBBBB
  CCCCCCCC
  DDDDDDDD
  "

  plt <- patchwork::wrap_plots(
    plt4,
    plt3,
    plt2,
    plt1,
    design = layout,
    heights = c(2, 1.5, 2.5, 3.5)
  ) &
    ggplot2::theme(plot.margin = ggplot2::margin(l = 3, r = 3))

  ggplot2::ggsave(
    file,
    plt,
    width = 8,
    height = 4.25,
    create.dir = TRUE
  )
}


plot_tern <- function(
  alpha,
  beta,
  filename,
  width = 6,
  height = 2.5,
  step = 0.005,
  normalize = FALSE
) {
  # --- checks ---
  stopifnot(is.numeric(beta) & length(beta) == length(alpha))
  stopifnot(is.list(alpha))
  stopifnot(all(sapply(alpha, length) == 3))

  alphas <- mapply(\(x, y) x * y, alpha, beta, SIMPLIFY = FALSE)

  n <- 1 / step

  # --- simplex grid ---
  df <- expand.grid(
    x = 0:n,
    y = 0:n
  ) |>
    transform(z = n - x - y) |>
    dplyr::filter(z >= 0) |>
    dplyr::mutate(
      x = x / n,
      y = y / n,
      z = z / n
    )

  # --- compute densities dynamically ---
  density_matrix <- do.call(
    cbind,
    lapply(alphas, function(alpha) {
      brms::ddirichlet(as.matrix(df[, c("x", "y", "z")]), alpha)
    })
  )

  colnames(density_matrix) <- paste0("density", seq_len(ncol(density_matrix)))
  df <- cbind(df, density_matrix)

  df_long <- df |>
    tidyr::pivot_longer(
      dplyr::starts_with("density"),
      names_to = "parameter",
      values_to = "density"
    ) |>
    dplyr::filter(!is.na(density)) |>
    dplyr::mutate(density_scaled = scales::rescale(density), .by = parameter) |>
    dplyr::mutate(
      parameter = factor(
        parameter,
        levels = paste0("density", seq_along(alphas)),
        labels = purrr::map2_chr(
          alpha,
          beta,
          ~ paste0(
            "&alpha; = [",
            paste(round(.x, 1), collapse = ", "),
            "]",
            ", &beta; = ",
            round(.y, 1)
          )
        )
      )
    )

  # --- plot ---
  plt <- ggtern::ggtern(df_long, ggplot2::aes(x = x, y = y, z = z)) +
    ggplot2::facet_wrap(~parameter, nrow = 1) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = if (normalize) density_scaled else density
      ),
      size = 0.01
    ) +
    ggplot2::scale_color_gradientn(
      colors = cetcolor::cet_pal(n = 5, name = "l2"),
      guide = "none",
      values = scales::rescale(x = c(0, 1, 3, 8, 13), from = c(0, 13)),
    ) +
    ggtern::scale_L_continuous(
      breaks = 0:5 / 5,
      labels = 0:5 / 5,
      name = "w"
    ) +
    ggtern::scale_T_continuous(
      breaks = 0:5 / 5,
      labels = 0:5 / 5,
      name = "b"
    ) +
    ggtern::scale_R_continuous(
      breaks = 0:5 / 5,
      labels = 0:5 / 5,
      name = "s"
    ) +
    ggplot2::theme_light(base_size = 7) +
    ggtern::theme_classic(base_size = 7) +
    ggplot2::theme(
      strip.text = ggtext::element_markdown(
        face = "bold",
        color = "black",
        size = ggplot2::rel(1.1)
      ),
      strip.background = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "TeX Gyre Pagella"),
      tern.axis.title.L = ggtext::element_markdown(
        margin = ggplot2::margin(r = 2.5)
      ),
      tern.axis.title.T = ggtext::element_markdown(),
      tern.axis.title.R = ggtext::element_markdown(
        margin = ggplot2::margin(l = 5)
      )
    )
  ggplot2::ggsave(filename, plt, width = width, height = height, units = "in")
}

# Plot Hyperpriors

plot_hyperpriors <- function(filename, width = 8, height = 4) {
  p_priors <- data.frame(
    name = c("&phi; ~ exp(1)", "&mu; ~ unif(0, 1)"),
    xdist = c(
      distributional::dist_exponential(rate = 1),
      distributional::dist_uniform(0, 1)
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(xdist = xdist)) + # Map the distribution objects to the aesthetic
    ggplot2::facet_wrap(~name, scales = "free", nrow = 1) + # Create separate plots for each prior
    ggdist::stat_slab(
      normalize = "panels",
      expand = TRUE,
      scale = 1,
      fill = "black"
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_y_continuous(
      name = "Density",
      expand = ggplot2::expansion(),
      limits = c(0, 1.5)
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        `&mu; ~ unif(0, 1)` = ggplot2::scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2),
          expand = ggplot2::expansion()
        ),
        `&phi; ~ exp(1)` = ggplot2::scale_x_continuous(
          limits = c(0, 8),
          breaks = seq(0, 8, 2),
          expand = ggplot2::expansion()
        )
      )
    ) +
    ggplot2::theme_linedraw(base_family = "TeX Gyre Pagella") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      axis.title.y = ggplot2::element_text(size = 25),
      axis.text = ggplot2::element_text(size = 22),
      axis.text.y = ggplot2::element_text(margin = margin(l = 0, r = -5)),
      axis.text.x = ggplot2::element_text(margin = margin(t = 5)),
      strip.text = ggtext::element_markdown(color = "white", size = 25),
      strip.background = ggplot2::element_rect(fill = "black"),
      axis.text.y.left = ggplot2::element_text(margin = margin(r = 10))
    )

  ggplot2::ggsave(
    filename,
    plot = p_priors,
    width = width,
    height = height,
    bg = "white"
  )
}


# Animation: Binomial distribution

plot_anim_binom <- function(filename) {
  waypoints <- tibble::tribble(
    ~point_id , ~prob ,
            1 , 0.05  ,
            2 , 0.20  ,
            3 , 0.50  ,
            4 , 0.80  ,
            5 , 0.95  ,
            6 , 0.05
  )

  n_steps <- 30
  fixed_N <- 100

  plot_data <- waypoints |>
    dplyr::mutate(next_prob = dplyr::lead(prob)) |>
    dplyr::filter(!is.na(next_prob)) |>
    dplyr::group_by(point_id) |>
    dplyr::reframe(
      step = 1:n_steps,
      curr_prob = prob + (next_prob - prob) * (step / n_steps),
      curr_mu = fixed_N * curr_prob,
      curr_sigma = sqrt(fixed_N * curr_prob * (1 - curr_prob)),
      label_text = paste0("&theta;<sub>i</sub> = ", round(curr_prob, 2))
    ) |>
    dplyr::mutate(frame_id = dplyr::row_number())

  expanded_data <- plot_data |>
    dplyr::cross_join(data.frame(x = 0:fixed_N)) |>
    dplyr::mutate(y = dbinom(x, size = fixed_N, prob = curr_prob))

  p <- ggplot2::ggplot(expanded_data, aes(x = x, y = y)) +
    ggplot2::geom_col(fill = "black", width = 0.8) +

    ggtext::geom_richtext(
      mapping = ggplot2::aes(label = label_text),
      x = 50,
      y = 0.18,
      label.color = NA,
      fill = NA,
      size = 6.5,
      family = "Optima",
      color = "black"
    ) +
    ggplot2::labs(
      title = "<b>Binomial Distribution</b> (n<sub>i</sub> = 100)",
      y = "Probability Mass",
      x = "Number of Black Marbles (y<sub>i</sub>)"
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 0.2), expand = FALSE) +
    ggplot2::theme_light(base_family = "Optima") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 18),
      axis.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown()
    ) +
    gganimate::transition_time(frame_id)

  # 5. Render
  final_animation <- gganimate::animate(
    p,
    nframes = nrow(plot_data),
    fps = 30,
    width = 5,
    device = "ragg_png",
    height = 5,
    units = "in",
    res = 300,
    renderer = gganimate::gifski_renderer()
  )

  gganimate::anim_save(filename)
}

plot_anim_beta <- function(filename) {
  waypoints <- tibble::tribble(
    ~point_id , ~alpha , ~beta ,
            1 , 0.5    , 0.5   ,
            2 , 1.0    , 1.0   ,
            3 , 2.0    , 2.0   ,
            4 , 9      , 9     ,
            5 , 9      , 1     ,
            6 , 1      , 9     ,
            7 , 0.2    , 0.2
  )

  n_steps <- 60

  plot_data <- waypoints |>
    dplyr::mutate(
      next_alpha = dplyr::lead(alpha),
      next_beta = dplyr::lead(beta)
    ) |>
    dplyr::filter(!is.na(next_alpha)) |>
    dplyr::group_by(point_id) |>
    dplyr::reframe(
      step = 1:n_steps,
      curr_alpha = alpha + (next_alpha - alpha) * (step / n_steps),
      curr_beta = beta + (next_beta - beta) * (step / n_steps),
      curr_mu = curr_alpha / (curr_alpha + curr_beta),
      curr_phi = curr_alpha + curr_beta,
      label_text = sprintf(
        "&alpha;: %5.1f   &beta;: %5.1f <br> &mu;:    %5.1f   &phi;:  %5.1f",
        curr_alpha,
        curr_beta,
        curr_mu,
        curr_phi
      )
    ) |>
    dplyr::mutate(frame_id = dplyr::row_number())

  expanded_data <- plot_data |>
    dplyr::cross_join(data.frame(x = seq(0, 1, length.out = 300))) |>
    dplyr::mutate(y = dbeta(x, curr_alpha, curr_beta))

  p <- ggplot2::ggplot(expanded_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_area(fill = "black", alpha = 0.9) +
    ggplot2::geom_line(color = "black", linewidth = 1.2) +
    ggtext::geom_richtext(
      mapping = ggplot2::aes(label = label_text),
      x = 0.5,
      y = 8.5,
      label.color = NA,
      fill = NA,
      size = 6.5,
      family = "Optima",
      color = "black"
    ) +
    ggplot2::labs(
      title = "Beta Distribution",
      y = "Density",
      x = "P(<theta;<sub>i</sub>)"
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 10), expand = FALSE) +
    ggplot2::theme_light(base_family = "Optima") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.title.x = ggtext::element_markdown(),
      plot.subtitle = ggplot2::element_blank()
    ) +
    gganimate::transition_time(frame_id) +
    gganimate::view_follow(fixed_y = TRUE)

  final_animation <- gganimate::animate(
    p,
    nframes = nrow(plot_data),
    fps = 30,
    width = 5,
    device = "ragg_png",
    height = 5,
    units = "in",
    res = 300,
    renderer = gganimate::gifski_renderer()
  )

  gganimate::anim_save(filename)
}
