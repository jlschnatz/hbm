library(targets)
library(tarchetypes)
library(stantargets)
tar_source()

pkgs <- c(
  "ggplot2",
  "systemfonts",
  "ragg",
  "dplyr",
  "tidyr",
  "distributional",
  "patchwork",
  "ggdist",
  "scico",
  "latex2exp",
  "ggh4x",
  "geomtextpath",
  "cmdstanr",
  "posterior"
)

tar_option_set(
  packages = pkgs,
  format = "qs",
  seed = 25
)

mapped <- tar_map(
  values = tibble::tibble(
    mu_sim = c(0.5, 0.2),
    phi_sim = c(0.8, 20),
    label = paste0("mu", mu_sim, "_phi", phi_sim)
  ),

  # Simuliere Daten
  tar_target(
    data_sim,
    sim_betabinom(
      k = 7,
      mu = mu_sim,
      phi = phi_sim,
      n_trials = 100
    )
  ),

  tar_target(table_data, data_sim[["table_data"]]),
  tar_target(data_stan_marbles, data_sim[["stan_data"]]),
  tar_target(marble_data, make_marble_data(table_data)),

  # Plots
  tar_file(
    plt_marbles,
    plot_marbles(
      marble_data,
      paste0("figures/dotplot_", label, ".svg")
    )
  ),

  tar_file(
    plt_marbles_arrow,
    plot_marbles(
      marble_data,
      paste0("figures/dotplot_arrow_", label, ".svg"),
      arrow = TRUE
    )
  ),

  tar_files(
    plt_marble_single,
    plot_marbles_single(
      marble_data,
      paste0("figures/single/dotplot_", label)
    ),
    cue = tar_cue(
      mode = "thorough",
      file = FALSE,
      depend = FALSE,
      format = FALSE
    )
  ),

  # Posterior Sampling
  tar_stan_mcmc(
    name = fit_stan_marbles,
    stan_files = "stan/beta_binomial.stan",
    data = data_stan_marbles,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 10000,
    iter_sampling = 5000,
    max_treedepth = 15,
    adapt_delta = .99
  ),

  tar_target(
    plt_slab,
    plot_slab(
      fit_stan_marbles_mcmc_beta_binomial,
      table_data,
      paste0("figures/slab_", label, ".svg")
    )
  ),

  tar_file(
    test,
    plot_combined(
      marble_data,
      fit_stan_marbles_mcmc_beta_binomial,
      table_data,
      paste0("figures/panel_", label, ".svg"),
      arrow = TRUE
    )
  ),

  tar_file(
    plt_hyper,
    plot_hyper(
      fit_stan_marbles_mcmc_beta_binomial,
      mu,
      phi,
      filename = paste0("figures/hyper_", label, ".svg")
    )
  ),
  names = label,
  unlist = TRUE
)


quarto_source1 <- tar_file(
  quarto_source1,
  "manuscript/stan-code1.qmd"
)

quarto_source2 <- tar_file(
  quarto_source2,
  "manuscript/stan-code1.qmd"
)

quarto_cmd1 <- tar_file(quarto_cmd1, {
  # Define the expected output file
  out_file <- "manuscript/stan-code1.pdf"
  system2(
    command = "quarto",
    args = c(
      "render",
      quarto_source1,
      "--to",
      "pdf"
    )
  )

  out_file
})


quarto_cmd2 <- tar_file(quarto_cmd2, {
  # Define the expected output file
  out_file <- "manuscript/stan-code2.pdf"
  system2(
    command = "quarto",
    args = c(
      "render",
      quarto_source2,
      "--to",
      "pdf"
    )
  )

  out_file
})


# Shape bias
data_shapebias <- tar_target(
  data_shapebias,
  create_data()
)

data_stan_shapebias <- tar_target(
  data_stan_shapebias,
  generate_stan_data(
    data_shapebias$training_matrix,
    data_shapebias$x_dax,
    data_shapebias$x_choices
  )
)

fit_stan_shapebias <- tar_stan_mcmc(
  fit_stan_shapebias,
  "stan/dirichlet_multinomial.stan",
  data_stan_shapebias,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 10000,
  iter_sampling = 5000,
  max_treedepth = 15,
  adapt_delta = .99
)

plt_shapebias <- tar_file(
  plt_shapebias,
  plot_shapebias(
    fit_stan_shapebias_mcmc_dirichlet_multinomial,
    "figures/shape_bias.svg"
  )
)

# Beta distribution
plt_beta <- tar_file(plt_beta, plot_beta_static("figures/beta-dist.svg", width = 8, height = 1.5))

test <- tar_file(test, shapebias_to_csv(data_shapebias, "data/shape-bias.csv"))

typst_source_shapebias <- tar_file(
  typst_source_shapebias,
  "manuscript/shape_bias_structure.typ"
)

typst_shapebias <- tar_file(
  typst_shapebias,
  {
    outfile <- "figures/shape_bias_structure.svg"
    system2(
      command = "typst",
      args = c(
        "compile",
        "--format",
        "svg",
        "--root",
        "..",
        typst_source_shapebias,
        outfile
      )
    )
    outfile
  }
)



alpha <- tar_target(
  alpha,
  list(c(1/3, 1/3, 1/3), c(1/3, 1/3, 1/3), c(3/6, 2/6, 1/6), c(3/6, 2/6, 1/6))
)

beta <- tar_target(
  beta,
  c(2, 20, 2, 20)
)

plt_tern <- tar_file(
  plt_tern,
  plot_tern(
    alpha,
    beta,
    "figures/tern.svg",
    width = 5.5,
    height = 1.5,
    normalize = TRUE,
    step = 0.01
  )
)

plt_hyperprior <- tar_file(
  plt_hyperprior,
  plot_hyperpriors("figures/hyperpriors.svg", width = 8, height = 4)
)

plt_anim_binom <- tar_file(
  plt_anim_binom,
  plot_anim_binom("img/binomial_distribution_tour.gif")
)

plt_anim_beta <- tar_file(
  plt_anim_beta,
  plot_anim_beta("img/beta_distribution_tour.gif")
)

# list all targets
list(
  mapped,
  c(quarto_source1, quarto_cmd1, quarto_source2, quarto_cmd2),
  # Shape bias
  c(data_shapebias, data_stan_shapebias, fit_stan_shapebias, plt_shapebias),
  plt_beta,
  test,
  typst_source_shapebias,
  typst_shapebias,
  c(alpha, beta, plt_tern),
  plt_hyperprior,
  plt_anim_binom,
  plt_anim_beta
)
