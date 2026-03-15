create_data <- function() {
  # Define Global Dimensions
  N <- 4 # Training categories
  M <- 4 # Dimensions (Shape, Texture, Color, Size)
  F <- 10 # Feature limit (10)

  # Build Training Data Array (y_data)
  training_matrix <- cbind(
    c(1, 1, 1, 1),
    c(1, 2, 2, 2), # Category 1
    c(2, 3, 3, 1),
    c(2, 4, 4, 2), # Category 2
    c(3, 5, 5, 1),
    c(3, 6, 6, 2), # Category 3
    c(4, 7, 7, 1),
    c(4, 8, 8, 2) # Category 4
  )

  rownames(training_matrix) <- c(
    "$m_1$: Shape",
    "$m_2$: Texture",
    "$m_3$: Color",
    "$m_4$: Size"
  )
  colnames(training_matrix) <- paste0(
    rep(paste0("cat", 1:4), each = 2),
    paste0("_", 1:2)
  )

  # Define the Test Data
  # The Dax object
  x_dax <- c(5, 9, 9, 1)

  # The three choice objects (Columns 2, 3, and 4)
  x_choices <- matrix(
    c(
      c(5, 10, 10, 1), # Choice 1: Shape Match
      c(6, 9, 10, 1), # Choice 2: Texture Match
      c(6, 10, 9, 1) # Choice 3: Color Match
    ),
    nrow = 3,
    byrow = TRUE
  )

  return(list(
    training_matrix = training_matrix,
    x_dax = x_dax,
    x_choices = x_choices
  ))
}

generate_stan_data <- function(training_matrix, x_dax, x_choices) {
  # Define Global Dimensions
  N <- 4 # Training categories
  M <- 4 # Dimensions (Shape, Texture, Color, Size)
  F <- 10 # Feature limit (10)

  # Convert to Stan compatible 3-dimensional array
  y_data <- array(0, dim = c(N, M, F))
  for (cat_idx in 1:N) {
    col1 <- (cat_idx * 2) - 1
    col2 <- (cat_idx * 2)
    for (dim_idx in 1:M) {
      y_data[cat_idx, dim_idx, training_matrix[dim_idx, col1]] <- y_data[
        cat_idx,
        dim_idx,
        training_matrix[dim_idx, col1]
      ] +
        1
      y_data[cat_idx, dim_idx, training_matrix[dim_idx, col2]] <- y_data[
        cat_idx,
        dim_idx,
        training_matrix[dim_idx, col2]
      ] +
        1
    }
  }

  # 4. Package for Stan
  stan_data <- list(
    N = N,
    M = M,
    F = F,
    y = y_data,
    x_dax = x_dax,
    x_choices = x_choices
  )

  return(stan_data)
}

sim_betabinom <- function(k, mu, phi, n_trials) {
  #withr::with_seed(seed, {
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
  #})
}


make_marble_data <- function(table_data) {
  table_data |>
    dplyr::rename(successes = y) |>
    dplyr::mutate(failures = n - successes) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      type = list(c(rep("black", successes), rep("white", failures)))
    ) |>
    tidyr:::unnest(type) |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      index = dplyr::row_number() - 1,
      x = index %% 10,
      y = index %/% 10
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      id = factor(
        paste("Bag", id),
        levels = paste("Bag", seq_len(nrow(table_data)))
      )
    )
}

shapebias_to_csv <- function(data_shapebias, filename) {
  df <- as.data.frame(data_shapebias$training_matrix)
  df$x_dax <- data_shapebias$x_dax
  choices_t <- t(data_shapebias$x_choices)
  colnames(choices_t) <- paste0("choice", 1:ncol(choices_t))
  df <- cbind(df, choices_t)
  df <- cbind(feature = rownames(df), df)
  rownames(df) <- NULL
  write.csv(df, filename, row.names = FALSE)
}


