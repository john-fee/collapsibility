# Simulation functions -------
simulate_predictors <- function(n,sigma,x_threshold){
  mvtnorm::rmvnorm(
    n = n,
    sigma = sigma
  ) %>%
    as.data.frame() %>%
    setNames(c("W","X","Z")) %>%
    mutate(
      X = if_else(X > x_threshold,1,0)
    )
}

simulate_linear_model <- function(n,sigma,y_formula,x_threshold,epsilon_sd){
  simulate_predictors(
    n = n,
    sigma = sigma,
    x_threshold = x_threshold
  ) %>%
    mutate(
      epsilon = rnorm(n = n,sd = epsilon_sd),
      Y = {{y_formula}} + epsilon,
    )
}

simulate_multiple_datasets <- function(n,sigma,y_formula,n_simulations,x_threshold,epsilon_sd = 1){
  is_cov_matrix_valid <- all(eigen(sigma)$values != 0)
  stopifnot("Error: supplied covariance matrix must be nonsingular" = is_cov_matrix_valid)

  purrr::map(
    .x = seq(1,n_simulations),
    .f = ~ simulate_linear_model(
      n = n,
      sigma = sigma,
      y_formula = {{y_formula}},
      x_threshold = x_threshold,
      epsilon_sd = epsilon_sd
    ) %>%
      mutate(sim_id = .x) %>%
      relocate(sim_id)
  ) %>%
    # Bind into dataframe and nest under sim_id so we can perform calculations for
    # each simulation easily
    bind_rows() %>%
    group_by(sim_id) %>%
    nest()
}

get_coef_df <- function(sim_results,true_coef_df){
  sim_results %>%
    mutate(
      betas = map(fitted_model, ~ coef(.x))
    ) %>%
    select(sim_id,betas) %>%
    unnest_wider(betas) %>%
    pivot_longer(cols = c(everything(), -sim_id),names_to = "parameter",values_to = "estimate") %>%
    left_join(
      true_coef_df,
      by = "parameter"
    )
}

# Presentation helpers -----

plot_coefficient_distribution <- function(coef_df,title){
  parameter_string = coef_df %>%
    ungroup() %>%
    distinct(parameter,true_value) %>%
    mutate(pair = glue::glue("{parameter} = {true_value}")) %>%
    pull(pair) %>%
    paste(collapse = ", ")

  coef_df %>%
    ggplot(aes(x = estimate,y = parameter,fill = parameter)) +
    #geom_boxplot(outliers = FALSE) +
    geom_violin(draw_quantiles = c(.5),) +
    geom_point(aes(x = true_value,color = "True value of parameter"),shape = 23,fill = "red",size = 3) +
    scale_color_manual(name = "",values = c("True value of parameter" = "red")) +
    scale_fill_brewer(name = "Parameter estimates for",type = "qual",palette = 3) +
    scale_x_continuous(name = "Parameter estimates") +
    labs(
      y = "Variables",
      title = title,
      subtitle = glue::glue("True parameter values are {parameter_string}")
    ) +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )
}

summarize_coef_df <- function(coef_df,caption){
  coef_df %>%
    group_by(parameter) %>%
    rename(variable = parameter) %>%
    summarize(
      parameter_estimate = mean(estimate),
      true_value = true_value[1],
      bias = mean(estimate - true_value)
    ) %>%
    janitor::clean_names(case = "sentence") %>%
    knitr::kable(caption = caption)
}












#
# Y = {{y_formula}},
# Y_probs = plogis(Y),
# outcome = rbinom(n = n(),size = 1,prob = Y_probs)
#
#
#
#
# fit_models <- function(nested_sim_data,model_formula){
#   nested_sim_data %>%
#     # Fit logistic regression and save model + predicted probs
#     mutate(
#       fitted_model = map(data,~ glm(outcome ~ X + Z + W,family = binomial(link = "logit"),data = .x)),
#       predicted_proba = map2(.x = data,.y = fitted_model,.f = ~ predict(.y,type = "response"))
#     )
# }

# predicted_value = map2(.x = data,.y = fitted_model,.f = ~ predict(.y))
