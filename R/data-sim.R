library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
theme_set(cowplot::theme_cowplot())
set.seed(1)



cov_matrix = matrix(
  c(
    4,0,0,4
  ),
  nrow = 2,
  ncol = 2,
  byrow = TRUE
)

simulate_dataset <- function(n,sigma,y_formula){
  mvtnorm::rmvnorm(
    n = n,
    sigma = sigma
  ) %>%
    as.data.frame() %>%
    setNames(c("X","Z")) %>%
    mutate(
      W = rnorm(n()),
      Y = {{y_formula}},
      Y_probs = plogis(Y),
      outcome = rbinom(n = n(),size = 1,prob = Y_probs)
    )
}

simulate_multiple_datasets <- function(n,sigma,y_formula,n_simulations){
  purrr::map(
    .x = seq(1,n_simulations),
    .f = ~ simulate_dataset(
      n = n,
      sigma = sigma,
      y_formula = {{y_formula}}
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

fit_models <- function(nested_sim_data,model_formula){
  nested_sim_data %>%
    # Fit logistic regression and save model + predicted probs
    mutate(
      fitted_model = map(data,~ glm(outcome ~ X + Z + W,family = binomial(link = "logit"),data = .x)),
      predicted_proba = map2(.x = data,.y = fitted_model,.f = ~ predict(.y,type = "response"))
    )
}

results <- simulate_multiple_datasets(
  n = 100,
  sigma = cov_matrix,
  y_formula = 3*W + X + 1.5*Z,
  n_simulations = 5000
) %>%
  fit_models(model_formula = W + X + Z)


# results %>%
#   unnest(c(predicted_proba,data)) %>%
#   select(-fitted_model) %>%
#   mutate(prob_diff = predicted_proba - Y_probs) %>%
#   #pivot_longer(cols = c(Y_probs,predicted_proba),names_to = "Group",values_to = "probability") %>%
#   ggplot(aes(x = prob_diff)) +
#   geom_density()

coef_df <- results %>%
  mutate(
    betas = map(fitted_model, ~ coef(.x))
  ) %>%
  select(sim_id,betas) %>%
  unnest_wider(betas) %>%
  pivot_longer(cols = c(everything(), -sim_id),names_to = "parameter",values_to = "estimate") %>%
  left_join(
    data.frame(
      parameter = c("(Intercept)","X","Z","W"),
      true_value = c(0,1,1.5,3)
    ),
    by = "parameter"
  )

coef_df %>%
  ggplot(aes(x = estimate,y = parameter,fill = parameter)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(aes(x = true_value,color = "True value of parameter"),shape = 23,fill = "red",size = 3) +
  scale_color_manual(name = "",values = c("True value of parameter" = "red")) +
  scale_fill_brewer(name = "Parameters",type = "qual",palette = 3) +
  #geom_vline(aes(xintercept = true_value),color = "red",linetype = "dashed") +
  scale_x_continuous(name = "Estimates") +
  labs(
    y = "Parameters",
    title = "Distribution of logistic regression parameter estimates \nfit on data simulated from the same model",
    subtitle = "True parameter values are Intercept = 0, W = 3, X = 1, Z = 1.5"
  )

coef_df %>%
  group_by(parameter) %>%
  summarize(
    estimate = mean(estimate),
    true_value = true_value[1],
    bias = mean(estimate - true_value)
  )

