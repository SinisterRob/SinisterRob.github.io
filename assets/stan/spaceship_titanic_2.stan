data {
  int<lower=0> N; // Total number of passenger observations.
  int<lower=0> K; // Number of predictors.
  matrix[N, K] X; // Design matrix of predictors.
  int y[N]; // Response vector (Transported, 0 or 1)
}

parameters {
  vector[K] beta; // Coefficients for predictors.
}

model {
  y ~ bernoulli_logit(X * beta); // Logistic regression
}

generated quantities {
  int y_pred[N]; // Posterior predictions of Transported status.
  y_pred = bernoulli_logit_rng(X * beta); // Generate predicted outcomes for each observation.
}
