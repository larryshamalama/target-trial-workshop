set.seed(123)

n <- 10000

ATE <- 2 # true ATE, the target of inference

X <- rnorm(n, mean = 1) # confounder
A <- rbinom(n, size=1, prob=plogis(-1 + X)) # treatment
Y <- ATE * A + sqrt(abs(X)) + X + rnorm(n) # outcome

data <- data.frame(Y = Y, A = A, X = X)

# Incorrect analysis

mean(Y[A == 1]) - mean(Y[A == 0]) # not equal to ATE defined above!

# IPTW analysis

trt_model <- glm(A ~ X, family = binomial())
prob_trt <- predict(trt_model, type = "response")
iptw <- 1/ifelse(A, prob_trt, 1 - prob_trt)

ate_manual <- sum((Y*iptw)[A == 1])/sum(iptw[A == 1]) - sum((Y*iptw)[A == 0])/sum(iptw[A == 0])

# IPTW analysis using `WeightIt` package

# Install and load the `WeightIt` package
if (!require(WeightIt)) {
  install.packages("WeightIt")
  library(WeightIt)
}

trt_model_wi <- weightit(A ~ X, data = data, method = "ps", family = "binomial")
iptw_wi <- trt_model_wi$weights

ate_weightit <- with(
  data,
  sum(weight.out$weights[A == 1] * Y[A == 1])/sum(weight.out$weights[A == 1]) -
  sum(weight.out$weights[A == 0] * Y[A == 0])/sum(weight.out$weights[A == 0])
)

all.equal(iptw_wi, iptw, tolerance = 1e-8) # returns TRUE

# Outcome modelling
out_model <- lm(Y ~ A + sqrt(abs(X)) + X)
coef(out_model)["A"] # coefficient in front of "A" is the estimated ATE