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

sum((Y*iptw)[A == 1])/sum(iptw[A == 1]) - sum((Y*iptw)[A == 0])/sum(iptw[A == 0])

# Outcome modelling
out_model <- lm(Y ~ A + sqrt(abs(X)) + X)
coef(out_model)["A"] # coefficient in front of "A" is the estimated ATE