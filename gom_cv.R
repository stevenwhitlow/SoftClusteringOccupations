library(caret)
set.seed(500)
num_folds <- 10
num_types <- 5
elbo_test <- array(0, dim = c(num_types,num_folds))
mean_elbo_test <- array(0, dim = c(5))

list_initial_vals_alpha <-c(5/3, 6/3, 1/3, 6/3, 1/3, 3/3, 1/3)
list_initial_vals_theta <-c(4/3, 2/3, 5/3, 3/3, 5/3, 1/3, 3/3)

getInitialization <- function(initial_alpha, initial_theta, K) {
  Total <- 968
  # Number of variables
  J <- 128
  # we only have one replicate for each of the variables
  Rj <- rep(1, J)
  # Nijr indicates the number of ranking levels for each variable.
  # Since all our data is multinomial it should be an array of all 1s
  Nijr <- array(1, dim = c(Total, J, max(Rj)))
  # Number of sub-populations
  # There are 3 choices for each of the variables ranging from 0 to 2.
  Vj <- rep(3, J)
  # we initialize alpha to .2
  alpha <- rep(list_initial_vals_alpha[initial_alpha], K)
  # All variables are multinomial
  dist <- rep("multinomial", J)
  # obs are the observed responses. it is a 4-d array indexed by i,j,r,n
  # note that obs ranges from 0 to 2 for each response
  obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
  tasks_wide %>% select(-c(1:2)) %>% as.matrix -> obs[, , 1, 1]
  
  # Initialize theta randomly with Dirichlet distributions
  set.seed(500)
  theta <- array(0, dim = c(J, K, max(Vj)))
  for (j in 1:J) {
    theta[j, , ] <- gtools::rdirichlet(K, rep(list_initial_vals_theta[initial_theta], Vj[j]))
  }
  
  # Create the mixedMemModel
  # This object encodes the initialization points for the variational EM algorithim
  # and also encodes the observed parameters and responses
  initial <- mixedMemModelVarInf(Total = Total, J = J, Rj = Rj,
                                 Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                                 theta = theta, dist = dist, obs = obs)
  computeELBO(initial)
  out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
  
  initial <- mixedMemModelVarInf(Total = Total, J = J, Rj = Rj,
                                 Nijr = Nijr, K = K, Vj = Vj, alpha = out$alpha,
                                 theta = out$theta, dist = dist, obs = obs)
  computeELBO(initial)
  out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
  
  return(list("lambda" = out$lambda, "theta" = out$theta))
}

for (type_number in 2:8){
  
  
for (i in 1:num_folds){
training.samples <- tasks_wide$`Active Learning` %>% createFolds(k=num_folds, list = FALSE)
tasks_wide %>% filter(training.samples != i) -> train.data
tasks_wide %>% filter(training.samples == i) -> test.data
TotalTrain <- nrow(train.data)
TotalTest <- nrow(test.data)

Total <- 968
# Number of variables
J <- 128
# we only have one replicate for each of the variables
Rj <- rep(1, J)
# Nijr indicates the number of ranking levels for each variable.
# Since all our data is multinomial it should be an array of all 1s
Nijr_train <- array(1, dim = c(TotalTrain, J, max(Rj)))
Nijr_test <- array(1, dim = c(TotalTest, J, max(Rj)))
# Number of sub-populations
K <- type_number
# There are 3 choices for each of the variables ranging from 0 to 2.
Vj <- rep(3, J)
# we initialize alpha to .2
alpha <- rep(list_initial_vals_alpha[type_number-1], K)
# All variables are multinomial
dist <- rep("multinomial", J)
# obs are the observed responses. it is a 4-d array indexed by i,j,r,n
# note that obs ranges from 0 to 2 for each response
obsTrain <- array(0, dim = c(TotalTrain, J, max(Rj), max(Nijr_train)))
obsTest <- array(0, dim = c(TotalTest, J, max(Rj), max(Nijr_test)))
train.data %>% select(-c(1:2)) %>% as.matrix -> obsTrain[, , 1, 1]
test.data %>% select(-c(1:2)) %>% as.matrix -> obsTest[, , 1, 1]

# Initialize theta randomly with Dirichlet distributions
theta <- array(0, dim = c(J, K, max(Vj)))
for (j in 1:J) {
  theta[j, , ] <- gtools::rdirichlet(K, rep(list_initial_vals_theta[type_number-1], Vj[j]))
}

# Create the mixedMemModel
# This object encodes the initialization points for the variational EM algorithim
# and also encodes the observed parameters and responses
initial <- mixedMemModelVarInf(Total = TotalTrain, J = J, Rj = Rj,
                         Nijr = Nijr_train, K = K, Vj = Vj, alpha = alpha,
                         theta = theta, dist = dist, obs = obsTrain)
computeELBO(initial)
st = proc.time()
#printStatus 1 indicates that status updates will be printed
# printMod 25 indicates that status updates will only be printed ever 25th step
out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
end <- proc.time()
computeELBO(out)

initial_test <- mixedMemModelVarInf(Total = TotalTest, J = J, Rj = Rj,
                         Nijr = Nijr_test, K = K, Vj = Vj, alpha = out$alpha,
                         theta = out$theta, dist = dist, obs = obsTest)
st = proc.time()
#printStatus 1 indicates that status updates will be printed
# printMod 25 indicates that status updates will only be printed ever 25th step
out_test <- mmVarInfFit(initial_test, printStatus = 1, printMod = 25, stepType=1)
end <- proc.time()
elbo_test[type_number,i] <- computeELBO(out_test)
time <- end - st
}
mean_elbo_test[type_number] = mean(elbo_test[type_number,])
}