library(caret)
library(reshape2)

set.seed(500)
num_folds <- 5
num_types <- 7

total_theta <- 4
total_alpha <- 2
total_seeds <- 3
initial_theta <- c(2/3, 3/3, 4/3, 5/3)
initial_alpha <- c(0.6, 1.6)
seed_array <- c(500, 250, 750)
elbo_test <- array(0, dim = c(num_types,total_seeds, total_theta,total_alpha,num_folds))
mean_elbo_test <- array(0, dim = c(num_types,total_seeds,total_theta,total_alpha))
max_elbo_test <- array(0, dim = c(num_types))

#list_initial_vals_alpha <-c(5/3, 6/3, 1/3, 6/3, 1/3, 3/3, 1/3)
#list_initial_vals_theta <-c(4/3, 2/3, 5/3, 3/3, 5/3, 1/3, 3/3)
Total <- 968
# Number of variables
J <- 128
# we only have one replicate for each of the variables
Rj <- rep(1, J)
Vj <- rep(3, J)
dist <- rep("multinomial", J)
num_completed <- 0 

training.samples <- tasks_wide$`Active Learning` %>% createFolds(k=num_folds, list = FALSE)

for (seed in 1:total_seeds){
  set.seed(seed_array[seed])
for (type_number in 2:num_types){
  K <- type_number
for (num_alpha in 1:total_alpha){
  alpha <- rep(initial_alpha[num_alpha], K)
for (num_theta in 1:total_theta){
for (i in 1:num_folds){
  num_completed <- num_completed + 1
print(paste0("Model: ", num_completed, "/", (7 - 1)*(total_seeds)*(total_alpha)*(total_theta)*(num_folds), ". Seed: ", seed, ". Number of topics: ", type_number, ". Theta: ", initial_theta[num_theta], "Alpha: ", initial_alpha[num_alpha], ". Testing set is fold ", i, "/", num_folds, "."))
tasks_wide %>% filter(training.samples != i) -> train.data
tasks_wide %>% filter(training.samples == i) -> test.data
TotalTrain <- nrow(train.data)
TotalTest <- nrow(test.data)
Nijr_train <- array(1, dim = c(TotalTrain, J, max(Rj)))
Nijr_test <- array(1, dim = c(TotalTest, J, max(Rj)))

theta <- array(0, dim = c(J, K, max(Vj)))
for (j in 1:J) {
  theta[j, , ] <- gtools::rdirichlet(K, rep(initial_theta[num_theta], Vj[j]))
}

# There are 3 choices for each of the variables ranging from 0 to 2.
# All variables are multinomial

# obs are the observed responses. it is a 4-d array indexed by i,j,r,n
# note that obs ranges from 0 to 2 for each response
obsTrain <- array(0, dim = c(TotalTrain, J, max(Rj), max(Nijr_train)))
obsTest <- array(0, dim = c(TotalTest, J, max(Rj), max(Nijr_test)))
train.data %>% select(-c(1:2)) %>% as.matrix -> obsTrain[, , 1, 1]
test.data %>% select(-c(1:2)) %>% as.matrix -> obsTest[, , 1, 1]

# Create the mixedMemModel
# This object encodes the initialization points for the variational EM algorithim
# and also encodes the observed parameters and responses
start <- proc.time()
initial <- mixedMemModelVarInf(Total = TotalTrain, J = J, Rj = Rj,
                         Nijr = Nijr_train, K = K, Vj = Vj, alpha = alpha,
                         theta = theta, dist = dist, obs = obsTrain)
computeELBO(initial)
#printStatus 1 indicates that status updates will be printed
# printMod 25 indicates that status updates will only be printed ever 25th step
out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
computeELBO(out)

#Can run again for 2-step process (see AOAS 2015 paper)
#initial <- mixedMemModelVarInf(Total = TotalTrain, J = J, Rj = Rj,
#                               Nijr = Nijr_train, K = K, Vj = Vj, alpha = out$alpha,
#                               theta = out$theta, dist = dist, obs = obsTrain)
#computeELBO(initial)
#out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
#computeELBO(out)

initial_test <- mixedMemModelVarInf(Total = TotalTest, J = J, Rj = Rj,
                         Nijr = Nijr_test, K = K, Vj = Vj, alpha = out$alpha,
                         theta = out$theta, dist = dist, obs = obsTest)
#printStatus 1 indicates that status updates will be printed
# printMod 25 indicates that status updates will only be printed ever 25th step
out_test <- mmVarInfFit(initial_test, printStatus = 1, printMod = 25, stepType=0)
end <- proc.time()
elbo_test[type_number,seed,num_theta,num_alpha,i] <- computeELBO(out_test)
print(end - start)
}
#mean_elbo_test[type_number,num_theta,num_alpha] = mean(elbo_test[type_number,num_theta,num_alpha,])
}}
#max_elbo_test[type_number] = max(mean_elbo_test[type_number,,])
}
}

elbo_df <- melt(data = elbo_test, varnames=c("NumTypes","Seed", "InitializationTheta", "InitializationAlpha", "NumFold"), value.name="ELBO")
elbo_df %>% group_by(NumTypes,Seed, InitializationTheta, InitializationAlpha) %>% mutate(meanInitial = mean(ELBO)) -> elbo_df
elbo_df %>% group_by(NumTypes) %>% filter(NumTypes!=1) %>% slice_max(order_by = meanInitial, n=1, with_ties= FALSE) %>% select(NumTypes, InitializationTheta, InitializationAlpha, Seed, meanInitial)
write.csv(elbo_df, "elbo_kfold_initializations.csv")