library(caret)
library(reshape2)

num_folds <- 5
num_types <- 7

initial_theta <- c(2/3, 3/3, 4/3, 5/3)
total_theta <- length(initial_theta)

initial_alpha <- c(0.6, 1.6)
total_alpha <- length(initial_alpha)

seed_array <- c(500, 250, 750)
total_seeds <- length(seed_array)


elbo_test <- array(0, dim = c(num_types,total_seeds, total_theta,total_alpha,num_folds))
mean_elbo_test <- array(0, dim = c(num_types,total_seeds,total_theta,total_alpha))
max_elbo_test <- array(0, dim = c(num_types))

num_completed <- 0 

training.samples <- tasks_wide$`Active Learning` %>% createFolds(k=num_folds, list = FALSE)

# Total number of runs -> (num_types-1)*Total_seeds * total_alpha * total_theta * num_folds
for (seed in 1:total_seeds){
# Skip degenerate model with K=1:
for (type_number in 2:num_types){
for (num_alpha in 1:total_alpha){
for (num_theta in 1:total_theta){
for (i in 1:num_folds){
    num_completed <- num_completed + 1
    print(paste0("Model: ", num_completed, "/", (num_types - 1)*(total_seeds)*(total_alpha)*(total_theta)*(num_folds), ". Seed: ", seed, ". Number of topics: ", type_number, ". Theta: ", initial_theta[num_theta], "Alpha: ", initial_alpha[num_alpha], ". Testing set is fold ", i, "/", num_folds, "."))
  
    tasks_wide %>% select(-c(1:2)) %>% filter(training.samples != i) -> train_data
    tasks_wide %>% select(-c(1:2)) %>% filter(training.samples == i) -> test_data

    total_train <- nrow(train_data)
    total_test <- nrow(test_data)

    # There are 128 tasks in the data
    elbo_test[type_number,seed,num_theta,num_alpha,i]  <- cv_fold_elbo(train_data, test_data, 
                                                                       total_train, total_test,
                                                                       128,
                                                                       type_number, initial_alpha[num_alpha], initial_theta[num_theta], seed_array[seed])

}
}
}
}
}

elbo_df <- melt(data = elbo_test, varnames=c("NumTypes","Seed", "InitializationTheta", "InitializationAlpha", "NumFold"), value.name="ELBO") %>%
    group_by(NumTypes,Seed, InitializationTheta, InitializationAlpha) %>%
    filter(NumTypes!=1) %>%
    mutate(meanInitial = mean(ELBO))

write.csv(elbo_df, "./model_output/elbo_kfold_initializations.csv")

elbo_df %>% group_by(NumTypes) %>%
    slice_max(order_by = meanInitial, n=1, with_ties= FALSE) %>%
    select(NumTypes, InitializationTheta, InitializationAlpha, Seed, meanInitial) %>%
    write.csv("./model_output/elbo_kfold_best_initializations.csv")
