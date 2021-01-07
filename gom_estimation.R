library(tidyverse)

clean_for_gom <- function() {

    library(readxl)

    # Keep only the occupation codes and titles and the task names and values
    keep <- c("O*NET-SOC Code", "Title", "Element Name", "Data Value")

    # For each category: read in from Excel file
    # Keep only the level scale for each task, and keep only the columns in the "keep" vector

    Abilities <- read_excel("./data/Abilities.xlsx")
    Abilities <- subset(Abilities, `Scale ID`=="LV")
    Abilities <- Abilities[, names(Abilities) %in% keep, drop = F]

    Skills <- read_excel("./data/Skills.xlsx")
    Skills <- subset(Skills, `Scale ID`=="LV")
    Skills <- Skills[, names(Skills) %in% keep, drop = F]

    Activities <- read_excel("./data/Activities.xlsx")
    Activities <- subset(Activities, `Scale ID`=="LV")
    Activities <- Activities[, names(Activities) %in% keep, drop = F]

    tasks <<- bind_rows(Abilities, Skills, Activities)
    tasks_wide <<- tasks %>% spread(`Element Name`,`Data Value`)

    for (j in 3:ncol(tasks_wide)) {
      tasks_wide[[colnames(tasks_wide %>% select(c(j)))]] <<- ntile(tasks_wide[[colnames(tasks_wide %>% select(c(j)))]],3) - 1
    }

    gom_data <<- tasks_wide %>% select(-c(1:2))
}

estimate_gom <- function(data, total, J, K, initial_alpha, initial_theta, seed, permutation = NULL, returnELBO = FALSE) {
    library(mixedMem)
    library(gtools)

    # Vector of responses for each task
    Rj <- rep(1, J)

    # Matrix of responses for each observation and task
    Nijr <- array(1, dim = c(total, J, max(Rj)))

    # Each task takes values from 0-2.
    Vj <- rep(3, J)

    #Propagate data to matrix of observations
    obs <- array(0, dim = c(total, J, max(Rj), max(Nijr)))
    data %>% as.matrix -> obs[, , 1, 1]

    # Each variable is multinomial
    dist <- rep("multinomial", J)

    alpha <- rep(initial_alpha, K)

    # Initialize theta randomly with Dirichlet distributions
    set.seed(seed)
    theta <- array(0, dim = c(J, K, max(Vj)))
    for (j in 1:J) {
        theta[j, , ] <- rdirichlet(K, rep(initial_theta, Vj[j]))
    }

    # Create model object using model parameters and data
    model_starting_values <- mixedMemModelVarInf(Total = total, J = J, Rj = Rj,
                                                 Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                                                 theta = theta, dist = dist, obs = obs)
    computeELBO(model_starting_values)
    output_model <- mmVarInfFit(model_starting_values, printStatus = 1, printMod = 10)
    computeELBO(output_model)
    if(!missing(permutation)) {
        output_model <- permuteLabels(output_model, permutation)
    }

    if(isTRUE(returnELBO)) {
        return(computeELBO(output_model))
    } else {
        return(output_model)
    }
}

fit_gom <- function(data, total, J, K, alpha, theta) {
    library(mixedMem)

    Rj <- rep(1, J)
    Nijr <- array(1, dim = c(total, J, max(Rj)))

    # Each task takes values from 0-2.
    Vj <- rep(3, J)

    obs <- array(0, dim = c(total, J, max(Rj), max(Nijr)))
    data %>% as.matrix -> obs[, , 1, 1]

    dist <- rep("multinomial", J)

    # Create model object using model parameters and data
    model_starting_values <- mixedMemModelVarInf(Total = total, J = J, Rj = Rj,
                                   Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                                   theta = theta, dist = dist, obs = obs)
    computeELBO(model_starting_values)
    out <- mmVarInfFit(model_starting_values, printStatus = 1, stepType = 0)
    computeELBO(out)
    return(out)
}

cv_fold_elbo <- function(dataTrain, dataTest, totalTrain, totalTest, J, K, initial_alpha, initial_theta, seed) {
    library(mixedMem)

    out <- estimate_gom(dataTrain, totalTrain, J, K, initial_alpha, initial_theta, seed)
    #Can run again for 2-step process (see AOAS 2015 paper)

    out_test_ELBO <- fit_gom(dataTest, totalTest, J, K, out$alpha, out$theta) %>%
                     computeELBO()

    return(out_test_ELBO)
}

extract_classifications <- function(gom_output, filename, return_classifications = FALSE) {
    lambda <- as.data.frame.table((gom_output$phi) / (rowSums(gom_output$phi)))
    lambda <- rename(lambda, occnumber=Var1)
    lambda <- rename(lambda, group=Var2)

    tasks_wide %>% select(c(1,2)) -> occupations
    occupations$occnumber = paste0("Ind",1:nrow(occupations))

    classifications <- inner_join(occupations, lambda)
    classifications$group <- substr(classifications$group, 6,6)
    classifications <- classifications %>% spread(group, Freq)
    for (num_group in 1:(gom_output$K)) {
        classifications <- rename(classifications, !!paste0("freq", num_group) := !!paste0("", num_group, ""))
    }
    if(!missing(filename)){
        write.csv(classifications, filename)
    }
    if(return_classifications == TRUE){
        return(classifications)
    }
}
