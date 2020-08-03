  library(readxl)
  library(tidyverse)
  library(mixedMem)
  setwd("/Users/steven/Documents/School/Second Year/950/paper/")
  Abilities <- read_excel("~/Downloads/Abilities.xlsx")
  Abilities <- subset(Abilities, `Scale ID`=="LV")
  
  keep <- c("O*NET-SOC Code", "Title", "Element Name", "Data Value")
  Abilities <- Abilities[, names(Abilities) %in% keep, drop = F]
  
  Skills <- read_excel("~/Downloads/Skills.xlsx")
  Skills <- subset(Skills, `Scale ID`=="LV")
  Skills <- Skills[, names(Skills) %in% keep, drop = F]
  
  Activities <- read_excel("~/Downloads/Work Activities.xlsx")
  Activities <- subset(Activities, `Scale ID`=="LV")
  Activities <- Activities[, names(Activities) %in% keep, drop = F]
  
  tasks <- bind_rows(Abilities, Skills, Activities)
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`<=3), 0))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>3), 1))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`<=(7/3)), 0))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>(7/3) & `Data Value`<=(14/3)), 1))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>(14/3)), 2))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`<=2), 0))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>2 & `Data Value`<=3), 1))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>3 & `Data Value`<=4), 2))
  #tasks <- tasks %>% mutate(`Data Value` = replace(`Data Value`, which(`Data Value`>4), 3))
  tasks_wide <- tasks %>% spread(`Element Name`,`Data Value`)
  
  for (j in 3:ncol(tasks_wide)){
    tasks_wide[[colnames(tasks_wide %>% select(c(j)))]] <- ntile(tasks_wide[[colnames(tasks_wide %>% select(c(j)))]],3) - 1
    print(j)
  }
  
  Total <- 968
  # Number of variables
  J <- 128
  # we only have one replicate for each of the variables
  Rj <- rep(1, J)
  # Nijr indicates the number of ranking levels for each variable.
  # Since all our data is multinomial it should be an array of all 1s
  Nijr <- array(1, dim = c(Total, J, max(Rj)))
  # Number of sub-populations
  K <-7
  # There are 3 choices for each of the variables ranging from 0 to 2.
  Vj <- rep(3, J)
  # All variables are multinomial
  dist <- rep("multinomial", J)
  # obs are the observed responses. it is a 4-d array indexed by i,j,r,n
  # note that obs ranges from 0 to 2 for each response
  obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
  tasks_wide %>% select(-c(1:2)) %>% as.matrix -> obs[, , 1, 1]
  
  # Initialize theta randomly with Dirichlet distributions
  set.seed(750)
  #initializations <- getInitialization(1/3,3/3,K)
  alpha <- rep(1.6, K)
  theta <- array(0, dim = c(J, K, max(Vj)))
  for (j in 1:J) {
    theta[j, , ] <- gtools::rdirichlet(K, rep(1, Vj[j]))
  }
  
  # Create the mixedMemModel
  # This object encodes the initialization points for the variational EM algorithim
  # and also encodes the observed parameters and responses
  initial <- mixedMemModelVarInf(Total = Total, J = J, Rj = Rj,
                           Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                           theta = theta, dist = dist, obs = obs)
  computeELBO(initial)
  st = proc.time()
  #printStatus 1 indicates that status updates will be printed
  # printMod 25 indicates that status updates will only be printed ever 25th step
  out <- mmVarInfFit(initial, printStatus = 1, printMod = 25)
  end <- proc.time()
  computeELBO(out)
  computeBIC(out)
  time <- end - st
  
  #hellingerDist <- (1/sqrt(2)) * sqrt(rowSums( (sqrt(out$theta[, 1, ]) 
  #                                              - sqrt(out$theta[, 2, ]))^2))
  #par(mar = par('mar') + c(10,0,0,0))
  #barplot(sort(out$theta[,1,3], decreasing = T),
  #        names.arg = tasks_wide %>% select(-c(1,2)) %>% colnames(order(out$theta[,1,3], decreasing = T)),
  #        main = "Hellinger Distance",
  #        cex.names = .7, las = 2, ylab = "Hellinger Distance",
  #        ylim = c(0, 1))
  #mtext("Between Conservatives and Liberals")
  
  #colnames(tasks_wide %>% select(-c(1,2)))[order(out$theta[,1,3], decreasing = T)][1:3]
  out <- permuteLabels(out, c(6, 2, 4, 5, 1, 3, 7))
  lambda.point <- out$phi / rowSums(out$phi)
  lambda <- as.data.frame.table(lambda.point)
  lambda <- rename(lambda, occnumber=Var1)
  lambda <- rename(lambda, puregroup=Var2)
  
  tasks_wide %>% select(c(1,2)) -> occupations
  occupations$occnumber = paste0("Ind",1:nrow(occupations))
  
  classifications <- inner_join(occupations, lambda)
  classifications$puregroup <- substr(classifications$puregroup, 6,6)
  classifications <- classifications %>% spread(puregroup, Freq)
  for (num_group in 1:(K)){
    classifications <- rename(classifications, !!paste0("freq", num_group) := !!paste0("", num_group, ""))
  }
  #classifications <- rename(classifications, freq0=`0`)
  #classifications <- rename(classifications, freq1=`1`)
  #classifications <- rename(classifications, freq2=`2`)
  #theta_2 <- as.data.frame(cbind(colnames(tasks_wide %>% select(-c(1,2))),out$theta[,,2]),)
  
  
  write.csv(classifications,'classifications.csv')
