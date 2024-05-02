#this file assumes the data being read in is finalized i.e. the relevant variables have been defined

library(tidyverse) #because you'd be insane not to
library(ggthemes) #only for some default plot themes 
library(sf) #only in event rds file is sf

basePath <- 'G:\\My Drive\\phd\\2nd year\\term 2\\io\\hw 2\\'

df <- basePath%>%
  paste0('inputs\\Markets_Data.rds')%>%
  #chances are whoever finalized the data will save it as a csv, so read_csv obvs
  read_rds%>%
  as_tibble%>%
  #mutate is only here since I want to try different vars I AM NOT SAYING THIS IS WHAT WE USE
  mutate(popDensity = Pop2021 / LANDAREA)%>%
  rename(Avg_fam_size = `Avg_fam_size `)

#market ID var
idVars <- 'DGUID'
#whatever variable is used as the desired count of establishments 
nVar <- 'numberOfEstablishments_1kmBuffer'
#market population var (to ensure this is what the market size units are measured in)
popVar <- "Pop2021"
#market size vars , popVar and anything else 
yVars <- c(popVar, "Pop2016")
#demand shifter vars 
zVars <- c("Single_detached_count", "Avg_fam_size", "Avg_children_if_children")
#demand and cost shifting vars 
wVars <- 'popDensity'

nMax <- max(df[,nVar])



#this is just used in the liklihood function
nEstablishmentsDummyMatrix <- model.matrix(~factor(get(nVar)), df)

nEstablishmentsDummyMatrix[,1] <- nEstablishmentsDummyMatrix[,2:nrow(unique(df[,nVar]))]%>%
  apply(1, function(x) max(x) < 1)%>%
  as.integer

profitFun <- function(w, y, z, lambda, beta, gamma_L, alpha, gamma_n, n){
  
  S <- y %*% lambda
  
  V_n <- cbind(w, z) %*% beta + alpha[1] 
  
  if(n > 1) V_n <- V_n - sum(alpha[2:n])
  
  F_n <- w %*% gamma_L + sum(gamma_n[1:n])
  
  profit_n <- S * V_n - F_n
  
  return(profit_n)
  
}

logLikelihood <- function(data, params){
  
  W <- data[,wVars]%>%
    data.matrix()
  
  Y <- data[,yVars]%>%
    data.matrix()
  
  Z <- data[,zVars]%>%
    data.matrix()
  
  lambda <- matrix(c(1, params[paramLocations[[1]]]), 
                   nrow = length(paramLocations[[1]]) + 1)
  
  beta <- matrix(params[paramLocations[[2]]], 
                 nrow = length(paramLocations[[2]]))
  
  gamma_L <- matrix(params[paramLocations[[3]]],
                    nrow = length(paramLocations[[3]]))
  
  alpha <- matrix(params[paramLocations[[4]]], 
                  nrow = length(params[paramLocations[[4]]]))
  
  gamma_n <- matrix(params[paramLocations[[5]]], 
                    nrow = length(params[paramLocations[[5]]]))
  
  pi_n <- map(1:nMax, ~profitFun(W, Y, Z, 
                                 lambda,#matrix(rep(.001, 2), nrow = 2),
                                 beta,#matrix(rep(.001, 4), nrow = 4),
                                 gamma_L,#matrix(rep(.001, 1), nrow = 1),
                                 alpha,#matrix(rep(.001, 5), nrow = 5),
                                 gamma_n,#matrix(rep(.001, 5), nrow = 5),
                                 .))
  
  ll <- cbind(1 - pnorm(pi_n[[1]], lower.tail = FALSE),
              map(1:(nMax - 1), ~ (pnorm(pi_n[[.]], lower.tail = FALSE) - pnorm(pi_n[[. + 1]], lower.tail = FALSE))[,1])%>%
                simplify2array(),
              pnorm(pi_n[[nMax]], lower.tail = FALSE))%>% 
    log%>%
    #right now these are here since no matter what I input I get probabilities of zero or one
    replace(is.na(.) | is.nan(.) | is.infinite(.), 0)
  
  ll <- rowSums(ll * nEstablishmentsDummyMatrix)
  
  return(-sum(ll))
  
}

initParams <- list(lambda = rep(-0.005, length(yVars) - 1),
                   beta = rep(-0.0031, length(c(wVars, zVars))),
                   gamma_L = rep(-0.0021, length(wVars)),
                   alpha = rep(-0.001, nMax),
                   gamma_n = rep(5000000, nMax))

paramLocations <- initParams%>%
  map_dbl(~length(.))%>%
  lag(default = 0)

paramLocations <- 1:length(initParams)%>%
  map(function(i) (1:length(initParams[[i]])) + sum(paramLocations[1:i]))

params <- unlist(initParams)

optim(params, logLikelihood, hessian = TRUE, data = df, method = 'BFGS',
      control = list(maxit = 500,
                     pgtol = 1,
                     trace = 4))


#figure 2
df%>%
  count(popRange = round(2*get(popVar) / 1000)/2)%>%
  ggplot(aes(x = popRange, y = n))+
  geom_col(fill = 'black')+
  theme_minimal()+
  labs(y = 'Number of Towns',
       x = 'Town Population Range (000s)',
       caption = 'each town is at least 1k, bins are increments of 500')

#table 2 as a chart
df%>%
  count(nIncumbents = get(nVar))%>%
  ggplot(aes(x = nIncumbents, y = n))+
  geom_col(fill = 'black')+
  theme_minimal()+
  labs(y = 'Number of Towns',
       x = 'Number of Establishments')

#figure 3
df%>%
  count(popRange = round(get(popVar) / 1000),
        nIncumbents = factor(get(nVar), 
                             levels = get(nVar)%>%
                               unique%>%
                               sort%>%
                               rev))%>%
  ggplot(aes(x = popRange, y = n, fill = nIncumbents))+
  geom_col()+
  theme_minimal()+
  scale_fill_economist()+
  labs(y = 'Number of Towns',
       x = 'Town Population Range (000s)',
       caption = 'each town is at least 1k, bins are increments of 1k')

#table 3 data
df%>%
  summarise(across(all_of(c(nVar, yVars, wVars, zVars)),
                   list(mean = ~mean(.),
                        stDev = ~sd(.),
                        min = ~min(.),
                        max = ~max(.)), .names = '{.col}__{.fn}'))%>%
  pivot_longer(everything())%>%
  separate(name, c('variable', 'summary'), sep = '__')%>%
  pivot_wider(names_from = summary, values_from = value)













