#this file assumes the data being read in is finalized i.e. the relevant variables have been defined
reqPaq <- c('tidyverse', 'ggthemes', 'sf')
installPaq <- reqPaq[!reqPaq %in% installed.packages()]
if(length(installPaq) > 0) install.packages(installPaq)

library(tidyverse) #because you'd be insane not to
library(ggthemes) #only for some default plot themes 
library(sf) #only in event rds file is sf

#obviously, if your name is not Lance Taylor you should update this to some other folder
basePath <- 'G:\\My Drive\\phd\\2nd year\\term 2\\io\\hw 2\\'

dfOg <- basePath%>%
  #assuming the input file name stays constant, otherwise you'll want to update this
  paste0('inputs\\Markets_Data.rds')%>%
  #chances are whoever finalized the data will save it as a csv, so read_csv obvs
  read_rds%>%
  as_tibble%>%
  #mutate is only here since I want to try different vars I AM NOT SAYING THIS IS WHAT WE USE
  mutate(popDensity = Pop2021 / LANDAREA)%>%
  #rename is only here because who the hell puts a space at the end of a column name
  rename_with(~str_squish(.))

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

#we don't have enough obs with more than 2 vets, I think that is causing it to fail
df <- dfOg%>%
  mutate(!!nVar := ifelse(get(nVar) > 2, 2, get(nVar)),
         #also centering and rescaling all non market size vars
         across(all_of(c(wVars, zVars)), ~((.)-mean(.))/sd(.)),
         #any market size var is in terms of deviations from mean 2021 population
         across(all_of(yVars), ~((.) - mean(get(popVar)))/sd(get(popVar))))

#biggest number of establishments in a pop centre
nMax <- max(df[,nVar])

#this is just used in the liklihood function
nEstablishmentsDummyMatrix <- model.matrix(~factor(get(nVar)), df)

nEstablishmentsDummyMatrix[,1] <- nEstablishmentsDummyMatrix[,2:nrow(unique(df[,nVar]))]%>%
  apply(1, function(x) max(x) < 1)%>%
  as.integer

#deterministic component of profict, given params. ripped from paper
profitFun <- function(w, y, z, lambda, beta, gamma_L, alpha, gamma_n, n){
  
  S <- y %*% lambda
  
  V_n <- cbind(w, z) %*% beta + alpha[1] 
  
  if(n > 1) V_n <- V_n - sum(alpha[2:n])
  
  F_n <- w %*% gamma_L + sum(gamma_n[1:n])
  
  profit_n <- S * V_n - F_n
  
  return(profit_n)
  
}

#the likelihood is ripped from the paper as well. I had to add in a little bump when zeroes happened since for any
#initial parameters there would only be probabilities of one or zero which was problematic for taking logs 
logLikelihood_constrained <- function(data, params, nFrom){
  
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
                                 min(., nFrom)))
  
  ll <- cbind(1 - pnorm(pi_n[[1]], lower.tail = FALSE),
              map(1:max(c(1, (nFrom - 1))), ~ (pnorm(pi_n[[.]], lower.tail = FALSE) - pnorm(pi_n[[. + 1]], lower.tail = FALSE))[,1])%>%
                simplify2array(),
              pnorm(pi_n[[nMax]], lower.tail = FALSE))
  
  #unsure how Kosher this is, but I was having issues with starting values otherwise
  for(i in 1:nrow(ll)){
    
    l <- ll[i,]
    
    check <- which(l < 0.00001)
    
    l[check] <- 0.0001
    
    l <- l / sum(l)
    
    ll[i,] <- l
    
  }
  
  ll <- rowSums(log(ll) * nEstablishmentsDummyMatrix)
  
  out <- -sum(ll)
  
  out
  
}

#when nMax = allCoefsForN..., same as unconstrained (tested to verify - copy pasted to the bottom)
results <- map(1:nMax,
    function(allCoefsForNbeyondThisAreTheSame){
  
      #I initially set up initParams as a list, not knowing that optim() refused list inputs. The result is things are a little messier
      initParams <- list(lambda = rep(0.01, length(yVars) - 1),
                         beta = rep(0.01, length(c(wVars, zVars))),
                         gamma_L = rep(0.01, length(wVars)),
                         alpha = rep(0.01, allCoefsForNbeyondThisAreTheSame),
                         gamma_n = rep(0.01, allCoefsForNbeyondThisAreTheSame))
      
      #part of the aforementioned messiness
      paramLocations <- initParams%>%
        map_dbl(~length(.))%>%
        lag(default = 0)
      
      paramLocations <<- 1:length(initParams)%>%
        map(function(i) (1:length(initParams[[i]])) + sum(paramLocations[1:i]))
      
      params <- unlist(initParams)
      
      results <- optim(params, logLikelihood_constrained, data = df, nFrom = allCoefsForNbeyondThisAreTheSame, method = "Nelder-Mead", hessian = TRUE,
                       control = list(abstol = .000000025,
                                      maxit = 50000,
                                      reltol = 1e-11,
                                      trace = 4,
                                      ndeps = rep(.00001, length(params))))
      results
  
})

resultsUnconstrained <- results[[nMax]]


#table 5 (entry threshold estimates and likelihood ratio tests for threshold proportionality)
#figure 4 S_nMax/S_n vs N plot
#table 6 likelihood ratio tests for equal fixed costs 
#table 7 likelihood ratio exclusion tests for the market size regressors 
#table 8 likelihood ratio exclusion tests for the variable profits regressors 
#table 9 entry threshold for alternate market definitions 


#figure 2
figure2Data <- dfOg%>%
  count(popRange = round(2*get(popVar) / 1000)/2)

figure2 <- figure2Data%>%
  ggplot(aes(x = popRange, y = n))+
  geom_col(fill = 'black')+
  theme_minimal()+
  labs(y = 'Number of Towns',
       x = 'Town Population Range (000s)',
       caption = 'each town is at least 1k, bins are increments of 500')

figure2

#table 2 as a chart
table2Data <- dfOg%>%
  count(nIncumbents = get(nVar))

table2Figure <- table2Data%>%
  ggplot(aes(x = nIncumbents, y = n))+
  geom_col(fill = 'black')+
  theme_minimal()+
  labs(y = 'Number of Towns',
       x = 'Number of Establishments')

table2Figure

#figure 3
figure3Data <- dfOg%>%
  count(popRange = round(get(popVar) / 1000),
        nIncumbents = factor(get(nVar), 
                             levels = get(nVar)%>%
                               unique%>%
                               sort%>%
                               rev))

figure3 <- figure3Data%>%
  ggplot(aes(x = popRange, y = n, fill = nIncumbents))+
  geom_col()+
  theme_minimal()+
  scale_fill_economist()+
  labs(y = 'Number of Towns',
       x = 'Town Population Range (000s)',
       caption = 'each town is at least 1k, bins are increments of 1k')

figure3

#table 3 data
table3Data <- dfOg%>%
  summarise(across(all_of(unique(c(nVar, yVars, wVars, zVars))),
                   list(mean = ~mean(.),
                        stDev = ~sd(.),
                        min = ~min(.),
                        max = ~max(.)), .names = '{.col}__{.fn}'))%>%
  pivot_longer(everything())%>%
  separate(name, c('variable', 'summary'), sep = '__')%>%
  pivot_wider(names_from = summary, values_from = value)

#table 4 (results under baseline spec)

table4Data <- data.frame(term = c(yVars[-1],
                                  c(wVars, zVars),
                                  wVars,
                                  'V_1 (a_1)',
                                  'F_1 (g_1)',
                                  2:nMax%>%
                                    paste0('V_', .-1, ' - V_', ., ' (a_', ., ')'),
                                  2:nMax%>%
                                    paste0('F_', ., ' - F_', .-1, ' (g_', ., ')')),
                         value = resultsUnconstrained$par,
                         se = resultsUnconstrained$hessian%>%
                           diag%>%
                           sqrt)%>%
  rownames_to_column('regTerm')

#table5a entry threshold estimates 
table5aData <- (1:nMax)%>%
  map_dfr(function(n){
    
    num <- table3Data%>%
      inner_join(table4Data%>%
                   filter(str_detect(regTerm, 'gamma'))%>%
                   mutate(wVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(numerator = sum(value * mean) + table4Data$value[table4Data$regTerm == 'gamma_n1'])
    
    if(n > 1) num <- num + sum(table4Data$value[str_detect(table4Data$regTerm, 'gamma_n[^1][0-9]*$')])
    
    den <- table3Data%>%
      inner_join(table4Data%>%
                   filter(str_detect(regTerm, 'beta'))%>%
                   mutate(xVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(denominator = sum(value * mean) + table4Data$value[table4Data$regTerm == 'alpha1'])
    
    if(n > 1) den <- den - sum(table4Data$value[str_detect(table4Data$regTerm, 'alpha[^1][0-9]*$')])
    
    data.frame(N = n,
               num = num$numerator,
               den = den$denominator)
    
  })%>%
  mutate(S_n = num / den)

























#old, but in case needed

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
              pnorm(pi_n[[nMax]], lower.tail = FALSE))
  
  #unsure how Kosher this is, but I was having issues with starting values otherwise
  for(i in 1:nrow(ll)){
    
    l <- ll[i,]
    
    check <- which(l < 0.00001)
    
    l[check] <- 0.0001
    
    l <- l / sum(l)
    
    ll[i,] <- l
    
  }
  
  ll <- rowSums(log(ll) * nEstablishmentsDummyMatrix)
  
  out <- -sum(ll)
  
  out
  
}



allCoefsForNbeyondThisAreTheSame <- 1
initParams <- list(lambda = rep(0.01, length(yVars) - 1),
                   beta = rep(0.01, length(c(wVars, zVars))),
                   gamma_L = rep(0.01, length(wVars)),
                   alpha = rep(0.01, allCoefsForNbeyondThisAreTheSame),
                   gamma_n = rep(0.01, allCoefsForNbeyondThisAreTheSame))

#part of the aforementioned messiness
paramLocations <- initParams%>%
  map_dbl(~length(.))%>%
  lag(default = 0)

paramLocations <- 1:length(initParams)%>%
  map(function(i) (1:length(initParams[[i]])) + sum(paramLocations[1:i]))

params <- unlist(initParams)



results <- optim(params, logLikelihood_constrained, data = df, nFrom = allCoefsForNbeyondThisAreTheSame, method = "Nelder-Mead", hessian = TRUE,
                 control = list(abstol = .000000025,
                                maxit = 50000,
                                reltol = 1e-11,
                                trace = 4,
                                ndeps = rep(.00001, length(params))))
results




#I initially set up initParams as a list, not knowing that optim() refused list inputs. The result is things are a little messier
initParams <- list(lambda = rep(0.01, length(yVars) - 1),
                   beta = rep(0.01, length(c(wVars, zVars))),
                   gamma_L = rep(0.01, length(wVars)),
                   alpha = rep(0.01, nMax),
                   gamma_n = rep(0.01, nMax))

#part of the aforementioned messiness
paramLocations <- initParams%>%
  map_dbl(~length(.))%>%
  lag(default = 0)

paramLocations <- 1:length(initParams)%>%
  map(function(i) (1:length(initParams[[i]])) + sum(paramLocations[1:i]))

params <- unlist(initParams)

#I can only get it to run when we cap n at 2, and I use Nelder-Mead
#NOTE: in original paper, I do not know how they handled the W variables in their table
#W can show up in V or F, and so should have a gamma_l and beta coef 
#but their W variable LANDV is only reported with a gamma_l
results <- optim(params, logLikelihood, data = df, method = "Nelder-Mead", hessian = TRUE,
                 control = list(abstol = .000000025,
                                maxit = 50000,
                                reltol = 1e-11,
                                trace = 4,
                                ndeps = rep(.00001, length(params))))

results_unconstrained <- results




