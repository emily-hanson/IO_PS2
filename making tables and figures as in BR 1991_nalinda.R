#prereq stuff----
#this file assumes the data being read in is finalized i.e. the relevant variables have been defined
reqPaq <- c('tidyverse', 'ggthemes', 'sf')
installPaq <- reqPaq[!reqPaq %in% installed.packages()]
if(length(installPaq) > 0) install.packages(installPaq)

library(tidyverse) #because you'd be insane not to
library(ggthemes) #only for some default plot themes 
library(sf) #only in event rds file is sf

##end prereq stuff----

#obviously, if your name is not Lance Taylor you should update this to some other folder
basePath <- getwd()

industryFile <- Markets_Data_moreInd

#whatever variable is used as the desired count of establishments 
nVar <- 'nDentists_1kmBuffer'
alternate_nVar <- c("nDentists_2kmBuffer", "nDentists_5kmBuffer")
#market population var (to ensure this is what the market size units are measured in)
popVar <- "C1_COUNT_TOTAL_Population..2021"
#market size vars , popVar and anything else 
yVars <- c(popVar, 'C1_COUNT_TOTAL_Owner','C1_COUNT_TOTAL_Population.percentage.change..2016.to.2021','C1_COUNT_TOTAL_Commuteout')
#demand shifter vars 
zVars <- c('C1_COUNT_TOTAL_65.years.and.over','C1_COUNT_TOTAL_Average.number.of.children.in.census.families.with.children',
	'C1_COUNT_TOTAL_Average.household.size','C1_COUNT_TOTAL_Average.total.income.in.2020.among.recipients....',
	'C1_COUNT_TOTAL_Unemployed','C1_COUNT_TOTAL_Dwellingperpop','C1_COUNT_TOTAL_Average.value.of.dwellings....')
#demand and cost shifting vars 
wVars <- c('C1_COUNT_TOTAL_Population.density.per.square.kilometre',"C1_COUNT_TOTAL_Dwellingperkm")

#this sets what the upper bond of establishments considered in i.e. original bresnahan and reiss paper had everything
#at 5 and beyond pooled as 5+ (or less for some industries)
poolOver <- 4

##data and function setup ----

#fileName <- industryFile%>%
  #str_remove('\\..+$')

#outFolder <- paste0(basePath, fileName, '_outputs/')

#if(!dir.exists(outFolder)) {
	#dir.create(outFolder)}

#dfOg <- basePath%>%
  ##assuming the input file name stays constant, otherwise you'll want to update this
  #paste0(industryFile)%>%
  ##chances are whoever finalized the data will save it as a csv, so read_csv obvs
  #read_csv%>%
  #as_tibble%>%
  ##mutate is only here since I want to try different vars I AM NOT SAYING THIS IS WHAT WE USE
  ##mutate(popDensity = Pop2021 / LANDAREA)%>%
  ##rename is only here because who the hell puts a space at the end of a column name
  ##I will remove these later, they are just since the fed in data wasn't entirely cleaned
  #rename_with(~str_squish(.))%>%
  #mutate(`C1_COUNT_TOTAL_65 years and over` = str_extract(`C1_COUNT_TOTAL_65 years and over`, '[0-9]+')%>% as.numeric)

dfOg <- as_tibble(industryFile)%>%
	rename_with(~str_squish(.))

#we don't have enough obs with more than 2 vets, I think that is causing it to fail
df <- dfOg%>%
  mutate(!!nVar := ifelse(get(nVar) > poolOver, poolOver, get(nVar)),
         #also centering and rescaling all non market size vars
         across(all_of(c(wVars, zVars)), ~((.)-mean(.))/sd(.)),
         #any market size var is in terms of deviations from mean 2021 population
         across(all_of(yVars), ~((.) - mean(get(popVar)))/sd(get(popVar))))%>%
  select(any_of(c(nVar, wVars, zVars, yVars)))

#biggest number of establishments in a pop centre
nMax <- max(df[,nVar])

#this is just used in the liklihood function for the outcomes
outcomeMatrixFun <- function(data, outcome){
  
  temp <- data%>%
    mutate(n = get(outcome)%>%
             factor(levels = unique(.)%>%sort))
  
  nEstablishmentsDummyMatrix <- model.matrix(~n, temp)
  
  nEstablishmentsDummyMatrix[,1] <- nEstablishmentsDummyMatrix[,2:nrow(unique(temp[,outcome]))]%>%
    apply(1, function(x) max(x) < 1)%>%
    as.integer
  
  nEstablishmentsDummyMatrix
  
}

#deterministic component of profict, given params. ripped from paper
profitFun <- function(w, y, x, lambda, beta, gamma_L, alpha, gamma_n, n){
  
  S <- y %*% lambda
  
  xb <- x %*% beta
  
  if(ncol(xb) == 0) xb <- matrix(rep(0, nrow(xb)), nrow = nrow(xb))
  
  V_n <- xb + alpha[1] 
  
  if(length(alpha) > 1 & n > 1) V_n <- V_n - sum(alpha[2:min(c(n, length(alpha)))])
  
  F_n <- w %*% gamma_L + gamma_n[1] 
  
  if(length(gamma_n) > 1 & n > 1) F_n <- F_n - sum(gamma_n[2:min(c(n,length(gamma_n)))])
  
  profit_n <- S * V_n - F_n
  
  return(profit_n)
  
}

paramsFromList <- function(init){
  
  paramLocations <- init%>%
    map_dbl(~length(.))%>%
    lag(default = 0)
  
  #the key thing is the global assignment here, so within optim() it can be found
  paramLocations <<- 1:length(init)%>%
    map(function(i) if(length(init[[i]]) > 0) (1:length(init[[i]])) + sum(paramLocations[1:i]))
  
  unlist(init)
  
}

#the likelihood is ripped from the paper as well. I had to add in a little bump when zeroes happened since for any
#initial parameters there would only be probabilities of one or zero which was problematic for taking logs 
#note this is unconstrained when nFrom = nMax
logLikelihood_constrained <- function(data, params, nFrom, w, y, x, outcomeMatrix){
  
  W <- data[,w]%>%
    data.matrix()
  
  Y <- data[,y]%>%
    data.matrix()
  
  X <- data[,x]%>%
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
  
  pi_n <- map(1:nMax, ~profitFun(W, Y, X, 
                                 lambda,#matrix(rep(.001, 2), nrow = 2),
                                 beta,#matrix(rep(.001, 4), nrow = 4),
                                 gamma_L,#matrix(rep(.001, 1), nrow = 1),
                                 alpha,#matrix(rep(.001, 5), nrow = 5),
                                 gamma_n,#matrix(rep(.001, 5), nrow = 5),
                                 min(., nFrom)))
  
  ll <- cbind(1 - pnorm(pi_n[[1]], lower.tail = FALSE),
              map(1:max(c(1, (nMax - 1))), ~ (pnorm(pi_n[[.]], lower.tail = FALSE) - pnorm(pi_n[[. + 1]], lower.tail = FALSE))[,1])%>%
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
  
  ll <- rowSums(log(ll) * outcomeMatrix)
  
  out <- -sum(ll)
  
  out
  
}

## end data and function setup----

##ordered probits used up to table 7, 8 and 9 later----

#when nMax = allCoefsForN..., same as unconstrained (tested to verify - copy pasted to the bottom)
#use this for table 4 and 5
results <- map(1:nMax,
    function(allCoefsForNbeyondThisAreTheSame){
      
      initParams <- list(lambda = rep(0.01, length(yVars) - 1),
                         beta = rep(0.01, length(c(wVars, zVars))),
                         gamma_L = rep(0.01, length(wVars)),
                         alpha = rep(0.01, allCoefsForNbeyondThisAreTheSame),
                         gamma_n = rep(0.01, allCoefsForNbeyondThisAreTheSame))
  
      params <- paramsFromList(initParams)
      
      results <- optim(params, logLikelihood_constrained, data = df, nFrom = allCoefsForNbeyondThisAreTheSame, 
                       w = wVars, y = yVars, x = c(wVars, zVars), outcomeMatrix = outcomeMatrixFun(df, nVar), 
                       method = "Nelder-Mead", hessian = TRUE,
                       control = list(abstol = .000000025,
                                      maxit = 50000,
                                      reltol = 1e-11,
                                      trace = 4,
                                      ndeps = rep(.00001, length(params))))
      results
  
})

resultsUnconstrained <- results[[nMax]]


#all firms in market have same fixed cost, i.e. table 6, aka F_n = gamma_1 + gamma_l W (no sum gamma_n)
initParams_sameCosts <- list(lambda = rep(0.01, length(yVars) - 1),
                             beta = rep(0.01, length(c(wVars, zVars))),
                             gamma_L = rep(0.01, length(wVars)),
                             alpha = rep(0.01, nMax),
                             gamma_n = rep(0.01, 1))

params <- paramsFromList(initParams_sameCosts)

results_no_gamma2orhigher <- optim(params, logLikelihood_constrained, data = df, nFrom = nMax, 
                                   w = wVars, y = yVars, x = c(wVars, zVars), outcomeMatrix = outcomeMatrixFun(df, nVar),
                                   method = "Nelder-Mead", hessian = TRUE,
                                   control = list(abstol = .000000025,
                                                  maxit = 50000,
                                                  reltol = 1e-11,
                                                  trace = 4,
                                                  ndeps = rep(.00001, length(params))))

#check if only current town population matters for market size 
initParams_sameSize<- list(lambda = NULL,
                             beta = rep(0.01, length(c(wVars, zVars))),
                             gamma_L = rep(0.01, length(wVars)),
                             alpha = rep(0.01, nMax),
                             gamma_n = rep(0.01, 1))

params <- paramsFromList(initParams_sameSize)

results_noOtherPopVars <- optim(params, logLikelihood_constrained, data = df, nFrom = nMax, 
                                   w = wVars, y = popVar, x = c(wVars, zVars), outcomeMatrix = outcomeMatrixFun(df, nVar), 
                                   method = "Nelder-Mead", hessian = TRUE,
                                   control = list(abstol = .000000025,
                                                  maxit = 50000,
                                                  reltol = 1e-11,
                                                  trace = 4,
                                                  ndeps = rep(.00001, length(params))))

##end ordered probits used----


#tables and figures up to table 7as in Bresnahan and Reiss 1991----

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
        nIncumbents = ifelse(get(nVar) >= poolOver, paste0(nMax, '+'), as.character(get(nVar))),
        nIncumbents = factor(nIncumbents, 
                             levels = as.character(0:poolOver)%>%
                               replace(. == poolOver, paste0(poolOver, '+'))%>%
                               rev))

figure3 <- figure3Data%>%
  ggplot(aes(x = popRange, y = n, fill = nIncumbents))+
  geom_col()+
  theme_minimal()+
  scale_fill_economist()+
  labs(y = 'Number of Towns',
       x = 'Town Population Range (000s)',
       fill = 'number of firms',
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
  rownames_to_column('regTerm')%>%
  bind_rows(data.frame(term = 'Log likelihood', 
                       value = resultsUnconstrained$value, 
                       regTerm = 'Log likelihood'))

#table5a entry threshold estimates 
table5aData <- (1:nMax)%>%
  map_dfr(function(n){
    
    num <- table3Data%>%
      inner_join(table4Data%>%
                   filter(str_detect(regTerm, 'gamma'))%>%
                   mutate(wVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(numerator = sum(value * mean) + table4Data$value[table4Data$regTerm == 'gamma_n1'])
    
    if(n > 1) num <- num + sum(table4Data$value[table4Data$regTerm %in% paste0('gamma_n', 2:n)])
    
    den <- table3Data%>%
      inner_join(table4Data%>%
                   filter(str_detect(regTerm, 'beta'))%>%
                   mutate(xVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(denominator = sum(value * mean) + table4Data$value[table4Data$regTerm == 'alpha1'])
    
    if(n > 1) den <- den - sum(table4Data$value[table4Data$regTerm %in% paste0('alpha', 2:n)])
    
    data.frame(N = n,
               num = num$numerator,
               den = den$denominator)
    
  })%>%
  mutate(S_n = num / den,
         S_n_over_S_nMinusOne = S_n / lag(S_n),
         S_nName = paste0('S_', N),
         SratioName = paste0('S_', N, '/S_', lag(N)))

#note since we were minimizing with optim(), output was the minimized negative LL, so LRT has signs switched
table5bData <- map_dfr(1:(nMax - 1),
                       function(n) data.frame(LRT = -2*(resultsUnconstrained$value - results[[n]]$value),
                                              nFrom = n))%>%
  mutate(df = 2*(nMax - nFrom))

#not making plot yet since with only vets we had just one firm and one relevant ratio...
figure4Data <- table5aData%>%
  mutate(S_nMaxOverS_n = last(S_n)/S_n)

figure4 <- figure4Data%>%
  ggplot(aes(x = factor(N), y = S_nMaxOverS_n))+
  geom_point()+
  theme_minimal()+
  labs(y = paste0('S_', nMax, '/S_N'),
       x = 'numer of firms')

figure4

#table 6 likelihood ratio tests for equal fixed costs 
table6RegResults <- data.frame(term = c(yVars[-1],
                                  c(wVars, zVars),
                                  wVars,
                                  'V_1 (a_1)',
                                  'F_1 (g_1)',
                                  2:nMax%>%
                                    paste0('V_', .-1, ' - V_', ., ' (a_', ., ')')),
                         value = results_no_gamma2orhigher$par,
                         se = results_no_gamma2orhigher$hessian%>%
                           diag%>%
                           sqrt)%>%
  rownames_to_column('regTerm')

table6Data <- (1:nMax)%>%
  map_dfr(function(n){
    
    num <- table3Data%>%
      inner_join(table6RegResults%>%
                   filter(str_detect(regTerm, 'gamma'))%>%
                   mutate(wVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(numerator = sum(value * mean) + table6RegResults$value[table6RegResults$regTerm == 'gamma_n'])
    
    den <- table3Data%>%
      inner_join(table6RegResults%>%
                   filter(str_detect(regTerm, 'beta'))%>%
                   mutate(xVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(denominator = sum(value * mean) + table6RegResults$value[table6RegResults$regTerm == 'alpha1'])
    
    if(n > 1) den <- den - sum(table6RegResults$value[table6RegResults$regTerm %in% paste0('alpha', 2:n)])
    
    data.frame(N = n,
               num = num$numerator,
               den = den$denominator)
    
  })%>%
  mutate(S_n = num / den,
         S_n_over_S_nMinusOne = S_n / lag(S_n),
         Srat = paste0('S_', N, '/S_', lag(N)))%>%
  filter(N > 1)%>%
  select(-c(N:S_n))%>%
  pivot_wider(names_from = Srat, values_from = S_n_over_S_nMinusOne)%>%
  mutate(LikelihoodValue = results_no_gamma2orhigher$value,
         LRT = -2*(resultsUnconstrained$value - results_no_gamma2orhigher$value),
         degreesFreedom = nMax - 1)%>%
  relocate(matches('^S'), .after = degreesFreedom)

#table 7 likelihood ratio tests for equal market definitions 
table7RegResults <- data.frame(term = c(c(wVars, zVars),
                                        wVars,
                                        'V_1 (a_1)',
                                        'F_1 (g_1)',
                                        2:nMax%>%
                                          paste0('V_', .-1, ' - V_', ., ' (a_', ., ')')),
                               value = results_noOtherPopVars$par,
                               se = results_noOtherPopVars$hessian%>%
                                 diag%>%
                                 sqrt)%>%
  rownames_to_column('regTerm')

table7Data <- (1:nMax)%>%
  map_dfr(function(n){
    
    num <- table3Data%>%
      inner_join(table7RegResults%>%
                   filter(str_detect(regTerm, 'gamma'))%>%
                   mutate(wVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(numerator = sum(value * mean) + table7RegResults$value[table7RegResults$regTerm == 'gamma_n'])
    
    den <- table3Data%>%
      inner_join(table7RegResults%>%
                   filter(str_detect(regTerm, 'beta'))%>%
                   mutate(xVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(denominator = sum(value * mean) + table7RegResults$value[table7RegResults$regTerm == 'alpha1'])
    
    if(n > 1) den <- den - sum(table7RegResults$value[table7RegResults$regTerm %in% paste0('alpha', 2:n)])
    
    data.frame(N = n,
               num = num$numerator,
               den = den$denominator)
    
  })%>%
  mutate(S_n = num / den,
         S_n_over_S_nMinusOne = S_n / lag(S_n),
         Srat = paste0('S_', N, '/S_', lag(N)))%>%
  filter(N > 1)%>%
  select(-c(N:S_n))%>%
  pivot_wider(names_from = Srat, values_from = S_n_over_S_nMinusOne)%>%
  mutate(LikelihoodValue = results_noOtherPopVars$value,
         LRT = -2*(resultsUnconstrained$value - results_noOtherPopVars$value),
         variablesOmitted = yVars%>%
           subset(. != popVar)%>%
           paste(collapse = ', '))%>%
  relocate(matches('^S'), .after = variablesOmitted)

#end tables and figures up to table 7----

#table 8 likelihood ratio exclusion tests for the variable profits regressors 
#table 9 entry threshold for alternate market definitions 

#exclude variable profit regressors
zVarsForTable8 <- zVars[!zVars %in% table4Data$term[table4Data$value < table4Data$se]]

wVarsForTable8 <- wVars[!wVars %in% table4Data$term[table4Data$value < table4Data$se]]

initParams_noIntermarketProfitVariation <- list(lambda = rep(0.01, length(yVars) - 1),
                                                beta = rep(0.01, length(c(wVarsForTable8, zVarsForTable8))),
                                                gamma_L = rep(0.01, length(wVars)),
                                                alpha = rep(0.01, nMax),
                                                gamma_n = rep(0.01, 1))

params <- paramsFromList(initParams_noIntermarketProfitVariation)

results_noProfitVariation <- optim(params, logLikelihood_constrained, data = df, nFrom = nMax, 
                                w = wVars, y = yVars, x = c(wVarsForTable8, zVarsForTable8), outcomeMatrix = outcomeMatrixFun(df, nVar), 
                                method = "Nelder-Mead", hessian = TRUE,
                                control = list(abstol = .000000025,
                                               maxit = 50000,
                                               reltol = 1e-11,
                                               trace = 4,
                                               ndeps = rep(.00001, length(params))))

#table 7 likelihood ratio tests for equal market definitions 
table8RegResults <- data.frame(term = c(yVars[-1],
                                        c(wVarsForTable8, zVarsForTable8),
                                        wVars,
                                        'V_1 (a_1)',
                                        'F_1 (g_1)',
                                        2:nMax%>%
                                          paste0('V_', .-1, ' - V_', ., ' (a_', ., ')')),
                               value = results_noProfitVariation$par,
                               se = results_noProfitVariation$hessian%>%
                                 diag%>%
                                 sqrt)%>%
  rownames_to_column('regTerm')

table8Data <- (1:nMax)%>%
  map_dfr(function(n){
    
    num <- table3Data%>%
      inner_join(table8RegResults%>%
                   filter(str_detect(regTerm, 'gamma'))%>%
                   mutate(wVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(numerator = sum(value * mean) + table8RegResults$value[table8RegResults$regTerm == 'gamma_n'])
    
    den <- table3Data%>%
      inner_join(table8RegResults%>%
                   filter(str_detect(regTerm, 'beta'))%>%
                   mutate(xVar = TRUE),
                 by = c('variable' = 'term'))%>%
      summarise(denominator = sum(value * mean) + table8RegResults$value[table8RegResults$regTerm == 'alpha1'])
    
    if(n > 1) den <- den - sum(table8RegResults$value[table8RegResults$regTerm %in% paste0('alpha', 2:n)])
    
    data.frame(N = n,
               num = num$numerator,
               den = den$denominator)
    
  })%>%
  mutate(S_n = num / den,
         S_n_over_S_nMinusOne = S_n / lag(S_n),
         Srat = paste0('S_', N, '/S_', lag(N)))%>%
  filter(N > 1)%>%
  select(-c(N:S_n))%>%
  pivot_wider(names_from = Srat, values_from = S_n_over_S_nMinusOne)%>%
  mutate(LikelihoodValue = results_noOtherPopVars$value,
         LRT = -2*(resultsUnconstrained$value - results_noOtherPopVars$value),
         variablesOmitted = c(wVars, zVars)%>%
           subset(!. %in% c(wVarsForTable8, zVarsForTable8))%>%
           paste(collapse = ', '))%>%
  relocate(matches('^S'), .after = variablesOmitted)




rm(list = c('nVar', 'df'))

table9Data <- alternate_nVar%>%
  map_dfr(function(nVar){
    
    #we don't have enough obs with more than 2 vets, I think that is causing it to fail
    df <- dfOg%>%
      mutate(!!nVar := ifelse(get(nVar) > poolOver, poolOver, get(nVar)),
             #also centering and rescaling all non market size vars
             across(all_of(c(wVars, zVars)), ~((.)-mean(.))/sd(.)),
             #any market size var is in terms of deviations from mean 2021 population
             across(all_of(yVars), ~((.) - mean(get(popVar)))/sd(get(popVar))))%>%
      select(any_of(c(nVar, wVars, zVars, yVars)))
    
    #biggest number of establishments in a pop centre
    nMax <- max(df[,nVar])
    
    initParams <- list(lambda = rep(0.01, length(yVars) - 1),
                       beta = rep(0.01, length(c(wVars, zVars))),
                       gamma_L = rep(0.01, length(wVars)),
                       alpha = rep(0.01, nMax),
                       gamma_n = rep(0.01, nMax))
    
    params <- paramsFromList(initParams)
    
    resultsTemp <- optim(params, logLikelihood_constrained, data = df, nFrom = nMax, 
                         w = wVars, y = yVars, x = c(wVars, zVars), outcomeMatrix = outcomeMatrixFun(df, nVar),
                         method = "Nelder-Mead", hessian = TRUE,
                         control = list(abstol = .000000025,
                                        maxit = 50000,
                                        reltol = 1e-11,
                                        trace = 4,
                                        ndeps = rep(.00001, length(params))))
    
    tempResultsTable <- data.frame(term = c(yVars[-1],
                                            c(wVars, zVars),
                                            wVars,
                                            'V_1 (a_1)',
                                            'F_1 (g_1)',
                                            2:nMax%>%
                                              paste0('V_', .-1, ' - V_', ., ' (a_', ., ')'),
                                            2:nMax%>%
                                              paste0('F_', ., ' - F_', .-1, ' (g_', ., ')')),
                                   value = resultsTemp$par,
                                   se = resultsTemp$hessian%>%
                                     diag%>%
                                     sqrt)%>%
      rownames_to_column('regTerm')
    
    table9out <- (1:nMax)%>%
      map_dfr(function(n){
        
        num <- table3Data%>%
          inner_join(tempResultsTable%>%
                       filter(str_detect(regTerm, 'gamma'))%>%
                       mutate(wVar = TRUE),
                     by = c('variable' = 'term'))%>%
          summarise(numerator = sum(value * mean) + tempResultsTable$value[tempResultsTable$regTerm == 'gamma_n1'])
        
        if(n > 1) num <- num + sum(tempResultsTable$value[tempResultsTable$regTerm %in% paste0('gamma_n', 2:n)])
        
        den <- table3Data%>%
          inner_join(tempResultsTable%>%
                       filter(str_detect(regTerm, 'beta'))%>%
                       mutate(xVar = TRUE),
                     by = c('variable' = 'term'))%>%
          summarise(denominator = sum(value * mean) + tempResultsTable$value[tempResultsTable$regTerm == 'alpha1'])
        
        if(n > 1) den <- den - sum(tempResultsTable$value[tempResultsTable$regTerm %in% paste0('alpha', 2:n)])
        
        data.frame(N = n,
                   num = num$numerator,
                   den = den$denominator)
        
      })%>%
      mutate(S_n = num / den,
             S_n_over_S_nMinusOne = S_n / lag(S_n),
             SratioName = paste0('S_', N, '/S_', lag(N)))%>%
      filter(N > 1)%>%
      select(SratioName, S_n_over_S_nMinusOne)%>%
      pivot_wider(names_from = SratioName, values_from = S_n_over_S_nMinusOne)%>%
      mutate(distanceSpec = nVar)
    
    table9out
    
  })




#this is just saving stuff to the specified folder

ls()%>%
  str_subset('igure.{0,1}$')%>%
  walk(~ggsave(paste0(outFolder, ., '.png'), 
               plot = get(.)))

ls()%>%
  str_subset('[dD]ata')%>%
  walk(~write_csv(get(.), paste0(outFolder, ., '.csv')))















