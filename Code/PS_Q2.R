library(dplyr)
library(ggplot2)

# Constrain the likelihood

################################################################################
## Question 2
dfMW <- readRDS("Data/MarriedW.rds")
dfCW <- readRDS("Data/CohabW.rds")

ggplot(dfCW, aes(HrlWage)) + geom_histogram()
ggplot(dfMW, aes(HrlWage)) + geom_histogram()

ggplot(dfMW, aes(nonLaborInc)) + geom_histogram()
ggplot(dfCW, aes(nonLaborInc)) + geom_histogram()

################################################################################
# Functions used

# Gaussian Kernel
kernelG <- function(u) {
  exp(-0.5 * u^2) / sqrt(2 * pi)
}

# Get h - bandwith
h <- function(col) {
  hx <- 1.059*sd(col)*(length(col)**(-0.2))
  if(hx == 0){
    return(1)
  }else{
    return(hx)
  }
}


# Kernel Regression
KRegression <- function(Xv,Y,newx, hx){
  N <- nrow(Xv)
  Kprod <- rep(1, N)
  if(length(newx) > 1){
    for (j in 1:ncol(Xv)) {
      Kprod <- Kprod * kernelG((newx[j] - Xv[,j]) / hx[j])
    }
  }
  else{
    for (j in 1:ncol(Xv)) {
      Kprod <- Kprod * kernelG((newx - Xv[,j]) / hx[j])
    }
  }
  sum(Kprod * Y) / sum(Kprod)
}


# Retrieve M(Pr(P=1 given ...)) - Kernel 2
MPr <- function(newPr, Pr, W, Xv, gammas){
  hPr <- h(Pr)
  Kz <- kernelG((newPr - Pr)/hPr)
  return(sum(Kz*(W - as.vector(Xv %*% gammas)))/sum(Kz))
}

################################################################################
### a)
## Cohabiting couples - Estimate Pr(P=1 given y, z, x)

# y = nonLaborInc; z = completed educ, eda; x = eda, n_hij, constant

yzx <- dfCW %>% filter(Year == 2019) %>%
  select(nonLaborInc, constant, cs_p13_1, eda, n_hij) %>% as.matrix()
P <- dfCW %>% filter(Year == 2019) %>%
  select(Employed) %>% as.matrix()

# Kernel Regression - Pr(P = 1 | y,z,x)
hx <- apply(yzx, 2, h)
Pr <- apply(yzx, 1, KRegression,
            Xv=yzx, Y=P, hx=hx)


Phat <- ifelse(Pr>mean(Pr), 1, 0)
table(P,Phat)

# ggplot() + geom_histogram(mapping = aes(Pr[P==1]))
# ggplot() + geom_histogram(mapping = aes(Pr[P==0]))

################################################################################
## Cohabiting couples - Estimate gammas
## Using only w that we observe (Employed = 1)

index <- P == 1

Hwage <- dfCW %>% filter(Year == 2019) %>%
  select(HrlWage) %>% as.matrix()
hx <- h(Pr)

# In this one, we use only Employed == 1
e_w <- Hwage[index] - apply(as.matrix(Pr[index]), 1,KRegression,
                            Xv=as.matrix(Pr[index]), 
                            Y=Hwage[index], hx=hx)


# In both below, we use all observations in the estimation: E(Z given Pr)
# But retrieve the ones under Employed == 1
e_Z1 <- yzx[index,3] - apply(as.matrix(Pr[index]), 1,KRegression,
                             Xv=as.matrix(Pr), Y=yzx[,3], hx=hx)

e_Z2 <- yzx[index,4] - apply(as.matrix(Pr[index]), 1,KRegression,
                             Xv=as.matrix(Pr), Y=yzx[,4], hx=hx)


modelo <- lm(e_w ~  e_Z1  + e_Z2 + 0)
summary(modelo)

gammas <- as.numeric(coef(modelo))
gammas <- as.matrix(gammas, ncol = 1)

#saveRDS(Pr, "Data/Pr.rds")
#saveRDS(e_w, "e_w.rds")
#saveRDS(e_Z1, "e_Z1.rds")
#saveRDS(e_Z2, "e_Z2.rds")


################################################################################
## Cohabiting couples - Estimate w for not Employed
df <- dfCW %>%  filter(Year == 2019)
df$Wage_offer <- ifelse(index, df$HrlWage, 0)

Xv <- df %>% filter(Employed == 0) %>%
  select(cs_p13_1, eda) %>% as.matrix()

Xv1 <- df %>% filter(Employed == 1) %>%
  select(cs_p13_1, eda) %>% as.matrix()


Wage_Nobs <- Xv%*%gammas + apply(as.matrix(Pr[!index]), 1, MPr,
                                 Pr = as.matrix(Pr[index]),
                                 W = df$HrlWage[index],
                                 Xv = Xv1,
                                 gammas = gammas)

df$Wage_offer[!index] <- Wage_Nobs


# Predict observed wages to check the model fit

Wage_obs <- Xv1%*%gammas + apply(as.matrix(Pr[index]), 1, MPr,
                                Pr = as.matrix(Pr[index]),
                                W = df$HrlWage[index],
                                Xv = Xv1,
                                gammas = gammas)


df %>% filter(Employed == 0) %>%
  ggplot() + geom_histogram(mapping = aes(Wage_offer))

df %>% filter(Employed == 1) %>%
  ggplot() + geom_histogram(mapping = aes(Wage_offer))

df %>% filter(Employed == 1) %>%
  ggplot() + geom_point(mapping = aes(Wage_offer, Wage_obs))


################################################################################
## Cohabiting couples - Estimate Betas with likelihood
# Remove if both are zero, non labor income, and wage offer
# nrow(df)
# df <- df %>% filter(!((nonLaborInc == 0)&(Wage_offer==0)))
# nrow(df)

log_likelihood <- function(theta,df) {
  
  X <- df %>%
    select(constant, eda, n_hij) %>%
    as.matrix()
  
  eta <- as.vector(X %*% theta)
#  eta <- plogis(eta) # ensure Beta between 1 and 0
  
  index <- df$Employed == 1
  h_Emp <- df$hrsocup[index]
  w_Emp <- df$Wage_offer[index]
  y_Emp <- df$nonLaborInc[index]
  eta_Emp <- eta[index]
  
  w_nEmp <- df$Wage_offer[!index]
  y_nEmp <- df$nonLaborInc[!index]
  eta_nEmp <- eta[!index]
  
  # Employed
  e_Empl <- (w_Emp * (119 - h_Emp)) / (y_Emp + w_Emp*119) - eta_Emp
  ll1 <- sum(dnorm(e_Empl, log = TRUE))
  
  # Non Employed
  e_nEmpl <- (w_nEmp * 119) / (y_nEmp + w_nEmp * 119) - eta_nEmp
  ll2 <- sum(log(pmax(1e-10, 1 - pnorm(e_nEmpl))))
 
#  outside <- pmax(0, eta - 1)^2 + pmax(0, -eta)^2
   
  return(-(ll1 + ll2))
}


result <- optim(
  par = c(0,0,0),
  fn = log_likelihood,
  df = df,
  method = "BFGS"
#  control = list(maxit = 10000)
)

result$par

X <- df %>% select(constant, eda, n_hij) %>%
  # mutate(eda = scale(eda)[,1],
  #        n_hij = scale(n_hij)[,1]) %>% 
  as.matrix()

Beta <- X%*%result$par

#Beta <- plogis(Beta)
plot(Beta)
summary(Beta)

##### Testando a likelihood

log_likelihood <- function(Beta,df) {
  

  index <- df$Employed == 1
  h_Emp <- df$hrsocup[index]
  w_Emp <- df$Wage_offer[index]
  y_Emp <- df$nonLaborInc[index]

  w_nEmp <- df$Wage_offer[!index]
  y_nEmp <- df$nonLaborInc[!index]

  # Employed
  e_Empl <- (w_Emp * (119 - h_Emp)) / (y_Emp + w_Emp*119) - Beta
  ll1 <- sum(dnorm(e_Empl, log = TRUE))
  
  # Non Employed
  e_nEmpl <- (w_nEmp * 119) / (y_nEmp + w_nEmp * 119) - Beta
  ll2 <- sum(log(pmax(1e-10, 1 - pnorm(e_nEmpl))))
  
  #  outside <- pmax(0, eta - 1)^2 + pmax(0, -eta)^2
  
  return((ll1 + ll2))
}


plot(seq(0,2,0.01), apply(as.matrix(seq(0,2,0.01)), 1, df = df, FUN = log_likelihood))


################################################################################
### a)
## Married couples - Estimate Pr(P=1 given y, z, x)

# y = nonLaborInc; z = constant, completed educ, eda; x = eda, n_hij

yzx <- dfMW %>% filter(Year == 2019) %>%
  select(nonLaborInc, constant, cs_p13_1, eda, n_hij) %>% as.matrix()
P <- dfMW %>% filter(Year == 2019) %>%
  select(Employed) %>% as.matrix()

# Kernel Regression - Pr(P = 1 | y,z,x)
hx <- apply(yzx, 2, h)
Pr <- apply(yzx, 1, KRegression,
            Xv=yzx, Y=P, hx=hx)

# ggplot() + geom_histogram(mapping = aes(Pr[P==1]))
# ggplot() + geom_histogram(mapping = aes(Pr[P==0]))

################################################################################
## Married couples - Estimate gammas
## Using only w that we observe (Employed = 1)

index <- P == 1

Hwage <- dfMW %>% filter(Year == 2019) %>%
  select(HrlWage) %>% as.matrix()
hx <- h(Pr)

# In this one, we use only Employed == 1
e_w <- Hwage[index] - apply(as.matrix(Pr[index]), 1,KRegression,
                            Xv=as.matrix(Pr[index]), 
                            Y=Hwage[index], hx=hx)


# In both below, we use all observations in the estimation: E(Z given Pr)
# But retrieve the ones under Employed == 1
e_Z1 <- yzx[index,3] - apply(as.matrix(Pr[index]), 1,KRegression,
                             Xv=as.matrix(Pr), Y=yzx[,3], hx=hx)

e_Z2 <- yzx[index,4] - apply(as.matrix(Pr[index]), 1,KRegression,
                             Xv=as.matrix(Pr), Y=yzx[,4], hx=hx)


modelo <- lm(e_w ~  e_Z1  + e_Z2 + 0)
summary(modelo)

gammas <- as.numeric(coef(modelo))
gammas <- as.matrix(gammas, ncol = 1)

#saveRDS(Pr, "Data/Pr.rds")
#saveRDS(e_w, "e_w.rds")
#saveRDS(e_Z1, "e_Z1.rds")
#saveRDS(e_Z2, "e_Z2.rds")


################################################################################
## Cohabiting couples - Estimate w for not Employed
df <- dfMW %>%  filter(Year == 2019)
df$Wage_offer <- ifelse(index, df$HrlWage, 0)

Xv <- df %>% filter(Employed == 0) %>%
  select(cs_p13_1, eda) %>% as.matrix()
#  select(constant, niv_ins, eda) %>% as.matrix()

Xv1 <- df %>% filter(Employed == 1) %>%
  select(cs_p13_1, eda) %>% as.matrix()
#  select(constant, niv_ins, eda) %>% as.matrix()


Wage_Nobs <- Xv%*%gammas + apply(as.matrix(Pr[!index]), 1, MPr,
                                 Pr = as.matrix(Pr[index]),
                                 W = df$HrlWage[index],
                                 Xv = Xv1,
                                 gammas = gammas)

df$Wage_offer[!index] <- Wage_Nobs


df %>% filter(Employed == 0) %>%
  ggplot() + geom_histogram(mapping = aes(Wage_offer))

df %>% filter(Employed == 1) %>%
  ggplot() + geom_histogram(mapping = aes(Wage_offer))


################################################################################
## Cohabiting couples - Estimate Betas with likelihood
df <- df %>% filter(!((nonLaborInc == 0)&(Wage_offer==0)))



log_likelihood <- function(theta,df) {
  
  X <- df %>% select(constant, eda, n_hij) %>% as.matrix()
  #  X <- df %>% select(eda, n_hij) %>% as.matrix()
  #  X <- df %>% select(constant) %>% as.matrix()
  eta <- as.vector(X %*% theta)
  
  index <- df$Employed == 1
  h_Emp <- df$hrsocup[index]
  w_Emp <- df$Wage_offer[index]
  y_Emp <- df$nonLaborInc[index]
  eta_Emp <- eta[index]
  
  w_nEmp <- df$Wage_offer[!index]
  y_nEmp <- df$nonLaborInc[!index]
  eta_nEmp <- eta[!index]
  
  # Employed
  e_Empl <- (w_Emp * (168 - h_Emp)) / (y_Emp + w_Emp*168) - eta_Emp
  ll1 <- sum(dnorm(e_Empl, log = TRUE))
  
  # Non Employed
  e_nEmpl <- (w_nEmp * 168) / (y_nEmp + w_nEmp * 168) - eta_nEmp
  ll2 <- sum(log(pmax(1e-10, 1 - pnorm(e_nEmpl))))
  
  return(-(ll1 + ll2))
}




result <- optim(
  par = rep(1, 3),
  #  par = rep(0, 2),
  fn = log_likelihood,
  df = df,
  method = "BFGS",
  control = list(maxit = 10000)
)

result$par

X <- df %>% select(constant,eda, n_hij) %>% as.matrix()
Beta <- X%*%result$par

plot(Beta)
summary(Beta)








################################################################################
library(caret)
library(randomForest)

ctrl <- trainControl(method = "cv",       
                     number = 5,          
                     verboseIter = TRUE)  

set.seed(3110)
modelo_rf <- train(Employed ~ nonLaborInc + niv_ins + eda + n_hij, 
                   data = df, 
                   method = "rf", 
                   trControl = ctrl,
                   tuneLength = 3)

Pr_rf <- randomForest(Employed ~ nonLaborInc + niv_ins + eda + n_hij, data = dfCW)
Pr_rf <- as.matrix(predict(Pr_rf))


e_y_rf <- dfCW$Wage - predict(randomForest(Wage ~ niv_ins + eda, data = dfCW))


e_X2_rf <- X[,2] - predict(randomForest(x = Pr_rf, y = X[,2]))
e_X3_rf <- X[,3] - predict(randomForest(x = Pr_rf, y = X[,3]))



################################################################################
Xv1 <- df %>% filter(Employed == 1) %>%
  select(cs_p13_1, eda) %>% as.matrix()

MPR <- apply(as.matrix(seq(0,1,0.01)), 1, MPr,
             Pr = as.matrix(Pr[index]),
             W = df$HrlWage[index],
             Xv = Xv1,
             gammas = gammas)


plot(seq(0,1,0.01), MPR)



