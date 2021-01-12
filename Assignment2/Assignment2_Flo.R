
# Packes required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plm, 
               Formula,
               car,
               clubSandwich,
               lmtest,
               feisr)

# load the data
dfWorkers = read.csv("Data/NLSY.csv")


# covariance matrix with White SE
vcovW <- function(x) vcovHC(x, method="white1")

# pooled regression with all variables
pooledOLS_all <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW + REGS + ASVABC, data = dfWorkers, model="pooling")
summary(pooledOLS_all, vcov = vcovW)

# pooled regression with all variables, minus test scores
pooledOLS_minusAbility <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW + REGS, data = dfWorkers, model="pooling")
summary(pooledOLS_minusAbility, vcov = vcovW)

# pooled regression with all variables, for non-black workers and black workers
pooledOLS_nonBlack <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW + REGS + ASVABC, data = dfWorkers %>% filter(ETHBLACK == 0), model="pooling")
pooledOLS_Black <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW + REGS + ASVABC, data = dfWorkers %>% filter(ETHBLACK == 1), model="pooling")
summary(pooledOLS_nonBlack, vcov = vcovW)
summary(pooledOLS_Black, vcov = vcovW)

# random effects with all variables
RE_all <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers, model="random", index = "ID")
summary(RE_all, vcov = vcovW)

# fixed effects with all variables
FE_all <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers, model="within", index = "ID")
summary(FE_all, vcov = vcovW)


X_hat = aggregate(. ~ ID, dfWorkers,mean)


calc_X_hat <- function(X, index){
  
  aggregate(. ~ index, X, mean)
  
}