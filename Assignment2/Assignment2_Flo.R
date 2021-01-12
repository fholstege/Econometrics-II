
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
pooledOLS_all <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers, model="pooling")
summary(pooledOLS_all, vcov = vcovW)

# pooled regression with all variables, minus test scores
pooledOLS_minusAbility <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW , data = dfWorkers, model="pooling")
summary(pooledOLS_minusAbility, vcov = vcovW)

# pooled regression with all variables, for non-black workers and black workers
pooledOLS_nonBlack <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers %>% filter(ETHBLACK == 0), model="pooling")
pooledOLS_Black <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers %>% filter(ETHBLACK == 1), model="pooling")
summary(pooledOLS_nonBlack, vcov = vcovW)
summary(pooledOLS_Black, vcov = vcovW)

# random effects with all variables
RE_all <- plm(EARNINGS ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers, model="random", index = "ID")
summary(RE_all, vcov = vcovW)

# fixed effects model with all variables
FE_all <- plm(EARNINGS ~  S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data = dfWorkers, model="within", index = "ID")
summary(FE_all, vcov = vcovW)

# extract the fixed effects
Eta <- fixef(FE_all)

# get average over time per worker
X_hat <- data.frame(aggregate(. ~ ID, dfWorkers, mean))

# add to the fixed effects per worker
dfMundlak <- cbind(X_hat, Eta)

# check out the gamma -- e.g. the coefficient between Xhat and fixed effects 
Mundlak_result <- lm(Eta ~ S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC, data=dfMundlak )


# apply verbeek nijman test
VerbeekNijman(FE_all)

