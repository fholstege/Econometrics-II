
# Packes required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plm, 
               Formula,
               car,
               clubSandwich,
               lmtest,
               feisr,
               aod)

# load the data
dfWorkers = read.csv("Data/NLSY.csv")

# load files
source("VerbeekNijman.R")

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

#####################
# Mundlak
#####################



# get average over time per worker
X_hat <- data.frame(aggregate(. ~ ID, dfWorkers, mean))
colnames(X_hat)[c(-1)] <- paste(colnames(X_hat)[c(-1)], "AVG", sep = "_")

# add to individual variables
dfMundlak = merge(dfWorkers, X_hat, by = "ID")

Mundlak_model <- pggls(EARNINGS ~  S + AGE + AGESQ + ETHBLACK + URBAN + REGNE + REGNC + REGW  + ASVABC + S_AVG + AGE_AVG + AGESQ_AVG + ETHBLACK_AVG + URBAN_AVG + REGNE_AVG + REGNC_AVG + REGW_AVG, data=dfMundlak, model="random",index = c("ID"))
summary(Mundlak_model)

wald.test(b = coef(Mundlak_model), Sigma = vcov(Mundlak_model), Terms = 10:17)


# apply verbeek nijman test
VerbeekNijman(FE_all,5)


df_tally_id <- dfWorkers%>% group_by_("ID") %>% summarise(n = n())
df_obs_panel <- df_tally_id %>% filter(n>=minPanel)
