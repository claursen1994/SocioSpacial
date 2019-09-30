##########################################
#  Power analysis from OZ data 
##########################################
install.packages("simR")
library('simr')

####RtS##########
#Landing Position#
##################
#Re Run a more basic LMM 
LM2<- lmer(lineStartLand~ condition+ (condition|subject)+ (condition|item),data= dat2)
# set up "data":

b<- coef(summary(LM2))[1:2,1]  # slope/ intercepts in model
names(b)<- c("(Intercept)",   "condition1") # change names
rnd<- VarCorr(LM2)
res<- 3.8471

sub <- c(1:80)
item<- c(1:24)
cond <- c('Normal', 'Bold')



X <- expand.grid(condition=cond, sub= sub, item= item)

X$condition<- as.factor(X$condition)
contrasts(X$condition)<- c(0, 1)

# make fake data:
model1 <- makeLmer(y ~ condition + (condition|sub) + (condition|item), fixef=b, VarCorr= rnd, sigma=res, data=X)

summary(model1)

RtSPower=powerSim(model1, nsim= 100)


2+2
#############
#Launch Site#
#############
#Re run a more basic LMM 
LSM<- lmer(launch~ condition+ (condition|subject)+ (condition|item), REML= T,
           data= dat2)
#################
# set up "data":

b<- coef(summary(LSM))[1:2,1]  # slope/ intercepts in model
names(b)<- c("(Intercept)",   "condition1") # change names
rnd<- VarCorr(LSM)
res<- 3.8471

sub <- c(1:60)
item<- c(1:24)
cond <- c('Normal', 'Bold')



X <- expand.grid(condition=cond, sub= sub, item= item)

X$condition<- as.factor(X$condition)
contrasts(X$condition)<- c(0, 1)

# make fake data:
model1 <- makeLmer(y ~ condition + (condition|sub) + (condition|item), fixef=b, VarCorr= rnd, sigma=res, data=X)

summary(model1)

powerSim(model1, nsim= 100)
