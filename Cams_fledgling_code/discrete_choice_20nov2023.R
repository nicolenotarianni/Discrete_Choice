# download JAGS
# https://sourceforge.net/projects/mcmc-jags/

library(rjags); library(jagsUI); library(ggplot2)

# read in fledlging dataset
fledge <- read.csv("C:/Users/User/Desktop/nicole analyses/discrete choice/dsf_3.csv")

head(fledge)

# SID = subbrood ID - effectively fledgling/individual ID
# CID = choice ID; each selection event has one of these
# Use = 20 values per choice ID; 1 used, 19 randoms
# Distance = distance from feature; "-30" means 30m away

#Define data
T <- nrow(fledge) # sample size
chsets <- fledge$CID # choice sets column right from the dataset
alts <- fledge$Alts # each alternative within a choice sets; right from the dataset
use <- fledge$Use # binary 1/0, used/unused column
nchsets <- max(fledge$CID) # total number of choice sets (n=999; number of habitat use observations)
nalts <- max(fledge$Alts) # total number of alternatives (n=20)
ninds <- max(fledge$SID) # total number of subbroods/individuals in the dataset (n=43)
# covariates
early <- (fledge$Early) # Covariate 1 = "early successional"
mature <- (fledge$Mature) # Covariate 2 = "mature forest"
fw <- (fledge$FW)  # Covariate 3 = "forested wetland"
sw <- (fledge$SW) # Covariate 4 = "shrub wetland"
sapling <- (fledge$Sapling) # Covariate 5 = "sapling stand"
thinned <- (fledge$Thin) # Covariate 6 = "thinned forest"
edge <- (fledge$Edge) # Covariate 7 = "edge"


# delineate the matrix of variables
X1 <- cbind(early, mature, sapling, thinned, edge)
X1 <- scale(X1) # scale
npred <- ncol(X1) # number of predictors

# sub-brood numbers for each choice set
# this tells the model which choice set belongs to which bird
sub_id <- subset(fledge, Alts==20)$SID

# response for the model
y <- data.frame("CID" = fledge$CID, "Use" = fledge$Use, "Alts" = fledge$Alts)
ytest <- reshape(y, direction = "wide", idvar = "CID", timevar = "Alts") # why is this needed?

####################################

## Loop to transcribe matrix X to 'wide' format
# this is the matrix of the covariates in WIDE format

Z <- array(NA, dim=c(npred, nchsets, nalts)) # blank array to be populated below
#View(Z[,,1]) # ask for first matrix; 3rd dimension is "which slice of the array". z[row, col, matrix]; this is just for funsies

for(i in 1:T){               # For each row in the data file (T = total sample size)...
  for(j in 1:npred){         # For each predictor variable (5 predictor variables here)...
    Z[j, chsets[i], alts[i]] <- X1[i, j] 
    # ^ makes the 'Z' array (stack of matrices)
    # transforming the X data
    # Z will contain 20 matrices; one for each alt (notice it's the 3rd dimmension in the array)
    # 'j' rows,  
  }
} 

# ^^ the main point of this is to create a Z matrix
# we use this matrix below by multiplying it by the betas
# this will ultimately be used to generate the beta coefficients
# Y = a1 + b1*vegcov + b2*vegcov2 
# 'vegcov', 'vegcov2', etc. are being generated here.

#####################################################
# Package data for JAGS; needs to be a list; basically just a grab-bag of 
# things required for the model below
jags.data <- list(npred = npred, # number of predictor variables (integer)
                  sub_id = sub_id, 
                  ninds = ninds, # total number of individuals in the dataset (integer)
                  chsets = as.integer(chsets),
                  nchsets = as.integer(nchsets),
                  alts = as.integer(alts),
                  nalts = as.integer(nalts),
                  T = as.integer(T),
                  y = as.matrix(ytest[,2:21]),
                  X1 = X1,
                  Z = Z)

####################################
params <- c("mu", "beta", "y[999,1:20]") # mu = covariate, beta = individual
# mu and beta are parameters that we want our model to summarize in the output
# mu is the mean effect of each habitat covariate across birds
# beta is the effect on each sub-brood/individual (part of the random effect structure)
# there will be a mu for each covariate
# there will be a beta for each covariate x sub-brood combination

cat(file = "m1.txt", " # concatenate the model for JAGS; calling the model 'm1.txt'
    
model{
  
  ## Prior distributions (naive priors)
  for(a in 1:ninds){                    # for each subbrood/individual...
    for(j in 1:npred){                  # for each variable...
      beta[a,j] ~ dnorm(mu[j], tau[j])  # mean = mu[j], sd = tau[j]...assign the betas a prior
      }
    } # the beta priors are 'randomly' sampled from a normal distribution
      # mu and tau are the hyperparameters
      # mu is kinda like the 'average' response... and tau is the variation
      # betas are sampled from the distribution from which betas are sampled
      # hyperparameters effectively constrain the parameters
      # mu constrains the betas -- beta is the coefficient that describes each bird's response to a hab covariate
      # in other words, each beta (for bird a, and variable j) is sampled from a normal dist w/ mean mu and SD tau
      
  ## Hyperparameters -- the 'main'/'average' responses behaviors in the system
  for(j in 1:npred){        # for each predictor variable (n=5)...   
    mu[j] ~ dnorm(0, 0.01)  # mu, the mean of all betas, un-informative prior distribution; 
    # average effect of all covariates across birds; 0.01 is standard for uninformative distribution
    sig[j] ~ dunif(0, 100) # we are leaving tau and sigma be determined by the model
    tau[j] <- 1/(sig[j] * sig[j]) # these describe the among-bird variation
    }

  ## Likelihood
  for(i in 1:nchsets){                          # for each of the sets of choices (n=999)....
    y[i, 1:nalts] ~ dmulti(p[i, 1:nalts], 1)
    # the 'y' is the 0/1 response
    # this model says: y data are described by ('~') this multiplicative distribution
    # or... something along these lines
    # Cam and DJ may need to digest this part a bit more to fully comprehend
    
    for(k in 1:nalts){ # for each of the 20 alternatives 
      log(phi[i, k]) <- inprod(beta[sub_id[i],],Z[,i,k]) # this is basically doing the algebra for 'Y = a1 + b1*vegcov + b2*vegcov2...' 
      p[i,k] <- phi[i,k] / sum(phi[i,1:nalts]) # 'the math is sound, but we don't know what it is' - Dr. Fiss, 2023
      # the probability of choosing one of the alternatives in a choice set?
      }
    }
}

")

# define initial values
# using normal distributions and log normal distribution for mu/sig, respectively
# initial values don't impact the model results but may impact the length of time
# it takes for the model to converge
inits <- function(){list(mu = rnorm(npred, 0, 1), sig = rlnorm(npred, 0, 1))}	

##Call WinBUGS start MCMC
mod1 <- autojags("m1.txt", 
                 data = jags.data,
                 inits = inits,
                 parameters.to.save = params,
                 max.iter = 5000, # max = 5,000 iterations; autojags has the feature of stopping when you tell it to
                 n.chains = 3, # the number of chains is virtually always 3
                 n.thin = 3, # people thin the iteration chains bc they're autocorrelated. 
                 # Thinning of 3 is typical however we may later thin more
                 iter.increment = 500, # it will check for convergence every 500 iterations 
                 n.burnin = 100, # the first 100 iterations are bullshit. Throw out the 
                 # first 100 in this case. Sometimes you need to go more...
                 n.adapt = 1000, # adaptive iterations? 1,000 is apparently fine
                 parallel = T) # run in parallel on your computer. 1 chain on each of your cores.

mod1

#JAGS output for model 'm1.txt', generated by jagsUI.
#Estimates based on 3 chains of 1100 iterations,
#adaptation = 1000 iterations (sufficient),
#burn-in = 850 iterations and thin rate = 3,
#yielding 249 total samples from the joint posterior. 
#MCMC ran in parallel for 5.396 minutes at time 2023-11-20 12:21:00.

#            mean    sd   2.5%    50%  97.5% overlap0     f  Rhat n.eff
#mu[1]       0.964 0.283  0.472  0.940  1.548    FALSE 1.000 1.018    90
#mu[2]       0.371 0.176  0.031  0.370  0.745    FALSE 0.984 1.022    78
#mu[3]       1.117 0.471  0.238  1.097  2.033    FALSE 1.000 1.021   107
#mu[4]      -0.135 0.230 -0.560 -0.132  0.278     TRUE 0.707 1.037    51
#mu[5]       0.169 0.116 -0.058  0.168  0.427     TRUE 0.940 1.034    56
#beta[1,1]   2.249 1.162  0.289  2.100  5.052    FALSE 0.996 1.050    41
#beta[2,1]   1.889 0.726  0.561  1.831  3.461    FALSE 1.000 1.010   249
#beta[3,1]   0.537 0.871 -1.154  0.506  2.350     TRUE 0.731 0.995   249
#beta[4,1]   1.151 0.571  0.093  1.075  2.399    FALSE 0.984 1.062    36

# all of this shit (parameters and such) are posterior distributions
# "mean" is average parameter estimate for each, recognizing that each is actually a distribution
# "SD" is the SD of that bell curve 
# # 2.5%, 50%, 97.5% are the credible intervals
# f is... unclear
# R-hat is a convergence diagnostic. You want it to be less than 1.1. If any parameter is above 1.1,
# the whole model is considered to have not converged
# n.eff is the number of effective samples. It's related to autocorrelation.
# mu[1] probably has a ton of autocorrelation. If there's a lot of this (e.g., n.eff = 8),
# you'll want to increase the thinning...

#################################
### make predictions
totsamp <- 498
sel.sample <- sample(1:totsamp, size = totsamp) # 1 to totsamp in random order
early.new <- seq(min(X1[,1]), max(X1[,1]), length.out = totsamp) # sequence of newdata; 1 to 100
early.predict <- array(dim = c(totsamp, totsamp)) # array to hold predicted values

for(i in 1:totsamp){     # for each unique value in "early.new"...
  for(j in 1:totsamp){   # for each "random", simulated, value in sel.sample
    early.predict[i, j] <- # cell i,j in the "early.predict" array becomes:
      1/(1 + exp(-mod1$sims.list$mu[sel.sample[j],1] * early.new[i])) # odds/1+odds (probability scale)
    }
}

# lower and upper confidence bounds/credible intervals
LCB.early <- apply(early.predict, 1, quantile, prob = 0.025)
UCB.early <- apply(early.predict, 1, quantile, prob = 0.975)

# mean prediction
mean.rel <- 1/(1+exp(-mod1$mean$mu[1]*early.new))#odds/1+odds (probability scale)

plot(x = seq(min(early), max(early), length.out = 498), y = mean.rel, 
     type = "l", ylim = c(0,max(UCB.early)),
     xlab = "Distance to Early Successional",
     ylab = "Pobability of Use")
lines(x = seq(min(early), max(early), length.out = 498), y = LCB.early, lty  = 2)
lines(x = seq(min(early), max(early), length.out = 498), y = UCB.early, lty  = 2)




###cam fucking around
bird.rel.early<-matrix(NA, nrow=ninds, ncol=totsamp)

for (i in 1:ninds){
  for (j in 1:totsamp)
bird.rel.early[i,j]<-1/(1+exp(-mod1$mean$beta[i,1]*early.new[j]))
}


library(RColorBrewer)
n <- ninds
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col = sample(col_vector, n))


for(i in 1:ninds){
  # i = 5
  plot(x = seq(min(early), max(early), length.out = 498), y = mean.rel, 
       type = "l", ylim = c(0,max(UCB.early)),
       xlab = "Distance to Early Successional",
       ylab = "Pobability of Use", main = paste0("bird number ", i))
  bird_i <- bird.rel.early[i,]
  lines(seq(min(early), max(early), length.out = 498), bird_i, col = "red")
}
#lines(seq(min(early), max(early), length.out = 498), mean.rel, col = "black", lwd = 3)

