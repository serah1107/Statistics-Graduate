#### MCMC by JAGS - 2016-5-11(Wed)

# example 1) : (One sample normal data)  yi~N(mu,tau), tau=1/sigma^2 , 1=1,...,n

##  Generate data (y[1],...,y[n])

set.seed(1337)
y <- rnorm(n = 20, mean = 10, sd = 5)
mean(y)
sd(y)

library(rjags)
library(runjags)

### The model specification

## Likelihood function : L(mu,sigma | Data)
  example1.bug <- "model{
  for(i in 1:length(y)) {
    y[i] ~ dnorm(mu, tau)
   }
## prior distribution for parameters (mu, sigma)
   mu ~ dnorm(0, 0.0001)
   sigma ~ dlnorm(0, 0.0625)
   tau <- 1 / pow(sigma, 2)
## parameter of interst g(mu,sigma)
   cv <- mu/sigma
 }"

#### Running the model in JAGS

model <- jags.model(textConnection(example1.bug), data = list(y = y),
                               n.chains = 3, n.adapt= 10000)


update(model, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples <- coda.samples(model, variable.names=c("mu", "sigma","cv"), 
                                        n.iter=20000, thin=10)
plot(mcmc_samples) 
summary(mcmc_samples)
autocorr.plot(mcmc_samples[,3])


# By using the coda.samples function to generate the samples rather than 
#the jags.samples function it is possible to use the plotting and 
#summary functions defined by the coda package to plot  
# the trace plots and marginal densities of the two parameters and
# parameter of interest.


