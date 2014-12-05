library(hsmm)

pipar <- rep(1/3, 3)

tpmpar <- matrix(c(0, 0.5, 0.5, 
                   0.7, 0, 0.3,
                   0.8, 0.2, 0), 3, byrow = TRUE)                 
rdpar <- list(p = c(0.98, 0.98, 0.99))
odpar <- list(mean = c(-1.5, 0, 1.5), var = c(0.5, 0.6, 0.8))
sim <- hsmm.sim(n = 100, od = "norm", rd = "log", 
                pi.par = pipar, tpm.par = tpmpar,
                rd.par = rdpar, od.par = odpar, seed = 3539)
# Executing the Viterbi algorithm:
fit.vi <- hsmm.viterbi(sim$obs, od = "norm", rd = "log",
                       pi.par = pipar, tpm.par = tpmpar,
                       od.par = odpar, rd.par = rdpar)

# The first 15 values of the resulting path:
fit.vi$path[1:15]
# For comparison, the real/simulated path (first 15 values):
sim$path[1:15]

plot()
# Executing the EM algorithm:
fit <- hsmm(sim$obs, od = "norm", rd = "log",     
            pi.par = pipar, tpm.par = tpmpar,
            od.par = odpar, rd.par = rdpar)

# The log-likelihood:
fit$logl

# Ehe estimated parameters:
fit$para
# For comparison, the estimated parameters seperately together with the true parameter values
# are given below.
# Transition probability matrix:
tpmpar
fit$para$tpm
# Observation distribution:
odpar
fit$para$od
# Runlength distribution:
rdpar
fit$para$rd 



sim$obs
sim$path


