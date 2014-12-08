#####################################################################
# Hidden Markov model for Failure Prediction
# Author: Bikash Agrawal
# Date: 2nd Dec 2014
# Description:..................
#
######################################################################
#dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbook
dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
setwd(dir)
source("HMM.r")
nSim          = 500
States        = c("Healthy","Failure")
Symbols       = c(1:7)
len = 7
transProbs    = matrix(c(.70,.30,.30,.70), c(length(States),length(States)), byrow=TRUE)

emissionProbs = matrix(c(rep(1/len,len),c(0.8,0.1,0.1,0.1,0.1,0.1,0.9)), c(length(States),length(Symbols)), byrow=TRUE)

hmm = initHMM(States, Symbols, transProbs=transProbs, emissionProbs=emissionProbs)

sim = simHMM(hmm,nSim)
vit = viterbi(hmm, sim$observation)
f   = forward(hmm, sim$observation)
b   = backward(hmm, sim$observation)
#vt = viterbiTraining(hmm,sim$observation)
bw = baumWelch(hmm,sim$observation,10)
hmm = initHMM(States, Symbols, transProbs=bw$hmm$transProbs, emissionProbs=bw$hmm$emissionProbs)
sim = simHMM(hmm,5)

print(bw$hmm)
# todo: probObservations is not generic!
f[1,nSim]->i
f[2,nSim]->j

probObservations = (i + log(1+exp(j-i)))
posterior = exp((f+b)-probObservations)
x = list(hmm=hmm,sim=sim,vit=vit,posterior=posterior)

readline("Plot simulated failure:\n")
mn1 = "Failure Prediction"
xlb = "Error sequence."
ylb = ""
plot(x$sim$observation,ylim=c(-7.5,6),pch=3,main="Failure Prediction",xlab=xlb,ylab=ylb,bty="n",yaxt="n")
axis(2,at=1:7)
readline("Simulated, which die was used:\n")
text(0,-1.2,adj=0,cex=.8,col="black","True: gray = healthy")
for(i in 1:nSim)
{
  if(x$sim$states[i] == "Healthy")
    rect(i,-1,i+1,0, col = "gray", border = NA)
  else
    rect(i,-1,i+1,0, col = "black", border = NA)   
}

readline("Most probable path (viterbi):\n")
text(0,-3.2,adj=0,cex=.8,col="black","Most probable path")
for(i in 1:nSim)
{ 
  if(x$vit[i] == "Healthy")
    rect(i,-3,i+1,-2, col = "gray", border = NA)
  else
    rect(i,-3,i+1,-2, col = "black", border = NA)  
}

readline("Differences:\n")
text(0,-5.2,adj=0,cex=.8,col="black","Difference")
differing = !(x$sim$states == x$vit)
for(i in 1:nSim)
{
  if(differing[i])
    rect(i,-5,i+1,-4, col = rgb(.3, .3, .3), border = NA)
  else
    rect(i,-5,i+1,-4, col = rgb(.9, .9, .9), border = NA)  
}

readline("Posterior-probability:\n")
points(x$posterior[2,]-3, type="l")
#points(x$posterior[2,]-5, type="l")

readline("Difference with classification by posterior-probability:\n")
text(0,-7.2,adj=0,cex=.8,col="black","Difference by posterior-probability")
differing = !(x$sim$states == x$vit)
for(i in 1:nSim)
{
  if(posterior[1,i]>0.5)
  {
    if(x$sim$states[i] == "Fair")
      rect(i,-7,i+1,-6, col = rgb(.9, .9, .9), border = NA)
    else
      rect(i,-7,i+1,-6, col = rgb(.3, .3, .3), border = NA)
  }else
  {
    if(x$sim$states[i] == "Unfair")
      rect(i,-7,i+1,-6, col = rgb(.9, .9, .9), border = NA)
    else
      rect(i,-7,i+1,-6, col = rgb(.3, .3, .3), border = NA)
  }
}

readline("Difference with classification by posterior-probability > .95:\n")
text(0,-7.2,adj=0,cex=.8,col="black","Difference by posterior-probability > .95")
differing = !(x$sim$states == x$vit)
for(i in 1:nSim)
{
  if(posterior[2,i]>0.95 || posterior[2,i]<0.05)
  {
    if(differing[i])
      rect(i,-7,i+1,-6, col = rgb(.3, .3, .3), border = NA)
    else
      rect(i,-7,i+1,-6, col = rgb(.9, .9, .9), border = NA)
  }
  else
  {
    rect(i,-7,i+1,-6, col = rgb(.9, .9, .9), border = NA)
  }
}



invisible(x)