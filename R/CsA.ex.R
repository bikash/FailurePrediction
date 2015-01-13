loglikelihood=function(par,type,x,start,m,T,freq=1,fct_dmat,fct_gamma,
                       fct_delta,ddl,dml,parameters,debug=FALSE)
{
# Arguments:
# par: vector of parameter values for log-likelihood evaluation
# type: vector of parameter names used to split par vector into types
# x: matrix of observed sequences (row:id; column:occasion/time)
# start: matrix with a row for each id and two columns
# 1) first observed state, 2) first occasion observed
# m: number of states
# T: number of occasions; sequence length
# freq: vector of history frequencies or 1
# fct_dmat: function to create D from parameters
# fct_gamma: function to create gamma - transition matrix
# fct_delta: function to create initial state probability distribution matrix
# ddl: design data list of parameters for each id
# model: formulas for each parameter type
# Other variables:
# parlist: list of parameter vectors split by type (eg Phi, p in CJS)


# gamma: array of transition matrices - one for each id, time
# dmat: array of observation probability matrices - one for each id, time
#
# Create list of parameter matrices from single input parameter vector
# First split parameter vector by prameter type (type)
parlist=split(par,type)
pars=list()
# For each parameter type call function reals to compute vector
# of real parameter values; then use laply and split to create
# a matrix of parameter values with a row for each id and column for
# each occasion.
for(parname in names(parameters))
{  
R=reals(ddl=ddl[[parname]],dml=dml[[parname]],
        parameters=parameters[[parname]],parlist=parlist[[parname]])
pars[[parname]]=laply(split(R,ddl[[parname]]$id),function(x) x)
}

# compute four dimensional arrays of id- and occasion-specific
# observation and transition matrices using parameter values
dmat=fct_dmat(pars,m,F=start[,2],T)
gamma=fct_gamma(pars,m,F=start[,2],T)
# compute matrix of initial state distribution for each id
delta=fct_delta(pars,m,F=start[,2],T,start)
# loop over each encounter history in sapply and
# create log-likelihood vector - an element for each x
# sum is total log-likelihood across individuals
# return negative log-likelihood
neglnl=-sum(freq*sapply(1:nrow(x),function(id)
  HMMLikelihood(x[id,],start[id,2],m,T,
                dmat=dmat[id,,,],gamma=gamma[id,,,],
                delta=delta[id,])))
return(neglnl)
}


reals=function(ddl,dml,parameters,parlist)
 { 
# Computes real estimates for HMM models using inverse of
# link from design matrix and for a particular parameter
# type (parname); handles fixed parameters assigned by
  
  # non-NA value in field named fix in the ddl dataframe.
  dm=dml$fe
  # Currently for log,logit or identity link, return the inverse values
  values=switch(parameters$link,
                log=exp(as.vector(dm%*%parlist)),
                logit=plogis(as.vector(dm%*%parlist)),
                identity=as.vector(dm%*%parlist))
  if(!is.null(ddl$time.interval))values=values^ddl$time.interval
  # if some reals are fixed, set reals to their fixed values
  if(!is.null(ddl$fix))
    values[!is.na(ddl$fix)]=ddl$fix[!is.na(ddl$fix)]
  # return vector of reals
  return(values)
}


cjs_dmat=function(pars,m,F,T)
 {
# add first occasion p=1
pmat=array(NA,c(nrow(pars$p),T,2,2))
for (i in 1:nrow(pmat))
  {

  pmat[i,F[i],,]=matrix(c(0,1,1,0),nrow=2,ncol=2,byrow=TRUE)
  for(j in F[i]:(T-1))
    {
  p=pars$p[i,j]
  pmat[i,j+1,,]=matrix(c(1-p,1,p,0),nrow=2,ncol=2,byrow=TRUE)
  }
}

  pmat
}
  cjs_gamma=function(pars,m,F,T)
    {
  # create four dimensional (4-d) array with a matrix for each id and occasion
  # from pars$Phi which is a matrix of id by occasion survival probabilities
  phimat=array(NA,c(nrow(pars$Phi),T-1,m,m))
  for (i in 1:nrow(phimat))
    for(j in F[i]:(T-1))
      {
  phi=pars$Phi[i,j]
  phimat[i,j,,]=matrix(c(phi,1-phi,0,1),nrow=2,ncol=2,byrow=TRUE)
    }
  phimat
}
  cjs_delta=function(pars,m,F,T,start)
   { 
  if(is.list(m))m=m$ns*m$na+1
  delta=matrix(0,nrow=nrow(start),ncol=m)
  delta[cbind(1:nrow(start),start[,1])]=1
  delta
  }
  
  

