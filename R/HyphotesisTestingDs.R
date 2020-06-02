#' Large sample test
#'
#' Hyphotesis test for large samples: n>30
#' @param n sample size
#' @param ux mean of the sample
#' @param sx standard deviation of the sample
#' @param u0 hyphotesis value
#' @param type is character "R","L"or"C"
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export

LargeTest<-function(n,ux,sx,u0,type,alpha){
  # Calculate ns
  ns<-sx/sqrt(n);
  # Get Zscore with ns.
  zscore<-(ux-u0)/ns;
  print(zscore)
  p<-NULL
  # P-value
  if(type=="R"){
    # H0:u<=u0
    c<-"<="
    p<-pnorm(zscore,lower.tail = FALSE)
    print(p)
  }
  if(type=="L"){
    # H0:u>=u0
    c<-">="
    p<-pnorm(zscore,lower.tail = TRUE)
    print(p)
  }
  if(type=="C"){
    # H0:u=u0
    c<-"="
    if(zscore>0)
    {
      p<-(pnorm(zscore,lower.tail = FALSE))*2;
    }else{
      p<-pnorm(zscore,lower.tail = TRUE)*2;
    }
  }
  if(p<=alpha){
    H0<-FALSE
  }else{H0<-TRUE}
  vres<-data.frame(H0,p,zscore)
  return(vres)
}

#' Proportion test
#'
#' Hyphotesis test for a population proportion n*p0 and n(1-p0)>10
#'
#' Let X be the number of succeses in "n" independent Bernoulli trials with probability p: X~Bin(n,p)\cr
#'
#' H1:p>p0   H0:p<=p0 "R"\cr
#' H1:p<p0   H0:p>=p0 "L"\cr
#' H1:p!=p0  H0:p=p0  "C"\cr
#'
#' @param n sample size
#' @param px  of the sample
#' @param p0 hyphotesis value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export

########################### POPULATION PROPORTION #########################################
### np0 & n(1-p0) >10!!!!!!!
ProportionTest<-function(n,px,p0,type,alpha){
  # Get Zscore with ns.
  zscore<-(px-p0)/sqrt((p0*(1-p0)/n));
  print(zscore)
  # P-value
  if(type=="R"){
    # H0:p<=p0
    c<-"<="
    p<-1-pnorm(zscore)
    print(p)
  }
  if(type=="L"){
    # H0:p>=p0
    c<-">="
    p<-pnorm(zscore)
    print(p)
  }
  if(type=="C"){
    # H0:p=p0
    c<-"="
    if(zscore>0)
    {
      p<-(1-pnorm(zscore))*2;
      print(p)
    }else{
      p<-pnorm(zscore)*2;
      print(p)
    }
  }
  if(p<=alpha){
    H0<-FALSE
  }else{H0<-TRUE}
  vres<-data.frame(H0,p,zscore)
  return(vres)
}


