#' Test for the Differences between two Proportions
#'
#' Hyphotesis test for the Differences between two Proportions Let X ∼ Bin(nX, pX) and let Y ∼ Bin(nY, pY).
#' Assume that there are at least 10 successes and 10 failures in each sample, and that X and Y are "INDEPENDENT".
#'
#' H1:px-py>0   H0:px-py<=0 ---->  "R"\cr
#' H1:px-py<0   H0:px-py>=0 ---->  "L"\cr
#' H1:px-py!=0  H0:px-py= 0  ---->  "C"\cr
#'
#' @param n x,y sample sizes
#' @param x Number of successes of X
#' @param y Number of successes of Y
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha Significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export

########################### TEST for DIFFERENCE between PROPORTIONS ###################################
DifferenceBetweenProportions<-function(nx,x,ny,y,type,alpha){
  #d0 == 0 always
  # Calculate px and py
  px<-x/nx;
  py<-y/ny;
  np<-(x+y)/(nx+ny);
  # Get Zscore with px and py.
  zscore<-(px-py)/sqrt(np*(1-np)*((1/nx)+(1/ny)));
  #print(zscore)
  p<-NULL
  # P-value
  if(type=="R"){
    # H0:px-py<=0 H1:px-py>0
    c<-"<="
    p<-1-pnorm(zscore)
    #print(p)
  }
  if(type=="L"){
    # H0:px-py>=p0 H1:px-py<0
    c<-">="
    p<-pnorm(zscore)
    #print(p)
  }
  if(type=="C"){
    # H0:px-py=p0 H1:px-py!=0
    c<-"="
    if(zscore>0)
    {
      p<-(1-pnorm(zscore))*2;
      #print(p)
    }else{
      p<-pnorm(zscore)*2;
      #print(p)
    }
  }
  if(p<=alpha){
    H0<-FALSE
  }else{H0<-TRUE}
  vres<-data.frame(H0,p,zscore)
  return(vres)
}
