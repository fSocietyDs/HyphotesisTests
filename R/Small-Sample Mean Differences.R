#' Small-Sample Test for the difference between two means
#'
#' Let X1,...,Xnx and Y1,...,Ynx be samples from normal populations with means μX and μY and standard deviations σX and σY, respectively.
#' Assume the samples are drawn "independently" of each other.
#'
#' H1:ux-uy>d0   H0:ux-uy<=d0 ---->  "R"\cr
#' H1:ux-uy<d0   H0:ux-uy>=d0 ---->  "L"\cr
#' H1:ux-uy!=d0  H0:ux-uy=d0  ---->  "C"\cr
#'
#' @param n x,y sample size of X and Y respectively
#' @param ux  mean of the sample X
#' @param uy  mean of the sample Y
#' @param sx sd fo the sample X
#' @param sy sd fo the sample Y
#' @param d0 Testing value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export


########################### SMALL-SAMPLE TEST FOR the DIFFERENCE between TWO MEANS #######################
SmallSampleMeanDifferences<-function(nx,ux,sx,ny,uy,sy,d0,type,alpha){
  # Calculate ns
  nsx<-sx^2/nx
  nsy<-sy^2/ny
  # Degrees of freedom v
  v<-(nsx+nsy)^2/((nsx^2/(nx-1))+(nsy^2/(ny-1)))
  # Rounded down to the nearest integer
  nv<-floor(v)
  # Get Zscore with ns.
  tscore<-(ux-uy-d0)/sqrt(nsx+nsy)
  #print(tscore)
  p<-NULL
  # P-value
  if(type=="R"){
    # H0:ux-uy<=d0 H1:ux-uy>d0
    c<-"<="
    p<-pt(q = tscore,df = nv,lower.tail = FALSE)
    #print(p)
  }
  if(type=="L"){
    # H0:ux-uy>=d0 H1:ux-uy<d0
    c<-">="
    p<-pt(q = tscore,df = nv,lower.tail = TRUE)
    #print(p)
  }
  if(type=="C"){
    # H0:ux-uy=d0 H1:ux-uy!=d0
    c<-"="
    if(tscore>0)
    {
      p<-pt(q = tscore,df = nv,lower.tail = FALSE)*2;
      #print(p)
    }else{
      p<-pt(q = tscore,df = nv,lower.tail = TRUE)*2;
      #print(p)
    }
  }
  if(p<=alpha){
    H0<-FALSE
  }else{H0<-TRUE}
  vres<-data.frame(H0,p,tscore,nv)
  return(vres)
}
