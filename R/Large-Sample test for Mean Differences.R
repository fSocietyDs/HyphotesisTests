#' Large-Sample Test for mean differences
#'
#' Hyphotesis test for Large-Sample Test for mean differences which n is large (e.g., nX > 30 and nY > 30)
#'
#' ASSUME the samples are drawn INDEPENDENTLY of each other.
#'
#' H1:ux-uy>d0   H0:ux-uy<=d0 ---->  "R"\cr
#' H1:ux-uy<d0   H0:ux-uy>=d0 ---->  "L"\cr
#' H1:ux-uy!=d0  H0:ux-uy=d0  ---->  "C"\cr
#'
#' @param n x,y sample sizes
#' @param ux  mean of the sample x
#' @param uy  mean of the sample y
#' @param sx sd fo the sample X
#' @param sy sd fo the sample Y
#' @param d0 Testing value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export


########################### LARGE-SAMPLE TEST FOR MEAN DIFFERENCES ######################################

LargeSampleTestMeanDiff<-function(nx,ux,sx,ny,uy,sy,d0,type,alpha){
  # Calculate ns
  nsx<-sx^2/nx;
  nsy<-sy^2/ny;
  # Get Zscore with ns.
  zscore<-(ux-uy-d0)/sqrt(nsx+nsy);
  #print(zscore)
  p<-NULL
  # P-value
  if(type=="R"){
    # H0:ux-uy<=d0 H1:ux-uy>d0
    c<-"<="
    p<-1-pnorm(zscore)
    #print(p)
  }
  if(type=="L"){
    # H0:ux-uy>=d0 H1:ux-uy<d0
    c<-">="
    p<-pnorm(zscore)
    #print(p)
  }
  if(type=="C"){
    # H0:ux-uy=d0 H1:ux-uy!=d0
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
    H0<-FALSE;H0
  }else{H0<-TRUE;H0}
  vres<-data.frame(H0,p,zscore)
  return(vres)
}
