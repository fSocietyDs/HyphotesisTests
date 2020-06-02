#' Small Sample Test for a population Mean
#'
#' Hyphotesis test for a Small Sample Test for a population Mean n<30 that comes from a NORMAL dist. sigma is unknown
#'
#' Let X be the number of succeses in "n" independent Bernoulli trials with probability p: X~Bin(n,p)\cr
#'
#' H1:p>p0   H0:p<=p0 ---->  "R"\cr
#' H1:p<p0   H0:p>=p0 ---->  "L"\cr
#' H1:p!=p0  H0:p=p0  ---->  "C"\cr
#'
#' @param n sample size
#' @param ux  mean of the sample
#' @param sx sd fo the sample
#' @param u0 Testing value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export


########################### SMALL-SAMPLE TEST FOR A POPULATION MEAN ####################################
SmallSampleMeanTest<-function(n,ux,sx,u0,type,alpha){
  # Get Tscore with ns.
  tscore<-(ux-u0)/(sx/sqrt(n))
  df=n-1
  #print(tscore)
  p<-NULL
  # P-value
  if(type=="R"){
    # H0:u<=u0 & H1:u>u0
    c<-"<="
    p<-pt(q = tscore,df = df,lower.tail = FALSE)
    #print(p)
  }
  if(type=="L"){
    # H0:u>=u0 & H1:u<u0
    c<-">="
    p<-pt(q = tscore,df = df,lower.tail = TRUE)
    #print(p)
  }
  if(type=="C"){
    # H0:u=u0 & H1:u!=u0
    c<-"="
    if(tscore>0)
    {
      p<-pt(q = tscore,df = df,lower.tail = FALSE)*2;
      #print(p)
    }else{
      p<-pt(q = tscore,df = df,lower.tail = TRUE)*2;
      #print(p)
    }
  }
  if(p<=0.05){
    H0<-FALSE;H0
  }else{H0<-TRUE;H0}
  vres<-data.frame(H0,p,tscore,df)
  return(vres)
}
