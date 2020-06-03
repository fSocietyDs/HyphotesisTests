#' Test with paired data
#'
#' Hyphotesis test for Paired Data. CHECK if there is any relation between variables e,g. Vehicle, and Tires A,B.
#' The samples are not INDEPENDET.Let (X1, Y1), …, (Xn, Yn) be a sample of ordered pairs whose differences D1,...,Dn are a sample
#' from a normal population with mean μD. Let sD be the sample standard deviation of D1, …,Dn.
#'
#' The the statistic test is Student's t with n-1 degrees of freedom. If the sample is LARGE use z instead of t
#'
#' H1:uD>u0   H0:uD<=u0 ---->  "R"\cr
#' H1:uD<u0   H0:uD>=u0 ---->  "L"\cr
#' H1:uD!=u0  H0:uD=u0  ---->  "C"\cr
#'
#' @param n sample size
#' @param uD  mean of the DIFFERENCES between samples
#' @param sD sd fo the DIFFERENCES between samples
#' @param u0 Testing value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export

# TEST FOR PAIRED DATA
PairedDataTest<-function(n,uD,sD,u0,type,alpha){
  # Get Tscore with ns.
  tscore<-(uD-u0)/(sD/sqrt(n));
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
  if(p<=alpha){
    H0<-FALSE}else{H0<-TRUE}
  vres<-data.frame(H0,p,tscore,df)
  return(vres)
}
