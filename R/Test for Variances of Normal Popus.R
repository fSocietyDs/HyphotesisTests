
#' Test for Variances of Normal Populations
#'
#' Hyphotesis test for a normal population concerning variances.
#'
#' Let X1, …,Xn be a simple random sample from a N(μ, σ2) population. Let s^2 be the sample variance:\cr
#'
#' H1:σ^2>σ0^2   H0:σ^2<=σ0^2 ---->  "R"\cr
#' H1:σ^2<σ0^2   H0:σ^2>=σ0^2 ---->  "L"\cr
#' H1:σ^2!=σ0^2  H0:σ^2=σ0^2  ---->  "C"\cr
#'
#' @param n sample size
#' @param s2x Variance for the sample
#' @param s20 Testing variance value
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @param alpha significance value
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export
#'
#### TEST FOR VARIANCES OF NORMAL POPULATIONS
TestVarianceOfNormPop<-function(n,s2x,s20,type,alpha){
  df<-n-1
  chi.sq<-(df*s2x)/s20
  # P-value
  if(type=="R"){
    # H0:p<=p0
    p<-pchisq(chi.sq,df,lower.tail = FALSE)
  }
  if(type=="L"){
    # H0:p>=p0
    p<-pchisq(chi.sq,df,lower.tail = TRUE)
  }
  if(type=="C"){
    # H0:p=p0
    if(chi.sq>0)
    {
      p<-(pchisq(chi.sq,df,lower.tail = TRUE))*2;
    }else{
      p<-(pchisq(chi.sq,df,lower.tail = FALSE))*2;
    }
  }
  if(p<=alpha){
    H0<-FALSE
  }else{H0<-TRUE}
  vres<-data.frame(H0,p,chi.sq,df)
  return(vres)
}

#' F-Test for equality of variance for NOrmal Pop.
#'
#' Hyphotesis test for a normal population concerning variances. Null distribution is F: sx^2/sy^2
#'
#'
#' H0:σx^2/σy^2<=1 ---->  "R"\cr
#' H0:σx^2/σy^2=>1 ---->  "L"\cr
#' H0:σx^2/σy^2!=1  ---->  "C"\cr
#'
#' @param x Vector Values from aNorm Dist
#' @param y Vector Values from aNorm Dist
#' @param type is character "R" right tail ,"L" left or "C" centered
#' @return Data frame with if H0 is TRUE or FALSE, pvalue and z-score
#' @export
#'
FTestForEqualityOfVariances<-function(x,y,type){
  s2x<-(sd(x))^2
  s2y<-(sd(y))^2
  if(type=="R"){ans<-var.test(x,y, alternative = "greater")}
  if(type=="L"){ans<-var.test(y,x, alternative = "greater")}
  if(type=="C"){
    if(s2x/s2y>1){ans<-2*var.test(x,y, alternative = "greater")
      }else{ans<-2*var.test(x,y, alternative = "greater")}
  }
  return(ans)
}
