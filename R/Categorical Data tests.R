####### CATEGORICAL DATA #####

#' Goodness-Of-Fit Test
#'
#' Hyphotesis test for Multinomial distribution (k=number of outcomes). If H0 is TRUE the observation and Expected values
#' are very likely close to each other. Chi-square dist. Allways look to the right hand tail.
#'
#' EXPECTED values should be >5. Df=k-1.
#'
#' H0: p1=p01,p2=p02,p3=p03,..,pk=p0k
#'
#' @param x Vector with the observed values of the Categories
#' @return A list with K, expected values vector, chi-s, degrees of freedom, p value and if H0 is TRUE or FALSE
#' @export

GoodnessOfFitTest<-function(x,alpha){
  # Get K
  k<-length(x)
  # Degrees of freedom
  df<-k-1
  # Calculate total
  tot<-sum(x)
  # Create expected values vec..
  Evv<-rep(tot/length(x),length(x))
  # Get chi-squared value
  chi<-sum(((x-Evv)^2)/Evv)
  # Get pvalue
  p<-pchisq(chi,df,lower.tail = FALSE)
  if(p<=alpha){H0<-FALSE
  }else{H0<-TRUE}
  ans<-list("k"=k,"Ev"=Evv,"Chi-S"=chi,"Df"=df,"p"=p,"Accept H0?"=H0)
  return(ans)
}


# Import data frame as:
#             less1 X1to5 graterthan5
# Diseased      10     8          23
# Sensitized     9    19          11
# Normal        70   136         206

#' Chi-Square test for Homogeneity
#'
#' Hyphotesis test for multiple Multinomial distribution (k=number of outcomes). The null hypothesis is that
#' the probabilities of the outcomes are the same for each experiment. Chi-square dist. Allways look to the right hand tail.\cr
#' H0: for each column j, p1j=..=pIj
#'
#' Import data frame as:\cr
#'              less1 X1to5 graterthan5\cr
#' Diseased      10     8          23\cr
#' Sensitized     9    19          11\cr
#' Normal        70   136         206\cr
#'
#' EXPECTED values should be >5. Df=(I(rows)-1)(J(col)-1).
#'
#' @param data Contingency table dataframe!!!
#' @param alpha Significance level
#' @return A list with Marginals, expected values, chi-s, degrees of freedom, p value and if H0 is TRUE or FALSE
#' @export
ChiSquareTestHomogeneity<-function(data,alpha){
  # Transform dataframe as matrix
  x<-as.matrix(data[,c(-1)])
  rownames(x)<-data[,1]
  # Get marginals
  xm<-addmargins(x)
  # Calculate Proportions
  prop<-NULL
  for (i in 1:nrow(x)){
    cpp<-xm[i,ncol(xm)]/sum(x)
    prop<-cbind(prop,cpp)
  }
  # Calculate expected values
  ev<-NULL
  for (i in 1:nrow(x)){
    v<-xm[nrow(xm),i]*as.vector(prop)
    ev<-cbind(ev,v)
  }
  colnames(ev)<-colnames(x)
  rownames(ev)<-rownames(x)
  # Calculate Chi-square
  chi.sq<-sum((x-ev)^2/ev);
  # Calculate Degrees of freedom
  df<-(nrow(x)-1)*(ncol(x)-1)
  p<-1-pchisq(chi.sq,df)
  if(p<=alpha){H0<-FALSE
  }else{H0<-TRUE}

  ans<-list("Marginals"=xm,"Expected Values"=ev,"Chi-S"=chi.sq,"Df"=df,"p"=p,"Accept H0?"=H0)
  return(ans)
}
