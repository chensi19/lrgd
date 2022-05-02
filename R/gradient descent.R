#' Title
#'
#' @param Y
#' Response vector
#' @param X
#' Design matrix
#' @param beta
#' Regression coefficients
#' @param step
#' step size
#'
#' @return
#' @export
#'
#' @examples
#' X <- matrix(c(1,1,1,1,2,2,2,2),nrow=4)
#' Y <- c(1,1,1,1)
#' n = 2
#' step = c(0.001,0.0005,0.01,0.02,0.03,0.04,0.05,0.1)
#' par(mfrow=c(2,2))
#' for(i in 1:length(step)){
#'   grad_desc(Y,X,beta,step[i],2)
#' }

grad_desc<-function(Y,X,beta,step,n){
  beta<-rep(0,n)
  Qtrace<-c()
  Q<-function(Y,X,beta){
    t(Y-X%*%beta)%*%(Y-X%*%beta)
  }
  dQ<-function(Y,X,beta){
    -2*t(X)%*%(Y-X%*%beta)
  }
  for(i in 1:100){
    Qtrace<-c(Qtrace,Q(Y,X,beta))
    beta <- beta - step * dQ(Y,X,beta)
  }
  plot(c(1:100),Qtrace,xlab='iterations',ylab='loss function',col=2,pch=5)
}
