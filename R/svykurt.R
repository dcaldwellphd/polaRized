#' @title svykurt
#'
#' @description Calculates Pearson kurtosis with complex survey data
#'
#' @param x A formula (e.g., ~var1) specifying the variable on which to estimate kurtosis.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param excess Logical. The default (TRUE) subtracts 3 from the output, giving excess kurtosis.
#'
#' @return A 1*1 matrix with the kurtosis on x
#'
#' @examples
#' #' if (requireNamespace("survey")) {
#'  library(survey)
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata, ids = ~1, weights = ~weight)
#' # Print the excess kurtosis of a variable
#' svykurt(x = ~att_val, design = toydesign, na.rm = TRUE)
#'
#' @export
#'
#' @importFrom survey svymean svyvar


svykurt <- function(
    x,
    design,
    na.rm = FALSE,
    excess = TRUE
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  if (inherits(x,"formula")){
    ## do the right thing with factors
    mf<-model.frame(x,design$variables,na.action=na.pass)
    xx<-lapply(attr(terms(x),"variables")[-1],
               function(tt) model.matrix(eval(bquote(~0+.(tt))),mf))
    cols<-sapply(xx,NCOL)
    x<-matrix(nrow=NROW(xx[[1]]),ncol=sum(cols))
    scols<-c(0,cumsum(cols))
    for(i in 1:length(xx)){
      x[,scols[i]+1:cols[i]]<-xx[[i]]
    }
    colnames(x)<-do.call("c",lapply(xx,colnames))
  }
  else {
    if(typeof(x) %in% c("expression","symbol"))
      x<-eval(x, design$variables)
    else if(is.data.frame(x) && any(sapply(x,is.factor))){
      xx<-lapply(x, function(xi) {if (is.factor(xi)) 0+(outer(xi,levels(xi),"==")) else xi})
      cols<-sapply(xx,NCOL)
      scols<-c(0,cumsum(cols))
      cn<-character(sum(cols))
      for(i in 1:length(xx))
        cn[scols[i]+1:cols[i]]<-paste(names(x)[i],levels(x[[i]]),sep="")
      x<-matrix(nrow=NROW(xx[[1]]),ncol=sum(cols))
      for(i in 1:length(xx)){
        x[,scols[i]+1:cols[i]]<-xx[[i]]
      }
      colnames(x)<-cn
    }
  }

  x<-as.matrix(x)

  if (na.rm){
    nas<-rowSums(is.na(x))
    design<-design[nas==0,]
    if (length(nas)>length(design$prob))
      x<-x[nas==0,,drop=FALSE]
    else
      x[nas>0,]<-0
  }

  pweights <- 1/design$prob
  psum <- sum(pweights)
  mean_x <- svymean(x, design, na.rm = na.rm)
  var_x <- svyvar(x, design, na.rm = na.rm)

  m4 <- sum(pweights * (x - mean_x)^4) / psum
  kurt <- m4 / var_x^2

  if (excess) {
    kurt <- kurt - 3
  }

  class(kurt) <- "svykurt"

  return(kurt)

}

print.svykurt <- function(x) {
  m <- as.matrix(x, ncol = 1)
  rownames(m) <- names(x)
  colnames(m) <- "kurtosis"

  print(m)
}


data(api)
dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
svykurt(x = ~api00, dclus1, na.rm = TRUE)

