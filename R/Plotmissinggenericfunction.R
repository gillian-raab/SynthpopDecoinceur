#' Create missing chart
#' 
#' @param x a dataframe
#' @param reordonne a boolean
#' @return a ggplot graph
#' @examples 
#' library(reshape2)
#' library(ggplot2)
#' library(plyr)
#' library(magrittr)
#' X=cars
#' for(i in 1:40){
#'   X[sample(1:50,1,replace=TRUE),sample(1:2,1,replace=TRUE)]<-NA}
#' ggplot_missing(X,reordonne=TRUE)
#' ggplot_missing(X,reordonne=FALSE)createallautomaticRMD(schema="SDP")
 
ggplot_missing <- function(x,reordonne=FALSE){
  rownames(x)<-NULL
  as.data.frame((x %>% 
    is.na))->x
  if(reordonne){x<-x[order(as.matrix(x*1)%*%2^(ncol(x):1)),]}
  x$rownum<-1:nrow(x)
    ggplot(data = melt(x,id.vars="rownum"),
           aes(x = variable,
               y = rownum,
               fill = value)) +
    geom_tile() +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
if(FALSE){
}


#' Create missing chart
#' 
#' @param X a dataframe
#' @param reordonne a boolean
#' @param keep a boolean
#' @return a ggplot graph
#' @examples 
#' library(reshape2)
#' library(ggplot2)
#' library(plyr)
#' X=cars
#' X$year=sample(2012:2017,nrow(cars),replace=TRUE)
#' for(i in 1:40){
#'  X[sample(1:50,1,replace=TRUE),sample(1:2,1,replace=TRUE)]<-NA}
#' ggplot_missing2(X,keep="year")


ggplot_missing2 <- function(X,reordonne=TRUE,keep=NULL){

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  keep=intersect(keep,names(X))
  sele<-if(!identical(keep,integer(0))){-match(keep,names(X))}else{TRUE}
  
  rownames(X)<-NULL
  is.naf<-function(x){c("Present","Missing")[1+is.na(x)]}
  as.data.frame(lapply(X[sele],is.naf))->x
  sortcriteria=as.matrix((x=="Present")*1)%*%2^(ncol(x):1)
  if(reordonne){x<-cbind(X[keep],x)[if(length(keep)!=0){order(X[keep],  sortcriteria)}
                                    else{order(sortcriteria)},]}
  x$rownum<-1:nrow(x)
  if(length(keep)!=0){
  if(is.numeric(x[[keep]])){x[[keep]]<-as.character(x[[keep]])}}
  ggplot(data = melt(x,id.vars="rownum",measure.vars = names(x)[-match("rownum",names(x))] ),
         aes(x = variable,
             y = rownum)) +
    geom_tile(aes(fill = value)) +
    theme_minimal() + scale_fill_manual(values=c(gg_color_hue(length(unique(X[[keep]]))),"lightgray","darkgray"))+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}


