#' Simple General Transpose function
#' 
#' @param tableA A dataframe
#' @param id1 A list of variables (rows)
#' @param id2 A list of variables (columns of the transposed table)
#' @return A data frame
#' @examples 
#' N<-100;tableA<-data.frame(id1a=as.factor(sample(30,N, replace=TRUE)),
#' id1b=as.factor(sample(4,N, replace=TRUE)),
#' id2a=sample(letters[1:3],N,replace=TRUE),
#' id2b=sample(letters[1:4],N,replace=TRUE),
#' cont1=rchisq(N,1),
#' cont2=rchisq(N,1),
#' factor1=as.factor(sample(letters[1:3],N,replace=TRUE)),
#' factor2=as.factor(sample(letters[1:4],N,replace=TRUE)),
#' char1=sample(letters[1:3],N,replace=TRUE),
#' char2=sample(letters[1:4],N,replace=TRUE),stringsAsFactors=FALSE)
#' #Add missing
#' tableA[5:10]<-lapply(tableA[5:10], function(x){x[sample(N,10)]<-NA;x})
#' id1=c("id1a","id1b")
#' id2=c("id2a","id2b")
#' toto<-Generaltransposefunctionsimple(tableA,id1,id2)
Generaltransposefunctionsimple<-function(tableA,id1,id2){
  tableA$present<-1
  reshape2::dcast(reshape2::melt(tableA,
             id=c(id1,id2)),
        as.formula(paste0(id1,"~",paste(c(id2,"variable"),collapse="+"))),
        fill = 0,
        fun.aggregate=max,
        value.var="value")
}

#' General Transpose function
#' 
#' @param table A dataframe
#' @param id1 A list of variables (rows)
#' @param id2 A list of variables (columns of the transposed table)
#' @return A list: first element of the list is a dataframe, the transposed version of the orioginal table. Second element is a key to allow back transposition
#' @examples 
#' N<-100;tableA<-data.frame(id1a=as.factor(sample(30,N, replace=TRUE)),
#' id1b=as.factor(sample(4,N, replace=TRUE)),
#' id2a=sample(letters[1:3],N,replace=TRUE),
#' id2b=sample(letters[1:4],N,replace=TRUE),
#' cont1=rchisq(N,1),
#' cont2=rchisq(N,1),
#' factor1=as.factor(sample(letters[1:3],N,replace=TRUE)),
#' factor2=as.factor(sample(letters[1:4],N,replace=TRUE)),
#' char1=sample(letters[1:3],N,replace=TRUE),
#' char2=sample(letters[1:4],N,replace=TRUE),stringsAsFactors=FALSE)
#' #Add missing
#' tableA[5:10]<-lapply(tableA[5:10], function(x){x[sample(N,10)]<-NA;x})
#' id1=c("id1a","id1b")
#' id2=c("id2a","id2b")
#' toto<-Generaltransposefunction(tableA,id1,id2)

Generaltransposefunction<-function(tableA,id1,id2){
  presentindname<-paste0(deparse(substitute(tableA)),"present")
  initialnames<-names(tableA)
  names(tableA)<-gsub(pattern = "_",replacement = "",names(tableA))
  modifiedinitialnames<-names(tableA)
  id1<-gsub(pattern = "_",replacement = "",id1)
  id2<-gsub(pattern = "_",replacement = "",id2)
  initiallevels<-lapply(tableA[sapply(tableA,is.factor)],levels)
  
  tableA[id2]<-lapply(tableA[id2],as.factor)
  tableA[id2]<-lapply(tableA[id2],function(x){levels(x)<-stringr::str_replace_all(levels(x),"[^[:alnum:]]","");x})
  dependent<-setdiff(names(tableA),c(id1,id2))
  #convert all factors to character
  factors<-dependent[sapply(tableA[dependent],is.factor)]
  chars<-dependent[sapply(tableA[dependent],is.character)]
  num<-dependent[sapply(tableA[dependent],function(x){!is.factor(x)&!is.character(x)})]
  missnum<-as.data.frame(lapply(tableA[num],is.na))
  names(missnum)<-paste0("missingind",names(missnum))
  tableA<-cbind(tableA,missnum)
  tableA[factors]<-
    lapply(tableA[factors],
                          function(x){
                            as.character(levels(x)[x])
                          })
  tableA[c(factors,chars)]<-lapply(tableA[c(factors,chars)],
         function(x){
           x[is.na(x)]<-"missing"
           x
         })
  tableA$present<-1
  try(reshape2::dcast(reshape2::melt(tableA[c(id1,id2,num,names(missnum),"present")],
             id=c(id1,id2)),
        as.formula(paste0(paste(id1,collapse="+"),"~",paste(c(id2,"variable"),collapse="+"))),
        fill = 0,
        fun.aggregate=max,
        value.var="value"))->Tnum
  if(class(Tnum)=="try-error"){
    A<-reshape2::melt(tableA[c(id1,id2,num,names(missnum),"present")],
            id=c(id1,id2))
    aa<-unique(tableA[id1[1]])[[1]]
    a<-length(aa)
    Tnum<-   plyr::adply(0:10,1,function(i){
      reshape2::dcast(A[is.element(A[id1[1]][[1]],aa[(((a-1)%/%10)*i+1):min(a,(((a-1)%/%10)*(i+1)))]),],
                   as.formula(paste0(paste(id1,collapse="+"),"~",paste(c(id2,"variable"),collapse="+"))),
                   fill = 0,
                   fun.aggregate=max,
                   value.var="value")})
  }

    Tchar<-if(length(c(chars,factors))>0){
      reshape2::dcast(reshape2::melt(tableA[c(id1,id2,chars,factors)],
                      id=c(id1,id2)),
                 as.formula(paste0(paste(id1,collapse="+"),"~",paste(c(id2,"variable"),collapse="+"))),
                 fill = "",
                 fun.aggregate = function(x){x[1]},
                 value.var="value")}else{NULL}
 
  TtableA<-if(!is.null(Tchar)){merge(Tnum,Tchar,by=id1, all=TRUE)}else{Tnum}
  names(TtableA)<-gsub(pattern = "present",replacement=presentindname,names(TtableA))
  
   list(TtableA=TtableA,
      key=list(id1=id1,id2=id2, 
               num=num,
               chars=chars,
               factors=factors,
               initiallevels=initiallevels,
               variables=initialnames, 
               modifiedinitialnames=modifiedinitialnames,
               presentindname=presentindname
               ))  
  }

#' General Transpose function
#' 
#' @param table A dataframe
#' @param key A list of variables (columns of the transposed table)
#' @return A list: first element of the list is a dataframe, the transposed version of the orioginal table. Second element is a key to allow back transposition
#' @examples 
#' N<-100;tableA<-data.frame(id1a=as.factor(sample(30,N, replace=TRUE)),
#' id1b=as.factor(sample(4,N, replace=TRUE)),
#' id2a=sample(letters[1:3],N,replace=TRUE),
#' id2b=sample(letters[1:4],N,replace=TRUE),
#' cont1=rchisq(N,1),
#' cont2=rchisq(N,1),
#' factor1=as.factor(sample(letters[1:3],N,replace=TRUE)),
#' factor2=as.factor(sample(letters[1:4],N,replace=TRUE)),
#' char1=sample(letters[1:3],N,replace=TRUE),
#' char2=sample(letters[1:4],N,replace=TRUE),stringsAsFactors=FALSE)
#' #Add missing
#' tableA[5:10]<-lapply(tableA[5:10], function(x){x[sample(N,10)]<-NA;x})
#' id1=c("id1a","id1b")
#' id2=c("id2a","id2b")
#' toto<-Generaltransposefunction(tableA,id1,id2)
#' TtableA=toto$TtableA;key=toto$key
#' RtableA=GeneralReversetransposefunction(TtableA,key)
#' identical(tableA,RtableA)

GeneralReversetransposefunction<-function(TtableA,key){
  names(TtableA)<-gsub(pattern = key$presentindname,replacement="present",names(TtableA))
  Anum<-reshape2::melt(TtableA,id.vars=key$id1,measure.vars = setdiff(names(TtableA)[sapply(TtableA,is.numeric)],key$id1))
  varnum<-data.frame(do.call(rbind,strsplit(as.character(Anum$variable),"_")))
  names(varnum)<-c(key$id2,"variable")
  Anum<-cbind(Anum[-which("variable"==names(Anum))],varnum)
  Anum<-dcast(Anum,as.formula(paste0(paste(c(key$id1,key$id2),collapse="+"),"~variable")),
              value.var="value")
  Achar<-reshape2::melt(TtableA,id.vars=key$id1,measure.vars = setdiff(names(TtableA)[sapply(TtableA,is.character)],key$id1))
  varchar<-data.frame(do.call(rbind,strsplit(as.character(Achar$variable),"_")))
  names(varchar)<-c(key$id2,"variable")
  Achar<-cbind(Achar[-which("variable"==names(Achar))],varchar)
  Achar<-dcast(Achar,as.formula(paste0(paste(c(key$id1,key$id2),collapse="+"),"~variable")),
        value.var="value")
  A<-merge(Achar,Anum,by=c(key$id1,key$id2))
  if(is.element("present",names(A))){A<-A[A$present==1,]}
  numvar<-setdiff(names(Anum),c(key$id1,key$id2,"present",
                                grep("missingind",names(Anum),value = TRUE)))
  A[numvar]<-lapply(numvar,function(x){y<-A[[x]];
                   y[A[[paste0("missingind",x)]]==1]<-NA;
                   y})
  A<-A[-which(is.element(names(A),paste0("missingind",numvar)))]
  charvar<-setdiff(names(Achar),c(key$id1,key$id2))
  A[charvar]<-lapply(A[charvar],function(x){x[x=="missing"]<-NA;x})
  #reconvert character to factors
  A[key$factors]<-lapply(A[key$factors],as.factor)
  A[key$id2]<-lapply(key$id2,function(x){y=A[[x]];levels(y)<-key$initiallevels[[x]];y})
  
  A<-A[key$modifiedinitialnames]
  names(A)<-key$variables
  A
}
