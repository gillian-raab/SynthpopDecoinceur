#'tablename="TRAIT_LKP"
#'automaticdataf(tablename)
#'
#'
#'   


missing.summary<-function(X,info2=NULL){
  A<-plyr::ldply(.data = X, function(x){c(percentage.missing=round(mean(is.na(x)),2),count.missing=sum(is.na(x)))})
  names(A)[1]<-"COLUMN_NAME"
  if(!is.null(info2)){A<-merge(A,info2[c("COLUMN_NAME","CONSTRAINT_TYPE")],all.x=TRUE)}
  A[order(A$count.missing),]
}

var.summary<-function(X,datadic=NULL){
  lapply(names(X),function(x){
    nl=length(unique(X[[x]]))
    is.code=identical(names(table(table(X[[x]]))),"1")
    list(nlevels=nl,
         is.code=is.code,
         levels=if(nl<300&!is.code){sort(unique(X[[x]]))}else{sort(unique(X[[x]])[1:300])},
         counts=if(nl<300&!is.code){table(X[x],useNA="ifany")}else{NULL},
         densityplot=if(is.numeric(X[[x]])&nl>30){ggplot(data.frame(x=X[[x]]),aes(x))+xlab(x)+ geom_density(show.legend = TRUE)}else{NULL},
         hist=if(nl<30){qplot(X[[x]],xlab=x)}else{NULL},
         summary=summary(X[x])
    )})}

var.summary<-function(X,datadic=NULL){
  sapply(names(X),function(x){
    nl=length(unlist(unique(X[x])))
    is.code=identical(names(table(table(X[[x]]))),"1")
    list(nlevels=nl,
         is.code=is.code,
         levels=if(nl<300){sort(unique(X[[x]]))}else{sort(unique(X[[x]])[1:30])},
         counts=if(nl<300&!is.code){table(X[x],useNA="ifany")}else{NULL},
         densityplot=if(is.numeric(X[[x]])&nl>30){ggplot(data.frame(x=X[[x]]),aes(x))+xlab(x)+ geom_density(show.legend = TRUE)}else{NULL},
         #densityplot=ggplot(X[x],aes(x= names(X[x])[1]))+geom_density(show.legend = TRUE),
         hist=if(nl<30){qplot(X[[x]])}else{NULL},
         summary=summary(X[x])
    )})}

#dicoT=StudyDataTools::dico()
#Connect <- ConnectSDPf(Daniel2::pwd())


automaticdataf<-function(tablename,
                         folder=getwd(),
                         schema=NULL, 
                         dicoT=NULL, 
                         splitvar=NULL,
                         Connect=NULL,
                         Connectf=NULL,
                         alwaysexclude=NULL){
  if(is.null(Connect)){Connect<-Connectf()}
  variables<-setdiff(sqlColumns(Connect,schema = schema,sqtable = tablename)$COLUMN_NAME,alwaysexclude)
  n<-sqlQuery(Connect,paste0("select count(*) from ",schema,".",tablename))[[1]]
  rate<-round(10000000/n,2)
  X<-sqlQuery(Connect,
              paste0("select ",paste(variables,collapse=",")," from ",schema,".",tablename,if(n>100000){paste0(" SAMPLE(",rate,",1)")}else{character(0)}))
  pk<-sqlQuery(Connect,paste0("SELECT cols.table_name, cols.column_name, cols.position, cons.status, cons.owner
                              FROM all_constraints cons, all_cons_columns cols
                              WHERE cols.table_name = '",tablename,"'
                              AND cons.constraint_type = 'P'
                              AND cons.constraint_name = cols.constraint_name
                              AND cons.owner = cols.owner
                              AND cols.OWNER='",schema,"'
                              ORDER BY cols.table_name, cols.position;"))$COLUMN_NAME
  close(Connect)
  
  
  #variabledetailsummary<-lapply(X,variabledetailsummaryf)
  
  
  automaticdata<-
    list(nrow=n,
         variables=variables,
         tab1_variables=dicoT[dicoT$TABLE==tablename,],
         pk=pk,
         splitvar=NULL,
         varsum=var.summary(X,datadic=dicoT[dicoT$TABLE==tablename,]),
         missingsum=missing.summary(X),
         missinggraph=ggplot_missing(X),
         missinggraph2=if(any(is.element(splitvar,variables))){
           ggplot_missing2(X,keep=setdiff(splitvar,variables))}else{NULL})
  
  automaticdatafile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda"))
  save(automaticdata,file=automaticdatafile)
  automaticdatafile
}

lefichier<-function(x){if(file.exists(x)){x}else{NULL}}

automaticRmd<-function(tablename,
                       folder=getwd(),
                       specialprogram   =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.R"))),
                       specialreport    =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.Rmd"))),
                       specialdatafile  =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.rda"))),
                       automaticdatafile=file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda")),
                       replace=FALSE,
                       rerunspecial=FALSE,
                       schema=NULL,
                       dicoT=NULL,
                       Connectf=NULL,
                       splitvar=NULL){
  
  automaticdatafile<-automaticdataf(tablename,folder,schema=schema,splitvar=splitvar,dicoT=dicoT,Connectf=Connectf)
  load(automaticdatafile)
  variables<-automaticdata$variables
  rm(automaticdata)
  if(replace||!file.exists(file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd")))){
    
    specialtexte<-if(!is.null(specialreport)){readLines(specialreport)}else{character(0)}
    texte<-paste0(
'---
title: "Study ',tablename,'"
author: "Daniel Bonnery"
output: html_document
---

```{r setup, include=FALSE, warnings=FALSE, error=FALSE,results="hide"}
knitr::opts_chunk$set(echo = TRUE)
```

```{r r1, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
library("StudyMLDS")
library(printr)
library(knitr)
load("',automaticdatafile,'")
try(load("',specialdatafile,'"))
```

#Summary: 

## Quick facts
The number of rows is `r automaticdata$nrow`.

The primary keys are `r paste(automaticdata$pk,collapse=", ")`.

The number of potentially interesting variables  is `r length(automaticdata$variables)`.

```{r listofvar, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
dicoT=StudyMLDS::dico()   
tab1<-dicoT[dicoT$TABLE=="',tablename,'",]
```

## Key findings:

## Questions:

Posted on the tracking spreadsheet: 

## Recommandations:

Keep:

Move:

Replace:

Drop:
',
paste(specialtexte,collapse="\n")

,'
## Percentage of missing


#Details

`r 
if(automaticdata$nrow>100000){"The following is based on a sample"}else{character(0)}`

## Missing values
``` {r missingsummary,echo=FALSE}
kable(automaticdata$missingsum)
```

``` {r missinggraph,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE}
print(automaticdata$missinggraph)
if(!is.null(automaticdata$missinggraph2)){try(print(automaticdata$missinggraph2))}
```

## Variables summary

'
  )  
    
    dicoTsource<-if(!is.null(dicoT)){dicoT$COLUMN_NAME[dicoT$TABLE==tablename]}else{setdiff(variables,alwaysexclude)}
    dicoTsource<-setdiff(dicoTsource,'')  
    texte<-paste0(texte,do.call(paste0,lapply(setdiff(variables,alwaysexclude), function(variable){
      paste0("### ",variable,"
Information: 
```{r, echo=FALSE}
try(kable(dicoT[dicoT$TABLE=='",tablename,"'&dicoT$COLUMN_NAME=='",variable,"',]))
```

Number of levels is `r automaticdata$varsum['nlevels','",variable,"']`

Table of frequencies:
```{r,echo=FALSE}
if(!is.null(unlist(automaticdata$varsum['counts','",variable,"']))){
kable(as.data.frame(automaticdata$varsum['counts','",variable,"'][[1]]))}
```

Density plot
```{r,echo=FALSE}
print(automaticdata$varsum['densityplot','",variable,"'])
```

Histogram
```{r,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE}
print(automaticdata$varsum['hist','",variable,"'])
```

")
      
    })))
    Rmdfile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd"))
    cat(texte,file=Rmdfile)
    rmarkdown::render(input = Rmdfile,
                      output_file = paste0("study_",schema,"_",tablename,"_automatic.html"),
                      output_dir = folder)
  }
  }
