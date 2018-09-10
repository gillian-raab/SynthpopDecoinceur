#' export all tables from an installed R data package to sas7bdat data files in a given library
#' 
#' @param package package to be transformed
#' @param SAS_library_path path to export the data 
#' @return nothing
#' @examples 
#' exportRpackagedata_to_sas7bdatlibrary("datasets")

exportRpackagedata_to_sas7bdatlibrary<-function(package,path_to_export_to){
  data(package=package)->x
  sapply(as.data.frame(x$results,stringsAsFactors = F)$Item,function(y){
    eval(parse(text=paste0("data(",y,",package='",package,"')")))
    eval(parse(text=paste0("haven::write_sas(",y,",file.path(path_to_export_to,'",y,".sas7bdat'))")))
  })
}
