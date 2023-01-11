find_regio <- function(){
  map(.libPaths(),function(current_lib){
    if(any(list.files(current_lib) %in% "regiokit")){
      return(paste0(current_lib,"/regiokit"))
    }
  })%>%reduce(c)
}