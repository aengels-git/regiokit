#' Calculate Moran's I
#'
#' @param geometry data with geometry column that includes spatial information
#' @param var vector for which Moran's I should be calculated, should be in the same order as geometry
#' @param queen detect neighbors based on shared points yes or no
#' @param summary only report the most import estimates
#'
#' @return
#' @export
#'
#' @examples
morans_i <- function(geometry,var,queen=T,summary=T){
  neighbors <-poly2nb(geometry,queen = TRUE)
  lw <- nb2listw(neighbors)
  result <-moran.test(var,lw)
  if(summary==T){
    result<- tibble("Morans I"= result$estimate[1],
           "Standard deviate "=result$statistic,
           "p"=result$p.value,
           "sig"=case_when(result$p.value<0.001~"***",
                           result$p.value<0.01~"**",
                           result$p.value<0.05~"*",
                           T~""))
  }
  return(result)
}



#' Scatter Plot that compares lagged means of neighbors with values in a region 
#'
#' @param geometry data with geometry column that includes spatial information
#' @param var vector for which Moran's I should be calculated, should be in the same order as geometry
#' @param queen detect neighbors based on shared points yes or no
#'
#' @return
#' @export
#'
#' @examples
morans_plot<-function(geometry,var,queen=T){
  neighbors <-poly2nb(geometry,queen = queen)
  lw <- nb2listw(neighbors)
  W<-map2(neighbors,lw$weights,function(x,w){
    row<-rep(0,length(neighbors))
    row[x]<-w[1] #has to be assigned to rows because of the way matrix multiplication works!
    row
  })%>%reduce(rbind)%>%as.matrix()
  
  lagged_means<-as.vector(W%*%var)
  
  gg<-data.frame(lagged_means,var)%>%
    ggplot(.,aes(x=var,y=lagged_means))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("Lagged means of neighboring regions")
  return(gg)
}