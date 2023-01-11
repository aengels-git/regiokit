GermanPlot <- R6Class("GermanPlot", list(
  bl_data = NULL,
  nuts2_data=NULL,
  kreis_data = NULL,
  bbox = NULL,
  path=NULL,
  #' create a German Map
  #'
  #' @return R6
  #' @export
  #'
  #' @examples
  initialize = function() {
    self$path<-glue("{find_regio()}/regio_data")
    
    readRDS2<-function(path){
      readRDS(path)%>%
        rename_all(tolower)
    }
    
    self$bl_data = readRDS2(glue("{ self$path }/Bundesländer.rds"))
    self$nuts2_data = readRDS2(glue("{ self$path }/Nuts_2.rds"))
    self$kreis_data = readRDS2(glue("{ self$path }/Kreise.rds"))%>%rename(kreis=kennziffer)
    self$bbox<-bbox(st_coordinates(self$kreis_data$geometry))
  },
  osm=function(){

    return(tm_shape(read_osm(self$bbox)) + tm_rgb())
  },
  join_features=function(data, ebene="kreis"){
    #ebene: kreis, nuts2 oder bundesland 
    #entsprechend sollte auch die Join ID benannt sein!
    if(ebene=="kreis"){
      self$kreis_data<-self$kreis_data%>%
        left_join(data)
    }else if(ebene=="nuts2"){
      self$nuts2_data<-self$nuts2_data%>%
        left_join(data)
    }else{
      self$bl_data<-self$bl_data%>%
        left_join(data)
    }
  },
  plot_feature=function(feature,alpha=0.8,ebene="kreis"){
    if(ebene=="kreis"){
      tm_shape(self$kreis_data)+
        tm_fill(feature,alpha=alpha)
    }else if(ebene=="nuts2"){
      tm_shape(self$nuts2_data)+
        tm_fill(feature,alpha=alpha)
    }else{
      tm_shape(self$bl_data)+
        tm_fill(feature,alpha=alpha)
    }
    
  }
))