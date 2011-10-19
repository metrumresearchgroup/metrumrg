`Tmax` <-
function(data, id="ID", dv="DV", time="TIME") {
  ymax <- aggregate.data.frame(data[[dv]],by=list(data[[id]]),FUN=max)
  names(ymax) <- c(id,"DVmax")
  data <- merge(data,ymax)
  Tmax <- data[data[[dv]] == data$DVmax,]
  Tmax <- Tmax[!duplicated(Tmax[[id]]),c(id, time)]
  names(Tmax) <- c(id,"Tmax")
  return(Tmax)
}
`Tmin` <-
function(data, id="ID", dv="DV", time="TIME") {
  ymin <- aggregate.data.frame(data[[dv]],by=list(data[[id]]),FUN=min)
  names(ymin) <- c(id,"DVmin")
  data <- merge(data,ymin)
  Tmin <- data[data[[dv]] == data$DVmin,]
  Tmin <- Tmin[!duplicated(Tmin[[id]]),c(id,time)]
  names(Tmin) <- c(id,"Tmin")
  return(Tmin)
}
`AUC` <-
function(data, time="TIME", id="ID", dv="DV") {
  data <- data[order(data[[id]],-data[[time]]),]
  nrec <- length(data[[time]])
  data$diff <- c(data[[time]][-nrec] - data[[time]][-1],0)
  data$meanDV <- c((data[[dv]][-1] + data[[dv]][-nrec])/2,0)
  data$dAUC <- data$diff*data$meanDV
  data <- data[order(data[[id]],data[[time]]),]
  data <- data[duplicated(data[[id]]),]
  AUC <- aggregate.data.frame(data$dAUC,by=list(data[[id]]),FUN=sum)
  names(AUC) <- c(id,"AUC")
  return(AUC)
  }
