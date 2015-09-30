`sqrtm` <-
function (x) 
{
    xe <- eigen(x)
    xe1 <- xe$values
    if (all(xe1 >= 0)) {
        xev1 <- diag(sqrt(xe1), nrow = length(xe1))
    }
    else {
        i = 1
        while (i < (length(xe1) + 1)) {
            if (xe1[i] < 0) {
                xe1[i] = 0
            }
            i = i + 1
        }
        xev1 <- diag(sqrt(xe1), nrow = length(xe1))
    }
    xval1 <- cbind(xe$vectors)
    xval1i <- solve(xval1)
    y <- xval1 %*% xev1 %*% xval1i
}
`ind.cwres` <-
function (ind.data, H.names, G.names, OMEGA, SIGMA, IND.ETAS, 
    ...) 
{
    if (is.null(ind.data$MDV)) {
        ind.data1 <- ind.data
    }
    else {
        ind.data1 <- ind.data[ind.data$MDV == 0, ]
    }
    if (nrow(ind.data1) != 0) {
        H.EPS = as.matrix(subset(ind.data1, select = H.names))
        G.ETA = as.matrix(subset(ind.data1, select = G.names))
        TMP <- diag(H.EPS %*% SIGMA %*% t(H.EPS))
        IND.COV = diag(TMP, nrow = length(TMP)) + G.ETA %*% OMEGA %*% 
            t(G.ETA)
        EXP.F <- as.matrix(ind.data1$IPRE) - G.ETA %*% IND.ETAS
        FOCE.RES <- as.matrix(ind.data1$DV) - EXP.F
        SQRT.IND.COV <- sqrtm(IND.COV)
        IND.CWRES <- solve(SQRT.IND.COV, FOCE.RES)
        if (is.null(ind.data$MDV)) {
        }
        else {
            CWRES <- rep(0, length(ind.data[, 1]))
            ind.data2 <- cbind(ind.data, CWRES)
            ind.data2[ind.data2$MDV == 0, "CWRES"] <- IND.CWRES
            IND.CWRES <- as.matrix(ind.data2["CWRES"])
        }
    }
    else {
        CWRES <- rep(0, length(ind.data[, 1]))
        ind.data2 <- cbind(ind.data, CWRES)
        IND.CWRES <- as.matrix(ind.data2["CWRES"])
    }
    return(IND.CWRES)
}
`compute.cwres` <-
function (run.number=1, tab.prefix = "cwtab", sim.suffix = "", 
    est.tab.suffix = ".est", deriv.tab.suffix = ".deriv", old.file.convention = FALSE, 
    id = "ALL", printToOutfile = TRUE, onlyNonZero = TRUE, ...) 
{
    out.file = glue(tab.prefix, run.number, sim.suffix)
    full.dataset <- read.cwres.data(out.file, old.file.convention = old.file.convention, 
        est.tab.suffix = est.tab.suffix, deriv.tab.suffix = deriv.tab.suffix, 
        ...)
    if (is.null(full.dataset)) {
        return()
    }
    num.reps <- length(full.dataset)
    tot.cwres <- c()
    for (rep in 1:num.reps) {
        dataset <- full.dataset[[rep]]
        first.only.data <- dataset@data[!duplicated(dataset@data$ID), 
            ]
        all.etas <- dataset@eta
        all.etas <- cbind(first.only.data["ID"], all.etas)
        OMEGA <- as.matrix(dataset@omega)
        SIGMA <- as.matrix(dataset@sigma)
        H.names = c()
        i = 1
        while (i < (length(dataset@sigma) + 1)) {
            H.names = c(H.names, glue("H", i, "1"))
            i = i + 1
        }
        G.names = c()
        i = 1
        while (i < (length(dataset@omega) + 1)) {
            G.names = c(G.names, glue("G", i, "1"))
            i = i + 1
        }
        if (id == "ALL") {
            id.vals <- unique(dataset@data$ID)
            CWRES <- c()
            for (i in id.vals) {
                #ind.data <- subset(dataset@data, ID == i)#gives irritating warning in R CMD check
                ind.data <- dataset@data[dataset@data$ID == i,]
                ind.etas <- t(as.matrix(all.etas[all.etas$ID == 
                  i, colnames(all.etas) != "ID"]))
                CWRESI <- ind.cwres(ind.data, H.names, G.names, 
                  OMEGA, SIGMA, ind.etas, ...)
                CWRES <- c(CWRES, CWRESI)
            }
            CWRES <- as.matrix(CWRES)
            if (printToOutfile == TRUE) {
                if (old.file.convention) {
                  filename <- glue(out.file, ".cwres")
                }
                else {
                  filename <- out.file
                }
                data.cwres <- data.frame("ID"=dataset@data$ID)
          if(!is.null(dataset@data$MDV)) data.cwres$MDV=dataset@data$MDV
          if(!is.null(dataset@data$DV)) data.cwres$DV=dataset@data$DV
          if(!is.null(dataset@data$IPRE)) data.cwres$IPRE=dataset@data$IPRE
          if(!is.null(dataset@data$WRES)) data.cwres$WRES=dataset@data$WRES
          if(!is.null(CWRES)) data.cwres$CWRES=CWRES
          #tmp <- installed.packages(priority = "NA")
          #      if (length(grep("xpose4", tmp)) > 0) {
          #        xpose.version <- tmp["xpose4", "Version"]
          #        xpose.text <- glue("from Xpose version", xpose.version) 
          #          
          #      }
          #      else {
                  xpose.text <- "from Xpose 4.0-6.1"
          #      }
                if (rep == 1) {
                  append.table.message <- FALSE
                }
                else {
                  append.table.message <- TRUE
                }
                cat(paste("TABLE for CWRES computed using compute.cwres.R", 
                  xpose.text, "on", format(Sys.time(), "%a %b %d, %X, %Y"), 
                  "\n"), file = filename, append = append.table.message)
                newdata <- format(data.cwres, sci = TRUE)
                suppressWarnings(write.table(newdata, filename, 
                  row.names = FALSE, sep = " ", quote = FALSE, 
                  append = TRUE))
            }
            if (onlyNonZero == TRUE) {
                if (is.null(dataset@data$MDV)) {
                }
                else {
                  data.cwres <- cbind(dataset@data, CWRES)
                  tmp <- data.cwres[data.cwres$MDV == 0,]
                  CWRES <- tmp$CWRES
                }
            }
        }
        else {
            data1 <- dataset@data[dataset@data$ID == id, ]
            ind.etas <- t(as.matrix(all.etas[all.etas$ID == id, 
                colnames(all.etas) != "ID"]))
            CWRES <- ind.cwres(data1, H.names, G.names, OMEGA, 
                SIGMA, ind.etas, ...)
            if (onlyNonZero == TRUE) {
                if (is.null(data1$MDV)) {
                }
                else {
                  data1.cwres <- cbind(data1, CWRES)
                  tmp <- data1.cwres[data1.cwres$MDV == 0,]
                  CWRES <- tmp$CWRES
                }
            }
        }
        tot.cwres <- c(tot.cwres, CWRES)
    }
    return(tot.cwres)
}

