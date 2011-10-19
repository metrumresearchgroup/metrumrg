`is.cwres.readable.file` <- function (filename) file.exists(filename)[1]

`read.cwres.data` <-
function (filename, old.file.convention = FALSE, est.tab.suffix = ".est", 
    deriv.tab.suffix = ".deriv", ...) 
{
    tables.read <- FALSE
    if (old.file.convention) {
        if ((is.cwres.readable.file(glue(filename, ".50"))) && 
            (is.cwres.readable.file(glue(filename, ".52"))) && 
            (is.cwres.readable.file(glue(filename, ".54"))) && 
            (is.cwres.readable.file(glue(filename, ".56"))) && 
            (is.cwres.readable.file(filename))) {
            nsim <- 1
            num.fields <- count.fields(filename, skip = 1)
            if ((length(unique(num.fields)) != 1) || (unique(num.fields) == 
                8) || (unique(num.fields) == 3)) {
                tmp <- readLines(filename, n = -1)
                inds <- grep("TABLE", tmp)
                if (length(inds) != 1) {
                  cat("Multiple simulations not supported\n")
                  cat("using this old file convention\n")
                  return(NULL)
                }
                else {
                  data <- read.table(filename, skip = 1, h = TRUE)
                }
            }
            else {
                data <- read.table(filename, skip = 1, h = TRUE)
            }
            size.of.sim <- dim(data)[1]/nsim
            data[, "iteration.number"] <- sort(rep(1:nsim, size.of.sim))
            eta <- vector("list", nsim)
            theta <- vector("list", nsim)
            omega <- vector("list", nsim)
            sigma <- vector("list", nsim)
            eta[[1]] <- read.table(glue(filename, ".50"))
            theta[[1]] <- read.table(glue(filename, ".52"))
            omega[[1]] <- read.table(glue(filename, ".54"))
            sigma[[1]] <- read.table(glue(filename, ".56"))
            tables.read <- TRUE
        }
    }
    else {
        est.file <- glue(filename, est.tab.suffix)
        deriv.file <- glue(filename, deriv.tab.suffix)
        if ((is.cwres.readable.file(est.file)) && (is.cwres.readable.file(deriv.file))) {
            nsim <- 1
            num.fields <- count.fields(deriv.file, skip = 1)
            if ((length(unique(num.fields)) != 1) || (unique(num.fields) == 
                8) || (unique(num.fields) == 3)) {
                tmp <- readLines(deriv.file, n = -1)
                inds <- grep("TABLE", tmp)
                if (length(inds) != 1) {
                  inds <- inds[c(2:length(inds))]
                  inds2 <- inds + 1
                  tempfile <- glue(deriv.file, ".xptmp")
                  write.table(tmp[-c(inds, inds2)], file = tempfile, 
                    row.names = FALSE, quote = FALSE)
                  data <- read.table(tempfile, skip = 2, h = TRUE)
                  unlink(tempfile)
                  nsim <- length(inds) + 1
                }
                else {
                  data <- read.table(deriv.file, skip = 1, h = TRUE)
                }
            }
            else {
                data <- read.table(deriv.file, skip = 1, h = TRUE)
            }
            size.of.sim <- dim(data)[1]/nsim
            data[, "iteration.number"] <- sort(rep(1:nsim, size.of.sim))
            filename.extra <- est.file
            data.extra <- scan(filename.extra, sep = "\n", what = character(), 
                quiet = TRUE)
            eta.pat <- "^ *ETAS"
            theta.pat <- "^ *THETAS"
            omega.pat <- "^ *OMEGAS"
            sigma.pat <- "^ *SIGMAS"
            eta.pat.line <- grep(eta.pat, data.extra)
            theta.pat.line <- grep(theta.pat, data.extra)
            omega.pat.line <- grep(omega.pat, data.extra)
            sigma.pat.line <- grep(sigma.pat, data.extra)
            pat.lines <- list(eta.pat.line, theta.pat.line, omega.pat.line, 
                sigma.pat.line)
            pattern.lengths <- sapply(pat.lines, length)
            if (!all(pattern.lengths == nsim)) {
                cat(paste("The", est.file, "and", deriv.file, 
                  "files do not match in size\n"))
                return(NULL)
            }
            eta <- vector("list", nsim)
            theta <- vector("list", nsim)
            omega <- vector("list", nsim)
            sigma <- vector("list", nsim)
            for (i in 1:nsim) {
                tot.eta.rows <- theta.pat.line[i] - eta.pat.line[i] - 
                  1
                tot.theta.rows <- omega.pat.line[i] - theta.pat.line[i] - 
                  1
                tot.omega.rows <- sigma.pat.line[i] - omega.pat.line[i] - 
                  1
                if (i == nsim) {
                  tot.sigma.rows <- length(data.extra) - sigma.pat.line[i]
                }
                else {
                  tot.sigma.rows <- eta.pat.line[i + 1] - sigma.pat.line[i] - 
                    1
                }
                eta[[i]] <- read.table(filename.extra, skip = eta.pat.line[i], 
                  nrows = tot.eta.rows)
                theta[[i]] <- read.table(filename.extra, skip = theta.pat.line[i], 
                  nrows = tot.theta.rows)
                omega[[i]] <- read.table(filename.extra, skip = omega.pat.line[i], 
                  nrows = tot.omega.rows)
                sigma[[i]] <- read.table(filename.extra, skip = sigma.pat.line[i], 
                  nrows = tot.sigma.rows)
            }
            tables.read <- TRUE
        }
    }
    if (tables.read) {
        #setClass("nm.data", representation(data = "data.frame", 
        #    eta = "data.frame", theta = "data.frame", omega = "data.frame", 
        #    sigma = "data.frame"))
        all.data <- vector("list", nsim)
        for (i in 1:nsim) {
            all.data[[i]] <- new("nm.data", data = data[data$iteration.number == 
                i, ], eta = eta[[i]], theta = theta[[i]], omega = omega[[i]], 
                sigma = sigma[[i]])
        }
        if (is.null(data$MDV)) {
            cat("Assuming all dataset lines contain a measurement\n")
            cat("No MDV item defined in dataset\n")
        }
    }
    else {
        all.data <- NULL
        cat("Some or all of the required CWRES tables are\n", 
            "missing. Please see the online help for details\n", 
            "of what is required (?compute.cwres).\n")
    }
    return(all.data)
}
