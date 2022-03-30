pollutantmean <- function(directory, pollutant, id = 1:332) {
    files <- lapply(id, get_file_name)
    dfs   <- lapply(paste0(directory, files), read.csv) # Read into dataframes
    conc  <- lapply(dfs, function(x) { # index pollutant where not NA
                            x[!is.na(as.numeric(x[, pollutant])), pollutant]})
    mean(unlist(conc)) # Concatenate into vector and take mean
}

complete <- function(directory, id) {
    files <- lapply(id, get_file_name) # Get filenames
    dfs   <- lapply(paste0(directory, files), read.csv) # Read into df list
    comp  <- lapply(dfs, function(i) {i[!is.na(i[2]) & !is.na(i[3]), ]}) #!na
    df    <- data.frame() # make df
    for (i in dfs) { #bind dfs togther
        df <- rbind(df, i[!is.na(i[2]) & !is.na(i[3]), ])
    }
    comp  <- aggregate(df[, 4], by = list(df$ID), length)
    names(comp) <- c("id", "nobs")
    comp
}

corr <- function(directory, threshold = 0) {
    cases <- complete(directory, as.numeric(gsub(".csv", "", dir(directory))))
    files <- lapply(cases[cases$nobs > threshold, ][[1]], get_file_name) #names
    data  <- data.frame()
    for (f in files) { # read each file, append to data
        df   <- read.csv(paste0(directory, f))
        data <- rbind(data, df)
    }
    if (nrow(data) == 0) {
        return(c())
    } else {
        data  <- data[!is.na(data[2]) & !is.na(data[3]), ] #no NA vals
        spl   <- split(data, data$ID) #split into list by ID
        cr    <- unlist(lapply(spl, function(x) {cor(x$nitrate, x$sulfate)}))
        return(cr)
    }
}

get_file_name <- function(id) { # Pad with 0's, add .csv
    paste0(paste0(rep("0", 3 - nchar(id)), collapse = ""), id, ".csv")
}

