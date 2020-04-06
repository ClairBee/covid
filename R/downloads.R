
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GLOBAL DATA                                                                       ####

#' Load ECDC data (without refreshing download)
#'
#' @export
#'
ecdc <- function(dl.dt = NA) {

    fpath <- "~/PhD/Misc-notes/Covid-19/data"

    # if download date not specified, open most recent download
    if(is.na(dl.dt)) {
        fnm <- max(list.files(fpath, pattern = "ecdc.+xlsx", full.names = T))
    } else {
        fnm <- paste0(fpath, "/ecdc-",dl.dt,".xlsx")
    }

    data <- data.frame(read_excel(fnm))
    colnames(data) <- tolower(colnames(data))

    # number of days since first case
    data$day.0 <- stats::ave(data$daterep, data$geoid, FUN = min)
    data$ndays <- as.integer(difftime(data$daterep, data$day.0, units = "day"))

    # cumulative number of cases & deaths
    data <- data[order(data$daterep),]
    data$cCases <- stats::ave(data$cases, data$geoid, FUN = cumsum)
    data$cDeaths <- stats::ave(data$deaths, data$geoid, FUN = cumsum)

    # track since 100th case occurred
    date.c100 <- aggregate(daterep ~ geoid, data =  data[data$cCases > 100,], FUN = min)
    colnames(date.c100) <- c("geoid", "date.c100")
    data <- merge(data, date.c100, by = "geoid", all.x = T)
    data$n100days <- as.integer(difftime(data$daterep, data$date.c100, units = "day"))

    # track since 10th death occurred
    date.d10 <- aggregate(daterep ~ geoid, data =  data[data$cDeaths > 10,], FUN = min)
    colnames(date.d10) <- c("geoid", "date.d10")
    data <- merge(data, date.d10, by = "geoid", all.x = T)
    data$d10days <- as.integer(difftime(data$daterep, data$date.d10, units = "day"))

    data <- data[order(data$daterep),]

    # add weekly running total
    cases.lwd <- sapply(sort(unique(data$daterep)[-(1:7)]), function(dt) {
        lw <- data[difftime(dt, data$daterep, units = "days") %in% (0:6),]
        df <- setNames(merge(aggregate(cases ~ geoid, data = lw, FUN = "sum"),
                             aggregate(deaths ~ geoid, data = lw, FUN = "sum"), by = "geoid"),
                       c("geoid", "lw.cases", "lw.deaths"))
        df$daterep <- dt
        df
    }, simplify = F)

    cases.lwd <- data.frame(data.table::rbindlist(cases.lwd))
    data <- merge(data, cases.lwd, by = c("geoid", "daterep"), all.x = T)

    assign("data", data, envir = .GlobalEnv)
}

#' Download latest ECDC data
#'
#' Download latest Covid-19 infection data from ECDC website and prep for plotting etc
#'
#' @export
#'
get.ecdc <- function(dl.dt = format(Sys.Date(), "%Y-%m-%d")) {

    library(readxl); library(httr)

    # create the URL where the dataset is stored with automatic updates every day
    last.dl <- gsub("\\.xlsx","",
                    gsub("ecdc-","",
                         max(list.files("~/PhD/Misc-notes/Covid-19/data",
                                        pattern = "ecdc"))))
    #download the dataset from the website to a local temporary file
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", dl.dt, ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type = "ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

    # if file hasn't been updated today, get yesterday's data instead
    if(file.size(tf) < 10^5) {
        cat("No new ECDC data. Last download was",last.dl,"\n")
    } else {
        invisible(file.copy(from = tf,
                            to = paste0("~/PhD/Misc-notes/Covid-19/data/ecdc-", dl.dt, ".xlsx"),
                            overwrite = T))
        cat("Downloaded ECDC data for", dl.dt, "\n")
    }
    # read latest data
    ecdc()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UK DATA                                                                           ####

#' Load PHE data (without refreshing download)
#'
#' @export
#'
phe <- function() {

    # read in all daily files at UK county level
    fl <- list.files("~/PhD/Misc-notes/Covid-19/data", pattern = "UK-counties", full.names = T)
    dl <- lapply(fl, function(fnm) {
        dt <- as.POSIXct(gsub("\\.csv","",gsub("^.+UK-counties-","",fnm)), tz = "GMT")
        data.frame(read.csv(fnm), dl.dt = dt)
    })
    phe <- data.frame(data.table::rbindlist(dl))

    # days since 100th case & 10th death
    phe$c100days <- round(difftime(phe$dl.dt, as.POSIXct("2020-03-06", tz = "GMT"), units = "day"))
    phe$d10days <- round(difftime(phe$dl.dt, as.POSIXct("2020-03-15", tz = "GMT"), units = "day"))

    # add regional groupings
    phe <- merge(phe, utla(), by = "GSS_CD", all.x = T)
    phe <- phe[order(phe$dl.dt),]

    # clear out the commas and whitespace that someone inexplicably included
    phe$TotalCases <- as.integer(gsub("\\s.","",gsub(",","",phe$TotalCases)))

    # calculate daily cases
    phe <- data.frame(data.table::rbindlist(sapply(unique(phe$GSS_CD), function(ccd) {
        cdata <- phe[phe$GSS_CD == ccd,]
        cdata <- cdata[order(cdata$dl.dt),]
        cdata$NewCases <- c(NA, diff(as.integer(gsub(" .","",cdata$TotalCases))))
        cdata
    }, simplify = F)))

    # export
    assign("uk.data", phe, envir = .GlobalEnv)
}




#' Download latest PHE data at county level
#'
#' Download latest PHE data at county level
#'
#' @export
#'
get.phe <- function() {

    url <- "https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data"
    dl.dt <- format(Sys.time(), "%Y-%m-%d")

    # check against extant files
    fl <- list.files("~/PhD/Misc-notes/Covid-19/data", pattern = "UK-counties", full.names = T)

    GET(url, write_disk(tf <- tempfile(fileext = ".csv")))

    new <- read.csv(tf)
    prev <-read.csv(fl[length(fl)])

    if(!chk(new, prev)) {
        invisible(file.copy(from = tf,
                            to = paste0("~/PhD/Misc-notes/Covid-19/data/UK-counties-",
                                        dl.dt, ".csv")))
        cat("New download from PHE:", dl.dt, "\n")
    } else {
        cat("No new data available from PHE. Last download was on",
            gsub("\\.csv","",gsub("^.+UK-counties-","",fl[length(fl)])), "\n")
    }

    phe()
}
