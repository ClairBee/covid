
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GLOBAL DATA                                                                       ####

#' Load ECDC data (without refreshing download)
#'
#' @export
#'
ecdc <- function(dl.dt = NA) {

    # get data from ECDC (download string updated on 2020-04-13, data shifted to weekly on 2020-12-14)
    data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                     na.strings = "", fileEncoding = "UTF-8-BOM")

    colnames(data) <- tolower(colnames(data))
    data$daterep <- as.Date(data$daterep, format = "%d/%m/%Y")

    # add date of & days since first case
    data$day.0 <- stats::ave(data$daterep, data$geoid, FUN = min)
    data$ndays <- as.integer(difftime(data$daterep, data$day.0, units = "day"))

    data$deaths_weekly[is.na(data$deaths_weekly)] <- 0
    data$cases_weekly[is.na(data$cases_weekly)] <- 0

    # cumulative number of cases & deaths
    data <- data[order(data$daterep),]
    data$cCases <- stats::ave(data$cases_weekly, data$geoid, FUN = cumsum)
    data$cDeaths <- stats::ave(data$deaths_weekly, data$geoid, FUN = cumsum)

    # summarise data per country
    summ <- ddply(data, .(geoid), summarise,
                  "cnm" = min(gsub("_"," ",countriesandterritories)),
                  "continent" = min(continentexp),
                  "a3" = min(countryterritorycode),
                  "tcases" = sum(cases_weekly), "tdeaths" = sum(deaths_weekly), "pop" = mean(popdata2019))
    summ$cprop <- 100 * summ$tcases / summ$pop
    summ$dprop <- 100 * summ$tdeaths / summ$pop

    assign("data", data, envir = .GlobalEnv)
    assign("summ", summ, envir = .GlobalEnv)
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


#' Download data from Tom White's github repo
#'
#' @export
#'
tw.data <- function() {

    # download data from Tom White's repo
    phe <- read.csv(text=getURL("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"))

    phe$Date <- as.Date(phe$Date)
    suppressWarnings(phe$TotalCases <- as.integer(phe$TotalCases))

    phe$c100days <- round(difftime(phe$Date, as.POSIXct("2020-03-06", tz = "GMT"), units = "day"))
    phe$d10days <- round(difftime(phe$Date, as.POSIXct("2020-03-15", tz = "GMT"), units = "day"))

    phe <- phe[order(phe$Date), ]

    phe <- data.frame(data.table::rbindlist(sapply(unique(phe$AreaCode),
                                                   function(ccd) {
                                                       cdata <- phe[phe$AreaCode == ccd, ]
                                                       cdata$NewCases <- c(NA, diff(as.integer(gsub(" .", "", cdata$TotalCases))))
                                                       cdata
                                                   }, simplify = F)))
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
