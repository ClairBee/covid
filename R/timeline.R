

#' Add an event to the timeline (for adding to plots etc)
#'
#' Add key event to timeline .csv; these can be plotted against case & mortality trajectories for attribution
#'
#' @param str Short string describing event
#' @param date Date on which event occurred, yyyy-mm-dd; default is system date
#' @param geoid Country in which event is relevant; default is UK
#'
#' @export
#'
add.event <- function(str, date = format(Sys.Date(), "%Y-%m-%d"), geoid = "UK") {

    new <- data.frame("country" = geoid,
                      "event.date" = date,
                      "event" = str)
    prev <- read.csv("~/PhD/Misc-notes/Covid-19/data/Timeline.csv")
    events <- unique(rbind(prev, new))

    write.csv(events, "~/PhD/Misc-notes/Covid-19/data/Timeline.csv", row.names = F)
    assign("events", events, envir = .GlobalEnv)
}


#' Load timeline (for adding to plots etc)
#'
#' @param geoid Vector of country IDs to include. Default is NA, which includes all events
#'
#' @export
#'
events <- function(geoid = NA) {
    df <- read.csv("~/PhD/Misc-notes/Covid-19/data/Timeline.csv")

    if(!is.na(geoid[1])) {
        df <- df[df$country %in% geoid,]
    } else {
        df <- df
    }
    df[order(df$event.date, df$country),]
}




#' Load timeline with cumulative cases
#'
#' @param incl Vector of country codes to include
#'
#' @export
events.with.stats <- function(incl = NA) {

    ev <- events()
    if(is.na(incl)) incl <- unique(ev$country)
    ev <- ev[ev$country %in% incl,]

    if(format(max(data$daterep), "%Y-%m-%d") == format(Sys.Date(), "%Y-%m-%d")) {
        df <- data.frame(country = incl,
                         event.date = format(Sys.Date(), "%Y-%m-%d"),
                         event = "Today")
    } else {
        df <- data.frame(country = incl,
                         event.date = format(Sys.Date()-1, "%Y-%m-%d"),
                         event = "Yesterday")
    }

    ev <- merge(data, rbind(ev, df), all.y = T,
                by.x = c("daterep", "geoid"),
                by.y = c("event.date", "country"))

    ev <- ev[order(ev$geoid, ev$daterep, ev$event),]
    ev[,c("daterep", "geoid", "event", "ndays", "cCases", "cDeaths", "n100days", "d10days")]
}



#' Extract lockdown events with associated data
#'
#' @param ccd Vector of country codes to extract lockdowns for
#'
#' @export
#'
lockdowns <- function(ccd) {
    ld <- events(ccd)
    ld <- ld[grepl("ockdown", ld$event), ]
    if(nrow(ld) > 0) {
        ld$matchDate <- as.POSIXct(ld$event.date, tz = "UTC")
        ld <- merge(ld, data, all.x = T,
                    by.x = c("country", "matchDate"), by.y = c("geoid", "daterep"))
    }
    ld
}
