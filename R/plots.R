
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# SUPPORT FUNCTIONS                                                                 ####

#' Add line to plot showing trajectory of a given doubling rate
#'
#' @param nd Number of days for cases to double
#'
#' @export
#'
doubling <- function(nd = 3, label.at = 25, baseline = 100) {
    lines(nd*(0:20), 2^(0:20) * baseline, lty = "22", col = transp("dimgrey"))
    if(!is.na(label.at)) {
        text(label.at, 2^(label.at/nd) * baseline, cex = 0.6, col = "dimgrey", pos = 1,
             paste("Cases double \n every",nd,"days"))
    }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOTS - GLOBAL                                                                    ####

#' Replicate Chartr plot showing number of cases vs time since hundredth case
#'
#' @param incl Vector of country codes to include. Default is c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US").
#' @param ccols Vector of colours to use for each country. Default is black for first country and rainbow palette for the remainder.
#' @param doublings Add trajectories for cases doubling every n days.
#' @param show.China Boolean: show full China trajectory or truncate to European days since 100th confirmed case? Default is F (truncated).
#' @param log Boolean: plot on log scale? Default is T (show on log10 scale)
#' @param add.lockdowns Boolean: add points showing when lockdowns were introduced? Default is T.
#'
#' @export
#'
cases.since.c100 <- function(incl = c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US"),
                             ccols = c("black", rbow(length(incl)-1)),
                             doublings = 3, show.China = F, log = T, add.lockdowns = T,
                             main = "Selected case trajectories") {

    ecdc()

    if(show.China) {
        xrng <- c(0, max(data$n100days[data$geoid %in% incl]))
    } else {
        xrng <- c(0, max(data$n100days[data$geoid %in% incl[!incl == "CN"]]))
    }

    if(log) {
        plot(1, type = "n", log = "y", yaxt = "n", xlim = xrng,
             ylim = c(100, max(data$cCases[data$geoid %in% incl], na.rm = T)),
             xlab = "Days since 100th confirmed case",
             ylab = "Cumulative confirmed cases (log10)",
             main = main)

        axis(2, at = 10^(0:6), labels = format(10^(0:6), scientific = F))
        invisible(sapply(doublings, function(n) doubling(n)))
        legend.pos <- "bottomright"

    } else {
        plot(1, type = "n", xlim = xrng,
             ylim = c(0, max(data$cCases[data$geoid %in% incl], na.rm = T)),
             xlab = "Days since 100th confirmed case",
             ylab = "Cumulative confirmed cases",
             main = main)
        legend.pos <- "left"

    }

    title(main = paste("   Downloaded on", format(max(data$daterep), "%Y-%m-%d")),
          cex.main = 0.6, line = -1, adj = 0)
    invisible(sapply(1:length(incl), function(i) {
        id <- incl[i]
        lines(data$n100days[data$geoid == id], data$cCases[data$geoid == id], col = ccols[i],
              type = "o", pch = 20, cex = 0.4)
        points(max(data$n100days[data$geoid == id]), max(data$cCases[data$geoid == id]),
               col = ccols[i])
    }))
    if(add.lockdowns) {
        ld <- events()
        ld <- ld[grepl("ockdown", ld$event),]
        ld$matchDate <- as.POSIXct(ld$event.date, tz = "UTC")

        ld <- merge(ld, data, all.x = T,
                    by.x = c("country", "matchDate"),
                    by.y = c("geoid", "daterep"))

        invisible(sapply(1:length(incl), function(i) {
            id <- incl[i]
            ld.i <- ld[ld$country == id,]
            points(ld.i$n100days, ld.i$cCases, pch = 5, col =  ccols[i], cex = 0.8)
        }))
    }
    legend(legend.pos, legend = paste0(incl, "    "), col = ccols,
           lty = 1, pch = 1, bty = "n", nc = 2, cex = 0.8)
}


#' Replicate Chartr plot showing number of deaths vs time since hundredth case
#'
#' @param incl Vector of country codes to include. Default is c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US").
#' @param ccols Vector of colours to use for each country. Default is black for first country and rainbow palette for the remainder.
#' @param doublings Add trajectories for cases doubling every n days.
#' @param show.China Boolean: show full China trajectory or truncate to European days since 100th confirmed case? Default is F (truncated).
#' @param log Boolean: plot on log scale? Default is F (show on linear scale)
#'
#' @export
#'
deaths.since.d10 <- function(incl = c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US"),
                              ccols = c("black", rbow(length(incl)-1)),
                              doublings = NA, show.China = F, log = F,
                             main = "Selected mortality trajectories") {

    ecdc()
    if(show.China) {
        xrng <- c(0, max(data$d10days[data$geoid %in% incl]))
    } else {
        xrng <- c(0, max(data$d10days[data$geoid %in% incl[!incl == "CN"]]))
    }

    if(log) {
        plot(1, type = "n", log = "y", yaxt = "n", xlim = xrng,
             ylim = c(10, max(data$cDeaths[data$geoid %in% incl], na.rm = T)),
             xlab = "Days since 10th death",
             ylab = "Cumulative deaths attributed to Covid-19",
             main = main)
        axis(2, at = 10^(0:6), labels = format(10^(0:6), scientific = F))
        invisible(sapply(doublings, function(n) doubling(n, baseline = 10)))
        legend.pos <- "bottomright"

    } else {
        plot(1, type = "n", xlim = xrng,
             ylim = c(0, max(data$cDeaths[data$geoid %in% incl], na.rm = T)),
             xlab = "Days since 100th confirmed case",
             ylab = "Cumulative deaths attributed to Covid-19",
             main = main)
        legend.pos <- "left"

    }
    title(main = paste("   Downloaded on", format(max(data$daterep), "%Y-%m-%d")),
          cex.main = 0.6, line = -1, adj = 0)
    invisible(sapply(1:length(incl), function(i) {
        id <- incl[i]
        lines(data$d10days[data$geoid == id], data$cDeaths[data$geoid == id], col = ccols[i],
              type = "o", pch = 20, cex = 0.4)
        points(max(data$d10days[data$geoid == id]), max(data$cDeaths[data$geoid == id]),
               col = ccols[i])
    }))
    legend(legend.pos, legend = paste0(incl, "    "), col = ccols,
           lty = 1, pch = 1, bty = "n", nc = 2, cex = 0.8)
}



#' Plot daily cases
#'
#' @param incl Vector of country codes to include. Default is c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US").
#' @param ccols Vector of colours to use for each country. Default is black for first country and rainbow palette for the remainder.
#' @param show.China Boolean: show full China trajectory or truncate to European days since 100th confirmed case? Default is F (truncated).
#' @param smooth String: apply a spline smoother? Options are "o" (overlay points on spline)", "s" (smooth), or any other character (data only). Default is "o".
#' @param offset.dates Character: show days since 100th case ("c"), days since 10th death ("d"), or date? Default is date.
#' @param ymx Maximum value to show on y-axis
#' @param leg.ncols Number of columns in legend. Default is 2.
#'
#' @export
#'
daily.cases <- function(incl = c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US"),
                        ccols = c("black", rbow(length(incl) - 1)), offset.dates = NA,
                        show.China = F, smooth = "overlay", ymx = NA, leg.ncols = 2, main) {

    # get data from ECDC (download string updated on 2020-04-13)
    data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                     na.strings = "", fileEncoding = "UTF-8-BOM")

    colnames(data) <- tolower(colnames(data))
    data$daterep <- as.Date(paste0(data$year,"-",data$month,"-",data$day), format = "%Y-%m-%d")

    # add date of & days since first case
    data$day.0 <- stats::ave(data$daterep, data$geoid, FUN = min)
    data$ndays <- as.integer(difftime(data$daterep, data$day.0, units = "day"))

    # remove negative reported cases
    data <- data[data$cases >= 0,]
    data <- data[order(data$daterep),]
    data$cCases <- stats::ave(data$cases, data$geoid, FUN = cumsum)

    if(length(incl) == 0) incl <- "UK"

    dd <- switch(substr(offset.dates,1,1),
                 "d" = data$d10days,
                 "c" = data$n100days,
                 data$daterep)

    xlab <- switch(substr(offset.dates,1,1),
                   "d" = "Days since 10th confirmed death",
                   "c" = "Days since 100th confirmed case",
                   "Date")

    if (show.China) {
        xrng <- range(dd[data$geoid %in% incl][dd[data$geoid %in% incl] >= 0])
    } else {
        xrng <- range(dd[data$geoid %in% incl][dd[data$geoid %in% incl[!incl == "CN"]] >= 0])
    }

    if(missing(main)) main <- switch(substr(tolower(smooth),1,1),
                                    "o" = "Daily confirmed cases",
                                    "s" = "Smoothed daily confirmed cases",
                                    "Daily confirmed cases")


    if(is.na(ymx)) ymx <- max(data$cases[data$geoid %in% incl], na.rm = T) * 1.1

    if(substr(offset.dates,1,1) %in% c("c","d")) {
        plot(1, type = "n", xlim = xrng, main = main, ylim = c(0, ymx),
             xlab = xlab, ylab = "Daily confirmed cases")
    } else {
        plot(1, type = "n", xlim = xrng, main = main, ylim = c(0, ymx), xaxt = "n",
             xlab = xlab, ylab = "Daily confirmed cases")
        axis(1, at = pretty(dd), labels = format(pretty(dd), "%m-%d"))
    }
    title(main = paste("   Downloaded on", format(max(data$daterep), "%Y-%m-%d")),
          cex.main = 0.6, line = -1)

    invisible(sapply(rev(1:length(incl)), function(i) {
        id <- incl[i]
        if(substr(tolower(smooth),1,1) == "o") {
            ss <- smooth.spline(dd[data$geoid == id],
                                data$cases[data$geoid == id], df = 20)
            lines(ss, col = ccols[i])
            points(dd[data$geoid == id], data$cases[data$geoid == id],
                   col = transp(ccols[i]), pch = 20, cex = 0.5)
            sapply(which(abs(ss$y - data$cases[data$geoid == id]) > 10), function(p) {

                lines(rep(dd[data$geoid == id][p], 2),
                      c(data$cases[data$geoid == id][p], ss$y[p]),
                      col = transp(ccols[i]))
            })
        } else {
            if(substr(tolower(smooth),1,1) == "s") {
                ss <- smooth.spline(dd[data$geoid == id],
                                    data$cases[data$geoid == id], df = 20)
                lines(ss, col = ccols[i])
            } else {
                lines(dd[data$geoid == id], data$cases[data$geoid == id], type = "o",
                      col = transp(ccols[i]), pch = 20, cex = 0.5)
            }
        }
    }))

    legend("topleft", legend = paste0(incl, "    "), col = ccols,
           lty = 1, bty = "n", cex = 0.7, ncol = leg.ncols)
}



#' Plot daily deaths
#'
#' @param incl Vector of country codes to include. Default is c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US").
#' @param ccols Vector of colours to use for each country. Default is black for first country and rainbow palette for the remainder.
#' @param show.China Boolean: show full China trajectory or truncate? Default is F (truncated).
#' @param smooth Boolean: apply a spline smoother? Default is T.
#' @param add.lockdowns Boolean: add points showing when lockdowns were introduced? Default is F.
#' @param offset.dates Character: show days since 100th case ("c"), days since 10th death ("d"), or date? Default is date.
#' @param ymx Maximum value to show on y-axis
#' @param leg.ncols Number of columns in legend. Default is 2.
#'
#' @export
#'
daily.deaths <- function(incl = c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US"),
                         ccols = c("black", rbow(length(incl) - 1)), offset.dates = NA,
                         show.China = F, smooth = "o", ymx = NA, leg.ncols = 2, main) {

    # get data from ECDC (download string updated on 2020-04-13)
    data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                     na.strings = "", fileEncoding = "UTF-8-BOM")

    colnames(data) <- tolower(colnames(data))
    data$daterep <- as.Date(paste0(data$year,"-",data$month,"-",data$day), format = "%Y-%m-%d")

    # add date of & days since first case
    data$day.0 <- stats::ave(data$daterep, data$geoid, FUN = min)
    data$ndays <- as.integer(difftime(data$daterep, data$day.0, units = "day"))

    # remove negative reported cases
    data <- data[data$deaths >= 0,]
    data <- data[order(data$daterep),]
    data$cDeaths <- stats::ave(data$deaths, data$geoid, FUN = cumsum)

    dd <- switch(substr(offset.dates,1,1),
                 "d" = data$d10days,
                 "c" = data$n100days,
                 data$daterep)

    xlab <- switch(substr(offset.dates,1,1),
                   "d" = "Days since 10th confirmed death",
                   "c" = "Days since 100th confirmed case",
                   "Date")

    if(missing(main)) main <- switch(substr(tolower(smooth),1,1),
                                     "o" = "Daily reported deaths",
                                     "s" = "Smoothed reported deaths",
                                     "Daily reported deaths")

    if (show.China) {
        xrng <- range(dd[data$geoid %in% incl][dd[data$geoid %in% incl] >= 0])
    } else {
        xrng <- range(dd[data$geoid %in% incl][dd[data$geoid %in% incl[!incl == "CN"]] >= 0])
    }

    if(is.na(ymx)) ymx <- max(data$deaths[data$geoid %in% incl], na.rm = T)

    if(substr(offset.dates,1,1) %in% c("c","d")) {
        plot(1, type = "n", xlim = xrng, main = main, ylim = c(0, ymx),
             xlab = xlab, ylab = "Daily deaths")
    } else {
        plot(1, type = "n", xlim = xrng, main = main, ylim = c(0, ymx), xaxt = "n",
             xlab = xlab, ylab = "Daily deaths")
        axis(1, at = pretty(dd), labels = format(pretty(dd), "%m-%d"))
    }

    title(main = paste("   Downloaded on", format(max(data$daterep), "%Y-%m-%d")),
          cex.main = 0.6, line = -1)

    invisible(sapply(rev(1:length(incl)), function(i) {
        id <- incl[i]
        if(substr(tolower(smooth),1,1) == "o") {
            ss <- smooth.spline(dd[data$geoid == id],
                                data$deaths[data$geoid == id], df = 20)
            lines(ss, col = ccols[i])
            points(dd[data$geoid == id], data$deaths[data$geoid == id],
                   col = transp(ccols[i]), pch = 20, cex = 0.5)
            sapply(which(abs(ss$y - data$deaths[data$geoid == id]) > 10), function(p) {

                lines(rep(dd[data$geoid == id][p], 2),
                      c(data$deaths[data$geoid == id][p], ss$y[p]),
                      col = transp(ccols[i]))
            })
        } else {
            if(substr(tolower(smooth),1,1) == "s") {
                ss <- smooth.spline(dd[data$geoid == id],
                                    data$deaths[data$geoid == id], df = 20)
                lines(ss, col = ccols[i])
            } else {
                lines(dd[data$geoid == id], data$deaths[data$geoid == id], type = "o",
                      col = transp(ccols[i]), pch = 20, cex = 0.5)
            }
        }
    }))
    legend("topleft", legend = paste0(incl, "    "), col = ccols,
           lty = 1, bty = "n", cex = 0.7, ncol = leg.ncols)
}


#' Plot cumulative cases aligned with another country's trajectory
#'
#' @param c1 Country to use as baseline
#' @param c2 Vector of countries to compare
#' @param log Boolean: use log scale? Default is F
#' @param show.original Boolean: show actual dates as well as offset dates? Default is F
#' @param ccols Vector of colours to use for plotting countries in c2. Default is rainbow.
#'
#' @export
#'
lagged.cases <- function(c1, c2, log = F, show.original = F, show.lockdowns = F,
                         ccols = rbow(length(c2)+1)[-1]) {

    ecdc()

    l <- sapply(c2, lag, c1 = c1)
    c1.nm <- gsub("\\."," ",
                  gsub("\\_"," ",
                       unique(data$countriesandterritories[data$geoid == c1])))

    if(log) {
        suppressWarnings(plot(data$daterep[data$geoid == c1], data$cCases[data$geoid == c1],
                              type = "o", pch = 20, cex = 0.6, log = "y",
                              ylim = c(1, max(data$cCases[data$geoid %in% c(c1, c2)])),
                              xlab = "Date", ylab = "Cumulative cases (log10)", yaxt = "n",
                              main = paste0("Cumulative cases aligned with ", c1.nm)))
        axis(2, at = 10^(0:6), labels = format(10^(0:6), scientific = F))
    } else {
        plot(data$daterep[data$geoid == c1], data$cCases[data$geoid == c1],
             type = "o", pch = 20, cex = 0.6,
             xlab = "Date", ylab = "Cumulative cases",
             main = paste0("Cumulative cases aligned with ", c1.nm))
    }
    if(show.lockdowns) {
        ld <- lockdowns(c(c1, c2))
        points(ld$matchDate[ld$country == c1], ld$cCases[ld$country == c1],
               pch = 23)
    }

    sapply(1:length(c2), function(i) {
        lines(data$daterep[data$geoid == c2[i]] - l[i]*(24*60*60),
              data$cCases[data$geoid == c2[i]],
              type = "o", pch = 20, cex = 0.6, col = ccols[i])
        if(show.lockdowns) { if(c2[i] %in% ld$country) {
                points(ld$matchDate[ld$country == c2[i]] - l[i]*(24*60*60),
                       ld$cCases[ld$country == c2[i]],
                       pch = 23, col = ccols[i])
        }}
        if(show.original) {
            lines(data$daterep[data$geoid == c2[i]],
                  data$cCases[data$geoid == c2[i]],
                  type = "o", pch = 20, cex = 0.6, col = transp(ccols[i], 0.1))
            if(show.lockdowns) { if(c2[i] %in% ld$country) {
                points(ld$matchDate[ld$country == c2[i]],
                       ld$cCases[ld$country == c2[i]],
                       pch = 23, col = transp(ccols[i], 0.1))
            }}
        }

    })

    if(show.original) {
        ll <- c(c1, paste0(c2, " (adj by ", -l," days)    "), NA, paste0(c2, "(original)"))
        lc <- c("black", ccols, NA, transp(ccols, 0.2))
        nlc <- 2
    } else {
        ll <- c(c1, paste0(c2, " (adj by ", -l," days)"))
        lc <- c("black", ccols)
        nlc <- 1
    }

    legend("topleft", lty = 1, pch = 20, pt.cex = 0.6, cex = 0.8, bty = "n",
           legend = ll, col = lc, ncol = nlc)
}



#' Plot weekly cases vs cumulative
#'
#' @param incl Vector of country codes to include. Default is c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US").
#' @param ccols Vector of colours to use for each country. Default is black for first country and rainbow palette for the remainder.
#' @param log Boolean: plot on log scale? Default is T (show on log10 scale)
#' @param grid Boolean: add a grid? Default is F.
#' @param main Plot title: default is "Trajectory of confirmed cases"
#'
#' @export
#'
lw.cc.global <- function(incl = c("UK", "CN", "JP", "KR", "IT", "ES", "FR", "DE", "US"),
                         ccols = c("black", rbow(length(incl)-1)),
                         log = T, grid = F, deaths = T,
                         main = NA) {
    ecdc()

    if(deaths) {
        xrng <- c(1, max(data$cDeaths[data$geoid %in% incl], na.rm = T))
        yrng <- c(1, max(data$lw.deaths[data$geoid %in% incl], na.rm = T))
        if(is.na(main)) main <- "Trajectory of Covid-19 deaths"
        xlb <- "Total confirmed deaths"
        ylb <- "Deaths in past week"
    } else {
        xrng <- c(100, max(data$cCases[data$geoid %in% incl], na.rm = T))
        yrng <- c(100, max(data$lw.cases[data$geoid %in% incl], na.rm = T))
        if(is.na(main)) main <- "Trajectory of confirmed cases"
        xlb <- "Total confirmed cases"
        ylb <- "New confirmed cases in past week "
    }


    if(log) {
        plot(100, type = "n", log = "xy", xlim = xrng, ylim = yrng, xaxt = "n", yaxt = "n",
             main = main, xlab = paste(xlb, "(log10)"), ylab = paste(ylb, "(log10)"))
        axis(2, at = 10^(0:6), labels = format(10^(0:6), scientific = F))
        axis(1, at = 10^(0:6), labels = format(10^(0:6), scientific = F))
    } else {
        plot(100, type = "n", xlim = xrng, ylim = yrng,
             main = main, xlab = xlb, ylab = ylb)
    }
    if(grid) grid(col = transp("grey"), lty = 1)
    abline(0,1, col = transp("grey"))

    title(main = paste("   Downloaded on", format(max(data$daterep), "%Y-%m-%d")),
          cex.main = 0.6, line = 0.7)

    invisible(sapply(1:length(incl), function(i) {
        id <- incl[i]
        if(deaths) {
            lines(data$cDeaths[data$geoid == id], data$lw.deaths[data$geoid == id], col = ccols[i],
                  type = "o", pch = 20, cex = 0.4)
        } else {
            lines(data$cCases[data$geoid == id], data$lw.cases[data$geoid == id], col = ccols[i],
                  type = "o", pch = 20, cex = 0.4)
        }
    }))
    legend("topleft", legend = paste0(incl, "    "), col = ccols,
           lty = 1, bty = "n", pch = 1, nc = 2, cex = 0.8)
}




#' Plot cases vs population size
#'
#' @param incl Vector of country codes to include
#' @param icols Vector of colours to use for each country in incl
#' @param log Boolean: use a log scale? Default is T.
#' @param deaths Boolean: plot deaths? Default is F (plot cases)
#' @param legend.rows Integer: how many rows in legend? Default is 5.
#'
#' @export
#'
prop.cases <- function(incl = c("UK", "US", "CN", "IT", "BE", "BR", "DE", "ES", "FR", "IN"),
                       icols = c("magenta3", rbow(length(incl))[-1]), deaths = F,
                       legend.rows = 5, legend = T, add.ONS = T) {
    ecdc()

    log = T
    if(deaths) {
        ydata <- summ$tdeaths
        props <- round(summ$dprop[match(incl, summ$geoid)], 2)
        main <- "Total deaths vs population"
        ylab <- "Total deaths reported"
        hl <- c(0.001, 0.0005)
    } else {
        ydata <- summ$tcases
        props <- round(summ$cprop[match(incl, summ$geoid)], 2)
        main <- "Total cases vs population"
        props <- round(summ$cprop[match(incl, summ$geoid)], 2)
        ylab <- "Total cases reported"
        hl <- c(0.01, 0.005)
    }

    if(log) {
        pop.units <- 1e6; c.units <- 1
        suppressWarnings(plot(summ$pop/pop.units, ydata/c.units, col = "grey", log = "xy",
                              xlab = "Population (millions)", ylab = ylab, xaxt = "n"))
        axis(1, at = 10^{0:5}, labels = format(10^{0:5}, scientific = F))
        axis(1, at = 10^-{1:2}, labels = format(10^-{1:2}, scientific = F))
        lines(0.0001:1000,0.0001:1000*pop.units*hl[1], col = "dimgrey", lty = "22")
        lines(0.0001:1000,0.0001:1000*pop.units*hl[2], col = "dimgrey", lty = "22")
        text(c(0.008, 0.008), 0.008*pop.units*hl, paste0(hl*100,"%"),
             pos = c(3, 1), srt = par("pin")[2] / par("pin")[1] * 45, col = "dimgrey", cex = 0.8)
        title(paste(main, "(log scale)"))
        if(legend) legend.pos <- "topleft" else legend.pos <- NA
    } else {
        pop.units <- 1e6; c.units <- 1
        plot(summ$pop/pop.units, ydata/c.units, col = "grey",
             xlab = "Population (millions)", ylab = ylab)
        abline(c(0,0),pop.units*hl, col = "dimgrey", lty = "22")
        text(1, 3e5, "1%", pos = c(1,3), col = "dimgrey")
        title(main)
        if(legend) legend.pos <- "topright" else legend.pos <- NA
    }
    invisible(sapply(1:length(incl), function(i) {
        points(summ$pop[summ$geoid == incl[i]]/pop.units,
               ydata[summ$geoid == incl[i]]/c.units,
               bg = icols[i], pch = 21);
    }))
    legend(legend.pos, legend = paste0(incl, " (",props,")   "),
           pch = 21, pt.bg = icols, ncol = ceiling(length(incl)/legend.rows), cex = 0.8)
    title(paste0("Downloaded on ", format(max(data$daterep), "%Y-%m-%d")), line = -1, cex.main = 0.8)

    if(deaths & add.ONS) {
        fnm <- sort(list.files("~/PhD/Misc-notes/Covid-19/ONS", pattern = "2020\\.xls", full.names = T),
                    decreasing = T)[1]
        ons.occ <- sum(read.ons(fnm = fnm, snm = "Covid-19 - Weekly occurrences",
                 cols = "C:BC", header.row = "6", data.rows = "9")[,1], na.rm = T)

        if("UK" %in% incl) {fg.col <- "black"} else {fg.col <- "grey"}
        points(summ$pop[summ$geoid == "UK"]/pop.units, ons.occ/c.units, col = fg.col, pch = 4, lwd = 2)
    }
}




#' Plot reported cases & deaths for a given country
#'
#' @param ccd Country code (used to match with geoid in ECDC data)
#' @param main Main title for plot
#' @param lth Threshold for adding lines from points to spline. Default is 10.
#' @param ds Scaling factor to use when plotting deaths. Default is 5.
#' @param c.ymx (optional) maximum value of cases to plot
#' @param xmin Date to plot from. Default is as.Date("2020-03-01").
#'
#' @export
#'
c19.plot <- function(ccd = "UK", lth = 10, ds = 5, c.ymx, xmin = as.Date("2020-03-01"),
                     main = paste0("Reported cases & deaths in ",ccd)) {

    # only reload data if necessary
    if(!exists("summ")) ecdc()

    # remove negative reported cases
    cc <- data[data$geoid == ccd & data$cases >= 0,c("daterep", "cases")]
    cd <- data[data$geoid == ccd & data$deaths >= 0,c("daterep", "deaths")]

    if(is.na(xmin)) {
        xmin <- min(cc$daterep, cd$daterep)
    } else {
        cc <- cc[cc$daterep >= xmin,]
        cd <- cd[cd$daterep >= xmin,]
    }

    sc <- smooth.spline(cc, df = 21)
    sd <- smooth.spline(cd, df = 21)

    dd <- sort(unique(cc$daterep, cd$daterep))

    xrng <- c(xmin, max(cc$daterep, cd$daterep))
    if(missing(c.ymx)) c.ymx <- max(c(cc$cases, na.rm = T), max(cd$deaths, na.rm = T) * ds) * 1.1

    # create empty plot
    par(mar = c(5,4,4,4))
    plot(1, type = "n", xlim = xrng, ylim = c(0, c.ymx), xaxt = "n", main = "", xlab = "", ylab = "")
    axis(1, at = pretty(dd), labels = format(pretty(dd), "%m-%d"))
    title(main = main)
    title(main = paste("   Downloaded on", format(max(dd), "%Y-%m-%d")), cex.main = 0.6, line = -1)
    title(xlab = "Date", ylab = "Daily cases", line = 2.5)
    mtext("Daily deaths", side = 4, line = 2.5, col = "red2")

    lines(sc)
    points(cc, pch = 20, cex = 0.5, col = transp("black"))
    invisible(sapply(which(abs(sc$y - cc$cases) > lth), function(p) {
        lines(rep(cc[p, "daterep"], 2), c(cc[p,"cases"], sc$y[p]), col = transp("black"))
    }))

    lines(sd$x, sd$y * ds, col = "red2")
    points(cd$daterep, cd$deaths * ds, pch = 20, cex = 0.5, col = transp("red2"))
    invisible(sapply(which(abs(sd$y - cd$deaths) > lth), function(p) {
        lines(rep(cd[p, "daterep"], 2), c(cd[p,"deaths"], sd$y[p]) * ds, col = transp("red2"))
    }))
    axis(4, at = pretty(cc$cases), labels = pretty(cc$cases) / ds, col.tick = "red2", col.axis = "red2")

    legend("topleft", c("Cases", "Deaths"), col = c("black", "red2"), pch = 20, lty = 1, bty = "n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOTS - UK                                                                        ####

#' Plot UK case trajectory
#'
#' @export
#'
uk.cases <- function(pad.x = 0) {

    phe()

    plot(1, type = "n", log = "y",
         xlim = range(0, uk.data$c100days + pad.x),
         ylim = c(1, max(uk.data$TotalCases)),
         xlab = "Days since 100th case in UK",
         ylab = "Cumulative confirmed cases",
         main = "Confirmed cases per UK county")

    sapply(unique(uk.data$GSS_NM[substr(uk.data$GSS_CD,3,3) == "9"]), function(brgh) {
         lines(uk.data$c100days[uk.data$GSS_NM == brgh],
               uk.data$TotalCases[uk.data$GSS_NM == brgh],
               type = "o", pch = 20, cex = 0.5,
               col = transp(uk.data$reg.col[substr(uk.data$GSS_CD,3,3) == "9"]))
    })

    sapply(unique(uk.data$GSS_NM[substr(uk.data$GSS_CD,2,2) == "1"]), function(cty) {
        lines(uk.data$c100days[uk.data$GSS_NM == cty],
              uk.data$TotalCases[uk.data$GSS_NM == cty],
              type = "o", pch = 20, cex = 0.5,
              col = transp(uk.data$reg.col[substr(uk.data$GSS_CD,2,2) == "1"]))
    })

    sapply(c("Hertfordshire", "Norfolk"), function(cty) {
        lines(uk.data$c100days[uk.data$GSS_NM == cty],
              uk.data$TotalCases[uk.data$GSS_NM == cty],
              type = "o", pch = 20, cex = 0.5, lwd = 2,
              col = uk.data$reg.col[uk.data$GSS_NM == cty])
    })
    legend("bottomright", lty = 1, pch = 20, pt.cex = 0.5, bty = "n", cex = 0.8,
           legend = c("Hertfordshire", "Norfolk", "Greater London", "Other counties"),
           col = c(unique(uk.data$reg.col[uk.data$GSS_NM == "Hertfordshire"]),
                   unique(uk.data$reg.col[uk.data$GSS_NM == "Norfolk"]),
                   unique(uk.data$reg.col[substr(uk.data$GSS_CD,3,3) == "9"]),
                   unique(uk.data$reg.col[substr(uk.data$GSS_CD,2,2) == "1"])))
    title(main = paste("   Downloaded on",
                       format(max(uk.data$dl.dt), "%Y-%m-%d")),
          cex.main = 0.6, line = -1, adj = 0)
}

#' Plot new cases each day
#'
#' @export
#'
new.uk.cases <- function(pad.x = 0) {

    phe()
    plot(1, type = "n", xlim = range(0, uk.data$c100days + pad.x),
         ylim = c(1, max(uk.data$NewCases, na.rm = T)), xlab = "Days since 100th case in UK",
         ylab = "Daily confirmed cases",
         main = "Confirmed cases per UK county")

    sapply(unique(uk.data$GSS_NM[substr(uk.data$GSS_CD, 3, 3) == "9"]), function(brgh) {
        lines(uk.data$c100days[uk.data$GSS_NM == brgh],
              uk.data$NewCases[uk.data$GSS_NM == brgh],
              type = "o", pch = 20, cex = 0.5,
              col = transp(uk.data$reg.col[uk.data$GSS_NM == brgh]))
    })

    sapply(unique(uk.data$GSS_NM[substr(uk.data$GSS_CD, 2, 2) == "1"]), function(cty) {
        lines(uk.data$c100days[uk.data$GSS_NM == cty],
              uk.data$NewCases[uk.data$GSS_NM == cty],
              type = "o", pch = 20, cex = 0.5,
              col = transp(uk.data$reg.col[uk.data$GSS_NM == cty]))
    })

    sapply(c("Hertfordshire", "Norfolk"), function(cty) {
        lines(uk.data$c100days[uk.data$GSS_NM == cty],
              uk.data$NewCases[uk.data$GSS_NM == cty],
              type = "o", pch = 20, cex = 0.5, lwd = 2,
              col = uk.data$reg.col[uk.data$GSS_NM == cty])
    })

    legend("left", lty = 1, pch = 20, pt.cex = 0.5, bty = "n", cex = 0.8,
           legend = c("Hertfordshire", "Norfolk", "Greater London", "Other counties"),
           col = c(unique(uk.data$reg.col[uk.data$GSS_NM == "Hertfordshire"]),
                   unique(uk.data$reg.col[uk.data$GSS_NM == "Norfolk"]),
                   unique(uk.data$reg.col[substr(uk.data$GSS_CD, 3, 3) == "9"]),
                   unique(uk.data$reg.col[substr(uk.data$GSS_CD, 2, 2) == "1"])))

    title(main = paste("   Downloaded on", format(max(uk.data$dl.dt), "%Y-%m-%d")),
          cex.main = 0.6, line = -1, adj = 0)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MAPS                                                                              ####

#' Match summary to countries used in mapping
#'
#' @param countries Vector of country names to match, obtained from map("world", plot = F)$names
#'
#' @export
#'
mappable.summ <- function(...) {

    # get ECDC data if not already available
    if(!exists("summ")) ecdc()

    # tidy up ISO data
    iso3166$a3[iso3166$mapname == "Kosovo"] <- "XKX"
    iso3166$mapname <- gsub("\\(.+\\)", "", iso3166$mapname)

    # match countries to eCDC data using ISO codes
    zz <- merge(summ, iso3166, by = "a3", all.x = T)
    zz <- zz[,c("a3", "geoid", "mapname", "sovereignty",
                "tcases", "tdeaths", "pop", "cprop", "dprop")]

    # remove duplicates by sovereign nation
    zz.dup <- setNames(unique(zz[zz$geoid %in% zz$geoid[duplicated(zz$geoid)],-3]),
                       colnames(zz)[-4])
    zz.u <- setNames(zz[!zz$geoid %in% zz.dup$geoid,-4],
                     colnames(zz)[-4])
    zz <- rbind(zz.u, zz.dup)

    # ignore unmatched lines for now, but don't delete
    # zz[is.na(zz$mapname),]

    # match map names to ISO3166 codes, aggregating all polygons
    mapnames.org <- map("world", plot = F, ...)$names

    mc <- data.frame("mapname" = mapnames.org,
                     "main" = gsub("\\:.+","",mapnames.org))

    # use exact matches on main country (includes all polygons)
    df <- merge(mc, zz, by.x = "main", by.y = "mapname", sort = F, all.x = T)
    df[match(mapnames.org, df$mapname),]
}



#' Draw map & colour according to data from mappable.summ()
#'
#' @param varb Variable to use to colour map. Default is tcases.
#' @param cols Vector of colours to use in plotting. Default is rbow(11).
#' @param intvl Vector of intervals to be used to cut variable. Must be one less than length(cols).
#' @param ... Any other optional arguments to be passed to map("world", ...)
#'
#' @export
#'
map.totals <- function(varb = "tcases", cols = rbow(11), intvl, ...) {

    ms <- mappable.summ(...)

    if(missing(intvl)) {
        intvl <- seq(0, max(ms[,varb], na.rm = T), length.out = length(cols)-1)
    }

    reset.par()

    countries <- map("world", ..., plot = F)$names
    map("world", fill = T, col = cols[findInterval(ms[,varb], intvl)], ...)
}
