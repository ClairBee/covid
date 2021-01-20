

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOTS - GLOBAL                                                                    ####


#' Trajectory of recent vs total cases for a single country
#'
#' @param ccd Country code to plot. Default is UK
#' @param add Boolean: add to existing plot? Default is F (draw new plot)
#' @param deaths Boolean: plot deaths instead of cases? Default is F (plot cases)
#' @param flip Boolean: flip axes? Default is F (X = total cases, Y = last week)
#' @param ... Further graphical parameters to be passed to plotting function
#'
#' @export
#'
c19.trajectory <- function(ccd = "UK", add = F, deaths = F, flip = F, cex = 0.8, col = transp("black"), ...) {

    opch <- par()$pch
    par(pch = 20)
    cdata <- data[data$geoid == ccd,]
    if(deaths) {
        cdata$c <- cdata$cDeaths / cdata$popdata2019 * 1e6
        cdata$lw <- cdata$deaths_weekly / cdata$popdata2019 * 1e6
        c <- "deaths"
    } else {
        cdata$c <- cdata$cCases / cdata$popdata2019 * 1e6
        cdata$lw <- cdata$cases_weekly / cdata$popdata2019 * 1e6
        c <- "cases"
    }

    if(add) {
        if(flip) {
            points(cdata$lw, cdata$c, type = "o", col = col, cex = cex, ...)
        } else {
            points(cdata$c, cdata$lw, type = "o", col = col, cex = cex, ...)
        }
    } else {
        if(flip) {
            plot(cdata$cases_weekly, cdata$c, type = "o",
                 ylab = "Total cases/million", xlab = "Cases/million in last week",
                 main = paste("Trajectory of Covid-19",c), col = col, cex = cex, ...)
        } else {
            plot(cdata$c, cdata$lw, type = "o",
                 xlab = "Total cases/million", ylab = "Cases/million in last week",
                 main = paste("Trajectory of Covid-19",c), col = col, cex = cex, ...)
        }
    }
    par(pch = opch)
}

#' Replicate Chartr quadrant plot
#'
#' @export
#'
c19.quadrants <- function(cases = F, lw.th, c.th, label.at = 1.5, excl = "") {

    df <- merge(aggregate(daterep ~ geoid, data = data, FUN = "max"),
                data[,c("geoid", "daterep", "cCases", "cases_weekly", "cDeaths", "deaths_weekly", "popdata2019")],
                by = c("geoid", "daterep"))
    df <- df[!(df$geoid %in% excl),]

    upd <- max(data$daterep)

    # add max date
    if(cases) {
        df$c <- df$cCases / df$popdata2019 * 1e6
        df$lw <- df$cases_weekly / df$popdata2019 * 1e6
        main <- "Countries worst affected by Covid-19 (cases attributed)"
        ylab <- "Cases in last 7 days (per million people)"
        xlab <- "Total cases (per million people)"
        if(missing(lw.th)) lw.th <- 500
        if(missing(c.th)) c.th <- 10000
        xmx <- round_any(max(df$c, na.rm = T), 10000, ceiling)
        ymx <- round_any(max(df$lw, na.rm = T), 1000, ceiling)
    } else {
        df$c <- df$cDeaths / df$popdata2019 * 1e6
        df$lw <- df$deaths_weekly / df$popdata2019 * 1e6
        main <- "Countries worst affected by Covid-19 (deaths attributed)"
        ylab <- "Deaths in last 7 days (per million people)"
        xlab <- "Total deaths (per million people)"
        if(missing(lw.th)) lw.th <- 10
        if(missing(c.th)) c.th <- 200
        xmx <- round_any(max(df$c, na.rm = T), 500, ceiling)
        ymx <- round_any(max(df$lw, na.rm = T), 20, ceiling)
    }
    main <- paste(main, "- ", upd)

    plot(df$c, df$lw, pch = 20, cex = 0.5, xlab = "", ylab = "", xlim = c(0, xmx), ylim = c(0, ymx),
         main = main, cex.main = 0.8, cex.axis = 0.8, xaxs = "i", yaxs = "i")
    title(xlab = xlab, ylab = ylab, line = 2.5, cex.lab = 0.8)

    rect(c.th, -10, xmx, ymx, col = transp("orange"), border = NA)
    rect(-100, lw.th, xmx, ymx, col = transp("orange"), border = NA)
    rect(c.th * 10, -10, xmx, ymx, col = transp("orange"), border = NA)

    points(df$c, df$lw, pch = 20, cex = 0.6)
    invisible(sapply(df$geoid[df$c > c.th*label.at | df$lw > lw.th*label.at], function(ccd) {
        text(df$c[df$geoid == ccd], df$lw[df$geoid == ccd], labels = ccd,
             cex = 0.6, pos = 4, offset = 0.2)
    }))
    text(df$c[df$geoid == "UK"], df$lw[df$geoid == "UK"], labels = "UK",
         cex = 0.6, pos = 4, offset = 0.2)

}



#' Quick trajectory plots of latest ECDC data
#'
#' @export
#'
ecdc.daily <- function(excl = "") {
    ecdc()

    c19.quadrants(cases = T, excl = excl)
    c19.trajectory("UK", add = T)

    c19.quadrants(cases = F, excl = excl)
    c19.trajectory("UK", add = T, deaths = T)
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
