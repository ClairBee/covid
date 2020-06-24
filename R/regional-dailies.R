

#' Plot daily cases & deaths for all regions
#'
#' @export
#'
regional.dailies <- function() {
    ecdc()


    pdf("~/PhD/Misc-notes/Covid-19/plots/regional.pdf", height = 25, width = 10); {

        par(mfrow = c(5,2), mar = c(3,2,2,1))

        suppressWarnings({
            invisible(sapply(c("Europe", "Asia", "America", "Africa", "Oceania"), function(r) {

                if(length(summ$geoid[summ$continent == r & summ$tcases > 5000]) > 0) {
                    daily.cases(incl = summ$geoid[summ$continent == r & summ$tcases > 5000],
                                main = paste("Daily confirmed cases -",r))
                } else {
                    plot.new()
                }

                if(length(summ$geoid[summ$continent == r & summ$tdeaths > 500]) > 0) {
                    daily.deaths(incl = summ$geoid[summ$continent == r & summ$tcases > 500],
                                 main = paste("Daily confirmed deaths -",r))
                } else {
                    plot.new()
                }
            }))

            prop.cases(incl = unique(summ$geoid), icols = rev(rbow(6))[match(summ$continent, unique(summ$continent))],
                       legend = F)
            legend("topleft", unique(summ$continent), pt.bg = rev(rbow(6)), pch = 21, cex = 0.8,
                   ncol = 2, bty = "n")

            prop.cases(incl = unique(summ$geoid), icols = rev(rbow(6))[match(summ$continent, unique(summ$continent))],
                       legend = F, deaths = T)
            legend("topleft", unique(summ$continent), pt.bg = rev(rbow(6)), pch = 21, cex = 0.8,
                   ncol = 2, bty = "n")
        })
            ct <- ddply(data, .(daterep, continentexp), summarise,
                        cases = sum(cases), deaths = sum(deaths),
                        cCases = sum(cCases), cDeaths = sum(cDeaths),
                        region = min(continentexp))
            ccols <- rbow(length(unique(ct$region)))

            {
                plot(1, type = "n", xlim = range(ct$daterep), ylim = c(0, max(ct$cases, na.rm = T)),
                 xaxt = "n", xlab = "Date", ylab = "Daily confirmed cases",
                 main = "Daily confirmed cases per continent")
                axis(1, at = pretty(ct$daterep), labels = format(pretty(ct$daterep), "%m-%d"))
                title(main = paste("   Downloaded on", format(max(ct$daterep), "%Y-%m-%d")),
                      cex.main = 0.6, line = -1)

                invisible(sapply(1:length(unique(ct$region)), function(i) {
                    id <- unique(ct$region)[i]
                    ss <- smooth.spline(ct$daterep[ct$region == id],
                                        ct$cases[ct$region == id], df = 20)
                    lines(ss, col = ccols[i])
                    points(ct$daterep[ct$region == id], ct$cases[ct$region == id],
                           col = transp(ccols[i]), pch = 20, cex = 0.5)
                    sapply(which(abs(ss$y - ct$cases[ct$region == id]) > 10), function(p) {
                        lines(rep(ct$daterep[ct$region == id][p], 2),
                              c(ct$cases[ct$region == id][p], ss$y[p]), col = transp(ccols[i]))
                    })
                }))
                legend("topleft", unique(ct$region), col = ccols, lty = 1, pch = 20, cex = 0.8,
                       bty = "n")


                plot(1, type = "n", xlim = range(ct$daterep), ylim = c(0, max(ct$deaths, na.rm = T)),
                     xaxt = "n", xlab = "Date", ylab = "Daily deaths",
                     main = "Daily deaths per continent")
                axis(1, at = pretty(ct$daterep), labels = format(pretty(ct$daterep), "%m-%d"))
                title(main = paste("   Downloaded on", format(max(ct$daterep), "%Y-%m-%d")),
                      cex.main = 0.6, line = -1)

                invisible(sapply(1:length(unique(ct$region)), function(i) {
                    id <- unique(ct$region)[i]
                    ss <- smooth.spline(ct$daterep[ct$region == id],
                                        ct$deaths[ct$region == id], df = 20)
                    lines(ss, col = ccols[i])
                    points(ct$daterep[ct$region == id], ct$deaths[ct$region == id],
                           col = transp(ccols[i]), pch = 20, cex = 0.5)
                    sapply(which(abs(ss$y - ct$deaths[ct$region == id]) > 10), function(p) {
                        lines(rep(ct$daterep[ct$region == id][p], 2),
                              c(ct$deaths[ct$region == id][p], ss$y[p]), col = transp(ccols[i]))
                    })
                }))
                legend("topleft", unique(ct$region), col = ccols, lty = 1, pch = 20, cex = 0.8,
                       bty = "n")
            }

    }; dev.off()
}




