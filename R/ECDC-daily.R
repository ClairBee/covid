
#' Refresh ECDC data & plots
#'
#' Download latest data from ECDC & produce pdf of standard plots
#'
#' @param incl Vector of country codes to include
#' @param ccols Vector of colours to use for countries in incl
#'
#' @export
#'
ecdc.daily <- function(countries = c("UK", "CN", "KR", "IT", "ES", "US"),
                        ccols = c("black", "forestgreen", "blue3", "dodgerblue", "darkgoldenrod2", "red")) {

    ecdc()

    makepdf(paste0("~/PhD/Misc-notes/Covid-19/plots/",max(data$daterep),".pdf"), {

        par(mfrow = c(1,2))

        cases.since.c100(add.lockdowns = T, doublings = NA,
                         incl = countries, ccols = ccols)
        doubling(nd = 3, label.at = 33)
        deaths.since.d10(incl = countries, ccols = ccols, log = T, doublings = NA)

        daily.cases(incl = countries, ccols = ccols, smooth = F)
        daily.cases(incl = countries, ccols = ccols)

        prop.cases(incl = countries, icols = ccols)
        prop.cases(incl = countries, icols = ccols, deaths = T)

        lw.cc.global(incl = countries, ccols = ccols, deaths = F)
        lw.cc.global(incl = countries, ccols = ccols, deaths = T, main = "Trajectory of deaths")
    }, width = 14)

}
