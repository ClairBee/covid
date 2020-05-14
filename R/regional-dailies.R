

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
        })
    }; dev.off()
}




