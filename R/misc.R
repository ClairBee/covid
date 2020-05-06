
#' Lookup table of local authorities
#'
#' @export
#'
utla <- function() {
    read.csv("~/PhD/Misc-notes/Covid-19/data/UK-UTLA.csv")
}





#' Calculate lag between two countries
#'
#' @param c1 Country code of first country to compare
#' @param c2 Country code of second country to compare
#' @param diffs Boolen: return vector of differences on lags -50:50? Default is F (return lag).
#'
#' @export
#'
lag <- function(c1, c2, diffs = F) {

    dt <- merge(data[data$geoid == c1,c("daterep", "cCases")],
                data[data$geoid == c2,c("daterep", "cCases")],
                by = "daterep", suffix = c(".c1", ".c2"), all = T)
    dt[is.na(dt)] <- 0

    os.tss <- c(sapply(90:1, function(os) {
        sum((dt$cCases.c2[1:(nrow(dt)-os)] - dt$cCases.c1[(os+1):nrow(dt)])^2)
    }), sapply(0:90, function(os) {
        sum((dt$cCases.c1[1:(nrow(dt)-os)] - dt$cCases.c2[(os+1):nrow(dt)])^2)
    }))

    if(diffs) os.tss else (-90:90)[which.min(os.tss)]
}





