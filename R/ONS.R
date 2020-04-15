
#' Generic function: read ONS data
#' 
#' @param fnm File name to load from
#' @param snm Sheet name or index
#' @param cols Columns to read
#' @param date.row Row containing dates
#' @param data.rows Rows to read
#' 
#' @export
#' 
read.ons <- function(fnm, snm, cols, date.row, data.rows) {
    
    col.rng <- unlist(strsplit(cols, ":"))
    date.rng <- paste(paste0(col.rng, date.row), collapse = ":")
    
    col.rng[1] <- LETTERS[which(LETTERS == col.rng[1])-1]
    if(grepl(":",data.rows)) {
        data.rng <- paste(paste0(col.rng, unlist(strsplit(data.rows, ":"))), collapse = ":")
    } else {
        data.rng <- paste(paste0(col.rng, data.rows), collapse = ":")
    }
    
    data <- invisible(read_excel(fnm, sheet = snm, range = data.rng, col_names = F, .name_repair = "minimal"))
    df <- as.data.frame(t(as.matrix(data[,-1])))
    colnames(df) <- as.data.frame(data)[,1]
    df$date <- as.Date(as.matrix(read_excel(fnm, sheet = snm, range = date.rng,
                                            col_names = F, .name_repair = "minimal")))
    df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read specific data                                                                ####

#' Read totals 
#' 
#' @export
#' 
ons.totals <- function() {
    
    ons.ranges <- read.csv("./ONS-ranges.csv")
    data.frame(data.table::rbindlist(sapply(1:nrow(ons.ranges), function(i) {
        setNames(read.ons(fnm = ons.ranges$fnm[i], snm = ons.ranges$snm[i], 
                          cols = ons.ranges$cols[i], date.row = ons.ranges$date.r[i],
                          data.rows = ons.ranges$total.r[i]), c("total", "date"))
    }, simplify = F)))
}



#' Totals by cause of death
#' 
#' @export
#' 
ons.by.cause <- function() {
    
    ons.ranges <- read.csv("./ONS-ranges.csv")
    
    hist <- data.frame(data.table::rbindlist(sapply(1:(nrow(ons.ranges)-1), function(i) {
        setNames(read.ons(fnm = ons.ranges$fnm[i], snm = ons.ranges$snm[i], 
                          cols = ons.ranges$cols[i], date.row = ons.ranges$date.r[i],
                          data.rows = ons.ranges$by.cause[i]), c("resp", "date"))
    }, simplify = F)))
    
    this.year <- as.matrix(read_excel("./ONS/referencetablesweek142020.xlsx", 
                                      sheet = "Weekly figures 2020",
                                      range = "B14:BC15",
                                      col_names = F, .name_repair = "minimal")[,-1])
    ty.dates <- as.Date(as.matrix(read_excel("./ONS/referencetablesweek142020.xlsx", 
                                             sheet = "Covid-19 - Weekly occurrences", 
                                             range = "B6:BC6", col_names = F, .name_repair = "minimal")[,-1]))
    
    data.frame("date" = c(hist$date, ty.dates),
               "resp" = c(hist$resp, this.year[1,]),
               "c19" = c(rep(NA, nrow(hist)), this.year[2,]))
}