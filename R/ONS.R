
#' Generic function: read ONS data
#'
#' @param fnm File name to load from
#' @param snm Sheet name or index
#' @param cols Columns to read
#' @param header.row Row containing dates
#' @param data.rows Rows to read
#'
#' @export
#'
read.ons <- function(fnm, snm, cols, header.row , data.rows, rotate = F) {

    col.rng <- unlist(strsplit(cols, ":"))
    header.rng <- paste(paste0(col.rng, header.row), collapse = ":")

    col.rng[1] <- LETTERS[which(LETTERS == col.rng[1])-1]
    if(grepl(":",data.rows)) {
        data.rng <- paste(paste0(col.rng, unlist(strsplit(data.rows, ":"))), collapse = ":")
    } else {
        data.rng <- paste(paste0(col.rng, data.rows), collapse = ":")
    }

    data <- invisible(read_excel(fnm, sheet = snm, range = data.rng, col_names = F, .name_repair = "minimal"))
    df <- as.data.frame(t(as.matrix(data[,-1])))
    if(!is.na(data[1,1])) colnames(df) <- as.data.frame(data)[,1]

    header <- data.frame(read_excel(fnm, sheet = snm, range = header.rng,
                                    col_names = F, .name_repair = "minimal"))

    # check for annoying extra columns in data
    rm <- apply(header,1,"%in%", c("Year to date", "Weeks"))
    if(sum(rm) > 0) {
        df <- df[-which(rm),,drop = F]
        header <- header[,-which(rm),drop = F]
    }

    if(grepl("POSIX", class(header[1,1]))[1]) {
        df$date <- as.Date(unlist(format(header, "%Y-%m-%d")))
    } else {
        if(try_default(as.Date(header), T, quiet = T)) {
            df$cat <- unlist(header)
        } else {
            df$date <- as.Date(unlist(header))
        }
    }
    if(rotate) {
        qq <- as.data.frame(t(df[,1:(ncol(df)-1)]))
        rownames(qq) <- colnames(df)[1:(ncol(df)-1)]
        colnames(qq) <- df[,ncol(df)]
        qq
    } else {
        df
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Other data sources                                                                ####

#' Data frame containing age groups to use
#'
#' @export
#'
ages <- function() {
    data.frame("org" = c("0-4", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                             "75-79", "80-84", "85-89", "90+", "90-94", "95-99", "100 & over"),
                   "age" = c("0-4", "0-4", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                             "70-74", "75-79", "80-84", "85-89", "90+", "90+", "90+", "90+"))
}



#' Get 2018 population (initially only be age group)
#'
#' @export
#'
ons.popn <- function() {

    m.by.age <- as.data.frame(read_excel("./ONS/population2018.xls",
                                         sheet = "MALES",
                                         range = "A9:B29",
                                         col_names = F, .name_repair = "minimal"))
    f.by.age <- as.data.frame(read_excel("./ONS/population2018.xls",
                                         sheet = "FEMALES",
                                         range = "A9:B29",
                                         col_names = F, .name_repair = "minimal"))

    df <- data.frame("age" = factor(m.by.age[,1], levels = m.by.age[,1]),
                          "f" = f.by.age[,2],
                          "m" = m.by.age[,2])

    df <- merge(df, ages(), by.x = "age", by.y = "org", sort = F)
    df <- aggregate(. ~ age.y, data = df[,-1], FUN = sum, na.action = na.pass)

    df <- setNames(df[order(as.integer(gsub("\\+", "", gsub("-.+","", df$age)))),c(1,3,2)],
                   c("age", "pop.m", "pop.f"))
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
                          cols = ons.ranges$cols[i], header.row = ons.ranges$date.r[i],
                          data.rows = ons.ranges$total.r[i]), c("total", "date"))
    }, simplify = F)))
}



#' Totals by cause of death
#'
#' @export
#'
ons.by.cause <- function() {

    ons.ranges <- read.csv("./ONS-ranges.csv")

    hist <- data.frame(data.table::rbindlist(sapply(1:5, function(i) {
        setNames(read.ons(fnm = ons.ranges$fnm[i], snm = ons.ranges$snm[i],
                          cols = ons.ranges$cols[i], header.row = ons.ranges$date.r[i],
                          data.rows = ons.ranges$by.cause[i]), c("resp", "date"))
    }, simplify = F)))

    this.year <- setNames(read.ons(fnm = ons.ranges$fnm[6], snm = ons.ranges$snm[6],
             cols = ons.ranges$cols[6], header.row = ons.ranges$date.r[6],
             data.rows = ons.ranges$by.cause[6]),
             c("resp", "c19", "date"))

    data.frame("date" = c(hist$date, this.year$date),
               "resp" = c(hist$resp, this.year$resp),
               "c19" = c(rep(NA, nrow(hist)), this.year$c19))
}


#' Occurrences by age
#'
#' @param fnm File name to load from. Default is "./ONS/publishedweek152020.xlsx"
#' @param snm Sheet name or index. Default is "Covid-19 - Weekly occurrences"
#' @param cols Columns to read. Default is "C:BC"
#' @param header.row Row containing dates. Default is "6"
#' @param m.rows Rows to read for male mortality. Default is "34:53"
#' @param f.rows Rows to read for female mortality. Default is "56:75"
#'
#' @export
#'
occurrences.by.age <- function(fnm = "./ONS/publishedweek182020.xlsx",
                          snm = "Covid-19 - Weekly occurrences",
                          cols = "C:BC", header.row = "6", m.rows = "34:53", f.rows = "56:75") {

    df <- data.frame("cv19.m" = rowSums(read.ons(fnm = fnm, snm = snm, cols = cols,
                                                 header.row = header.row,
                                                 data.rows = m.rows, rotate = T),
                                        na.rm = T),
                     "cv19.f" = rowSums(read.ons(fnm = fnm, snm = snm, cols = cols,
                                                 header.row = header.row,
                                                 data.rows = f.rows, rotate = T),
                                        na.rm = T))
    df$pop.age <- rownames(df)
    df <- merge(df, ages(), by.x = "pop.age", by.y = "org")

    df <- aggregate(. ~ age, data = df[,-1], FUN = sum, na.action = na.pass)
    df <- merge(df, ons.popn(), by = "age")
    df <- df[order(as.integer(gsub("\\+", "", gsub("-.+","", df$age)))),]

    df$prop.m <- df$cv19.m / df$pop.m / 1000 * 100
    df$prop.f <- df$cv19.f / df$pop.f / 1000 * 100
    df
}
