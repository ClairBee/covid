
#' Refresh PHE data & plots
#'
#' Download latest data from PHE & produce pdf of standard plots
#'
#' @param highlight Vector of regions to highlight. Default is c("Hertfordshire", "Norfolk")
#' @param hcols Vector of colours to use for highlighted regions. Default is c("black", "orange")
#' @param archive Boolean: copy data to hard drive? Default is F
#'
#' @export
#'
phe.daily <- function(highlight = c("Hertfordshire", "Norfolk"),
                      hcols = c("black", "red3"),
                      archive = F) {

    tw.data()
    if(archive) {
        invisible(file.copy(from = "~/PhD/Misc-notes/Covid-19/data/phe-data.csv",
                            to = "~/PhD/Misc-notes/Covid-19/data/phe-data-prev.csv"),
                  overwrite = T)
        write.csv(uk.data, "~/PhD/Misc-notes/Covid-19/data/phe-data.csv", row.names = F)
    }

    ukd <- uk.data[uk.data$Country == "England",]
    mdt <- format(max(ukd$Date, na.rm = T), "%Y-%m-%d")

    makepdf("~/PhD/Misc-notes/Covid-19/plots/phe.pdf", width = 14, {

        par(mfrow = c(1,2))

        # daily
        {
            plot(ukd$Date, ukd$NewCases, type = "n",
                 xlim = as.Date(c("2020-03-01", mdt)),
                 ylim = c(0,max(ukd$NewCases[substr(ukd$AreaCode,1,1) == "E"], na.rm = T)),
                 xlab = "date", ylab = "New cases",
                 main = "Daily confirmed cases (England)")
            title(main = paste("   Downloaded on", mdt),
                  cex.main = 0.8, line = 0.5)

            invisible(sapply(unique(uk.data$Area[substr(uk.data$AreaCode, 1, 3) == "E09"]), function(brgh) {
                lines(uk.data$Date[uk.data$Area == brgh], uk.data$NewCases[uk.data$Area == brgh],
                      type = "o", pch = 20, cex = 0.5, col = transp("grey"))
            }))

            invisible(sapply(unique(uk.data$Area[substr(uk.data$AreaCode, 1, 2) == "E1"]), function(cty) {
                lines(uk.data$Date[uk.data$Area == cty], uk.data$NewCases[uk.data$Area == cty],
                      type = "o", pch = 20, cex = 0.5, col = transp("steelblue"))
            }))

            sapply(1:length(highlight), function(i) {
                lines(ukd$Date[ukd$Area == highlight[i]],
                      ukd$NewCases[ukd$Area == highlight[i]],
                      type = "o", pch = 20, cex = 0.5, lwd = 2, col = hcols[i])
            })

            legend("topleft", lty = 1, pch = 20, pt.cex = 0.5, bty = "n", cex = 0.8,
                   legend = c(highlight, "Greater London", "Other counties"),
                   col = c(hcols, transp("grey"), transp("steelblue")))

        }

        # Map recent
        {
            en <- uk.data
            en$mcty <- en$AreaCode
            en$mcty[en$mcty == "E06000058"] <- "E06000028"      # Bournemouth
            en$mcty[en$mcty == "E08000037"] <- "E06000020"      # Gateshead
            en$mcty[en$mcty == "E06000057"] <- "E06000048"      # Northumberland
            en$mcty[en$mcty == "E06000059"] <- "E10000009"      # Dorset

            lwk <- aggregate(NewCases ~ mcty, data = en[en$Date >= max(en$Date) - 7,], FUN = "sum")
            lwk <- lwk[lwk$NewCases >= 0,]

            nc.int <- c(0,0.9, 5,10,20,30,40,50,100,999)
            nc.col <- c(NA, rbow(length(nc.int)-1))

            lwk <- merge(lwk, read.csv("~/PhD/Misc-notes/Covid-19/data/county-px.csv"),
                         by.x = "mcty", by.y = "code", all.x = T)
            lwk$col <- nc.col[findInterval(lwk$NewCases, nc.int)]

            map("world", region = "uk", mar = c(1,1,2,1), ylim = c(50,56))
            points(lwk$x, lwk$y, pch = 20, col = lwk$col, cex = 0.8)
            legend("topright", legend = paste(round(nc.int)[1:(length(nc.int)-1)],
                                              round(nc.int)[2:(length(nc.int))], sep = "-"),
                   col = nc.col[-1], pch = 20)
            title(main = paste("Confirmed cases ", format(max(en$Date) - 7, "%b %d"), "-",  format(max(en$Date), "%b %d")))

        }

        # cumulative
        {
            # plot(1, type = "n", log = "y",
            #      xlim = range(0, ukd$c100days, na.rm = T),
            #      ylim = c(1, max(ukd$TotalCases, na.rm = T)),
            #      xlab = "Days since 100th case in UK",
            #      ylab = "Cumulative confirmed cases",
            #      main = "Confirmed cases per county (England)")
            #
            # # London boroughs
            # sapply(unique(ukd$Area[substr(ukd$AreaCode,3,3) == "9"]), function(brgh) {
            #     lines(ukd$c100days[ukd$Area == brgh],
            #           ukd$TotalCases[ukd$Area == brgh],
            #           type = "o", pch = 20, cex = 0.5, col = transp("grey"))
            # })
            #
            # sapply(unique(ukd$Area[substr(ukd$AreaCode,2,2) == "1"]), function(cty) {
            #     lines(ukd$c100days[ukd$Area== cty],
            #           ukd$TotalCases[ukd$Area == cty],
            #           type = "o", pch = 20, cex = 0.5,
            #           col = transp("steelblue"))
            # })
            #
            # sapply(1:length(highlight), function(i) {
            #     lines(ukd$c100days[ukd$Area == highlight[i]],
            #           ukd$TotalCases[ukd$Area == highlight[i]],
            #           type = "o", pch = 20, cex = 0.5, lwd = 2, col = hcols[i])
            # })
            #
            # legend("bottomright", lty = 1, pch = 20, pt.cex = 0.5, bty = "n", cex = 0.8,
            #        legend = c(highlight, "Greater London", "Other counties"),
            #        col = c(hcols, transp("grey"), transp("steelblue")))
            # title(main = paste("   Downloaded on", mdt),
            #       cex.main = 0.6, line = -1, adj = 0)
        }

    })
}
