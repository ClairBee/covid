
#' ONS weekly plots
#'
#' @export
#'
ons.weekly <- function() {

    org.wd <- getwd()
    setwd("~/PhD/Misc-notes/Covid-19")

    # deaths by cause
    {
        by.cause <- merge(ons.totals(), ons.by.cause(), by = "date", all = T)
        by.cause$nc19 <- by.cause$total - by.cause$c19
        by.cause$nc19[is.na(by.cause$nc19)] <- by.cause$total[is.na(by.cause$nc19)]

        by.cause$nresp <- by.cause$nc19 - by.cause$resp

        by.cause$year <- format(by.cause$date, "%Y")
        by.cause$day <- by.cause$date - as.Date(paste0(by.cause$year, "-01-01"))
        by.cause$yday <- as.Date("2020-01-01") + by.cause$day
    }

    # Covid-19 deaths by age group
    by.age <- occurrences.by.age()
    by.age$pd.m <- by.age$cv19.m / sum(by.age$cv19.m, by.age$cv19.f) * 100
    by.age$pd.f <- by.age$cv19.f / sum(by.age$cv19.m, by.age$cv19.f) * 100

    # Covid-19 deaths by region
    by.region <- read.ons(fnm = max(list.files("./ONS", pattern = "2020", full.names = T)),
                   snm = "Covid-19 - Weekly occurrences",
                   cols = "C:BC", header.row = "6", data.rows = "77:85", rotate = F)


    makepdf("./plots/ons.pdf", height = 7, width = 14, {
        par(mfrow = c(1,2))

        # total weekly deaths
        {
            plot(by.cause$yday[by.cause$year == "2015"], by.cause$total[by.cause$year == "2015"],
                 type = "o", pch = 20, cex = 0.7, col = rbow(6)[1], ylim = range(000,by.cause$total, na.rm = T),
                 xlab = "Date", ylab = "Weekly deaths",
                 main = "All deaths registered")
            invisible(sapply(2:6, function(i) {
                y <- unique(by.cause$year)[i]
                lines(by.cause$yday[by.cause$year == y], by.cause$total[by.cause$year == y],
                      type = "o", pch = 20, cex = 0.7, col = rbow(6)[i])
            }))
            legend("topright", legend = 2015:2020, col = rbow(6), lty = 1, pch = 20, bty = "n", ncol = 2)
        }

        # non-covid weekly deaths
        {
            plot(by.cause$yday[by.cause$year == "2015"], by.cause$nc19[by.cause$year == "2015"],
                 type = "o", pch = 20, cex = 0.7, col = rbow(6)[1], ylim = range(000,by.cause$nc19, na.rm = T),
                 xlab = "Date", ylab = "Deaths reported not mentioning Covid-19",
                 main = "Weekly deaths not mentioning Covid-19")
            invisible(sapply(2:6, function(i) {
                y <- unique(by.cause$year)[i]
                lines(by.cause$yday[by.cause$year == y], by.cause$nc19[by.cause$year == y],
                      type = "o", pch = 20, cex = 0.7, col = rbow(6)[i])
            }))
            lines(by.cause$yday[by.cause$year == "2020"], by.cause$c19[by.cause$year == "2020"],
                  type = "o", pch = 4, cex = 0.8, lty = "22")
            legend("topright", legend = 2015:2020, col = rbow(6), lty = 1, pch = 20, bty = "n", ncol = 2)
        }

        # weekly deaths involving non-covid respiratory conditions
        {
            plot(by.cause$yday[by.cause$year == "2015"], by.cause$resp[by.cause$year == "2015"],
                 type = "o", pch = 20, cex = 0.7, col = rbow(6)[1], ylim = range(000,by.cause$resp, na.rm = T),
                 xlab = "Date", ylab = "Weekly deaths registered",
                 main = "Weekly deaths from non-Covid respiratory conditions")
            invisible(sapply(2:6, function(i) {
                y <- unique(by.cause$year)[i]
                lines(by.cause$yday[by.cause$year == y], by.cause$resp[by.cause$year == y],
                      type = "o", pch = 20, cex = 0.7, col = rbow(6)[i])
            }))
            lines(by.cause$yday[by.cause$year == "2020"], by.cause$c19[by.cause$year == "2020"],
                  type = "o", pch = 4, cex = 0.8, lty = "22")
            legend("topright", legend = 2015:2020, col = rbow(6), lty = 1, pch = 20, bty = "n", ncol = 2)
        }

        # weekly deaths involving neither covid nor other respiratory conditions
        {
            plot(by.cause$yday[by.cause$year == "2015"], by.cause$nresp[by.cause$year == "2015"],
                 type = "o", pch = 20, cex = 0.7, col = rbow(6)[1], ylim = range(000,by.cause$nresp, na.rm = T),
                 xlab = "Date", ylab = "Weekly deaths registered",
                 main = "Weekly deaths from neither Covid nor other respiratory conditions")
            invisible(sapply(2:6, function(i) {
                y <- unique(by.cause$year)[i]
                lines(by.cause$yday[by.cause$year == y], by.cause$nresp[by.cause$year == y],
                      type = "o", pch = 20, cex = 0.7, col = rbow(6)[i])
            }))
            lines(by.cause$yday[by.cause$year == "2020"], by.cause$c19[by.cause$year == "2020"],
                  type = "o", pch = 4, cex = 0.8, lty = "22")
            legend("topright", legend = 2015:2020, col = rbow(6), lty = 1, pch = 20, bty = "n", ncol = 2)
        }


        # weekly deaths by region
        {
            matplot(by.region[,10], by.region[,1:9], type = "o", lty = 1, pch = 20, col = rbow(9),
                    cex = 0.6, xlim = range(by.region$date[!is.na(by.region$London)]),
                    ylab = "Deaths occurred", xlab = "Date",
                    main = "Weekly deaths from Covid-19 per region")
            legend("topleft", colnames(by.region)[1:9], col = rbow(9), lty = 1, pch = 20, cex = 0.8,
                   bty = "n", ncol = 2)

            plot.new()
        }


        # mortality by age group
        {
            barplot(by.age$prop.m, names.arg = by.age$age, col = "olivedrab", las = 2,
                    main = "Covid-19 mortality rate by age group",
                    xlab = "", ylab = "Percentage of age group dead")
            abline(h = seq(0,5,0.5), col = transp("dimgrey"))
            barplot(by.age$prop.m, col = "olivedrab", add = T, las = 2)
            barplot(by.age$prop.f, add = T, col = "darkgoldenrod2", las = 2)
            legend("left", fill = c("olivedrab", "gold"), legend = c("Male", "Female"), bg = "white")
        }

        # total proportion of Covid-19 deaths in each age group
        {
            barplot(by.age$pd.f, xlim = max(by.age$pd.f, by.age$pd.m) * c(-1,1),
                    horiz = T, xaxt = "n", las = 1, names.arg = by.age$age,
                    main = "Proportion of Covid-19 deaths in each age group",
                    ylab = "", xlab = "Percentage of Covid-19 deaths")
            abline(v = seq(-100,100,5), col = transp("dimgrey"))
            axis(1, at = seq(-100,100,10), labels = abs(seq(-100,100,10)))
            barplot(by.age$pd.f, add = T, col = "darkgoldenrod2", horiz = T, xaxt = "n")
            barplot(-by.age$pd.m, add = T, col = "olivedrab", horiz = T, xaxt = "n")

            legend("right", fill = c("olivedrab", "gold"), legend = c("Male", "Female"), bg = "white")
        }
    })
}
