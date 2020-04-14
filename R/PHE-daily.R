
#' Refresh PHE data & plots
#'
#' Download latest data from PHE & produce pdf of standard plots
#' 
#' 
phe.daily <- function(highlight = c("Hertfordshire", "Norfolk"),
                      hcols = c("black", "orange"),
                      archive = F) {
    
    tw.data()
    if(archive) {
        invisible(file.copy(from = "~/PhD/Misc-notes/Covid-19/data/phe-data.csv",
                            to = "~/PhD/Misc-notes/Covid-19/data/phe-data-prev.csv"))
        write.csv(uk.data, "~/PhD/Misc-notes/Covid-19/data/phe-data.csv", row.names = F)
    }
    
    ukd <- uk.data[uk.data$Country == "England",]
    mdt <- format(max(ukd$Date, na.rm = T), "%Y-%m-%d")
    
    makepdf(paste0("~/PhD/Misc-notes/Covid-19/plots/UK-", mdt,".pdf"), {
        
        plot(1, type = "n", log = "y",
             xlim = range(0, ukd$c100days, na.rm = T),
             ylim = c(1, max(ukd$TotalCases, na.rm = T)),
             xlab = "Days since 100th case in UK",
             ylab = "Cumulative confirmed cases",
             main = "Confirmed cases per county (England)")
        
        # London boroughs
        sapply(unique(ukd$Area[substr(ukd$AreaCode,3,3) == "9"]), function(brgh) {
            lines(ukd$c100days[ukd$Area == brgh],
                  ukd$TotalCases[ukd$Area == brgh],
                  type = "o", pch = 20, cex = 0.5, col = transp("grey"))
        })
        
        sapply(unique(ukd$Area[substr(ukd$AreaCode,2,2) == "1"]), function(cty) {
            lines(ukd$c100days[ukd$Area== cty],
                  ukd$TotalCases[ukd$Area == cty],
                  type = "o", pch = 20, cex = 0.5,
                  col = transp("steelblue"))
        })
        
        sapply(1:length(highlight), function(i) {
            lines(ukd$c100days[ukd$Area == highlight[i]],
                  ukd$TotalCases[ukd$Area == highlight[i]],
                  type = "o", pch = 20, cex = 0.5, lwd = 2, col = hcols[i])
        })
        
        legend("bottomright", lty = 1, pch = 20, pt.cex = 0.5, bty = "n", cex = 0.8,
               legend = c(highlight, "Greater London", "Other counties"),
               col = c(hcols, transp("grey"), transp("steelblue")))
        title(main = paste("   Downloaded on", mdt),
              cex.main = 0.6, line = -1, adj = 0)
    })
}
