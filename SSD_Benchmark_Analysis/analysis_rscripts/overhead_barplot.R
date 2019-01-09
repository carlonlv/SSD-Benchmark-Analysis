mydat <- read.csv("~/graphs_and_csvs/overhead.csv")
x_val <- paste(mydat$database, mydat$action, mydat$filesystem, sep = "\n")
new_dat <- data.frame(x_val, mydat$no_blktrace, mydat$yes_blktrace)
names(new_dat) <- c("Configuration", "No blktrace", "Yes blktrace")
colour <- c("green", "red")
heights <- rbind(new_dat$`No blktrace`, new_dat$`Yes blktrace`)
jpeg("overhead of blktrace for different configurations.jpg")
bp <- barplot(height = heights, 
        names.arg = x_val, cex.names = 0.54, 
        main = "Overhead of blktrace", 
        xlab = "Configurations", 
        ylab = "Number of Operations per second",
        ylim = c(0, 65000),
        col = colour, 
        beside = TRUE)
legend("topleft", c("Without blktrace", "With blktrace"), cex= .75, bty="n", fill=colour)
text(bp, heights, labels = format(heights, 4), pos = 3, cex = .60)
dev.off()