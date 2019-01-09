mydat <- read.csv("~/graphs_and_csvs/sizes.csv")
x_val <- paste(mydat$database, mydat$action, mydat$filesystem, sep = " ")
jpeg("blktrace Size for different configurations.jpg")
bp <- barplot(height = mydat$blktrace_size, 
        names.arg = x_val, cex.names = 0.65,
        main = "blktrace Size VS Configurations", 
        xlab = "Configurations", 
        ylab = "blktrace Size",
        ylim = c(0, 250),
        col = "black")
text(bp, mydat$blktrace_size, labels = format(mydat$blktrace_size, 4), pos = 3, cex = .75)
dev.off()