require(data.table)
draw_plots <- function(lista, name_of_lista) {
  home_dir <- "/home/lyujialu/"
  file_extension <- ".csv"
  print(paste("Drawing", name_of_lista))
  pch_lst <- c(16, 17, 18)
  col_lst <- c("darkblue", "darkred", "darkgreen")
  jpeg_filename <- paste(name_of_lista, "_scatter.jpg", sep = "")
  jpeg(jpeg_filename)
  xlima <- 0
  ylima <- 1
  for (i in c(1,2,3)) {
    file_name <- lista[i]
    file_name <- lista[i]
    print(paste("Importing data:", file_name))
    file_path <- paste(home_dir, file_name, file_extension, sep = "")
    mydat <- fread(file_path)
    print(paste("----There are", length(mydat$frequency), "data points in", file_name))
    xlima <- range(xlima, length(mydat$frequency))
    ylima <- range(ylima, mydat$frequency)
  }
  print(paste("xlim is", xlima))
  print(paste("ylim is", ylima))
  plot(xlima, ylima, type = "n", 
       xlab = "virtual page num", ylab = "frequencies", 
       main=paste("Scatter plot of" , name_of_lista))
  print("Adding lines.")
  for (i in c(1,2,3)) {
    file_name <- lista[i]
    file_path <- paste(home_dir, file_name, file_extension, sep = "")
    mydat <- fread(file_path)
    print("Creating x value.")
    virtual_page_num <- seq(from = 1, to = length(mydat$frequency), by = 1)
    print("Sorting frequency.")
    frequencies <- sort(mydat$frequency, decreasing = TRUE)
    print(paste("Plotting", file_name))
    lines(virtual_page_num, frequencies, type = "o",
          lty= i + 1, col=col_lst[i], pch=pch_lst[i])
  }
  legend("topright", c("btrfs_run_150M_300M","ext4_run_250M_1B","f2fs_run_250M_1B"), 
         col = col_lst, pch = pch_lst, lty = c(2,3,4))
  dev.off()
  
  snd_jpeg_filename <- paste(name_of_lista, "_log_scatter.jpg", sep = "")
  jpeg(snd_jpeg_filename)
  print("Plotting log scatter plot.")
  plot(xlima, ylima, type = "n", 
       xlab = "virtual page num", ylab = "frequencies", 
       main=paste("Scatter plot of" , name_of_lista), log = 'y')
  print("Adding lines.")
  for (i in c(1,2,3)) {
    file_name <- lista[i]
    file_path <- paste(home_dir, file_name, file_extension, sep = "")
    mydat <- fread(file_path)
    print("Creating x value.")
    virtual_page_num <- seq(from = 1, to = length(mydat$frequency), by = 1)
    print("Sorting frequency.")
    frequencies <- sort(mydat$frequency, decreasing = TRUE)
    print(paste("Plotting", file_name))
    lines(virtual_page_num, frequencies, type = "o",
          lty= i + 1, col=col_lst[i], pch=pch_lst[i])
  }
  legend("topright", c("btrfs_run_150M_300M","ext4_run_250M_1B","f2fs_run_250M_1B"), 
         col = col_lst, pch = pch_lst, lty = c(2,3,4))
  dev.off()
}

cassandra_reads <- c("cassandra_btrfs_run_150M_300M_read", 
                     "cassandra_ext4_run_250M_1B_read",
                     "cassandra_f2fs_run_250M_1B_read")

cassandra_writes <- c("cassandra_btrfs_run_150M_300M_write",
                      "cassandra_ext4_run_250M_1B_write",
                      "cassandra_f2fs_run_250M_1B_write")

cassandra_all <- c("cassandra_btrfs_run_150M_300M_all",
                   "cassandra_ext4_run_250M_1B_all",
                   "cassandra_f2fs_run_250M_1B_all")

rocksdb_reads <- c("rocksdb_btrfs_run_250M_1B_read",
                   "rocksdb_ext4_run_250M_1B_read",
                   "rocksdb_f2fs_run_250M_1B_read")

rocksdb_writes <- c("rocksdb_btrfs_run_250M_1B_write",
                    "rocksdb_ext4_run_250M_1B_write",
                    "rocksdb_f2fs_run_250M_1B_write")

rocksdb_all <- c("rocksdb_btrfs_run_250M_1B_all",
                 "rocksdb_ext4_run_250M_1B_all",
                 "rocksdb_f2fs_run_250M_1B_all")

draw_plots(cassandra_reads, "cassandra_reads")
draw_plots(cassandra_writes, "cassandra_writes")
draw_plots(cassandra_all, "cassandra_all")
draw_plots(rocksdb_reads, "rocksdb_reads")
draw_plots(rocksdb_writes, "rocksdb_writes")
draw_plots(rocksdb_all, "rocksdb_all")