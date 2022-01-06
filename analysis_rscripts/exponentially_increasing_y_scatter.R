require(data.table)
draw_plots <- function(file_name) {
  print(paste("Importing data:", file_name))
  home_dir <- "/home/lyujialu/"
  file_extension <- ".csv"
  file_path <- paste(home_dir, file_name, file_extension, sep = "")
  mydat <- fread(file_path)
  print(paste("----There are", length(mydat$frequency), "data points in", file_name))
  print("Creating x value.")
  virtual_page_num <- seq(from = 1, to = length(mydat$frequency), by = 1)
  print("Sorting frequency.")
  frequencies <- sort(mydat$frequency, decreasing = TRUE)
  head(frequencies)
  print("Creating jpeg file.")
  jpeg_filename <- paste(file_name, "_scatter.jpg", sep = "")
  jpeg(jpeg_filename)
  print("Plotting scatter plot.")
  plot(virtual_page_num, frequencies, main = paste("Scatter plot of" , file_name), log = 'y')
  print("Done plotting")
  dev.off()
  print("Creating 2nd jpeg file")
  snd_jpeg_filename <- paste(file_name, "_density_scatter.jpg", sep = "")
  jpeg(snd_jpeg_filename)
  print("Plotting density plot.")
  smoothScatter(virtual_page_num, frequencies, main = paste("Density Scatter Plot of" , file_name), log = 'y')
  dev.off()
  print("Creating 3rd jpeg file.")
  trd_jpeg_filename <- paste(file_name, "_hist.jpg", sep = "")
  jpeg(trd_jpeg_filename)
  print("Plotting histagram.")
  hist(frequencies, main = "Histagram of Frequency")
  dev.off()
  print("Done")
}
list_of_file_names <- c("cassandra_btrfs_run_150M_300M_all", 
                        "cassandra_f2fs_run_250M_1B_read", 
                        "rocksdb_ext4_run_250M_1B_read", 
                        "cassandra_btrfs_run_150M_300M_read", 
                        "cassandra_f2fs_run_250M_1B_write", 
                        "rocksdb_ext4_run_250M_1B_write", 
                        "cassandra_btrfs_run_150M_300M_write", 
                        "rocksdb_f2fs_run_250M_1B_all", 
                        "cassandra_ext4_run_250M_1B_all", 
                        "rocksdb_btrfs_run_250M_1B_all", 
                        "rocksdb_f2fs_run_250M_1B_read", 
                        "cassandra_ext4_run_250M_1B_read", 
                        "rocksdb_btrfs_run_250M_1B_read", 
                        "rocksdb_f2fs_run_250M_1B_write", 
                        "cassandra_ext4_run_250M_1B_write", 
                        "rocksdb_btrfs_run_250M_1B_write", 
                        "cassandra_f2fs_run_250M_1B_all", 
                        "rocksdb_ext4_run_250M_1B_all")
for (a in list_of_file_names) {
  draw_plots(a)
}
