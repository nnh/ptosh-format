# input配下を症例登録番号でソートして吐き出すテストプログラム
# Created date: 2019/1/22
# Author: mariko ohtsuka
# Initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
parent_path <- "/Users/admin/Desktop/NHOH-R-miniCHP"
# Setting of input/output path
input_path <- paste0(parent_path, "/input")
test_output_path <- paste0(parent_path, "/test")
file_list <- list.files(input_path)
for (i in 1:length(file_list)) {
  input_df <- read.csv(paste(input_path, file_list[i], sep="/"), as.is=T, na.strings="", fileEncoding="cp932")
  sortlist <- order(input_df[ , "症例登録番号"])
  output_df <- input_df[sortlist, ]
  write.csv(output_df, paste0(test_output_path, "/", file_list[i]), na='""', row.names=F, fileEncoding="cp932")
}
