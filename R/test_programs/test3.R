# test配下のファイルの症例登録番号を揃えて結合するテストプログラム
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
  sort_df <- input_df[sortlist, ]
  if (length(grep("ae_", file_list[i])) == 0 && (length(grep("committees_opinion", file_list[i])) == 0)) {
    cnt <- 1
    output_df <- data.frame(matrix(rep(NA, ncol(sort_df)), nrow=1))[numeric(0), ]
    colnames(output_df) <- colnames(sort_df)
    for (j in 1:nrow(sort_df)) {
        while (sort_df[j, "症例登録番号"] != cnt) {
          output_df <- rbind(output_df, rep(NA, ncol(output_df)))
          colnames(output_df) <- colnames(sort_df)
          cnt <- cnt + 1
        }
        colnames(output_df) <- colnames(sort_df)
        output_df <- rbind(output_df, sort_df[j, ])
        cnt <- cnt + 1
    }
    if (nrow(output_df) == 43) {
      output_df <- rbind(output_df, rep(NA, ncol(output_df)))
      colnames(output_df) <- colnames(sort_df)
    }
  }
  df_name <- strsplit(file_list[i], "_")
  assign(unlist(df_name)[2], output_df)
  write.csv(output_df, paste0(test_output_path, "/", file_list[i]), na='""', row.names=F, fileEncoding="cp932")
}
# 11列目までは不要
ptdata <- registration$症例登録番号
registration <- registration[, -c(1:11)]
ptdata <- cbind(ptdata, registration)
flowsheet1 <- flowsheet1[, -c(1:11)]
flowsheet2 <- flowsheet2[, -c(1:11)]
flowsheet3 <- flowsheet3[, -c(1:11)]
flowsheet4 <- flowsheet4[, -c(1:11)]
flowsheet5 <- flowsheet5[, -c(1:11)]
flowsheet6 <- flowsheet6[, -c(1:11)]
initial <- initial[, -c(1:11)]
initial2 <- initial2[, -c(1:11)]
initial3 <- initial3[, -c(1:11)]
followup <- followup[, -c(1:11)]
cancel <- cancel[, -c(1:11)]
ptdata <- cbind(ptdata, flowsheet1)
ptdata <- cbind(ptdata, initial)
ptdata <- cbind(ptdata, flowsheet2)
ptdata <- cbind(ptdata, flowsheet3)
ptdata <- cbind(ptdata, flowsheet4)
ptdata <- cbind(ptdata, flowsheet5)
ptdata <- cbind(ptdata, flowsheet6)
ptdata <- cbind(ptdata, initial2)
ptdata <- cbind(ptdata, initial3)
ptdata <- cbind(ptdata, followup)
ptdata <- cbind(ptdata, cancel)
write.csv(ptdata, paste0(test_output_path, "/ptdata.csv"), na='""', row.names=F, fileEncoding="cp932")
