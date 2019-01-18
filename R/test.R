# sheet.csvに変数名を適当につけるテストプログラム
# Created date: 2019/1/18
# Author: mariko ohtsuka
# Initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
parent_path <- "/Users/admin/Desktop/NHOH-R-miniCHP"
# Setting of input/output path
input_path <- paste0(parent_path, "/input")
external_path <<- paste0(parent_path, "/external")
# If the output folder does not exist, create it
output_path <- paste0(parent_path, "/output")
output_dst_path <- paste0(output_path, "/dst")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
if (file.exists(output_dst_path) == F) {
  dir.create(output_dst_path)
}
# Input sheet.csv, delete rows that 'variable' is NA
sheet_csv <- read.csv(paste0(external_path, "/sheet.csv"), as.is=T, na.strings="", fileEncoding="utf-8",
                      stringsAsFactors=F)
for (i in 1:nrow(sheet_csv)) {
  if (!is.na(sheet_csv[i, "FieldItem.label"])) {
    sheet_csv[i, "variable"] <- paste(sheet_csv[i, "Sheet.alias_name"], sheet_csv[i, "FieldItem.label"], i, sep="_")
  }
}
write.csv(sheet_csv, paste0(external_path, "/testsheet.csv"), na='""', row.names=F)

