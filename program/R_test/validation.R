# バリデーション
library(stringr)
CheckItems <- function(temp_sas, temp_r){
  if (nrow(temp_sas) != nrow(temp_r)){
    print(str_c("!!! NG row count !!!", "sas:", nrow(temp_sas), ", R:", nrow(temp_r)))
  } else {
    if (ncol(temp_sas) != ncol(temp_r)){
      print(str_c("!!! NG col count !!!", "sas:", ncol(temp_sas), ", R:", ncol(temp_r)))
    } else {
      # 各項目の比較
      for (i in 1:ncol(temp_sas)){
        temp_colname <- colnames(temp_sas)[i]
        for (j in 1:nrow(temp_sas)){
          if (is.na(temp_sas[j, temp_colname])){
            if (!is.na(temp_r[j, temp_colname])){
              print(str_c("!!! NG row:", j, ", col:", i, "SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
            }  
          } else {
            if (is.na(temp_r[j, temp_colname])){
              print(str_c("!!! NG row:", j, ", col:", i, "SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
            } else {
              if (temp_sas[j, temp_colname] != temp_r[j, temp_colname]){
                print(str_c("!!! NG row:", j, ", col:", i, "SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
              } else {
#                print("OK")
              }
            }
          }
        }
      }
    }
  }
}

input_sas_path <- "C:/Users/Mariko/Desktop/SAS_ads"
input_r_path <- "C:/Users/Mariko/Desktop/R_ads"
# SASとRのCSVを比較
sas_all_files <- list.files(input_sas_path)
r_all_files <- list.files(input_r_path)
sas_csv <- str_subset(sas_all_files, "^*.csv$")
# contentsは比較対象外
sas_csv <- sas_csv[!(str_detect(sas_csv, "^*contents.csv"))]
r_csv <- str_subset(r_all_files, "^*.csv$")
# output_option_csv.csv, output_sheet_csv.csvは比較対象外
r_csv <- r_csv[!(str_detect(r_csv, "output_option_csv.csv|output_sheet_csv.csv"))]
only_sas <- setdiff(sas_csv, r_csv)
only_r <- setdiff(r_csv, sas_csv)
csv <- intersect(sas_csv, r_csv)
for (i in 1:length(csv)){
  temp_sas <- read.csv(str_c(input_sas_path, "/", csv[i]))
  temp_r <- read.csv(str_c(input_r_path, "/", csv[i]))
  print(str_c("csv_check_start_", csv[i]))
  CheckItems(temp_sas, temp_r)
  assign(str_c("SAS_",csv[i]), temp_sas)
  assign(str_c("R_",csv[i]), temp_r)
  print(str_c("csv_check_end_", csv[i]))
}

