# バリデーション
library(stringr)
library(haven)
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
              print(str_c("!!! NG row:", j, ", col:", temp_colname, "SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
            }  
          } else {
            if (is.na(temp_r[j, temp_colname])){
              print(str_c("!!! NG row:", j, ", col:", temp_colname, "SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
            } else {
              if (str_squish(temp_sas[j, temp_colname]) != str_squish(temp_r[j, temp_colname])){
                print(str_c("!!! NG row:", j, ", col:", temp_colname, " SAS:", temp_sas[j, temp_colname], ",R:", temp_r[j, temp_colname]))
              } else {
                #print("OK")
              }
            }
          }
        }
      }
    }
  }
}
ConvertIntoString <- function(df){
  test_r <- lapply(df, function(x){
    temp <- x
    if (class(x) == "numeric"){
      for (j in 1:length(temp)){
        if (!is.na(temp[j])){
          # 小数点以下が０なら切り上げ
          int_x <- trunc(temp[j])
          dec_x <- temp[j] - int_x
          if (dec_x == 0){
            temp[j] <- int_x
          }
        }
      }
    }
    return(as.character(temp))
  })
  test_r <- rbind(data.frame(), test_r, stringsAsFactors=F)
  test_r[is.na(test_r)] <- ""
  return(test_r)
}

input_sas_path <- "//aronas/Datacenter/Users/ohtsuka/validation_R_SAS/SAS_output/output"
input_r_path <- "//aronas/Datacenter/Users/ohtsuka/validation_R_SAS/R_output/output"
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
  temp_sas <- read.csv(str_c(input_sas_path, "/", csv[i]), stringsAsFactors=F)
  temp_r <- read.csv(str_c(input_r_path, "/", csv[i]), stringsAsFactors=F)
  print(str_c("csv_check_start_", csv[i]))
  CheckItems(temp_sas, temp_r)
  assign(str_c("SAS_",csv[i]), temp_sas)
  assign(str_c("R_",csv[i]), temp_r)
  print(str_c("csv_check_end_", csv[i]))
}
# Rdaとsas7datを比較
input_sas7bdat_path <- "//aronas/Datacenter/Users/ohtsuka/validation_R_SAS/SAS_output"
input_rda_path <- "//aronas/Datacenter/Users/ohtsuka/validation_R_SAS/R_output"
rda <- str_replace(csv, ".csv", "")
for (i in 1:length(rda)){
  load(str_c(input_rda_path, "/", rda[i], ".rda"))
  # 数値型の列名を取得
  num_col <- NULL
  for (j in 1:ncol(get(rda[i]))){
    if (class(get(rda[i])[ , j]) == "numeric"){
      num_col <- c(num_col, colnames(get(rda[i]))[j])
    }
  }
  test_r <- get(rda[i])
  test_r <- ConvertIntoString(test_r)
  assign(str_c("sas_", rda[i]), as.data.frame(read_sas(str_c(input_sas7bdat_path, "/", rda[i], ".sas7bdat"))))
  # 数値であるはずの列を数値型に変換
  test_sas <- get(str_c("sas_", rda[i]))
  if (length(num_col) > 0){
    for (j in 1:length(num_col)){
      test_sas[ , num_col[j]] <- as.numeric(test_sas[ , num_col[j]])
    }
  }
  test_sas <- ConvertIntoString(test_sas)
  print(str_c("obj_check_start_", rda[i]))
  CheckItems(test_sas, test_r)
  print(str_c("obj_check_end_", rda[i]))
}

# 列の差分チェック
#r_colname <- colnames(R_ptdata.csv)
#sas_colname <- colnames(SAS_ptdata.csv)
#OK <- intersect(r_colname, sas_colname)
#SAS_NASHI <- setdiff(r_colname, sas_colname)
#R_NASHI <- setdiff(sas_colname, r_colname)
#CheckItems(SAS_ptdata.csv[, OK], R_ptdata.csv[, OK])
