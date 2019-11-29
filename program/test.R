# *** ptdataの出力が想定通りであればSASとバリデーション *** 
#' @title EditPath
#' @description クリップボードにコピーしたパスの区切り文字を変換する
#' @description Windowsでのみ動作
EditPath <- function(){
  input_path <- readClipboard(format=1, raw=F)
  print(input_path)
  return(gsub("\\\\", "/", input_path))
}
EditPath()
# ctrl+A -> RUN
# ↓のinputフォルダを使ってptosh-formatを実行し、その結果が既存の出力ファイルと同一ならば出力が想定通りであると判断する
target_path <- "//aronas/Datacenter/Users/ohtsuka/ptosh_format_test_tool/test_1"
target_output <- paste0(target_path, "/test_output")
# テスト対象のフォルダパス
input_path <- "C:/Users/Mariko/Documents/GitHub/ptosh-format/ads"
# ------ function ------
testdiff <- function(test_ptdata_1, test_ptdata_2){
  print(paste0("___test_start___"))
  if (nrow(test_ptdata_1) == nrow(test_ptdata_2)){
    print("row_count_ok")
  } else {
    print("!!!row_count_ng!!!")
    return(-1)
  }
  if (ncol(test_ptdata_1) == ncol(test_ptdata_2)){
    print("col_count_ok")
  } else {
    print("!!!col_count_ng!!!")
    return(-1)
  }
  for (i in 1:nrow(test_ptdata_1)){
    for (j in 1:ncol(test_ptdata_1)){
      if (is.na(test_ptdata_1[i, j])){
        if (!is.na(test_ptdata_2[i, j])){
          print(paste("col", j, "row", i, test_ptdata_1[i, j], test_ptdata_2[i, j]))
        }
      } else {
        if (is.na(test_ptdata_2[i, j])){
          print(paste("col", j, "row", i, test_ptdata_1[i, j], test_ptdata_2[i, j]))
        } else {
          if (test_ptdata_1[i, j] != test_ptdata_2[i, j]){
            print(paste("col", j, "row", i, test_ptdata_1[i, j], test_ptdata_2[i, j]))
          }
        }
      }
    }
  }
  print(paste0("___test_end___"))
  return(0)
}
# ------ main ------
test_ptdata_1 <- read.csv(paste0(input_path, "/ptdata.csv"), na.strings = "NA")
test_ptdata_2 <- read.csv(paste0(target_output, "/test_1_ptdata_labeled.csv"), na.strings = "NA")
print("グループあり、ラベルありでテスト_ptdata")
tmp <- testdiff(test_ptdata_1, test_ptdata_2)

ptdatarda <- paste0(input_path, "/ptdata.Rda")
load(ptdatarda)
test_ptdata_3 <- ptdata
# factorを数値に変換する
test <- sapply(test_ptdata_3, class)
test <- test[which(test == "factor")]
test <- names(test)
if (length(test) > 0){
  for (i in 1:length(test)){
    test_ptdata_3[ ,test[i]] <- as.numeric(test_ptdata_3[ ,test[i]])
  }
}
test_ptdata_4 <- read.csv(paste0(target_output, "/test_1_ptdata_num.csv"), stringsAsFactors=F, na.strings="")
print("グループあり、ラベルなしでテスト_ptdata")
tmp <- testdiff(test_ptdata_3, test_ptdata_4)
