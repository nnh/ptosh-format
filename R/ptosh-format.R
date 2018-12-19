#症例登録番号(I列)をキーにしてループを回す（繰り返しシートの可能性があるため）。
#変数名が定義されているフィールド番号を含む列名のデータに対し、マッピング上の変数名にて読み込む。
#SASでデータの次の列の文字列をFORMATとしてあてる。
#RであればValue Labelとして次の列の文字列をfactorとしてあてる(*)
#sheet.alias_nameのループが終わればデータセットを出力して終了。

# Format Ptosh data for analysis program
# 作成日: 2018/12/19
# 作成者: mariko ohtsuka

# library, function section ------
exit <- function(){
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
# constant section ------
# main section ------

# Initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
parent_path <- "/Users/admin/Desktop/NHOH-R-miniCHP"
# Setting of input/output path
input_path <<- paste0(parent_path, "/input")
external_path <<- paste0(parent_path, "/external")
# If the output folder does not exist, create it
output_path <<- paste0(parent_path, "/output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# input sheet.csv
sheet_csv <- read.csv(paste0(external_path, "/testsheet.csv"), as.is=T, fileEncoding="utf-8", stringsAsFactors=F)
# Check for duplicate 'variable'
# Check overlap from the beginning and end, OR of both
df_duplicated <- sheet_csv[duplicated(sheet_csv$variable) | duplicated(sheet_csv$variable, fromLast=T), ]
if (nrow(df_duplicated) != 0) {
  df_duplicated <- cbind(rownames(df_duplicated), df_duplicated)
  names(df_duplicated)[1] <- "row"
  write.csv(df_duplicated, paste0(output_path, "/variable_duplicated.csv"), row.names=F, fileEncoding="cp932")
  stop("not numeric !")
  exit()
}
# input ptosh_csv
alias_name <- sheet_csv[!duplicated(sheet_csv[1]), 1]
file_list <- list.files(input_path)
for (i in 1:length(alias_name)) {
  file_index <- grep(paste0("_", alias_name[i] , "_"), file_list)
  if (length(file_index) > 0) {
    assign(alias_name[i], read.csv(paste(input_path, file_list[file_index], sep="/"), as.is=T, na.strings="",
                                   fileEncoding="cp932"))
  }
}
