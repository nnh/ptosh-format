# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Author: mariko ohtsuka

# library, function section ------
#' Title
#'
#' @return
#' @export
#'
#' @examples
Exit <- function(){
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
CheckColname <- function(col_name, df){
  # Returns true if column "col_name" exists in data frame "df", false if it does not exist
  return(col_name %in% colnames(df))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
EditFactor <- function(df){
  dup_df <- df[!duplicated(df[1]), ]
  sortlist <- order(dup_df[1])
  sort_df <- dup_df[sortlist, ]
  return(sort_df)
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
  Exit()
}
# input ptosh_csv
alias_name <- sheet_csv[!duplicated(sheet_csv[1]), 1]
file_list <- list.files(input_path)
for (i in 1:length(alias_name)) {
  file_index <- grep(paste0("_", alias_name[i] , "_"), file_list)
  if (length(file_index) > 0) {
    assign(alias_name[i], read.csv(paste(input_path, file_list[file_index], sep="/"), as.is=T, na.strings="",
                                   fileEncoding="cp932"))
    #症例登録番号(I列)をキーにしてループを回す（繰り返しシートの可能性があるため）。
    #変数名が定義されているフィールド番号を含む列名のデータに対し、マッピング上の変数名にて読み込む。
    #SASでデータの次の列の文字列をFORMATとしてあてる。
    #RであればValue Labelとして次の列の文字列をfactorとしてあてる(*)
    df_itemlist <- subset(sheet_csv, sheet_csv[ ,1] == alias_name[i])
    df_sheet <- get(alias_name[i])
    temp_df_name <- paste0("temp_", alias_name[i])
    assign(temp_df_name, data.frame(id = df_sheet[9]))
    temp_df_sheet <- get(temp_df_name)
    for (j in 1:length(df_itemlist)) {
      column_name <- sheet_csv[j, "variable"]
      # Skip if there is no column with that name
      if (CheckColname(paste0("field", sheet_csv[j, 3]), df_sheet)) {
        temp_df_sheet <- cbind(temp_df_sheet, df_sheet[paste0("field", sheet_csv[j, 3])])
      }
      assign(temp_df_name, temp_df_sheet)
    }
  }
}
#sheet.alias_nameのループが終わればデータセットを出力して終了。
df_fact <- EditFactor(ae[c("field1", "有害事象名")])
temp_ae$field1 <- factor(temp_ae$field1, levels = df_fact[1], labels = df_fact[2])
