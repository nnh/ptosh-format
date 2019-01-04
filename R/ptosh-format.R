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
#' @title
#' Checking if the column name exists
#' @description
#' Returns column index if column "col_name" exists in data frame "df", -1 if it does not exist
#' @param
#' col_name : String of column name
#' df : String of data frame name
#' @return
#' the column index : if "col_name" exists
#' -1 : if "col_name" is not exist
#' @examples
#' colnames(df_test) : "field1" "field3" "field5"
#' CheckColname("field3", "df_test") : return 2
#' CheckColname("field2", "df_test") : return -1
CheckColname <- function(col_name, df){
  res = -1
  if (col_name %in% colnames(df)) {
    res <- which(col_name == colnames(df))
  }
  return(res)
}
# Constant section ------
# Main section ------

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
# Input option.csv
option_csv <- read.csv(paste0(external_path, "/option.csv"), as.is=T, fileEncoding="utf-8", stringsAsFactors=F)
# Generate factor from option.csv
option_name_list <- option_csv[!duplicated(option_csv[1]), "Option.name"]
for (i in 1:length(option_name_list)) {
  temp <- subset(option_csv, option_csv$Option.name == option_name_list[i])
  x <- factor
}
# Input sheet.csv
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
# Input ptosh_csv
# Set ptosh_csv's name list
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

    # Select sheet_csv's rows if sheet_csv$Sheet.alias_name and ptosh_csv$alias_name is same value
    df_itemlist <- subset(sheet_csv, sheet_csv[ ,1] == alias_name[i])
    # Set dataset from ptosh_csv
    df_sheet <- get(alias_name[i])
    # temp_df_name : value that add "temp" to the head of ptosh_csv name
    # temp_df_sheet : Dataset named temp_df_name, set ptosh_csv
    temp_df_name <- paste0("temp_", alias_name[i])
    assign(temp_df_name, data.frame(id = df_sheet[9]))
    temp_df_sheet <- get(temp_df_name)
    for (j in 1:length(df_itemlist)) {
      # Get column name from the value of sheet_csv$variable
      column_name <- sheet_csv[j, "variable"]
      # Skip if there is no column with that name
      target_column_name <- paste0("field", sheet_csv[j, 3])
      target_column_index <- CheckColname(target_column_name, df_sheet)
      if (target_column_index > 0) {
        temp_df_sheet <- cbind(temp_df_sheet, df_sheet[target_column_name])
        # Set option value
        if (nchar(df_itemlist[j, "Option.name"]) > 0) {
          temp_factor <- subset(option_csv, option_csv$Option.name == df_itemlist[j, "Option.name"])
          temp_df_sheet[ ,target_column_name] <- factor(temp_df_sheet[ ,target_column_name],
                                                        levels = as.matrix(temp_factor["Option..Value.code"]),
                                                        labels = as.matrix(temp_factor["Option..Value.name"]))
        }
      }
      assign(temp_df_name, temp_df_sheet)
    }
  }
}
#sheet.alias_nameのループが終わればデータセットを出力して終了。
#write.csv(temp_ae, paste(output_path, file_list[i], sep="/"), na='""', row.names=F)
