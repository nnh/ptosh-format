# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Author: mariko ohtsuka

# library, function section ------
#' @title
#' Exit function
#' @description
#' Exit from this program
#' @return
#' No return value
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
kPtoshRegistrationNumberColumnIndex <- 9  # ptosh_csv$registration_number
kSheetAliasNameColumnIndex <- 1  # sheet.csv$sheet.alias_name
kSheetFieldItemNameColumnIndex <- 3  # sheet.csv$FieldItem.name.tr..field......
kCtcae_convertflag <- 1
kOption_Ctcae <- "ctcae"
# Main section ------

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
# Input option.csv
option_csv <- read.csv(paste0(external_path, "/option.csv"), as.is=T, fileEncoding="utf-8", stringsAsFactors=F)
# Input sheet.csv, delete rows that 'variable' is NA
sheet_csv <- read.csv(paste0(external_path, "/testsheet.csv"), as.is=T, na.strings="", fileEncoding="utf-8",
                      stringsAsFactors=F)
sheet_csv <- subset(sheet_csv, !is.na(sheet_csv$variable))
# Check for duplicate 'variable'
# Check overlap from the beginning and end, OR of both
df_duplicated <- sheet_csv[duplicated(sheet_csv$variable) | duplicated(sheet_csv$variable, fromLast=T), ]
if (nrow(df_duplicated) != 0) {
  df_duplicated <- cbind(rownames(df_duplicated), df_duplicated)
  names(df_duplicated)[1] <- "row"
  write.csv(df_duplicated, paste0(output_path, "/variable_duplicated.csv"), row.names=F, fileEncoding="cp932")
  stop("Duplicate variable name")
  Exit()
}
# Input ptosh_csv
# Set ptosh_csv's name list
alias_name <- sheet_csv[!duplicated(sheet_csv[kSheetAliasNameColumnIndex]), kSheetAliasNameColumnIndex]
file_list <- list.files(input_path)
for (i in 1:length(alias_name)) {
  file_index <- grep(paste0("_", alias_name[i] , "_"), file_list)
  if (length(file_index) > 0) {
    ptosh_input <- paste0("rawdata_", alias_name[i])
    ptosh_output <- alias_name[i]
    assign(ptosh_input, read.csv(paste(input_path, file_list[file_index], sep="/"), as.is=T, na.strings="",
                                   fileEncoding="cp932"))
    # Select sheet_csv's rows if sheet_csv$Sheet.alias_name and ptosh_csv$alias_name is same value
    df_itemlist <- subset(sheet_csv, sheet_csv[ ,kSheetAliasNameColumnIndex] == alias_name[i])
    # Set dataset from ptosh_csv, sort by I column (Registration number)
    sortlist <- order(get(ptosh_input)[kPtoshRegistrationNumberColumnIndex])
    sort_ptosh_input <- get(ptosh_input)[sortlist, ]
    temp_ptosh_output <- data.frame(id = sort_ptosh_input[kPtoshRegistrationNumberColumnIndex])
    for (j in 1:length(df_itemlist$variable)) {
      # Get column name from the value of sheet_csv$variable
      column_name <- df_itemlist[j, "variable"]
      # Skip if there is no column with that name
      target_column_name <- paste0("field", sheet_csv[j, kSheetFieldItemNameColumnIndex])
      target_column_index <- CheckColname(target_column_name, sort_ptosh_input)
      save_output_colnames <- colnames(temp_ptosh_output)
      if (target_column_index > 0) {
        if ((df_itemlist[j, "FieldItem.field_type"] == kOption_Ctcae)
            && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
          if (kCtcae_convertflag == 1) {
            temp_ctcae <- sort_ptosh_input[target_column_index + 1]
          } else {
            temp_ctcae <- sort_ptosh_input[target_column_index]
          }
          temp_ptosh_output <- cbind(temp_ptosh_output, temp_ctcae, rep(NA, nrow(sort_ptosh_input)))
          colnames(temp_ptosh_output) <- c(save_output_colnames, paste0(column_name, "_term"),
                                           paste0(column_name, "_grade"))
        } else {
          temp_ptosh_output <- cbind(temp_ptosh_output, sort_ptosh_input[target_column_index])
          colnames(temp_ptosh_output) <- c(save_output_colnames, column_name)
        }
        # Set option value
        if (!is.na(df_itemlist[j, "Option.name"])) {
          temp_factor <- subset(option_csv, option_csv$Option.name == df_itemlist[j, "Option.name"])
          if (nrow(temp_factor) > 0) {
            if ((df_itemlist[j, "FieldItem.field_type"] == kOption_Ctcae)
                && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
              temp_factor_data <- temp_ptosh_output[ , column_name]
            } else {
              factor_data <- temp_ptosh_output[ , column_name]
            }
            temp_ptosh_output[ , column_name] <-   factor(temp_ptosh_output[ , column_name],
                                                   levels = as.matrix(temp_factor["Option..Value.code"]),
                                                   labels = as.matrix(temp_factor["Option..Value.name"]))
          }
        }
      }
      assign(ptosh_output, temp_ptosh_output)
    }
    # Output csv and R_dataframe
    write.csv(ptosh_output, paste0(output_path, "/", alias_name[i], ".csv"), na='""', row.names=F)
    save(list=ptosh_output, file=(paste0(output_dst_path, "/", alias_name[i], ".Rda")))
  }
}
