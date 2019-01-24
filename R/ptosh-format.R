# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Author: mariko ohtsuka

# library, function section ------
# install.packages("tidyr")
library("tidyr")
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
#' @title
#' Output csv and R_dataframe
#' @param
#' df : dataframe name
#' output_csv_path : output "*.csv" path
#' output_rda_path : output "*.Rda" path
#' @return
#' No return value
OutputDF <- function(df, output_csv_path, output_rda_path){
  # Output csv and R_dataframe
  write.csv(get(df), paste0(output_csv_path, "/", df, ".csv"), na='""', row.names=F, fileEncoding="cp932")
  save(list=df, file=(paste0(output_rda_path, "/", df, ".Rda")))
}
#' @title
#' Split CTCAE term and grade by '-'
#' @param
#' df : input data frame
#' column_index : Index of CTCAE column (field)
#' term_colname : column name (term)
#' grade_colname : column name (grade)
#' @return
#' data frame
#' @example
#' *input dataframe
#' field x
#' ----------
#' 10002272-3
#' 10048580-4
#' ----------
#' * return dataframe
#' term_colname|grade_colname
#' --------------------------
#' 10002272    |3
#' 10048580    |4
#' --------------------------
SplitCtcae <- function(df, column_index, term_colname, grade_colname){
  if (kCtcae_convertflag == 1) {
    temp_ctcae <- df[column_index + 1]
  } else {
    temp_ctcae <- df[column_index]
  }
  colnames(temp_ctcae) <- kOption_ctcae
  output_ctcae <- temp_ctcae %>% separate(kOption_ctcae, into = c(term_colname, grade_colname), sep = "-")
  return(output_ctcae)
}
#' --------------------------
#' @title
#' Create columns to set the value of the checkboxes
#' @param
#' ptosh_input : ptosh_csv
#' sheet_csv : sheet_csv, a row that value 'field_type' is 'checkbox'
#' ptosh_column_name : ptosh checkbox type field column name
#' @return
#' data frame
#' @example
#' * sheet.csv
#' Sheet.alias_name|FieldItem.name.tr..field......|FieldItem.field_type|Option.name|variable
#' -----------------------------------------------------------------------------------------
#' abc             |13                            |checkbox            |"B症状     |efgh
#'
#' * option.csv
#' Option#name|Option::Value#code
#' ------------------------------
#' B症状      |1
#' B症状      |3
#' B症状      |5
#'
#' * ptosh_csv
#' alias_name|SUBJID|field13
#' -------------------------
#' abc       |1     |NA
#' abc       |2     |3
#' abc       |3     |1,5
#'
#' * return dataframe
#' SUBJID|efgh_1|efgh_3|efgh_5
#' ---------------------------
#' 1     |F     |F     |F
#' 2     |F     |T     |F
#' 3     |T     |F     |T
CreateCheckboxColumns <- function(ptosh_input, sheet_csv, ptosh_column_name){
  option_csv <- ExtractOptionCsv(sheet_csv$Option.name)
  # Create dataframe of number of checkboxes column
  checkbox_df <- data.frame(matrix(rep(F), ncol=nrow(option_csv), nrow=nrow(ptosh_input)))
  colnames(checkbox_df) <- option_csv$Option..Value.code
  checkbox_field_name <- paste0("field", sheet_csv$FieldItem.name.tr..field......)
  for (i in 1:nrow(ptosh_input)) {
    checkbox_field_value <- ptosh_input[i, ptosh_column_name]
    if (!is.na(checkbox_field_value)) {
      # If multiple values, split by ','
      temp_checkbox_value <- strsplit(as.character(checkbox_field_value), ",")
      for (j in 1:length(temp_checkbox_value)) {
        checkbox_df[i, as.character(temp_checkbox_value[j])] <- T
      }
    }
  }
  colnames(checkbox_df) <- paste0(column_name, "_", option_csv$Option..Value.code)
  return(checkbox_df)
}
#' @title
#' Extract records from option.csv
#' @param
#' target_name : The value of Option.name
ExtractOptionCsv <- function(target_name){
  df <- subset(option_csv, option_csv$Option.name == target_name)
  return(df)
}

# Constant section ------
kPtoshRegistrationNumberColumnIndex <- 9  # ptosh_csv$registration_number
# If the MedDRA code is set in the field, set 0, else set 1
kCtcae_convertflag <- 0
kRegistration_colname <- "SUBJID"
kOption_ctcae <- "ctcae"
kOutput_DF <- "ptdata"

# Initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
if (exists(kOutput_DF)) {
  rm(list=kOutput_DF)
}
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
sheet_csv <- read.csv(paste0(external_path, "/sheet.csv"), as.is=T, na.strings="", fileEncoding="utf-8",
                      stringsAsFactors=F)
sheet_csv <- subset(sheet_csv, !is.na(sheet_csv$variable) & !is.na(sheet_csv$FieldItem.label))
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
# Input ptosh_csv ------
# Set ptosh_csv's name list
unique_sheet_csv <- sheet_csv[!duplicated(sheet_csv["Sheet.alias_name"]), ]
alias_name <- unique_sheet_csv$Sheet.alias_name
sheet_category <- unique_sheet_csv$Sheet.category
file_list <- list.files(input_path)
for (i in 1:length(alias_name)) {
  file_index <- grep(paste0("_", alias_name[i] , "_"), file_list)
  # If the csv file does not exist, skip and output warning
  if (length(file_index) > 0) {
    ptosh_input <- paste0("rawdata_", alias_name[i])
    ptosh_output <- alias_name[i]
    # ex. rawdata_ae <- read.csv(R-miniCHP_ae_181211_1841.csv)
    assign(ptosh_input, read.csv(paste(input_path, file_list[file_index], sep="/"), as.is=T, na.strings="",
                                   fileEncoding="cp932"))
    # Select sheet_csv's rows if sheet_csv$Sheet.alias_name and ptosh_csv$alias_name is same value
    df_itemlist <- subset(sheet_csv, sheet_csv[ ,"Sheet.alias_name"] == alias_name[i])
    # Set dataset from ptosh_csv, sort by I column (Registration number)
    sortlist <- order(get(ptosh_input)[kPtoshRegistrationNumberColumnIndex])
    sort_ptosh_input <- get(ptosh_input)[sortlist, ]
    # Extract the rows of "最終報告" is true if Sheet.category is "ae_report"
    if (sheet_category[i] == "ae_report") {
      sort_ptosh_input <- subset(sort_ptosh_input, sort_ptosh_input[ ,"最終報告"] == T)
    }
    # Create data frame with registration number only
    temp_ptosh_output <- data.frame(id = sort_ptosh_input[kPtoshRegistrationNumberColumnIndex])
    colnames(temp_ptosh_output) <- kRegistration_colname
    for (j in 1:nrow(df_itemlist)) {
      # Get column name from the value of sheet_csv$variable
      column_name <- df_itemlist[j, "variable"]
      # Skip if there is no column with that name
      target_column_name <- paste0("field", df_itemlist[j, "FieldItem.name.tr..field......"])
      target_column_index <- CheckColname(target_column_name, sort_ptosh_input)
      if (target_column_index > 0) {
        if ((df_itemlist[j, "FieldItem.field_type"] == kOption_ctcae)
            && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
          ctcae_term_colname <- paste0(column_name, "_trm")
          ctcae_grade_colname <- paste0(column_name, "_grd")
          temp_cbind_column <- SplitCtcae(sort_ptosh_input, target_column_index, ctcae_term_colname, ctcae_grade_colname)
        } else if ((df_itemlist[j, "FieldItem.field_type"] == "checkbox")
                   && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
          temp_cbind_column <- CreateCheckboxColumns(sort_ptosh_input, df_itemlist[j, ], target_column_name)
          df_itemlist[j, "Option.name"] <- NA
                  }
        else {
          temp_cbind_column <- sort_ptosh_input[target_column_index]
          colnames(temp_cbind_column) <- column_name
        }
        temp_ptosh_output <- cbind(temp_ptosh_output, temp_cbind_column)
        # Set option value
        if (!is.na(df_itemlist[j, "Option.name"]) | (df_itemlist[j, "FieldItem.field_type"] == kOption_ctcae)) {
          if (df_itemlist[j, "FieldItem.field_type"] == kOption_ctcae) {
            temp_factor_option_name <- "CTCAE"
          } else {
            temp_factor_option_name <- df_itemlist[j, "Option.name"]
          }
          temp_factor <- ExtractOptionCsv(temp_factor_option_name)
          if (nrow(temp_factor) > 0) {
            if ((df_itemlist[j, "FieldItem.field_type"] == kOption_ctcae)
                && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
              temp_factor_colname <- ctcae_term_colname
            } else {
              temp_factor_colname <- column_name
            }
            factor_data <- temp_ptosh_output[ , temp_factor_colname]
            temp_ptosh_output[ , temp_factor_colname] <-   factor(temp_ptosh_output[ , temp_factor_colname],
                                                   levels = as.matrix(temp_factor["Option..Value.code"]),
                                                   labels = as.matrix(temp_factor["Option..Value.name"]))
          }
        }
      }
      assign(ptosh_output, temp_ptosh_output)
    }
    # Edit output dataframe
    if (sheet_category[i] == "ae_report" || sheet_category[i] == "committees_opinion"
        || sheet_category[i] == "multiple") {
      OutputDF(ptosh_output, output_path, output_dst_path)
    } else {
      # Merge
      if (!exists(kOutput_DF)) {
        temp_merge_df <- data.frame(id = get(ptosh_output)[ , kRegistration_colname])
        colnames(temp_merge_df) <- kRegistration_colname
        assign(kOutput_DF, temp_merge_df)
      }
      assign(kOutput_DF, merge(get(kOutput_DF), get(ptosh_output), by=kRegistration_colname, all=T))
    }
  } else {
    stop(paste0("No input data", " : ", alias_name[i]))
    Exit()
  }
}
# Output merge dataframe
if (exists(kOutput_DF)) {
  OutputDF(kOutput_DF, output_path, output_dst_path)
} else {
  warning("No output ptdata")
}
