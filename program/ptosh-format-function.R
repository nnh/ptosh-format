# common function
# Created date: 2019/3/28
# Modification date: 2024/10/17
# Author: mariko ohtsuka
#' @title
#' Exit function
#' @description
#' Exit from this program
#' @return
#' No return value
# ------ library section ------
if(!require("tidyr")){
  install.packages("tidyr")
  library("tidyr")
}
if(!require("labelled")){
  install.packages("labelled")
  library("labelled")
}
Sys.setenv("TZ" = "Asia/Tokyo")
# Output log ------
# log output path
log_path <- file.path(here(), "log")
if (file.exists(log_path) == F) {
  dir.create(log_path)
}
sink(file.path(log_path, "log.txt"), split=T)
# ------ function section ------
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
#' @title OutputDF
#' @description Save r objects.
#' @param df : dataframe name.
#' @param output_csv_path : Currently unused.
#' @param output_rda_path : output "*.Rda" path.
#' @return No return value.
OutputDF <- function(df, output_csv_path, output_rda_path){
  save(list=df, file=(paste0(output_rda_path, "/", df, ".Rda")))
}
#' @title OutputCsv
#' @description Output csv files.
#' @param df : dataframe name.
#' @param output_csv_path : output "*.csv" path.
#' @return No return value.
OutputCsv <- function(df, output_csv_path){
  df_csv <- get(df)
  for (i in 1:ncol(df_csv)){
    # Output labels to csv
    if ("haven_labelled" %in% class(df_csv[ , i])){
      df_csv[ , i] <- to_character(df_csv[ , i])
    }
  }
  if (Sys.info()[["sysname"]] == "Windows") {
    write.csv(df_csv, paste0(output_csv_path, "/", df, ".csv"), na='""', row.names=F,
              fileEncoding="utf-8")
    con <- file(paste0(output_csv_path, "/", df, ".csv"), open = "r+b", encoding = "UTF-8")
    writeBin(charToRaw("\xEF\xBB\xBF"), con) # BOM
    close(con)
  } else {
    write.csv(df_csv, paste0(output_csv_path, "/", df, ".csv"), na='""', row.names=F,
              fileEncoding="cp932", eol=kOutput_csv_eol)
  }
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
#' list
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
#' * return[1] dataframe
#' SUBJID|efgh_1|efgh_3|efgh_5
#' ---------------------------
#' 1     |F     |F     |F
#' 2     |F     |T     |F
#' 3     |T     |F     |T
#' * return[2] vector
#' c("B症状_1", "B症状_3", "B症状_5")
CreateCheckboxColumns <- function(ptosh_input, sheet_csv, ptosh_column_name, column_name){
  option_csv <- ExtractOptionCsv(sheet_csv$Option.name)
  # Create dataframe of number of checkboxes column
  checkbox_df <- data.frame(matrix(rep(F), ncol=nrow(option_csv), nrow=nrow(ptosh_input)))
  colnames(checkbox_df) <- paste0("_", option_csv$Option..Value.code)
  checkbox_field_name <- paste0("field", sheet_csv$FieldItem.name.tr..field......)
  for (i in 1:nrow(ptosh_input)) {
    checkbox_field_value <- ptosh_input[i, ptosh_column_name]
    if (!is.na(checkbox_field_value)) {
      # If multiple values, split by ','
      temp_checkbox_value <- unlist(strsplit(as.character(checkbox_field_value), ","))
      for (j in 1:length(temp_checkbox_value)) {
        checkbox_df[i, paste0("_", temp_checkbox_value[j])] <- T
      }
    }
  }
  colnames(checkbox_df) <- paste0(column_name, "_", option_csv$Option..Value.code)
  temp_var_labels <- paste0(sheet_csv$FieldItem.label, "_", option_csv$Option..Value.code)
  return(list(checkbox_df, temp_var_labels))
}
#' @title
#' Extract records from option.csv
#' @param
#' target_name :
#'  The value of Option.name
#' @return
#' data frame
ExtractOptionCsv <- function(target_name){
  df <- subset(option_csv, option_csv$Option.name == target_name)
  return(df)
}
#' @title
#' check unique value
#' @param
#' input_df : dataframe for the check
#' duplicated_df : dataframe for the result output
#' column_name : column name for the check
#' @return
#' data frame
CheckDuplicated <- function(input_df, duplicated_df, column_name){
  temp_df <- input_df[duplicated(input_df[ ,column_name]) | duplicated(input_df[ ,column_name],fromLast=T), ]
  if (nrow(temp_df) != 0) {
    temp_df <- cbind(rownames(temp_df), temp_df)
    output_df <- rbind(duplicated_df, temp_df)
  } else {
    output_df <- duplicated_df
  }
  return(output_df)
}
#' @title
#' ConstAssignenvironment
#' @description
#' Define an unmodifiable variable
#' @param
#' x : Variable name to define
#' value : The value to define
#' e : environment
#' @return
#' Variable to define
#' @examples
#' ConstAssign("FOO", 1)
ConstAssign <- function(x, value, e=.GlobalEnv){
  if (!exists(x)) {
    assign(x, value, envir=e)
    lockBinding(x, e)
  }
}
#' @title
#' CreateDataFrame
#' @description
#' Create a data frame
#' @param
#' col_names : Data frame column name
#' row_count : Number of rows of data frame, Default 0
#' @return
#' data frame
#' @examples
#' CreateDataFrame(c("a", "b", "c"), 4)
#' CreateDataFrame(c("aaa", "bbb", "ccc"))
CreateDataFrame <- function(col_names, row_count=0){
  col_count <- length(col_names)
  if (row_count == 0) {
    temp_row_count <- 1
  } else {
    temp_row_count <- row_count
  }
  df <- data.frame(matrix(rep(NA, col_count * temp_row_count), ncol=col_count, nrow=temp_row_count))
  colnames(df) <- col_names
  if (row_count == 0) {
    df <- df[numeric(0), ]
  }
  return(df)
}
#' @title
#' SetAllocation
#' @description
#' Set allocation by subjid
#' @param
#' allocation_csv : Allocation source data frame
#' input_df : Data frame to be allocated
#' @return
#' data frame
#' @examples
#' SetAllocation(allocation_csv, ptdata)
SetAllocation <- function(allocation_csv, input_df){
  allocation_key <- colnames(allocation_csv)[kAllocationSubjidColumnIndex]
  # Sort input_df by subjid
  sort_input_df <- input_df[order(input_df[[kRegistration_colname]]), ]
  # Merge by subjid
  output_df <- merge(sort_input_df, allocation_csv, by.x=kRegistration_colname, by.y=allocation_key, all.x=T)
  # Change column name
  temp_colnames <- c(colnames(input_df), "group")
  colnames(output_df) <- temp_colnames
  return(output_df)
}
#' @title
#' ConvertClass
#' @description
#' Convert to numeric or string
#' @param
#' df : vector
#' @return
#' converted vector
ConvertClass <- function(convert_class, df){
  numCheck <- NumericCheck(df)
  if (("integer" %in% convert_class) && numCheck){
    temp <- as.numeric(df)
  } else {
    temp <- as.character(df)
  }
  return(temp)
}
#' @title NumericCheck
#' @param values : Vector of values to be tested
#' @return Returns true if the value can be converted to a number, otherwise false
NumericCheck <- function(values) {
  value_converted_in_number <- sapply(values, function(x){temp <- suppressWarnings(as.numeric(x))})
  return(!anyNA(value_converted_in_number))
}
#' @title ReadCsvSetEncoding
#' @description Reads a file with the specified encoding.
#' @param target_file Full path of the target file.
#' @param target_encoding Encoding to specify.
#' @return Returns a data frame. If the file fails to read, NA is returned.
ReadCsvSetEncoding <- function(target_file, target_encoding, colClassesList){
  temp <- tryCatch(
    read.csv(target_file, as.is=T, fileEncoding=target_encoding, stringsAsFactors=F, na.strings="", colClasses=colClassesList),
    warning = function(e){ return(NA) }
  )
  return(temp)
}
#' @title ReadCsvFile
#' @description Reads a file with the specified encoding.
#' @param target_path The folder path where the target file resides.
#' @param filename Target file name.
#' @return Returns a data frame. If the file fails to read, NA is returned.
ReadCsvFile <- function(target_path, filename, colClassesList=NA){
  target <- file.path(target_path, filename)
  temp <- ReadCsvSetEncoding(target, 'UTF-8-BOM', colClassesList)
  if (!is.data.frame(temp)){
    temp <- ReadCsvSetEncoding(target, 'utf-8', colClassesList)
  }
  if (!is.data.frame(temp)){
    temp <- ReadCsvSetEncoding(target, 'cp932', colClassesList)
  }
  return(temp)
}
GetFileIndex <- function(aliasName) {
  file_index <- grep(paste0(trial_name, "_", aliasName, kRawDataFoot), file_list)
  if (length(file_index) > 0) {
    return(file_index)
  } else {
    stop(paste0("No input data", " : ", aliasName))
  }
}
GetSheetCsvForRawData <- function(aliasName) {
  target <- sheet_csv[sheet_csv$Sheet.alias_name == aliasName, c("FieldItem.name.tr..field......", "FieldItem.field_type")]
  target$colClass <- ifelse(target$FieldItem.field_type == "char", "character", NA)
  target <- target[!is.na(target$colClass), ]
  if (nrow(target) == 0) {
    return(NA)
  }
  targetNames <- paste0("field", target$FieldItem.name.tr..field......)
  res <- target$colClass
  names(res) <- targetNames
  return(res)
}
GetPtoshInput <- function(aliasName, file_index) {
  colClassesList <- GetSheetCsvForRawData(aliasName)
  ptosh_input <- paste0("rawdata_", aliasName)
  # ex. rawdata_ae <- read.csv(R-miniCHP_ae_181211_1841.csv)
  assign(ptosh_input, ReadCsvFile(rawdata_path, file_list[file_index], colClassesList))
  sortlist <- order(get(ptosh_input)[ ,kPtoshRegistrationNumberColumnIndex])
  sort_ptosh_input <- get(ptosh_input)[sortlist, ]
  # Extract the rows of "最終報告" is true if Sheet.category is "ae_report"
  if (sheet_category[i] == "ae_report") {
    sort_ptosh_input <- subset(sort_ptosh_input, sort_ptosh_input[ ,"最終報告"] == "true")
  }
  return(sort_ptosh_input)
}
EditCtcae <- function(target_item, temp_var_labels, ptosh_input, target_column_index, ptosh_output) {
  column_name <- target_item[ , "variable", drop=T]
  fieldType <- target_item[, "FieldItem.field_type", drop=T]
  itemLabel <- target_item[, "FieldItem.label", drop=T]
  ctcae_term_colname <- NA
  if (fieldType == kOption_ctcae && !is.na(fieldType)) {
    ctcae_term_colname <- paste0(column_name, "_trm")
    ctcae_grade_colname <- paste0(column_name, "_grd")
    temp_var_labels <- c(temp_var_labels, paste0(itemLabel, "有害事象名"),
                         paste0(itemLabel, "グレード"))
    temp_cbind_column <- SplitCtcae(ptosh_input, target_column_index, ctcae_term_colname, ctcae_grade_colname)
  } else {
    temp_cbind_column <- ptosh_input[target_column_index]
    # Convert from character to date if field type is date
    if (fieldType == "date") {
      temp_cbind_column[1] <- as.Date(apply(temp_cbind_column, 1, as.character))
    }
    colnames(temp_cbind_column) <- column_name
    temp_var_labels <- c(temp_var_labels, itemLabel)
  }
  temp_ptosh_output <- cbind(ptosh_output, temp_cbind_column)
  return(list(ctcae_term_colname=ctcae_term_colname, ptosh_output=temp_ptosh_output, temp_var_labels=temp_var_labels))
}
EditCheckBox <- function(ptosh_input, target_item, temp_var_labels, ptosh_output, target_column_name) {
  fieldType <- target_item[, "FieldItem.field_type", drop=T]
  column_name <- target_item[ , "variable", drop=T]
  if (fieldType == "checkbox" && !is.na(fieldType)) {
    checkboxcolumns_list <- CreateCheckboxColumns(ptosh_input, target_item, target_column_name, column_name)
    temp_cbind_column <- checkboxcolumns_list[[1]]
    temp_var_labels <- c(temp_var_labels, checkboxcolumns_list[[2]])
    ptosh_output <- cbind(ptosh_output, temp_cbind_column)
    option_name <- NA
  } else {
    option_name <- target_item[ , "Option.name", drop=T]
  }
  return(list(temp_var_labels=temp_var_labels, ptosh_output=ptosh_output, option_name=option_name))
}
EditOptionValue <- function(target_item, ptosh_output, ctcae_term_colname) {
  optionName <- target_item[ , "Option.name", drop=T]
  fieldType <- target_item[, "FieldItem.field_type", drop=T]
  column_name <- target_item[ , "variable", drop=T]
  # Set option value
  if (is.na(optionName) && fieldType != kOption_ctcae) {
    return(ptosh_output)
  }
  if (fieldType == kOption_ctcae) {
    temp_factor_option_name <- "CTCAE"
  } else {
    temp_factor_option_name <- optionName
  }
  temp_factor <- ExtractOptionCsv(temp_factor_option_name)
  if (nrow(temp_factor) == 0) {
    return()
  }
  if (fieldType == kOption_ctcae && !is.na(fieldType)) {
    temp_factor_colname <- ctcae_term_colname
  } else {
    temp_factor_colname <- column_name
  }
  factor_data <- ptosh_output[ , temp_factor_colname]
  if (!all(is.na(factor_data))) {
    temp_labels <- ConvertClass(class(ptosh_output[,temp_factor_colname]), temp_factor[,"Option..Value.code"])
    names(temp_labels) <- temp_factor["Option..Value.name"][[1]]
    if (is.numeric(ptosh_output[ , temp_factor_colname])) {
      temp_labels_values <- as.numeric(temp_labels)
      ptosh_output[ , temp_factor_colname] <- labelled(ptosh_output[ , temp_factor_colname], setNames(temp_labels_values, names(temp_labels)))
    } else {
      # 数値型でない場合は現行の処理を行う
      ptosh_output[ , temp_factor_colname] <- labelled(ptosh_output[ , temp_factor_colname], temp_labels)
    }
  }
  option_used <<- c(option_used, temp_factor["Option.name"], recursive=T)
  return(ptosh_output)
}
OutputMergeExcludedSheet <- function(aliasName) {
  if (exists("allocation_csv")) {
    temp <- SetAllocation(allocation_csv, get(aliasName))
  } else {
    temp <- get(aliasName)
  }
  assign(aliasName, temp, envir=.GlobalEnv)
  OutputDF(aliasName, output_path, output_path)
}
CreatePtdata <- function(aliasName) {
  temp_merge_df <- data.frame(id = get(aliasName)[ , kRegistration_colname])
  colnames(temp_merge_df) <- kRegistration_colname
  return(temp_merge_df)  
}
EditOutputDf <- function(aliasName) {
  file_index <- GetFileIndex(aliasName)
  ptosh_input <- GetPtoshInput(aliasName, file_index)
  # Create data frame with registration number only
  ptosh_output <- data.frame(id = ptosh_input[kPtoshRegistrationNumberColumnIndex])
  colnames(ptosh_output) <- kRegistration_colname
  df_itemlist <- subset(sheet_csv, sheet_csv[ ,"Sheet.alias_name"] == aliasName)
  output_df <- NULL
  for (i in 1:nrow(df_itemlist)) {
    # Skip if there is no column with that name
    temp_ptosh_output <- EditOutputDfItems(df_itemlist[i, ], ptosh_input, ptosh_output)
    if (!is.null(temp_ptosh_output)) {
      if (is.null(output_df)) {
        output_df <- temp_ptosh_output
      } else {
        temp_ptosh_output_modified <- temp_ptosh_output[, !names(temp_ptosh_output) %in% kRegistration_colname, drop=F]
        output_df <- output_df %>% cbind(temp_ptosh_output_modified)
      }
    }
  }
  return(output_df)
}
EditOutputDfItems <- function(target_item, ptosh_input, ptosh_output) {
  target_column_name <- paste0("field", target_item[ , "FieldItem.name.tr..field......", drop=T])
  target_column_index <- CheckColname(target_column_name, ptosh_input)
  if (target_column_index < 0) {
    return(NULL)
  }
  temp_var_labels <- "症例登録番号"
  
  temp <- EditCtcae(target_item, temp_var_labels, ptosh_input, target_column_index, ptosh_output)
  ctcae_term_colname <- temp$ctcae_term_colname
  ptosh_output <- temp$ptosh_output
  temp_var_labels <- temp$temp_var_labels
  
  temp <- EditCheckBox(ptosh_input, target_item, temp_var_labels, ptosh_output, target_column_name)
  target_item[ , "Option.name"] <- temp$option_name
  temp_var_labels <- temp$temp_var_labels
  ptosh_output <- temp$ptosh_output
  
  ptosh_output <- EditOptionValue(target_item, ptosh_output, ctcae_term_colname)
  
  var_label(ptosh_output) <- temp_var_labels
  return(ptosh_output)
}

# Constant section ------
ConstAssign("kPtoshRegistrationNumberColumnIndex", 9)  # ptosh_csv$registration_number
# If the MedDRA code is set in the field, set 0, else set 1
ConstAssign("kCtcae_convertflag", 0)
ConstAssign("kRegistration_colname", "SUBJID")
ConstAssign("kOption_ctcae", "ctcae")
ConstAssign("kOutput_DF", "ptdata")
ConstAssign("kMerge_excluded_sheet_category", c("ae_report", "committees_opinion", "multiple"))
ConstAssign("kOutput_csv_eol", "\r\n")  # output_csv's line feed code
ConstAssign("kSheet_csv_name", "sheets.csv")
ConstAssign("kOption_csv_name", "options.csv")
ConstAssign("kAllocationSubjidColumnIndex", 1)
ConstAssign("kAllocationAllocationColumnIndex", 2)
ConstAssign("kRawDataFoot", "_[0-9]{6}_[0-9]{4}.csv")
if (Sys.info()[["sysname"]] == "Windows") {
  temp_delimiter <- "/"
} else {
  temp_delimiter <- "/"
}
ConstAssign("kDelimiter", temp_delimiter)
temp_parent_path <- strsplit(here(), kDelimiter)
parent_path <- paste(unlist(temp_parent_path)[2:lengths(temp_parent_path) - 1], collapse=temp_delimiter)
input_path <- file.path(parent_path, "input")
ext_path <- file.path(input_path, "ext")
rawdata_path <- file.path(input_path, "rawdata", sep=temp_delimiter)
output_path <- file.path(here(), "ads")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}