# common function
# Created date: 2019/3/28
# Author: mariko ohtsuka
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
  df_csv <- get(df)
  for (i in 1:ncol(df_csv)){
    # Output labels to csv
    if (class(df_csv[ , i]) == "haven_labelled"){
      df_csv[ , i] <- to_character(df_csv[ , i])
    }
  }
  if (Sys.info()[["sysname"]] == "Windows") {
    write.csv(df_csv, paste0(output_csv_path, "/", df, ".csv"), na='""', row.names=F,
              fileEncoding=kOutput_csv_fileEncoding)
  } else {
    write.csv(df_csv, paste0(output_csv_path, "/", df, ".csv"), na='""', row.names=F,
              fileEncoding=kOutput_csv_fileEncoding, eol=kOutput_csv_eol)
  }
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
CreateCheckboxColumns <- function(ptosh_input, sheet_csv, ptosh_column_name){
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
  sort_input_df <- input_df[order(input_df[kRegistration_colname]), ]
  # Merge by subjid
  output_df <- merge(sort_input_df, allocation_csv, by.x=kRegistration_colname, by.y=allocation_key, all.x=T)
  # Change column name
  temp_colnames <- c(colnames(input_df), "group")
  colnames(output_df) <- temp_colnames
  return(output_df)
}
