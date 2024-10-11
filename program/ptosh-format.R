# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Modification date: 2024/9/30
# Author: mariko ohtsuka
# Version: 1.0.0
# library, function section ------
if(!require("tidyr")){
  install.packages("tidyr")
  library("tidyr")
}
if(!require("here")){
  install.packages("here")
  library("here")
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
# Function section ------
source(file.path(here(), "program", "ptosh-format-function.R"))
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
# Initialize ------
if (exists(kOutput_DF)) {
  rm(list=kOutput_DF)
}
# Input option.csv
option_csv <- ReadCsvFile(ext_path, kOption_csv_name)
option_used <- NULL
# Input sheet.csv, delete rows that 'variable' is NA
sheet_csv <- ReadCsvFile(ext_path, kSheet_csv_name)
sheet_csv <- subset(sheet_csv, !is.na(sheet_csv$variable) & !is.na(sheet_csv$FieldItem.label))
unique_sheet_csv <- sheet_csv[!duplicated(sheet_csv["Sheet.alias_name"]), ]
alias_name <- unique_sheet_csv$Sheet.alias_name
sheet_category <- unique_sheet_csv$Sheet.category
merge_excluded_alias_name <- unique_sheet_csv[which(unique_sheet_csv$Sheet.category
                                                         %in% kMerge_excluded_sheet_category), "Sheet.alias_name"]
merge_alias_name <- alias_name[-which(alias_name %in% merge_excluded_alias_name)]
# Create an empty data frame
df_duplicated <- data.frame(matrix(rep(NA, ncol(sheet_csv) + 1), nrow=1))[numeric(0), ]
for (i in 1:length(merge_excluded_alias_name)) {
  temp_duplicated_df <- subset(sheet_csv, sheet_csv$Sheet.alias_name == merge_excluded_alias_name[i])
  df_duplicated <- CheckDuplicated(temp_duplicated_df, df_duplicated, "variable")
}
temp_duplicated_df <- subset(sheet_csv, sheet_csv$Sheet.alias_name %in% merge_alias_name)
df_duplicated <- CheckDuplicated(temp_duplicated_df, df_duplicated, "variable")
# Check for duplicate 'variable'
# Check overlap from the beginning and end, OR of both
if (nrow(df_duplicated) != 0) {
  colnames(df_duplicated) <- c("row", colnames(sheet_csv))
  write.csv(df_duplicated, file.path(output_path, "variable_duplicated.csv"), row.names=F, fileEncoding="cp932")
  stop("Duplicate variable name")
  Exit()
}
# Input ptosh_csv ------
# Set ptosh_csv's name list
file_list <- list.files(rawdata_path)
# Get trial name
# Replace ex.) "xxx_ae_20190301_1122.csv" -> "xxx_ae"
temp_file_list <- gsub(kRawDataFoot, "", file_list)
# Replace ex.) "xxx_ae" -> "xxx"
gsub_alias_name <- paste0("_", alias_name)
gsub_alias_name <- paste(gsub_alias_name, collapse="|")
temp_file_list <- gsub(gsub_alias_name, "", temp_file_list)
# Count trial name
df_trial_name <- as.data.frame(as.matrix(table(temp_file_list)))
temp_trial_name <- subset(df_trial_name, df_trial_name[ ,1] == max(df_trial_name[1]))
trial_name <- row.names(temp_trial_name)
# If there are multiple matches, try all
# read allocation
for (i in 1:length(trial_name)) {
  file_index <- grep(paste0("^allocation_" , trial_name[i], kRawDataFoot), file_list)
  if (length(file_index) == 1) {
    allocation_csv <- read.csv(file.path(rawdata_path, file_list[file_index]), as.is=T, na.strings="", fileEncoding="UTF-8-BOM")
    # sort subjid
    allocation_csv <- allocation_csv[order(allocation_csv[[kAllocationSubjidColumnIndex]]), ]
    break()
  }
}
for (i in 1:length(alias_name)) {
  temp_var_labels <- "症例登録番号"
  file_index <- grep(paste0(trial_name, "_", alias_name[i], kRawDataFoot), file_list)
  # If the csv file does not exist, skip and output warning
  if (length(file_index) > 0) {
    ptosh_input <- paste0("rawdata_", alias_name[i])
    ptosh_output <- alias_name[i]
    # ex. rawdata_ae <- read.csv(R-miniCHP_ae_181211_1841.csv)
    assign(ptosh_input, ReadCsvFile(rawdata_path, file_list[file_index]))
    # Select sheet_csv's rows if sheet_csv$Sheet.alias_name and ptosh_csv$alias_name is same value
    df_itemlist <- subset(sheet_csv, sheet_csv[ ,"Sheet.alias_name"] == alias_name[i])
    # Set dataset from ptosh_csv, sort by I column (Registration number)
    sortlist <- order(get(ptosh_input)[ ,kPtoshRegistrationNumberColumnIndex])
    sort_ptosh_input <- get(ptosh_input)[sortlist, ]
    # Extract the rows of "最終報告" is true if Sheet.category is "ae_report"
    if (sheet_category[i] == "ae_report") {
      sort_ptosh_input <- subset(sort_ptosh_input, sort_ptosh_input[ ,"最終報告"] == "true")
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
          temp_var_labels <- c(temp_var_labels, paste0(df_itemlist[j, "FieldItem.label"], "有害事象名"),
                               paste0(df_itemlist[j, "FieldItem.label"], "グレード"))
          temp_cbind_column <- SplitCtcae(sort_ptosh_input, target_column_index, ctcae_term_colname, ctcae_grade_colname)
        } else {
          temp_cbind_column <- sort_ptosh_input[target_column_index]
          # Convert from character to date if field type is date
          if (df_itemlist[j, "FieldItem.field_type"] == "date") {
            temp_cbind_column[1] <- as.Date(apply(temp_cbind_column, 1, as.character))
          }
          colnames(temp_cbind_column) <- column_name
          temp_var_labels <- c(temp_var_labels, df_itemlist[j, "FieldItem.label"])
        }
        temp_ptosh_output <- cbind(temp_ptosh_output, temp_cbind_column)
        # Checkbox
        if ((df_itemlist[j, "FieldItem.field_type"] == "checkbox")
            && !is.na(df_itemlist[j, "FieldItem.field_type"])) {
          checkboxcolumns_list <- CreateCheckboxColumns(sort_ptosh_input, df_itemlist[j, ], target_column_name)
          temp_cbind_column <- checkboxcolumns_list[[1]]
          temp_var_labels <- c(temp_var_labels, checkboxcolumns_list[[2]])
          temp_ptosh_output <- cbind(temp_ptosh_output, temp_cbind_column)
          df_itemlist[j, "Option.name"] <- NA
        }
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
            if (!all(is.na(temp_ptosh_output[ , temp_factor_colname]))) {
              temp_labels <- ConvertClass(class(temp_ptosh_output[,temp_factor_colname]),
                                          temp_factor[,"Option..Value.code"])
              names(temp_labels) <- temp_factor["Option..Value.name"][[1]]
              if (is.numeric(temp_ptosh_output[ , temp_factor_colname])) {
                temp_labels_values <- as.numeric(temp_labels)
                temp_ptosh_output[ , temp_factor_colname] <- labelled(temp_ptosh_output[ , temp_factor_colname], 
                                            setNames(temp_labels_values, names(temp_labels)))
              } else {
                # 数値型でない場合は現行の処理を行う
                temp_ptosh_output[ , temp_factor_colname] <- labelled(temp_ptosh_output[ , temp_factor_colname], temp_labels)
              }
            }
            option_used <- c(option_used, temp_factor["Option.name"], recursive=T)
          }
        }
      }
      var_label(temp_ptosh_output) <- temp_var_labels
      assign(ptosh_output, temp_ptosh_output)
    }
    # Edit output dataframe
    if (sheet_category[i] %in% kMerge_excluded_sheet_category) {
      if (exists("allocation_csv")) {
        assign(ptosh_output, SetAllocation(allocation_csv, get(ptosh_output)))
      }
      OutputDF(ptosh_output, output_path, output_path)
    } else {
      # Merge
      if (!exists(kOutput_DF)) {
        temp_merge_df <- data.frame(id = get(ptosh_output)[ , kRegistration_colname])
        colnames(temp_merge_df) <- kRegistration_colname
        assign(kOutput_DF, temp_merge_df)
      }
      temp_var_labels <- c(unlist(var_label(get(kOutput_DF))), unlist(var_label(get(ptosh_output)))[-1])
      assign(kOutput_DF, merge(get(kOutput_DF), get(ptosh_output), by=kRegistration_colname, all=T))
      var_label(ptdata) <- temp_var_labels
    }
  } else {
    stop(paste0("No input data", " : ", alias_name[i]))
    Exit()
  }
}
# Output merge dataframe
if (exists(kOutput_DF)) {
  if (exists("allocation_csv")) {
    assign(kOutput_DF, SetAllocation(allocation_csv, get(kOutput_DF)))
  }
  OutputDF(kOutput_DF, output_path, output_path)
} else {
  warning("No output ptdata")
}
# Output used option list
output_option_csv <- subset(option_csv, Option.name %in% unique(option_used))
OutputDF("output_option_csv", output_path, output_path)
OutputCsv("output_option_csv", output_path)
# Output variable name list
output_sheet_csv <- sheet_csv[ ,c("Sheet.alias_name", "FieldItem.label", "variable")]
OutputDF("output_sheet_csv", output_path, output_path)
OutputCsv("output_sheet_csv", output_path)
# Reset the log output destination
sink()
