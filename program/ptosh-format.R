# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Modification date: 2024/10/16
# Author: mariko ohtsuka
# Version: 1.0.0
if(!require("here")){
  install.packages("here")
  library("here")
}
# Function section ------
source(file.path(here(), "program", "ptosh-format-function.R"))
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

TEST20241016_2 <- function(target_item, ptosh_input, ptosh_output) {
  target_column_name <- paste0("field", target_item[ , "FieldItem.name.tr..field......", drop=T])
  target_column_index <- CheckColname(target_column_name, ptosh_input)
  if (target_column_index == 0) {
    return()
  }
  temp_var_labels <- "症例登録番号"

  temp <- EditCtcae(target_item, temp_var_labels, ptosh_input, target_column_index, ptosh_output)
  ctcae_term_colname <- temp$ctcae_term_colname
  ptosh_output <- temp$ptosh_output
  temp_var_labels <- temp$temp_var_labels

  temp <- EditCheckBox(ptosh_input, target_item, temp_var_labels, ptosh_output)
  target_item[ , "Option.name"] <- temp$option_name
  temp_var_labels <- temp$temp_var_labels
  ptosh_output <- temp$ptosh_output
  
  ptosh_output <- EditOptionValue(target_item, ptosh_output, ctcae_term_colname)

  var_label(ptosh_output) <- temp_var_labels
  return(ptosh_output)
}
TEST20241016 <- function(aliasName) {
  file_index <- GetFileIndex(aliasName)
  ptosh_input <- GetPtoshInput(aliasName, file_index)
  # Create data frame with registration number only
  ptosh_output <- data.frame(id = ptosh_input[kPtoshRegistrationNumberColumnIndex])
  colnames(ptosh_output) <- kRegistration_colname
  df_itemlist <- subset(sheet_csv, sheet_csv[ ,"Sheet.alias_name"] == aliasName)
  output_df <- NULL
  for (j in 1:nrow(df_itemlist)) {
    # Skip if there is no column with that name
    temp_ptosh_output <- TEST20241016_2(df_itemlist[j, ], ptosh_input, ptosh_output)
    if (j == 1) {
      output_df <- temp_ptosh_output
    } else {
      output_df <- output_df %>% inner_join(temp_ptosh_output, by="SUBJID", relationship="many-to-many")
    }
  }
  assign(aliasName, output_df, envir=globalenv())
}

for (i in 1:length(alias_name)) {
  TEST20241016(alias_name[i])
}
OutputMergeExcludedSheet <- function(aliasName) {
  if (exists("allocation_csv")) {
    temp <- SetAllocation(allocation_csv, get(aliasName))
  } else {
    temp <- get(aliasName)
  }
  assign(aliasName, temp)
  OutputDF(aliasName, output_path, output_path)
}
CreatePtdata <- function(aliasName) {
  temp_merge_df <- data.frame(id = get(aliasName)[ , kRegistration_colname])
  colnames(temp_merge_df) <- kRegistration_colname
  return(temp_merge_df)  
}
for (i in 1:length(alias_name)) {
  # Edit output dataframe
  if (sheet_category[i] %in% kMerge_excluded_sheet_category) {
    OutputMergeExcludedSheet(alias_name[i])
  } else {
    # Merge
    if (!exists(kOutput_DF)) {
      assign(kOutput_DF, CreatePtdata(alias_name[i]))
    }
    temp_var_labels <- c(unlist(var_label(get(kOutput_DF))), unlist(var_label(get(alias_name[i])))[-1])
    assign(kOutput_DF, merge(get(kOutput_DF), get(alias_name[i]), by=kRegistration_colname, all=T))
    var_label(ptdata) <- temp_var_labels
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
