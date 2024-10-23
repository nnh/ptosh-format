# Format Ptosh data for analysis program
# Created date: 2018/12/19
# Modification date: 2024/10/23
# Author: mariko ohtsuka
# Version: 1.0.1
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
if (length(merge_excluded_alias_name) > 0) {
  merge_alias_name <- alias_name[-which(alias_name %in% merge_excluded_alias_name)]
} else {
  merge_alias_name <- alias_name
}
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
  print(df_duplicated)
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
    allocation_csv <- ReadCsvFile(rawdata_path, file_list[file_index], NA)
    # sort subjid
    allocation_csv <- allocation_csv[order(allocation_csv[[kAllocationSubjidColumnIndex]]), ]
    break()
  }
}

assign(kOutput_DF, NULL)
for (i in 1:length(alias_name)) {
  temp <- EditOutputDf(alias_name[i])
  assign(alias_name[i], temp, envir=.GlobalEnv)
  if (!is.null(temp)) {
    res <- MergeData(alias_name[i], sheet_category[i], get(kOutput_DF))
    if (!is.null(res)) {
      assign(kOutput_DF, res, envir=.GlobalEnv)
    }
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
