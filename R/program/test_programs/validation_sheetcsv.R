# validation用Sheet.csvをCP932とUTF-8で作成するテストプログラム
# Created date: 2019/1/28
# Author: mariko ohtsuka
# Initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
parent_path <- "/Users/admin/Desktop/NHOH-R-miniCHP/validation"
# Setting of input/output path
input_path <- paste0(parent_path, "/ipt")
download_csv_path <- paste0(parent_path, "/download")
output_prt_path <- paste0(parent_path, "/output")
output_cp932_path <- paste0(output_prt_path, "/shift_jis")
output_utf8_path <- paste0(output_prt_path, "/utf-8")
col1 <- paste0("Sheet", "#", "alias_name")
col2 <- paste0("Sheet", "#", "category")
col3 <- paste0("Sheet", "#", "is_serious?")
col4 <- paste0("FieldItem", "#", "label")
col5 <- paste0("FieldItem", "#", "name.tr('field', '')")
col6 <- paste0("FieldItem", "#", "field_type")
col7 <- paste0("Option", "#", "name")
col8 <- "variable"
output_colnames <- c(col1,col2,col3,col4,col5,col6,col7,col8)
# 最新のフォーマットのsheet.csv
input_df <- read.csv(paste(input_path, "sheet.csv", sep="/"), as.is=T, na.strings="", fileEncoding="cp932")
# Googleドライブからダウンロードしたsheet.csvのフォーマットが異なるため最新のsheet.csvに変数を移行する
download_df <- read.csv(paste(download_csv_path, "sheet.csv", sep="/"), as.is=T, na.strings="", fileEncoding="UTF-8-BOM")
# シート名、フィールド番号でソート
sortlist <- order(input_df$Sheet.alias_name, pmax(input_df$Sheet.alias_name, input_df$FieldItem.name.tr..field......))
sort_input_df <- input_df[sortlist, ]
rownames(sort_input_df) <- c(1:nrow(sort_input_df))
sortlist <- order(download_df$Sheet.alias_name, pmax(download_df$Sheet.alias_name, download_df$FieldItem.name.tr..field......))
sort_download_df <- download_df[sortlist, ]
rownames(sort_download_df) <- c(1:nrow(sort_download_df))
# boolのT/Fを文字列にする
sort_input_df$Sheet.is_serious. <- ifelse(is.na(sort_input_df$Sheet.is_serious. == T), "true", ifelse(sort_input_df$Sheet.is_serious. == F, "false", NA))
# field番号を文字列にする
sort_input_df$FieldItem.name.tr..field...... <- as.character(sort_input_df$FieldItem.name.tr..field......)
# 0_動作確認
# - "variable"がNAでない項目が出力されている。NAの項目は出力されない。
# - ctcaeが仕様通り出力されている。
# - optionが仕様通り出力されている。
# - checkboxが仕様通り出力されている。
df_0 <- sort_input_df
# cancel$治療中止理由
df_0[15, "variable"] <- "cancel_reason"
# flowsheet1$発熱性好中球減少症
df_0[79, "variable"] <- "Febrile_neutropenia"
# initial$B症状_選択
df_0[446, "variable"] <- "B_symptom"
# registration$診断年月日
df_0[616, "variable"] <- "Diagnosis_date"
output_df <- df_0
colnames(output_df) <- output_colnames
sub_dirname <- "/0_"
write.csv(output_df, paste0(output_cp932_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="cp932")
write.csv(output_df, paste0(output_utf8_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="utf-8")
# 1_ googleスプレッドシートのsheet.csv$variableの内容をそのまま移行
# - ptdataに変数の重複あり（cancel$提出日とcancel$治療中止日）
# - aeとsae_report間に重複あり
df_1 <- sort_input_df[ , colnames(sort_input_df) != "variable"]
df_1 <- cbind(df_1, sort_download_df$variable)
colnames(df_1) <- colnames(input_df)
output_df <- df_1
colnames(output_df) <- output_colnames
sub_dirname <- "/1_"
write.csv(output_df, paste0(output_cp932_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="cp932")
write.csv(output_df, paste0(output_utf8_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="utf-8")

# 1_を流用し下記箇所を変更
# 2_ ptdataに変数の重複なし
# ae、sae_report、committees_opinion間に重複あり
df_2 <- df_1
# cancel$提出日のvariableを空白にする
df_2[9, "variable"] <- NA
# committees_opinion$厚生労働省への報告のvariableを"aeterm"にする
df_2[19, "variable"] <- "AETERM"
output_df <- df_2
colnames(output_df) <- output_colnames
sub_dirname <- "/2_"
write.csv(output_df, paste0(output_cp932_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="cp932")
write.csv(output_df, paste0(output_utf8_path, sub_dirname, "/sheet.csv"), na='""', row.names=F, fileEncoding="utf-8")
