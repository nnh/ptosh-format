#' @title test20211129.R
#' @description Test Script
#' @author mariko.ohtsuka
# Create test files.
library(tidyverse)
library(here)
library(RUnit)
input_csv <- data.frame(c(1, 2, 3),
                        c('アイウエオ', 'かきくけこ', '漢字'),
                        c('aaa', 'bbb', 'ccc'))
colnames(input_csv) <- c('テスト列１', 'テスト列２', 'テスト列３')
write.csv(input_csv, here('test-utf8.csv'), row.names=F, fileEncoding='utf-8')
write.csv(input_csv, here('test-shiftjis.csv'), row.names=F, fileEncoding='cp932')
# UTF8-BOMのファイルは自分で作る

source(here('program', 'ptosh-format-function.R'))
utf8 <- ReadCsvFile(here(), 'test-utf8.csv')
utf8bom <- ReadCsvFile(here(), 'test-utf8bom.csv')
shiftjis <- ReadCsvFile(here(), 'test-shiftjis.csv')
'compare utf8, utf8-bom' %>% print()
checkEquals(utf8, utf8bom) %>% print()
'compare utf8, shiftjis' %>% print()
checkEquals(utf8, shiftjis) %>% print()
'Compare the files of SAS and R processing results.' %>% print()
rm(list=ls())
#' @title ConvAllChar
#' @description Sort the columns, set the type to string.
#' @param input_df a data frame.
#' @return a data frame.
ConvAllChar <- function(input_df){
  temp <- input_df %>% select(order(colnames(input_df))) %>% map(~{as.character(.)}) %>% bind_rows()
  # NA -> ''
  temp <- temp %>% map(~{ifelse(is.na(.), '', .)}) %>% bind_rows()
  return(temp)
}
library(tidyverse)
library(here)
library(RUnit)
library(haven)
load(here('ads', 'R', 'ptdata.rda'))
r_ptdata <- ConvAllChar(ptdata)
rm(ptdata)
ptdata <- read_sas(here('ads', 'SAS', 'ptdata.sas7bdat'))
sas_ptdata <- ConvAllChar(ptdata)
sas_ptdata$Var_Obs <- NULL
'CA199 check' %>% print()
checkEqualsNumeric(is.numeric(r_ptdata$CA199), is.numeric(sas_ptdata$CA199)) %>% print()
'CEA check' %>% print()
checkEqualsNumeric(is.numeric(r_ptdata$CEA), is.numeric(sas_ptdata$CEA)) %>% print()
checkEquals(r_ptdata, sas_ptdata)
