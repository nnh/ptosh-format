# テスト用ファイル作成
library(tidyverse)
library(here)
input_csv <- data.frame(c(1, 2, 3),
                        c('アイウエオ', 'かきくけこ', '漢字'),
                        c('aaa', 'bbb', 'ccc'))
colnames(input_csv) <- c('テスト列１', 'テスト列２', 'テスト列３')
write.csv(input_csv, here('test-utf8.csv'), row.names=F, fileEncoding='utf-8')
write_excel_csv(input_csv, here('test-utf8bom.csv'))
write.csv(input_csv, here('test-shiftjis.csv'), row.names=F, fileEncoding='cp932')
