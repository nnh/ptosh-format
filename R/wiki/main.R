# install.packages("rmarkdown")
# install.packages("here")
library(rmarkdown)
library(here)
output_parent_path <- "/Users/admin/Documents/GitHub/ptosh-format.wiki"
pic_path <- paste0(output_parent_path, "/images")
message_f <- F
rmarkdown::render(here("R", "wiki", "common.Rmd"), output_format="powerpoint_presentation",
                  output_dir="/Users/admin/Documents/GitHub/ptosh-format.wiki/common.pptx")
output_parent_path <- "/Users/admin/Documents/GitHub/ptosh-format.wiki"
pic_path <- "https://github.com/nnh/ptosh-format/wiki/images"
message_f <- T
rmarkdown::render(here("R", "wiki", "common.Rmd"), output_format="github_document",
                  output_dir="/Users/admin/Documents/GitHub/ptosh-format.wiki/common.md")
