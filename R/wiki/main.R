# install.packages("rmarkdown")
# install.packages("here")
library(rmarkdown)
library(here)
output_parent_path <- "/Users/admin/Documents/GitHub/ptosh-format.wiki"
pic_path <- paste0(output_parent_path, "/images")
rmarkdown::render(here("R", "wiki", "common.Rmd"), output_format="powerpoint_presentation",
                  output_dir=here("R", "wiki", "common.pptx"))
output_parent_path <- "/Users/admin/Documents/GitHub/ptosh-format.wiki"
pic_path <- "https://github.com/nnh/ptosh-format/wiki/images/"
rmarkdown::render(here("R", "wiki", "common.Rmd"), output_format="github_document",
                  output_dir="/Users/admin/Documents/GitHub/ptosh-format.wiki/common.md")
