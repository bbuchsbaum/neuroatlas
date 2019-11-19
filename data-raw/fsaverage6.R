setwd("data-raw")

library(neurosurf)
fnames <- list.files("fsaverage6", "[lr]h.[iopw].*", full.names=TRUE)

fsaverage <- lapply(fnames, function(fn) {
  print(fn)
  neurosurf::read_surf(fn)
})

names(fsaverage) <- gsub("\\.", "_", basename(fnames))
usethis::use_data(fsaverage)


#' This is data to be included in my package
#'
#' @author My Name \email{blahblah@@roxygen.org}
#' @references \url{data_blah.com}
"data-name"
