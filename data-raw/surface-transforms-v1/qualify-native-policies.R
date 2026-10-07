#!/usr/bin/env Rscript
# Supplemental full-density all-source policy checks using exported fixed weights.
args <- commandArgs(TRUE); stopifnot(length(args)==1L)
base <- args[1]; devtools::load_all(quiet=TRUE)
receipt <- jsonlite::read_json(file.path(base,'consumer-receipt.json'))
reports <- list()
for (name in names(receipt$routes)) {
  folder <- file.path(base,name)
  meta <- jsonlite::read_json(file.path(folder,'case.json'),simplifyVector=TRUE)
  cache <- file.path(base,'cache/surface-operators-v1')
  candidates <- list.files(cache,pattern='[.]rds$',full.names=TRUE)
  operators <- lapply(candidates,readRDS)
  op <- Filter(function(x) identical(x$integrity,receipt$routes[[name]]$operator_id),operators)[[1]]
  p <- op$plan; ns <- p$n_moving; nt <- p$n_reference
  # Read-only operator copy: all-source policy is tested directly through engine;
  # public API domain bindings are exercised in the primary consumer run.
  p$source_mask <- rep(TRUE,ns); p$target_mask <- rep(TRUE,nt)
  value <- sin(seq_len(ns)*0.12345)
  result <- neurotransform::apply_surface_resampling(p,cbind(one=1,value=value),details=TRUE)
  stopifnot(all(result$available),max(abs(result$values[,1]-1))<=1e-12,
            max(abs(result$values[,2]))<=1+1e-12)
  # Independent row accumulation checks entire pinned-mask output, not just samples.
  getd <- function(file,ncol=1L) {
    con <- file(file.path(folder,file),'rb');on.exit(close(con))
    x <- readBin(con,'double',n=file.info(file.path(folder,file))$size/8,size=8,endian='little')
    matrix(x,ncol=ncol,byrow=TRUE)
  }
  values <- getd('values.bin',5L); actual <- getd('output.bin',5L)
  p <- op$plan
  groups <- split(seq_along(p$vals),p$rows)
  expected <- matrix(NA_real_,nt,5)
  for (r in seq_len(nt)) {
    ix <- groups[[r]]
    ix <- ix[p$source_mask[p$cols[ix]]]
    if (length(ix) && p$target_mask[r])
      expected[r,] <- colSums(values[p$cols[ix],,drop=FALSE]*p$vals[ix])/sum(p$vals[ix])
  }
  stopifnot(identical(is.na(actual),is.na(expected)))
  error <- max(abs(actual-expected),na.rm=TRUE)
  stopifnot(error<=1e-12)
  reports[[name]] <- list(full_rows=nt,all_source_pass=TRUE,
                          full_masked_continuous_max_error=error)
  cat(name,error,'\n')
}
jsonlite::write_json(list(status='PASS',cases=reports,
  script_sha256=digest::digest(file='data-raw/surface-transforms-v1/qualify-native-policies.R',algo='sha256')),
  file.path(base,'policy-receipt.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
