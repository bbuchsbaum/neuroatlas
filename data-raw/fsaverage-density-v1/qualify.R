#!/usr/bin/env Rscript
# Candidate evaluation: no registry admission is needed or inferred.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, !dir.exists(args[1]))
out <- args[1]
dir.create(out, recursive = TRUE)
devtools::load_all(quiet = TRUE)
base <- 'data-raw/fsaverage-density-v1'
contract <- jsonlite::read_json(file.path(base, 'contract-v1.json'))
sha <- function(p) digest::digest(file = p, algo = 'sha256')
stopifnot(neuroatlas:::.surface_engine_revision_verified(contract$engine_revision))
binding_path <- Sys.getenv('NEUROATLAS_ENGINE_BINDING')
stopifnot(file.copy(binding_path, out))
binding_paths <- binding_path
consumer_sources <- c('R/surface_assets.R', 'R/surface_domain.R',
  'R/surface_transform.R', file.path('inst/extdata', c('surface-inputs-v1.json',
  'surface-domains-v1.json', 'surface-density-inputs-v1.json',
  'surface-density-domains-v1.json')))
consumer_hashes <- as.list(setNames(vapply(consumer_sources, sha, character(1)), consumer_sources))
write_array <- function(x, path, integer = FALSE) {
  con <- file(path, 'wb'); on.exit(close(con))
  writeBin(if (integer) as.integer(t(x)) else as.double(t(x)), con,
           size = if (integer) 4L else 8L, endian = 'little')
}
export_case <- function(name, src, dst, op, sampled = TRUE) {
  folder <- file.path(out, name); dir.create(folder)
  p <- op$plan
  write_array(src$sphere, file.path(folder, 'source.bin'))
  write_array(src$triangles, file.path(folder, 'faces.bin'), TRUE)
  write_array(dst$sphere, file.path(folder, 'target.bin'))
  write_array(cbind(p$rows - 1L, p$cols - 1L, p$vals),
              file.path(folder, 'weights.bin'))
  indices <- if (sampled) {
    minimum <- tapply(p$vals, p$rows, min)
    unique(c(sample.int(p$n_reference, contract$random_targets_per_route),
      as.integer(names(sort(minimum)))[seq_len(contract$small_positive_weight_targets_per_route)]))
  } else seq_len(p$n_reference)
  manifest <- list(name = name, source = src$domain, target = dst$domain,
    engine = op$specification$engine, sampled_targets_zero_based = indices - 1L,
    files = lapply(list.files(folder, full.names = TRUE), function(f)
      list(file = basename(f), sha256 = sha(f))))
  jsonlite::write_json(manifest, file.path(folder, 'case.json'), auto_unbox = TRUE,
                       pretty = TRUE, digits = 17)
  name
}
make_geometry <- function(v, f, mask = rep(TRUE, nrow(v)), name = 'toy') {
  d <- surface_domain(name, 'L', paste0(nrow(v), 'v'), v, f, mask,
                      'analytic', 'native-v1')
  surface_geometry(d, v, f, mask)
}
f <- rbind(c(0,2,4),c(2,1,4),c(1,3,4),c(3,0,4),
           c(2,0,5),c(1,2,5),c(3,1,5),c(0,3,5))
v <- rbind(c(1,0,0),c(-1,0,0),c(0,1,0),c(0,-1,0),c(0,0,1),c(0,0,-1))
geometry_for_query <- function(q) {
  q <- q / sqrt(sum(q*q))
  pivot <- diag(3)[which.min(abs(q)), ]
  b <- pivot - sum(pivot*q)*q; b <- b/sqrt(sum(b*b))
  c <- c(q[2]*b[3]-q[3]*b[2], q[3]*b[1]-q[1]*b[3], q[1]*b[2]-q[2]*b[1])
  make_geometry(v %*% rbind(q,b,c), f)
}
# Freeze perturbations before examining oracle or Workbench output.
perturb <- c(0, 1e-2, -1e-2, 1e-4, -1e-4, 1e-6, -1e-6, 1e-8, -1e-8)
queries <- rbind(diag(3), c(1,1,1)/sqrt(3), c(1,1,0)/sqrt(2))
for (epsilon in perturb) {
  w <- c(0.375, 0.625-epsilon, epsilon)
  shift <- (-1 + sqrt(1 + 3*(1-sum(w*w))))/3
  queries <- rbind(queries, w+shift)
}
cases <- list(); checks <- list(); set.seed(contract$seed)
for (i in seq_len(nrow(queries))) {
  src <- make_geometry(v, f)
  dst <- geometry_for_query(queries[i, ])
  op <- get_surface_transform(src, dst, NULL)
  if (i <= 3L) {
    row <- which(op$plan$rows == 1L)
    stopifnot(length(row)==1L,op$plan$vals[row]==1,op$plan$cols[row]==c(1L,3L,5L)[i])
  }
  y <- apply_surface_transform(surface_data(diag(6), src$domain), op)
  reordered <- make_geometry(v*7, f[8:1,3:1])
  op2 <- get_surface_transform(reordered, dst, NULL)
  z <- apply_surface_transform(surface_data(diag(6), reordered$domain), op2)
  stopifnot(max(abs(y$values-z$values)) <= 1e-12)
  cases[[length(cases)+1L]] <- export_case(sprintf('synthetic-%02d', i), src, dst, op, FALSE)
}
# Tiny positive contributor alone must survive exclusion of the other two.
epsilon <- 1e-8; w <- c(0.375,0.625-epsilon,epsilon)
q <- w + (-1 + sqrt(1+3*(1-sum(w*w))))/3
dst <- geometry_for_query(q)
src <- make_geometry(v, f, c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE))
op <- get_surface_transform(src,dst,NULL)
for (kind in c('continuous','label')) {
  x <- surface_data(c(0,0,0,0,17,0), src$domain, kind)
  y <- apply_surface_transform(x,op)
  stopifnot(y$values[1] == 17, y$coverage$available[1],
            y$coverage$source_weight_mass[1] > 0)
  x <- surface_data(c(0,0,0,0,NA,0), src$domain, kind)
  for (policy in c('propagate','omit')) {
    y <- apply_surface_transform(x,op,policy)
    stopifnot(is.na(y$values[1]), !y$coverage$available[1])
  }
  err <- try(apply_surface_transform(x,op,'error'),silent=TRUE)
  stopifnot(inherits(err,'try-error'))
}
src <- make_geometry(v,f,rep(FALSE,6))
y <- apply_surface_transform(surface_data(rep(1,6),src$domain),
                              get_surface_transform(src,dst,NULL))
stopifnot(all(is.na(y$values)), !any(y$coverage$available))
# Exact dyadic ties: aggregate smallest key, largest smallest source index.
src <- make_geometry(v,f); dst <- geometry_for_query(c(1,1,0))
op <- get_surface_transform(src,dst,NULL)
x <- surface_data(c(9,0,2,0,0,0),src$domain,'label')
stopifnot(apply_surface_transform(x,op,label_method='aggregate')$values[1] == 2,
          apply_surface_transform(x,op,label_method='largest')$values[1] == 9)
checks$synthetic_policy <- TRUE
geometries <- list(); areas <- list()
for (hemi in c('L', 'R')) for (density in c('164k', '41k', '10k')) {
  name <- paste('fsaverage', density, hemi, sep = '-')
  geometry <- get_surface_geometry('fsaverage', density, hemi, cache_dir = args[2])
  geometries[[name]] <- geometry
  catalog <- neuroatlas:::.surface_input_json(if (density == '164k')
    'surface-domains-v1.json' else 'surface-density-domains-v1.json')
  lock <- neuroatlas:::.surface_input_json(if (density == '164k')
    'surface-inputs-v1.json' else 'surface-density-inputs-v1.json')
  area_path <- catalog$domains[[name]]$assets$area
  asset <- Filter(function(a) a$path == area_path, lock$assets)[[1]]
  areas[[name]] <- as.numeric(neuroatlas:::.read_locked_surface_input(
    asset, lock, args[2], TRUE, FALSE)$data[[1]])
}
route_reports <- list()
for (hemi in c('L','R')) for (density in c('41k', '10k')) for (down in c(TRUE,FALSE)) {
  names <- c(paste0('fsaverage-164k-',hemi),paste0('fsaverage-',density,'-',hemi))
  if (!down) names <- rev(names)
  src <- geometries[[names[1]]]; dst <- geometries[[names[2]]]
  name <- paste(names,collapse='_to_')
  cat('Building',name,'\n')
  op <- get_surface_transform(src, dst, cache_dir = file.path(out, 'cache'))
  stopifnot(identical(op, get_surface_transform(src, dst,
    cache_dir = file.path(out, 'cache'), offline = TRUE)))
  cases[[length(cases)+1L]] <- export_case(name,src,dst,op)
  unit <- src$sphere/sqrt(rowSums(src$sphere^2))
  values <- cbind(constant=rep(if(hemi=='L')0.25 else 0.75,nrow(unit)),
    x=unit[,1],y=unit[,2],z=unit[,3],bounded=sin(7*unit[,1])*cos(5*unit[,2]))
  y <- apply_surface_transform(surface_data(values,src$domain),op)
  # Boundaries and omission exercise probability through the actual consumer.
  probabilities <- cbind(one=rep(1,nrow(unit)),zero=0,partial=0.25)
  for (policy in c('propagate','omit')) {
    probabilities[1,3] <- NA_real_
    prob <- apply_surface_transform(surface_data(probabilities,src$domain,'probability'),op,policy)
    stopifnot(max(abs(prob$values[prob$coverage$available[,1],1]-1))<=1e-12,
      all(prob$values[prob$coverage$available[,2],2]==0),
      max(abs(prob$values[,3]-0.25),na.rm=TRUE)<=1e-12)
  }
  available <- y$coverage$available
  stopifnot(all(is.finite(op$plan$vals)),all(op$plan$vals>0),
    all(op$plan$support=='triangle'),
    max(abs(y$values[available[,1],1]-values[1,1]))<=1e-12,
    max(abs(y$values[available]))<=1+1e-12,
    all(is.na(y$values[!dst$cortex,])))
  # Full arrays, not sampled: reorder both face order and winding.
  sr <- src; sr$triangles <- src$triangles[nrow(src$triangles):1,3:1]
  sr$domain <- surface_domain(src$domain$template,hemi,src$domain$density,
    sr$sphere,sr$triangles,sr$cortex,src$domain$registration,src$domain$revision,
    vertex_area=areas[[names[1]]])
  op2 <- get_surface_transform(sr,dst,NULL)
  y2 <- apply_surface_transform(surface_data(values,sr$domain),op2)
  difference <- max(abs(y$values-y2$values),na.rm=TRUE)
  stopifnot(difference<=1e-12,identical(y$coverage$available,y2$coverage$available))
  table <- data.frame(key=c(0,11,12),name=c('zero','positive','negative'))
  labels <- ifelse(unit[,1]>=0,11,12); labels[unit[,3]>0.8] <- 0
  for (policy in c('aggregate','largest')) {
    lab <- apply_surface_transform(surface_data(labels,src$domain,'label',table),op,
                                   label_method=policy)
    stopifnot(all(lab$values[lab$coverage$available] %in% table$key),
              identical(lab$label_table,table))
    write_array(matrix(lab$values),file.path(out,name,paste0(policy,'.bin')))
  }
  write_array(values,file.path(out,name,'values.bin'))
  write_array(y$values,file.path(out,name,'output.bin'))
  write_array(matrix(as.integer(src$cortex)),file.path(out,name,'source-mask.bin'),TRUE)
  write_array(matrix(as.integer(dst$cortex)),file.path(out,name,'target-mask.bin'),TRUE)
  write_array(matrix(labels),file.path(out,name,'labels.bin'))
  analytic <- dst$sphere/sqrt(rowSums(dst$sphere^2))
  error <- y$values[,2]-analytic[,1]
  awrmse <- sqrt(weighted.mean(error^2,areas[[names[2]]],na.rm=TRUE))
  png(file.path(out,paste0(name,'.png')),width=1400,height=650)
  par(mfrow=c(1,2),mar=c(4,4,3,1))
  lon <- atan2(analytic[,2],analytic[,1]); lat <- asin(analytic[,3])
  palette <- hcl.colors(101,'Blue-Red 3')
  colors <- palette[pmax(1,pmin(101,round(51+50*error/max(abs(error),na.rm=TRUE))))]
  plot(lon,lat,col=colors,pch=16,cex=.25,main=paste(name,'x-ramp error'),
       xlab='Sphere longitude',ylab='Sphere latitude')
  plot(lon,lat,col=ifelse(!dst$cortex,'grey80',ifelse(is.na(lab$values),'black',
       c('gold','steelblue','tomato')[match(lab$values,table$key)])),pch=16,cex=.25,
       main='Labels and medial wall',xlab='Sphere longitude',ylab='Sphere latitude')
  dev.off()
  route_reports[[name]] <- list(from_domain_id=src$domain$id, to_domain_id=dst$domain$id, available=sum(available[,1]),target_vertices=nrow(analytic),
    face_order_max_error=difference,area_weighted_x_ramp_rmse=awrmse,
    maximum_x_ramp_error=max(abs(error),na.rm=TRUE),operator_id=op$integrity)
}
# Finalize manifests after all policy arrays have been written.
case_hashes <- list()
for (name in unlist(cases)) {
  folder <- file.path(out,name); path <- file.path(folder,'case.json')
  manifest <- jsonlite::read_json(path)
  files <- list.files(folder,full.names=TRUE,pattern='[.]bin$')
  manifest$files <- lapply(files,function(f) list(file=basename(f),sha256=sha(f)))
  jsonlite::write_json(manifest,path,auto_unbox=TRUE,pretty=TRUE,digits=17)
  case_hashes[[name]] <- sha(path)
}
receipt <- list(case_manifest_sha256=case_hashes,consumer_source_sha256=consumer_hashes,
  engine_revision=contract$engine_revision,
  upstream_bindings=lapply(binding_paths,
    function(path) list(file=basename(path),sha256=sha(path))),
  contract_sha256=sha(file.path(base,'contract-v1.json')),
  script_sha256=sha(file.path(base,'qualify.R')),input_lock_sha256=sha('inst/extdata/surface-density-inputs-v1.json'),
  engine=op$specification$engine,seed=contract$seed,cases=cases,
  synthetic_policy=checks$synthetic_policy,routes=route_reports,
  status='consumer checks passed; independent oracle pending')
jsonlite::write_json(receipt,file.path(out,'consumer-receipt.json'),pretty=TRUE,
                     auto_unbox=TRUE,digits=17)
