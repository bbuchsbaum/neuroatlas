# Smoke tests for .require_suggest(), the shared gate that protects
# Suggests-only dependencies (scico, Rnanoflann, downloader). These don't
# try to uninstall real packages; they just confirm the happy path is a
# no-op and the miss path raises a classed condition with a useful message.

test_that(".require_suggest returns invisibly when the package is installed", {
  # `tools` is always installed (base R). This covers the happy path.
  out <- withVisible(.require_suggest("tools"))
  expect_true(out$value)
  expect_false(out$visible)
})

test_that(".require_suggest raises neuroatlas_error_missing_suggest when package is absent", {
  # A package name that cannot exist on CRAN (dots aren't valid in package
  # names), so requireNamespace() always returns FALSE.
  expect_error(
    .require_suggest("this.package.does.not.exist"),
    class = "neuroatlas_error_missing_suggest"
  )
})

test_that(".require_suggest mentions the requested feature in its message", {
  err <- tryCatch(
    .require_suggest("this.package.does.not.exist", feature = "unit testing"),
    neuroatlas_error_missing_suggest = function(e) e
  )
  expect_true(inherits(err, "neuroatlas_error_missing_suggest"))
  msg <- conditionMessage(err)
  expect_match(msg, "unit testing")
  expect_match(msg, "install.packages")
})
