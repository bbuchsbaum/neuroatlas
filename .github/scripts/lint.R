# The package gate covers maintained package, test and vignette code.
# Qualification protocols and data builders have separate dependency environments
# and hash-bound execution checks; downloaded/frozen evidence is never reformatted.
devtools::load_all(quiet = TRUE)
lints <- lintr::lint_package(
  exclusions = list("data-raw", "tests/testthat/_problems"),
  show_progress = TRUE
)
if (length(lints)) {
  print(lints)
  stop(length(lints), " project lint findings must be resolved.")
}
message("Project lint gate passed.")
