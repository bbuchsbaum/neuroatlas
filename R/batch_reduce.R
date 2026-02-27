#' Batch Reduce Multiple Volumes by an Atlas
#'
#' Applies \code{\link{reduce_atlas}} to multiple input volumes (or file
#' paths) and combines the results into a single tibble with a
#' \code{subject} column identifying each input.
#'
#' @param inputs A named list of inputs. Each element can be:
#'   \itemize{
#'     \item A \code{NeuroVol} (3D) or \code{NeuroVec} (4D) object
#'     \item A character string file path (read via \code{neuroim2::read_vol})
#'   }
#'   If unnamed, subjects are auto-named \code{"sub_001"}, \code{"sub_002"}, etc.
#' @param atlas An atlas object.
#' @param stat_func Function to apply within each ROI (default: \code{mean}).
#' @param ... Additional arguments passed to \code{\link{reduce_atlas}}.
#' @param format Character, output format passed to \code{reduce_atlas}.
#'   Default \code{"long"}.
#' @param parallel Logical. If \code{TRUE}, uses
#'   \code{future.apply::future_lapply()} for parallel processing.
#'   Requires the \pkg{future.apply} package.
#' @param .progress Logical. If \code{TRUE} (default), displays a
#'   \code{cli} progress bar.
#'
#' @return A tibble with a \code{subject} column prepended to the
#'   \code{reduce_atlas} output for each input.
#'
#' @examples
#' \dontrun{
#' # With NeuroVol objects
#' vols <- list(sub01 = vol1, sub02 = vol2, sub03 = vol3)
#' results <- batch_reduce(vols, atlas, mean)
#'
#' # With file paths
#' files <- list(sub01 = "path/to/sub01.nii.gz",
#'               sub02 = "path/to/sub02.nii.gz")
#' results <- batch_reduce(files, atlas, mean, parallel = TRUE)
#' }
#'
#' @seealso \code{\link{reduce_atlas}} for single-volume extraction
#'
#' @importFrom dplyr bind_rows
#' @importFrom neuroim2 read_vol
#' @export
batch_reduce <- function(inputs, atlas, stat_func = mean, ...,
                          format = "long", parallel = FALSE,
                          .progress = TRUE) {
  if (!is.list(inputs)) {
    stop("'inputs' must be a list of NeuroVol/NeuroVec objects or file paths")
  }

  # Auto-name if unnamed
  if (is.null(names(inputs))) {
    names(inputs) <- sprintf("sub_%03d", seq_along(inputs))
  } else {
    # Fill in blanks
    unnamed <- names(inputs) == "" | is.na(names(inputs))
    if (any(unnamed)) {
      names(inputs)[unnamed] <- sprintf("sub_%03d", which(unnamed))
    }
  }

  # Worker function
  .process_one <- function(input, subject_name) {
    # Load if file path
    if (is.character(input)) {
      input <- neuroim2::read_vol(input)
    }

    result <- reduce_atlas(atlas, input, stat_func, ..., format = format)
    result$subject <- subject_name
    result
  }

  subject_names <- names(inputs)

  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package 'future.apply' is required for parallel processing. ",
           "Install it with install.packages('future.apply')")
    }

    results <- future.apply::future_mapply(
      .process_one,
      inputs,
      subject_names,
      SIMPLIFY = FALSE,
      future.seed = TRUE
    )
  } else {
    if (.progress && requireNamespace("cli", quietly = TRUE)) {
      cli::cli_progress_bar("Processing subjects", total = length(inputs))
    }

    results <- mapply(
      function(input, sname) {
        out <- .process_one(input, sname)
        if (.progress && requireNamespace("cli", quietly = TRUE)) {
          cli::cli_progress_update()
        }
        out
      },
      inputs,
      subject_names,
      SIMPLIFY = FALSE
    )

    if (.progress && requireNamespace("cli", quietly = TRUE)) {
      cli::cli_progress_done()
    }
  }

  # Combine all results
  combined <- dplyr::bind_rows(results)

  # Move subject column to front
  col_order <- c("subject", setdiff(names(combined), "subject"))
  combined <- combined[, col_order]

  combined
}
