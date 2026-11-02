# Returns True/False strings with an initial None, as in the assignment pseudocode.
crossunder <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2)) stop("Both arrays must be numeric")
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  if (length(arr1) == 0L) stop("Both arrays must be non-empty")
  signals <- rep("False", length(arr1))
  signals[1] <- "None"
  if (length(arr1) > 1L) {
    for (i in 2:length(arr1)) {
      if (!any(!is.finite(c(arr1[i - 1L], arr1[i], arr2[i - 1L], arr2[i]))) &&
          arr1[i] < arr2[i] && arr1[i - 1L] >= arr2[i - 1L]) signals[i] <- "True"
    }
  }
  signals
}
