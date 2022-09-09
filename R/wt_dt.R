#https://www.anycodings.com/1questions/1240944/how-to-pretty-print-data-tables-using-the-same-pretty-printing-of-tibbles

#' @keywords internal
print_wt_dt <- function(x, ...) {
  print_txt <- capture.output(print(tibble::as_tibble(x), ...))
  print_txt[1] <- sub('tibble', 'data.table', print_txt[1])
  cat(print_txt, sep = '\n')
  invisible(x)
}

#' @export
print.wt_dt <- function(x, ...) {
  print_wt_dt(x, ...)
}
