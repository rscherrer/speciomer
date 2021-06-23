# Function to remove an axis
rm_axis <- function(axis, line_too = TRUE) {

  # Build the call
  call <- c("title", "ticks", "text")
  if (line_too) call <- c(call, "line")
  call <- sprintf("axis.%s.%s = element_blank()", call, axis)
  call <- paste0("theme(", reduce(call, paste, sep = ", "), ")")

  # Evaluate the call
  eval(parse_expr(call))

}
