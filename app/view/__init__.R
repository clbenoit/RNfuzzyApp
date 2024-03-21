#' @export
global <- new.env()
global$answer <- 42

#' @export
set_answer <- function(new_answer) {
  global$answer <- new_answer
}