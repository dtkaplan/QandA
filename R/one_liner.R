#' Grade one-line expressions
#'
#' One line expressions consist of just a single expression.
#'
#' @param user user code as a character string
#' @param soln solution code as a character string
#'
#' @export
one_liner <- function(user, soln,
                     correct = "Good",
                     incorrect = "Bad") {
  # default values for constructing the returned object
  is_correct <- FALSE
  message <- ""
  # Convert them to calls, checking whether assignment
  # is being requested
  soln_expr <- str2lang(soln)
  user_expr <- str2lang(user)
  if (inherits(soln_expr, "<-")) {
    soln_name <- as.character(soln_expr[[2]])
    if (inherits(user_expr, "<- ")) {
      # Check that the names match
      user_name <- as.character(user_expr[[2]])
      if (user_name != soln_name) {
        message = glue::glue("You used {user_name} for assignment. The instructions call for {soln_name}.")
      }
    } else {
      message = glue::glue("You were asked to assign to the name '{soln_name}'.")
    }
  }
  # If the message was set, return score as incorrect
  if (message != "") {
    retval <- list(
      message = message,
      correct = FALSE
    )
    class(retval) <- "grader_graded"
    return(retval)
  }

  if (inherits(user_expr, "<-")) {
    user_expr <- user_expr[[3]] # ignore the assigment
  }

  gradethis::grade_code(
    grader_args =
    list(user_quo = user_expr,
         solution_quo = soln_expr),
    correct = correct,
    incorrect = incorrect)

}
