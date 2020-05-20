#' Save learnr  exercise checking inputs to a file
#'
#' @export
save_check_inputs <- function(...) {
  inputs <- list(...)
  # What parse-time checking would look like
  parse_result <- QandA::parse_check(inputs$user_code)

  fname <- inputs$label
  timestamp <- format(Sys.time(), "%j-%H-%M-%OS")
  save_name <- glue::glue("~/Downloads/{fname}-{timestamp}.rda")

  # Take this out when you get gradethis working
  has_plot <- grepl("plot", inputs$user_code)
  message <- ifelse(has_plot, "Good!", "What happened to plot()?")

  retval <- list(message = message,
                 correct = !has_plot,
                 type = "warning",
                 location = "append",
                 # Add this to indicate that the
                 # result occurred during parsing phase
                 parsing_phase = FALSE)


  if (! parse_result$correct) {
    parse_result$location <- "append"
    parse_result$type <- "warning"
    parse_result$parsing_phase <- TRUE
    save(inputs, parse_result, file = save_name)
    return(parse_result)
  } else {
    # Do the code evaluation here and return
    result_env <- new.env()
    # 1. Evaluate the setup code.
    setup_output <-
      evaluate::evaluate(inputs$setup, envir = result_env)
    # 2. Evaluate the user code in the context set up by the
    #    setup code
    final_output <-
      evaluate::evaluate(inputs$user_code,
                         envir = result_env)


    # GET THE FINAL RESULT
    user_parsed <- parse(text = inputs$user_code)
    final_value <- eval(user_parsed[length(user_parsed)], envir = result_env)

    # temporary
    check_result <- retval

    save(inputs, setup_output, final_output, final_value,
         result_env, check_result, file = save_name)

    # setup inputs to include results of evaluation
    inputs$last_value <- final_value
    inputs$envir_result <- result_env
    inputs$evaluate_result <- final_output

    grade_res <- do.call(gradethis::grade_learnr, inputs)
    # save(inputs, setup_output, final_output, final_value,
    #      result_env, check_result, grade_res, file = save_name)

    return(grade_res)
  }
}


