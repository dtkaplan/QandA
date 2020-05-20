#' A question to pick among several choices
#'
#'
#' @export
choices <- function(
  prompt="The question",
  bad =list("Blue" = "Blue is too bright!",
                "Red" = "Red is too distracting"),
  good = list("Green" = "Yuck!!"),
  points = 5,
  need_all = length(good) > 1,
  allow_retry = TRUE
) {
  summary_word <- rightwrong_word()
  if (!is.null(points))
    prompt <- glue::glue("{prompt}         . . . . . {points} points")

  bads <- list(length(bad))
  for (k in 1:length(bad)) {
    bads[[k]] <- answer(names(bad)[k],
                          correct = FALSE,
                          message = bad[[k]])
  }

  goods <- list(length(good))
  for (k in 1:length(good)) {
    goods[[k]] <- answer(names(good)[k],
                          correct = TRUE,
                          message = good[[k]])
  }
  arguments <- c(
    list(
      text = prompt,
      options = list(points = points),
      correct = summary_word[1],
      incorrect = summary_word[2],
      allow_retry = allow_retry),

    sample(c(bads, goods))
  )

  qtype = ifelse(need_all, question_checkbox, question_radio)

  do.call(qtype, arguments)

}

#' @export
choice_list <- function(
  prompt = "The question",
  choices = list("Blue" = "Blue is too bright!",
                  "Red" = "Red is too distracting",
                  "*Green*" = "Green is a good color"),
  points = 5,
  need_all = NULL,
  allow_retry = TRUE,
  random_answer_order = FALSE)
  {
  summary_word <- rightwrong_word()
  if (is.null(need_all)) {
    n_correct <- sum(grepl("^\\+.*\\$", names(choices)))
    need_all <- n_correct > 1
  }
  if (need_all)
    prompt <- glue::glue("{prompt} (Check all that apply.)")

  if (!is.null(points))
    prompt <- glue::glue("{prompt}         . . . . . {points} points")
  answers <- list()
  for (k in 1:length(choices)) {
    text <- names(choices)[k]
    response <- choices[[k]]
    # Find out if it's correct
    if (grepl("^\\+.*\\+$", text)) {
      text <- gsub("^\\+|\\+$", "", text)
      correct <- TRUE
    } else {
      correct <- FALSE
    }
    if (response != "") response <- paste0(response,"</br>")
    answers[[k]] <- learnr::answer(text,
                          correct = correct,
                          message = response)
  }

  arguments <- list(
      text = prompt,
      options = list(points = points),
      correct = summary_word[1],
      incorrect = summary_word[2],
      allow_retry = allow_retry,
      random_answer_order = random_answer_order)

  qtype = ifelse(need_all, question_checkbox, question_radio)

  do.call(qtype, c(arguments, answers))
}
