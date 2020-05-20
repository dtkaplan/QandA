#' Yes/No questions
#'
#' @details The words used for true and false and the words used to indicate
#' a correct answer are selected at random. But they are determined when
#' the document is compiled, so they will  be random only  from one question to another
#' not from multiple responses to the same question.
#'
#' @param prompt Character  string asking the true or false question.
#' @param istrue logical containing answer to the question `TRUE` or `FALSE`
#' @param right character string to give for correct answer
#' @param wrong character string  to give for wrong answer
#' @param points Number: How many points is this  question worth. `NULL`  if
#' you don't want to set a number of points.
#'
#' @export
yesno <- function(
  prompt = "Is the moon made of blue cheese?",
  istrue = TRUE,

  right = "Good job",
  wrong = "Not so good!",
  points = 5
) {
  word <- yesno_word()
  summary_word <- rightwrong_word()
  if (!is.null(points))
    prompt <- glue::glue("{prompt}         . . . . . {points} points")
  learnr::question_radio(
    text = prompt,
    options = list(points = points),
    answer(word[1], correct=istrue,
           message = ifelse(istrue, right,  wrong)),
    answer(word[2], correct= !istrue,
           message = ifelse(istrue, wrong, right)),
    correct = summary_word[1],
    incorrect = summary_word[2]

  )
}

#'
#'
yesno_word  <- function() {
  choices <- list(
    c("yes",  "no"),
    c("right",  "wrong"),
    c("true", "false"),
    c("yup", "nope"),
    c("It's correct", "It's not.")
  )
  res  <- choices[[sample(1:length(choices), size=1)]]
  names(res) <- c("true", "false")

  res
}

rightwrong_word  <- function() {
  choices <- list(
    c("Good.", "Sorry."),
    c("Right!", "No."),
    c("Correct", "No."),
    c("Correct", "Wrong."),
    c("Right",  "Incorrect"),
    c("You're right", "Sorry,  you're mistaken."),
    c("Congrats!", "Sorry"),
    c("Perfect", "You're on the wrong track." ),
    c("Right on the nail!", "A miss. Sorry."),
    c("Hit the target!", "Missed it."),
    c("On the mark!", "Sorry."),
    c("Spot on!", "Better luck next time.")
  )

  res  <- choices[[sample(1:length(choices), size=1)]]
  names(res) <- c("right", "wrong")

  res
}
