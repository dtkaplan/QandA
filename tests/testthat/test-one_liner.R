test_that("Assignment name checking works", {
  res <- one_liner("t <- 15^2", "q <- 15^2")
  expect_true(inherits(res, "grader_graded"))
  expect_false(res$correct)
  expect_true("You were asked assign to the name 'q'", res$message)
  # By design, the following works, since the assignment
  # will be ignored in grading
  res2 <- one_liner("t <- 15^2", "15^2")
  expect_true(res2$correct)
  res3 <- one_liner("15^2", "t <- 15^2")
  expect_false(res3$correct)
  res4 <- one_liner("15^2", "15^2")
  expect_true(res3$correct)
  })

test_that("The right message is returned", {
  res <- one_liner("15^2", "15^2", correct="Foobar", incorrect="googoo")
  expect_true(res$correct)
  expect_true(grepl("Foobar", res$message))
  res2 <- one_liner("15^2", "2^2", correct="Foobar", incorrect="googoo")
  expect_false(res2$correct)
  expect_true(grepl("googoo", res2$message))
  }
)
