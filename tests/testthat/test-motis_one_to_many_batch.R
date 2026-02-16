test_that("motis_one_to_many_batch requires valid data_dir", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     id = c("X", "Y"))

  expect_error(
    motis_one_to_many_batch(one, many, data_dir = "/nonexistent/path"),
    "No such file or directory"
  )
})
