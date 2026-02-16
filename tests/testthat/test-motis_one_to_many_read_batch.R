test_that("motis_one_to_many_read_batch parses basic response with distance", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  # Two origins, each with 3 destinations
  writeLines(c(
    '[{"duration":600,"distance":5000},{"duration":1200,"distance":10000},{"duration":900,"distance":7500}]',
    '[{"duration":300,"distance":2500},{"duration":800,"distance":6000},{"duration":450,"distance":3500}]'
  ), resp_file)

  writeLines(c(
    "A\tX\tY\tZ",
    "B\tX\tY\tZ"
  ), meta_file)

  result <- motis_one_to_many_read_batch(resp_file, meta_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)
  expect_named(result, c("from_id", "to_id", "duration_s", "distance_m"))

  expect_equal(result$from_id, c("A", "A", "A", "B", "B", "B"))
  expect_equal(result$to_id, c("X", "Y", "Z", "X", "Y", "Z"))
  expect_equal(result$duration_s, c(600, 1200, 900, 300, 800, 450))
  expect_equal(result$distance_m, c(5000, 10000, 7500, 2500, 6000, 3500))
})

test_that("motis_one_to_many_read_batch handles empty objects (no route)", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  writeLines(
    '[{"duration":600,"distance":5000},{},{"duration":900,"distance":7500}]',
    resp_file
  )
  writeLines("A\tX\tY\tZ", meta_file)

  result <- motis_one_to_many_read_batch(resp_file, meta_file)

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$duration_s[2]))
  expect_true(is.na(result$distance_m[2]))
  expect_equal(result$duration_s[1], 600)
  expect_equal(result$duration_s[3], 900)
})

test_that("motis_one_to_many_read_batch handles duration-only response", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  writeLines('[{"duration":600},{"duration":1200}]', resp_file)
  writeLines("A\tX\tY", meta_file)

  result <- motis_one_to_many_read_batch(resp_file, meta_file)

  expect_equal(nrow(result), 2)
  expect_named(result, c("from_id", "to_id", "duration_s"))
  expect_equal(result$duration_s, c(600, 1200))
})

test_that("motis_one_to_many_read_batch swaps columns with arrive_by", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  writeLines('[{"duration":600}]', resp_file)
  writeLines("A\tX", meta_file)

  result <- motis_one_to_many_read_batch(resp_file, meta_file, arrive_by = TRUE)

  # With arrive_by, "one" (A) was the destination, "many" (X) was the origin

  expect_equal(result$from_id, "X")
  expect_equal(result$to_id, "A")
})

test_that("motis_one_to_many_read_batch output_callback streams chunks", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  writeLines(c(
    '[{"duration":100}]',
    '[{"duration":200}]',
    '[{"duration":300}]'
  ), resp_file)
  writeLines(c("A\tX", "B\tX", "C\tX"), meta_file)

  collected <- list()
  callback <- function(chunk) {
    collected[[length(collected) + 1L]] <<- chunk
  }

  result <- motis_one_to_many_read_batch(
    resp_file, meta_file,
    chunk_size = 2L,
    output_callback = callback
  )

  expect_null(result)
  # 3 lines with chunk_size=2 → 2 chunks (2 lines + 1 line)
  expect_length(collected, 2)
  expect_equal(nrow(collected[[1]]), 2)
  expect_equal(nrow(collected[[2]]), 1)
})

test_that("motis_one_to_many_read_batch errors on mismatched file lengths", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  writeLines(c('[{"duration":100}]', '[{"duration":200}]'), resp_file)
  writeLines("A\tX", meta_file)

  expect_error(
    motis_one_to_many_read_batch(resp_file, meta_file),
    "different numbers of lines"
  )
})

test_that("motis_one_to_many_read_batch handles scientific notation", {
  resp_file <- tempfile()
  meta_file <- tempfile()
  on.exit(unlink(c(resp_file, meta_file)))

  # MOTIS often returns scientific notation
  writeLines(
    '[{"duration":7.02E2,"distance":5.920600620528969E3}]',
    resp_file
  )
  writeLines("A\tX", meta_file)

  result <- motis_one_to_many_read_batch(resp_file, meta_file)

  expect_equal(result$duration_s, 702)
  expect_equal(result$distance_m, 5920.600620528969, tolerance = 1e-6)
})
