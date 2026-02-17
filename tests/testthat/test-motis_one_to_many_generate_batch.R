test_that("motis_one_to_many_generate_batch works", {
  skip_if_not_installed("curl")

  output_file <- tempfile(fileext = ".txt")
  meta_file <- paste0(output_file, ".meta")
  on.exit(unlink(c(output_file, meta_file)))

  one <- data.frame(lat = 59.3, lon = 18.0)
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2))

  n <- motis_one_to_many_generate_batch(quiet = TRUE, one, many, output_file, mode = "WALK", max = 3600)
  expect_equal(n, 1L)

  lines <- readLines(output_file)
  expect_length(lines, 1)

  # Check for endpoint
  expect_match(lines, "/api/v1/one-to-many")
  # Check for escaped coordinates in 'one' (59.3;18) -> 59.3%3B18
  expect_match(lines, "one=59.3%3B18")
  # Check for escaped coordinates in 'many' (59.4;18.1,59.5;18.2)
  expect_match(lines, "many=59.4%3B18.1%2C59.5%3B18.2")
  # Check for other params
  expect_match(lines, "mode=WALK")
  expect_match(lines, "max=3600")

  # Check metadata sidecar file
  expect_true(file.exists(meta_file))
  meta_lines <- readLines(meta_file)
  expect_length(meta_lines, 1)
  meta_parts <- strsplit(meta_lines, "\t")[[1]]
  # Fallback to sequential IDs (no "id" column in data.frame)
  expect_equal(meta_parts, c("1", "1", "2"))
})

test_that("motis_one_to_many_generate_batch writes correct IDs from named columns", {
  skip_if_not_installed("curl")

  output_file <- tempfile(fileext = ".txt")
  meta_file <- paste0(output_file, ".meta")
  on.exit(unlink(c(output_file, meta_file)))

  one <- data.frame(lat = 59.3, lon = 18.0, name = "origin_A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     name = c("dest_X", "dest_Y"))

  motis_one_to_many_generate_batch(quiet = TRUE, 
    one, many, output_file, mode = "WALK",
    one_id_col = "name", many_id_col = "name"
  )

  meta_lines <- readLines(meta_file)
  meta_parts <- strsplit(meta_lines, "\t")[[1]]
  expect_equal(meta_parts, c("origin_A", "dest_X", "dest_Y"))
})

test_that("motis_one_to_many_generate_batch appends to both query and meta files", {
  skip_if_not_installed("curl")

  output_file <- tempfile(fileext = ".txt")
  meta_file <- paste0(output_file, ".meta")
  on.exit(unlink(c(output_file, meta_file)))

  one_1 <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  one_2 <- data.frame(lat = 59.4, lon = 18.1, id = "B")
  many <- data.frame(lat = c(59.5, 59.6), lon = c(18.2, 18.3),
                     id = c("X", "Y"))

  motis_one_to_many_generate_batch(quiet = TRUE, one_1, many, output_file, mode = "CAR")
  motis_one_to_many_generate_batch(quiet = TRUE, one_2, many, output_file, mode = "CAR",
                                   append = TRUE)

  query_lines <- readLines(output_file)
  expect_length(query_lines, 2)

  meta_lines <- readLines(meta_file)
  expect_length(meta_lines, 2)

  meta_1 <- strsplit(meta_lines[1], "\t")[[1]]
  meta_2 <- strsplit(meta_lines[2], "\t")[[1]]
  expect_equal(meta_1, c("A", "X", "Y"))
  expect_equal(meta_2, c("B", "X", "Y"))
})

test_that("motis_one_to_many_generate_batch validates params", {
  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file))
  
  one <- "59.3,18.0"
  many <- "59.4,18.1"
  
  # Invalid mode
  expect_error(
    motis_one_to_many_generate_batch(quiet = TRUE, one, many, output_file, mode = "INVALID"),
    "'arg' should be one of"
  )
  
  # Invalid additional param type
  expect_error(
    motis_one_to_many_generate_batch(quiet = TRUE, one, many, output_file, mode = "WALK", maxTransfers = "a"),
    "Parameter 'maxTransfers' must be numeric/integer"
  )
})
