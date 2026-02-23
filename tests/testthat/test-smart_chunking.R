
test_that(".smart_chunk_dispatch handles API defaults", {
  d <- rmotis:::.smart_chunk_dispatch(100, 1000, engine = "api")
  expect_equal(d$batch_size, 16L)
  expect_equal(length(d$dest_chunks), 1)
})

test_that(".smart_chunk_dispatch handles large destination counts", {
  # 50k dests, max 10k per batch -> 5 chunks
  d <- rmotis:::.smart_chunk_dispatch(10, 50000, engine = "api", max_destinations_per_batch = 10000)
  expect_equal(length(d$dest_chunks), 5)
  expect_equal(length(d$dest_chunks[[1]]), 10000)
})

test_that(".smart_chunk_dispatch handles Batch defaults", {
  d <- rmotis:::.smart_chunk_dispatch(100, 1000, engine = "batch")
  expect_equal(d$batch_size, 1000L)
  expect_equal(length(d$dest_chunks), 1)
})

test_that(".spatial_sort_points works", {
  pts <- cbind(lat = c(1, 2, 1.5), lon = c(1, 2, 1.5))
  idx <- rmotis:::.spatial_sort_points(pts, method = "z-order")
  expect_type(idx, "integer")
  expect_length(idx, 3)
  # 1.5, 1.5 should be between (1,1) and (2,2)
  expect_equal(idx, c(1, 3, 2))
})
