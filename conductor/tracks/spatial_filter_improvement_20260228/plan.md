# Implementation Plan: Spatial Filter Improvement

## Phase 1: Research & Benchmarking (Confirm Performance)
- [ ] Task: Create a benchmark script to compare filtering methods.
    - [ ] Implement a script in `local-testing/benchmark_spatial_filter.R`.
    - [ ] Generate synthetic point sets (1k, 10k, 100k).
    - [ ] Benchmark three methods:
        1. Custom Bounding Box (Current Implementation).
        2. {sf} Bounding Box (`st_bbox` / `st_intersects`).
        3. {sf} Circular Check (`st_is_within_distance`).
    - [ ] Measure execution time and accuracy (dropped point count vs. exact radius).
- [ ] Task: Analyze benchmark results and document findings.
- [ ] Task: Conductor - User Manual Verification 'Phase 1: Research & Benchmarking' (Protocol in workflow.md)

## Phase 2: Refactoring & API Update (Red Phase)
- [ ] Task: Create failing tests for the new `spatial_filter_km` argument.
    - [ ] Create `tests/testthat/test-spatial_filter_km.R`.
    - [ ] Write tests confirming `spatial_filter_km` drops points correctly based on a fixed radius.
    - [ ] Write tests ensuring the old `spatial_filter` argument is deprecated/removed.
- [ ] Task: Verify that existing tests fail after removing old arguments.

## Phase 3: Implementation (Green Phase)
- [ ] Task: Update the internal one-to-many infrastructure.
    - [ ] Modify `.motis_one_to_many_infra` in `R/api_one_to_many.R`.
    - [ ] Replace `spatial_filter` and `max_speed_kmh` logic with `spatial_filter_km`.
    - [ ] Update `.process_batch_mirai` and `.process_parallel_batch` to handle the new argument.
- [ ] Task: Update public function signatures and documentation.
    - [ ] Update `motis_one_to_many()` in `R/api_one_to_many.R`.
    - [ ] Update `motis_one_to_many_intermodal()` in `R/api_one_to_many_intermodal.R`.
    - [ ] Update `motis_one_to_many_batch()` (deprecated wrapper).
    - [ ] Ensure documentation clearly defines `spatial_filter_km` as straight-line distance.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Implementation' (Protocol in workflow.md)

## Phase 4: Final Validation & Cleanup
- [ ] Task: Run full test suite and verify coverage.
- [ ] Task: Update documentation (pkgdown rebuild if necessary).
- [ ] Task: Conductor - User Manual Verification 'Phase 4: Final Validation & Cleanup' (Protocol in workflow.md)
