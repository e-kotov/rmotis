# Implementation Plan: Spatial Filter Improvement

## Phase 1: Research & Benchmarking (Confirm Performance)
- [x] Task: Create a benchmark script to compare filtering methods.
- [x] Task: Analyze benchmark results and document findings.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Research & Benchmarking' (Protocol in workflow.md) [checkpoint: fa88f98]


## Phase 2: Refactoring & API Update (Red Phase)
- [x] Task: Create failing tests for the new `spatial_filter_km` argument.
    - [x] Create `tests/testthat/test-spatial_filter_km.R`.
    - [x] Write tests confirming `spatial_filter_km` drops points correctly based on a fixed radius.
    - [x] Write tests ensuring the old `spatial_filter` argument is deprecated/removed.
    - [x] Add integration test case in `local-testing/test_lux_spatial_filter.R` using the Luxembourg server setup.
- [x] Task: Verify that existing tests fail after removing old arguments.

## Phase 3: Implementation (Green Phase)
- [x] Task: Update the internal one-to-many infrastructure.
    - [x] Modify `.motis_one_to_many_infra` in `R/api_one_to_many.R`.
    - [x] Replace `spatial_filter` and `max_speed_kmh` logic with `spatial_filter_km`.
    - [x] Update `.process_batch_mirai` and `.process_parallel_batch` to handle the new argument.
- [x] Task: Update public function signatures and documentation.
    - [x] Update `motis_one_to_many()` in `R/api_one_to_many.R`.
    - [x] Update `motis_one_to_many_intermodal()` in `R/api_one_to_many_intermodal.R`.
    - [x] Update `motis_one_to_many_batch()` (deprecated wrapper).
    - [x] Ensure documentation clearly defines `spatial_filter_km` as straight-line distance.
- [x] Task: Conductor - User Manual Verification 'Phase 3: Implementation' (Protocol in workflow.md)

## Phase 4: Final Validation & Cleanup
- [x] Task: Run full test suite and verify coverage.
- [x] Task: Update documentation (pkgdown rebuild if necessary).
- [x] Task: Conductor - User Manual Verification 'Phase 4: Final Validation & Cleanup' (Protocol in workflow.md) [checkpoint: 388c930]

## Phase: Review Fixes
- [x] Task: Apply review suggestions 09882bb
- [x] Task: Modernize test suite and eliminate warnings 1039988
