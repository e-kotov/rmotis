# Implementation Plan: Unified One-to-Many Routing Interface

## Phase 1: Research & Design [checkpoint: 0020bdc]
- [x] Task: Analyze current `motis_one_to_many` and `motis_one_to_many_batch` for common patterns. 60a8bf4
- [x] Task: Design the internal dispatcher and unified argument mapping. 60a8bf4
- [x] Task: Research Hilbert/Z-order sorting implementation in R (using `sf::st_geohash` or bit-interleaving). 60a8bf4
- [x] Task: Conductor - User Manual Verification 'Phase 1: Research & Design' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_1.R` to demonstrate spatial sorting.** 0020bdc

## Phase 2: Infrastructure & Helpers [checkpoint: 390b326]
- [x] Task: Implement `spatial_sort_points()` helper with Z-order/Hilbert support. 43cefc0
- [x] Task: Implement `smart_chunk_dispatch()` helper to calculate optimal batch sizes and destination splits. 43cefc0
- [x] Task: Implement `unified_output_handler()` to manage streaming to Parquet, CSV, and DuckDB. 43cefc0
- [x] Task: Create `temp_file_manager()` to handle lifecycle of intermediate files in `temp_dir`. 43cefc0
- [x] Task: Conductor - User Manual Verification 'Phase 2: Infrastructure & Helpers' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_2.R` to test chunking and temp files.** 390b326

## Phase 3: API Backend Refactor [checkpoint: a039712]
- [x] Task: Update `.motis_one_to_many_calc` to use the new infrastructure (spatial sort, smart chunking). 50c42ca
- [x] Task: Integrate `unified_output_handler` into the API backend. 50c42ca
- [x] Task: Add support for `max_destinations_per_batch` in the API backend. 50c42ca
- [x] Task: Conductor - User Manual Verification 'Phase 3: API Backend Refactor' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_3.R` to test the API backend.** a039712

## Phase 4: CLI Batch Backend Refactor [checkpoint: 61a92f9]
- [x] Task: Refactor `motis_one_to_many_batch` into an internal backend function `.motis_one_to_many_batch_cli`. 43bb757
- [x] Task: Align the CLI backend with the new `temp_file_manager` and `unified_output_handler`. 43bb757
- [x] Task: Ensure `spatial_sort` and `spatial_filter` are fully integrated. 43bb757
- [x] Task: Conductor - User Manual Verification 'Phase 4: CLI Batch Backend Refactor' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_4.R` to test the CLI backend.** 61a92f9

## Phase 5: Unification & Entry Point [checkpoint: 69acd33]
- [x] Task: Implement the new unified `motis_one_to_many()` entry point with `engine` dispatch. 32bc9e8
- [x] Task: Update documentation and examples for `motis_one_to_many()`. 32bc9e8
- [x] Task: Deprecate `motis_one_to_many_batch()` (or make it a thin wrapper). 32bc9e8
- [x] Task: Conductor - User Manual Verification 'Phase 5: Unification & Entry Point' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_5.R` for the final routing workflow.** 69acd33

## Phase 6: Testing & Validation [checkpoint: 6887c40]
- [x] Task: Create comprehensive tests for unified output formats (Parquet, CSV, DuckDB). 0fb81f0
- [x] Task: Create tests for 'smart' chunking logic with edge cases (single origin, millions of destinations). 0fb81f0
- [x] Task: Verify spatial sorting efficiency with a benchmark-like test. 0fb81f0
- [x] Task: Perform end-to-end validation with a local MOTIS instance. 0fb81f0
- [x] Task: Conductor - User Manual Verification 'Phase 6: Testing & Validation' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_6.R` to run benchmarks.** 6887c40

## Phase: Review Fixes
- [x] Task: Apply review suggestions e8c6d77
