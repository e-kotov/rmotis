# Implementation Plan: Unified One-to-Many Routing Interface

## Phase 1: Research & Design
- [ ] Task: Analyze current `motis_one_to_many` and `motis_one_to_many_batch` for common patterns.
- [ ] Task: Design the internal dispatcher and unified argument mapping.
- [ ] Task: Research Hilbert/Z-order sorting implementation in R (using `sf::st_geohash` or bit-interleaving).
- [ ] Task: Conductor - User Manual Verification 'Phase 1: Research & Design' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_1.R` to demonstrate spatial sorting.**

## Phase 2: Infrastructure & Helpers
- [ ] Task: Implement `spatial_sort_points()` helper with Z-order/Hilbert support.
- [ ] Task: Implement `smart_chunk_dispatch()` helper to calculate optimal batch sizes and destination splits.
- [ ] Task: Implement `unified_output_handler()` to manage streaming to Parquet, CSV, and DuckDB.
- [ ] Task: Create `temp_file_manager()` to handle lifecycle of intermediate files in `temp_dir`.
- [ ] Task: Conductor - User Manual Verification 'Phase 2: Infrastructure & Helpers' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_2.R` to test chunking and temp files.**

## Phase 3: API Backend Refactor
- [ ] Task: Update `.motis_one_to_many_calc` to use the new infrastructure (spatial sort, smart chunking).
- [ ] Task: Integrate `unified_output_handler` into the API backend.
- [ ] Task: Add support for `max_destinations_per_batch` in the API backend.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: API Backend Refactor' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_3.R` to test the API backend.**

## Phase 4: CLI Batch Backend Refactor
- [ ] Task: Refactor `motis_one_to_many_batch` into an internal backend function `.motis_one_to_many_batch_cli`.
- [ ] Task: Align the CLI backend with the new `temp_file_manager` and `unified_output_handler`.
- [ ] Task: Ensure `spatial_sort` and `spatial_filter` are fully integrated.
- [ ] Task: Conductor - User Manual Verification 'Phase 4: CLI Batch Backend Refactor' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_4.R` to test the CLI backend.**

## Phase 5: Unification & Entry Point
- [ ] Task: Implement the new unified `motis_one_to_many()` entry point with `engine` dispatch.
- [ ] Task: Update documentation and examples for `motis_one_to_many()`.
- [ ] Task: Deprecate `motis_one_to_many_batch()` (or make it a thin wrapper).
- [ ] Task: Conductor - User Manual Verification 'Phase 5: Unification & Entry Point' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_5.R` for the final routing workflow.**

## Phase 6: Testing & Validation
- [ ] Task: Create comprehensive tests for unified output formats (Parquet, CSV, DuckDB).
- [ ] Task: Create tests for 'smart' chunking logic with edge cases (single origin, millions of destinations).
- [ ] Task: Verify spatial sorting efficiency with a benchmark-like test.
- [ ] Task: Perform end-to-end validation with a local MOTIS instance.
- [ ] Task: Conductor - User Manual Verification 'Phase 6: Testing & Validation' (Protocol in workflow.md). **Verification: R script `local-testing/verify_phase_6.R` to run benchmarks.**
