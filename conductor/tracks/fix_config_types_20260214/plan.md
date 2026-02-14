# Implementation Plan - Fix Config YAML Types

This plan addresses the issue where `motis_configure_server()` and related functions write numeric values as floating-point doubles in `config.yml`, causing the MOTIS server to fail during parsing.

## Phase 1: Reproduce and Test (TDD)
- [ ] Task: Create a reproduction test file `tests/testthat/test-config_types.R`.
- [ ] Task: Write unit tests that verify `motis_set_config()` behavior (mocking file I/O if necessary) or just verifying the list transformation logic.
- [ ] Task: Confirm the tests fail (Red phase) when expecting integer types for numeric-but-whole inputs.

## Phase 2: Implement Casting Logic
- [ ] Task: Implement `.cast_config_types()` helper function in `R/helpers.R` that recursively traverses a list and:
    - Casts specific known fields to `integer` (e.g., `n_threads`, `port`, `db_size`, `*_results`, `update_interval`, `num_days`).
    - Heuristically casts any numeric value where `x == floor(x)` to `integer`.
- [ ] Task: Integrate `.cast_config_types()` into `motis_set_config()` in `R/processx.R` before calling `yaml::write_yaml()`.
- [ ] Task: Verify unit tests pass (Green phase).

## Phase 3: Automated Integration Verification
- [ ] Task: Create an automated verification script `local-testing/verify_server_start.R`.
- [ ] Task: The script will:
    1. Generate a config with numeric types using `motis_configure_server()`.
    2. Start the server using `motis_start_server()` with your local Sweden paths.
    3. Perform a health check query.
    4. Stop the server using `motis_stop_all(TRUE)`.
- [ ] Task: Execute the verification script and ensure it succeeds.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Automated Integration Verification' (Protocol in workflow.md)

## Phase 4: Final Cleanup and Checkpoint
- [ ] Task: Run full test suite `devtools::test()`.
- [ ] Task: Run `devtools::check()` to ensure no regressions.
- [ ] Task: Conductor - User Manual Verification 'Phase 4: Final Cleanup and Checkpoint' (Protocol in workflow.md)
