# Implementation Plan: Track one_to_many_intermodal_20260227

## Phase 1: Infrastructure Refactoring (Internal)
- [x] Task: Analyze and Refactor `R/api_one_to_many.R` d3419e8
    - [x] Task: Extract the core execution loop (API & Batch engines) into an internal function `.motis_one_to_many_infra` that supports parameter parameter passing for both street and intermodal routing. d3419e8
    - [x] Task: Create a smart URL builder `.build_one_to_many_url` that handles both `/api/v1/one-to-many` and `/api/experimental/one-to-many-intermodal`. d3419e8
    - [x] Task: Update existing `motis_one_to_many` to use the new internal infrastructure. d3419e8

## Phase 2: Implement Intermodal One-to-Many
- [ ] Task: Create `R/api_one_to_many_intermodal.R`
    - [ ] Task: Define `motis_one_to_many_intermodal()` with arguments: `one`, `many`, `time`, `arrive_by`, `max_travel_time` (minutes), `transit_modes`, `pre_transit_modes`, `post_transit_modes`, `direct_mode`, `max_transfers`, `min_transfer_time`, etc.
    - [ ] Task: Implement Roxygen documentation following `motis_plan` style: explicit key args + `@inheritDotParams motis.client::mc_oneToManyIntermodalPost`.
    - [ ] Task: Wire the function to `.motis_one_to_many_infra`.
- [ ] Task: Update `R/helpers.R` or `R/api_one_to_many.R` with any missing intermodal-specific helpers.

## Phase 3: Verification & Documentation
- [ ] Task: Local Testing with Luxembourg Data
    - [ ] Task: Set up a local MOTIS server using Luxembourg data (as per `private/2026-01-27-one-to-many-pt.R`).
    - [ ] Task: Verify `motis_one_to_many_intermodal` with both `engine = 'api'` and `engine = 'batch'`.
    - [ ] Task: Verify parallel execution with `backend = 'httr2'` and `backend = 'mirai'`.
- [ ] Task: Create Comprehensive Tests
    - [ ] Task: Add `tests/testthat/test-motis_one_to_many_intermodal.R`.
    - [ ] Task: Ensure existing `tests/testthat/test-motis_one_to_many.R` and related tests still pass.
