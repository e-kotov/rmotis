# Implementation Plan: Batch Query Generation for One-to-Many and One-to-All

## Phase 1: Infrastructure & One-to-Many [checkpoint: 0312559]

- [x] Task: Refactor shared batch helpers from `motis_plan_generate_batch.R` to `helpers.R` 807c756
- [x] Task: Implement `motis_one_to_many_generate_batch` 552c094
    - [x] Write Tests (Red Phase)
    - [x] Implement Function (Green Phase)
    - [x] Verify Coverage
- [x] Task: Conductor - User Manual Verification 'Infrastructure & One-to-Many' (Protocol in workflow.md) 0312559

## Phase 2: One-to-All

- [x] Task: Implement `motis_one_to_all_generate_batch` 0e7fc29
    - [x] Write Tests (Red Phase)
    - [x] Implement Function (Green Phase)
    - [x] Verify Coverage
- [~] Task: Conductor - User Manual Verification 'One-to-All' (Protocol in workflow.md)

## Phase 3: Finalization

- [ ] Task: Update package documentation and NAMESPACE
- [ ] Task: Final R CMD check and performance profiling
- [ ] Task: Conductor - User Manual Verification 'Finalization' (Protocol in workflow.md)
