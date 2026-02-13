# Implementation Plan: Batch Query Generation for One-to-Many and One-to-All

## Phase 1: Infrastructure & One-to-Many

- [x] Task: Refactor shared batch helpers from `motis_plan_generate_batch.R` to `helpers.R` 807c756
- [ ] Task: Implement `motis_one_to_many_generate_batch`
    - [ ] Write Tests (Red Phase)
    - [ ] Implement Function (Green Phase)
    - [ ] Verify Coverage
- [ ] Task: Conductor - User Manual Verification 'Infrastructure & One-to-Many' (Protocol in workflow.md)

## Phase 2: One-to-All

- [ ] Task: Implement `motis_one_to_all_generate_batch`
    - [ ] Write Tests (Red Phase)
    - [ ] Implement Function (Green Phase)
    - [ ] Verify Coverage
- [ ] Task: Conductor - User Manual Verification 'One-to-All' (Protocol in workflow.md)

## Phase 3: Finalization

- [ ] Task: Update package documentation and NAMESPACE
- [ ] Task: Final R CMD check and performance profiling
- [ ] Task: Conductor - User Manual Verification 'Finalization' (Protocol in workflow.md)
