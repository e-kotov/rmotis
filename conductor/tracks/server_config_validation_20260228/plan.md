# Implementation Plan: server_config_validation_20260228

## Phase 1: Registry Enhancement and Config Access [checkpoint: 0da9330]
- [x] Task: Update `motis_start_server()` to capture `config.yml` absolute path.
- [x] Task: Update server registry to store `config_path` and persist it.
- [x] Task: Implement/Enhance `read_motis_config()` to robustly extract `limits` section from YAML.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Registry Enhancement' (Protocol in workflow.md)

## Phase 2: Validation Hook Implementation
- [x] Task: Write tests for `.motis_validate_args()` covering local (error) and remote (warning) scenarios.
- [x] Task: Implement `.motis_validate_args()` helper in `helpers.R`.
- [x] Task: Implement a helper to generate the copy-pasteable reconfiguration command.
- [~] Task: Conductor - User Manual Verification 'Phase 2: Validation Hook' (Protocol in workflow.md)

## Phase 3: Function Integration
- [ ] Task: Integrate validation hook into `motis_plan()` and `motis_plan_txt_1()`.
- [ ] Task: Integrate validation hook into `motis_one_to_many()` and its batch/intermodal variants.
- [ ] Task: Integrate validation hook into `motis_one_to_all()`.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Function Integration' (Protocol in workflow.md)

## Phase 4: Final Validation and Documentation
- [ ] Task: Reproduce a "limit exceeded" error using the Luxembourg test dataset.
- [ ] Task: Verify "Silent Warning" for remote servers (mocking a remote URL).
- [ ] Task: Update roxygen documentation for API functions to mention the new limit validation.
- [ ] Task: Conductor - User Manual Verification 'Phase 4: Final Validation' (Protocol in workflow.md)
