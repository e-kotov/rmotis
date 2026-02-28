# Specification: server_config_validation_20260228

## Overview
Implement a centralized validation hook that cross-checks R function arguments against the MOTIS server configuration. This ensures users are notified early if their request exceeds server limits, providing actionable instructions for reconfiguration.

## Functional Requirements
1. **Server Registry Enhancement**:
   - Update `motis_start_server()` to record the absolute path of the `config.yml` file in the server registry.
   - Update registry schema to include a `config_path` field.

2. **Configuration Access**:
   - Implement a fast helper to read/parse `config.yml` limits.
   - Support for both local `config.yml` and basic detection for remote servers.

3. **Validation Hook (`.motis_validate_args`)**:
   - **Local Servers**: Perform "Hard Error" blocking if arguments (e.g., `num_itineraries`, `max_many`, `max_travel_time`) exceed server limits.
   - **Remote Servers**: Issue a "Silent Warning" informing the user that limits cannot be verified.
   - **Parameters to Validate**:
     - `plan_max_results` (vs. `num_itineraries`)
     - `onetomany_max_many` (vs. number of origins/destinations)
     - `onetoall_max_travel_minutes` (vs. `max_travel_time`)
     - `plan_max_search_window_minutes` (vs. `search_window`)
     - `routing_max_timeout_seconds`
     - `street_routing_max_prepost_transit_seconds`
     - `street_routing_max_direct_seconds`

4. **Actionable Feedback**:
   - Error messages must provide a copy-pasteable command sequence:
     1. `motis_stop_server()`
     2. `motis_config(..., limits = list(<param> = <value>))`
     3. `motis_start_server(...)`

## Acceptance Criteria
- [ ] `motis_one_to_many()` errors if input exceeds `onetomany_max_many`.
- [ ] `motis_plan()` errors if `search_window` or `num_itineraries` exceeds limits.
- [ ] Error message correctly constructs the reconfiguration command based on the detected mismatch.
- [ ] Remote servers only trigger a one-time warning per session.
- [ ] Server registry correctly persists the `config_path`.

## Out of Scope
- Validating non-limit parameters (e.g., dataset paths).
- Automatically restarting the server (user must do it manually for safety).
- Parsing complex Lua profiles for street routing.
