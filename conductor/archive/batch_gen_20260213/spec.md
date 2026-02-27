# Specification: Batch Query Generation for One-to-Many and One-to-All

## Overview
This track aims to provide high-performance, vectorized functions for generating MOTIS batch query files for the `one-to-many` and `one-to-all` endpoints. These functions will allow users to generate potentially millions of queries in seconds, formatted for the MOTIS server's batch processing capabilities.

## Requirements

### 1. `motis_one_to_many_generate_batch`
- **Goal:** Generate a text file of URLs for the `/api/v1/one-to-many` endpoint.
- **Input Parameters:**
    - `one`: Origin location (coord or ID).
    - `many`: Destination locations (vector/df/sf of coords or IDs).
    - `mode`: Routing mode (WALK, BIKE, CAR).
    - `max_travel_time`: Maximum travel time (seconds).
    - `arrive_by`: Logical, if TRUE it's many-to-one.
    - `output_file`: Path to write the queries.
    - `...`: Additional parameters.
- **Validation:** Must use `motis.client::mc_one_to_many()` logic to validate parameters.
- **Performance:** Must be fully vectorized.

### 2. `motis_one_to_all_generate_batch`
- **Goal:** Generate a text file of URLs for the `/api/v1/one-to-all` endpoint.
- **Input Parameters:**
    - `one`: Origin location (coord or ID).
    - `time`: Departure/arrival time.
    - `max_travel_time`: Maximum travel time (minutes).
    - `arrive_by`: Logical.
    - `output_file`: Path to write the queries.
    - `...`: Additional parameters.
- **Validation:** Must use `motis.client::mc_one_to_all()` logic to validate parameters.
- **Performance:** Must be fully vectorized.

### 3. Shared Infrastructure
- Reuse and potentially refactor internal helpers from `R/motis_plan_generate_batch.R` (like `.build_static_suffix`, `.format_time_utc`, etc.) into `R/helpers.R` if they are generally useful.
- Ensure consistent error handling using `{cli}`.

## Acceptance Criteria
- Both functions are exported.
- Documentation includes examples.
- Tests verify correct URL construction and parameter validation.
- Vectorized performance is maintained (generating 100k queries should take < 1s).
