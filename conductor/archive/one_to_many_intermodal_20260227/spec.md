# Specification: Track one_to_many_intermodal_20260227

## Overview
Implement a new function `motis_one_to_many_intermodal()` that calculates travel times from one point to many destinations (or vice versa) using public transit and other intermodal modes. The function will leverage the `/api/experimental/one-to-many-intermodal` endpoint and provide the same robust features as the existing street routing (batch mode, parallel processing, spatial filtering).

## Functional Requirements
1.  **New Function `motis_one_to_many_intermodal`**:
    -   **Clean API**: Separate from `motis_one_to_many` to avoid parameter confusion.
    -   **Arguments**:
        -   `one`, `many`: Origin and destinations (sf, data.frame, matrix, character).
        -   `time`: Departure/Arrival time (POSIXct). Defaults to `Sys.time()`.
        -   `max_travel_time`: Maximum duration in **minutes** (integer).
        -   `arrive_by`: Logical (default: `FALSE`).
        -   `transit_modes`: Allowed transit modes (default: `"TRANSIT"`).
        -   `pre_transit_modes`, `post_transit_modes`, `direct_mode` (defaults to `"WALK"`).
        -   `max_transfers`, `min_transfer_time`, etc.
        -   Execution args: `engine`, `parallel`, `backend`, `batch_size`, `output_path`, etc.
2.  **Shared Infrastructure (Internal)**:
    -   Refactor the robust routing logic from `R/api_one_to_many.R` into internal helper functions.
    -   Ensure both `motis_one_to_many` (street) and `motis_one_to_many_intermodal` (transit) use the same underlying engine for batching, parallelism, and spatial operations.
3.  **API Engine Support**:
    -   Support parallel HTTP requests via `httr2` or `mirai`.
    -   Implement spatial filtering based on `max_travel_time` (converted to distance radius).
4.  **Batch Engine Support**:
    -   Support the MOTIS CLI `batch` command for intermodal routing by constructing appropriate URL strings.
    -   Maintain parity in metadata (`.meta`) file handling.

## Non-Functional Requirements
-   **No Regression**: Existing `motis_one_to_many` behavior and API must remain unchanged.
-   **API Parity**: Match the MOTIS intermodal API parameter names and units (minutes).
-   **Scalability**: Robust handling of large destination sets.

## Acceptance Criteria
1.  `motis_one_to_many_intermodal(..., mode = "TRANSIT")` returns a tidy data frame with travel durations.
2.  `motis_one_to_many()` continues to work as before (street-level, `max` in seconds).
3.  Both functions support the `engine = "batch"` and `parallel = TRUE` workflows.
4.  Verified with Luxembourg local testing data.
