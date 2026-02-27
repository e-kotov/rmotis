# Specification: Unified One-to-Many Routing Interface

## Overview
Unify `motis_one_to_many()` and `motis_one_to_many_batch()` into a single, robust, and efficient interface. The new `motis_one_to_many()` will support multiple execution engines (API and Batch CLI) and provide a unified API for large-scale routing tasks with advanced features like smart chunking, spatial sorting, and flexible output formats.

## Functional Requirements

### 1. Unified Entry Point
- Use `motis_one_to_many()` as the primary function.
- Add an `engine` argument: `c("api", "batch")`.
  - `"api"`: Uses the POST API (current `motis_one_to_many` logic).
  - `"batch"`: Uses the MOTIS CLI batch command (current `motis_one_to_many_batch` logic).

### 2. Unified Arguments
- **Input**: `one` (origins), `many` (destinations).
- **Common Parameters**: `mode`, `arrive_by`, `max`, `maxMatchingDistance`, `withDistance`.
- **Engine-Specific**:
  - `data_dir`: Required only for `engine = "batch"`.
  - `.server`: Used for `engine = "api"`.
- **Control**:
  - `batch_size`: Number of origins per request (formerly `batch_size` in API, handles `chunk_size` logic in Batch).
  - `max_destinations_per_batch`: Optional limit to split destinations (replaces/generalizes `split`).
  - `temp_dir`: Path for temporary files (query, metadata, responses), defaults to `tempdir()`.
  - `keep_files`: Logical, whether to keep files in `temp_dir`.
- **Spatial Optimization**:
  - `spatial_sort`: Logical. If `TRUE`, use Hilbert or Z-order sorting for better spatial locality in MOTIS.
  - `spatial_filter`: Logical. Pre-filter destinations per origin based on `max` travel time and mode speed.

### 3. Unified Output Options
- `output_path`: Path to save results.
- `output_format`:
  - `"data.frame"`: Return a tidy data frame (in-memory).
  - `"parquet"`: Write to a single or partitioned Parquet file (requires `arrow`).
  - `"csv"`: Write to a CSV file.
  - `"duckdb"`: Write directly to a DuckDB table.
- `output_callback`: Support streaming results to a custom function for both engines.

### 4. Smart Chunking (Origin-First)
- Prioritize parallelizing across origins.
- Only split destinations if the total "cells" (origins * destinations) or destination count exceeds a threshold that risks memory or timeout issues.

## Non-Functional Requirements
- **Efficiency**: Minimize redundant Dijkstra sweeps.
- **Robustness**: Proper error handling and progress reporting for long-running tasks.
- **Consistency**: Arguments should follow existing `rmotis` and `tidyverse` conventions.

## Acceptance Criteria
- [ ] `motis_one_to_many()` successfully dispatches to both API and Batch engines.
- [ ] Large-scale datasets (e.g., 1,000 x 50,000) are processed efficiently with spatial sorting and filtering.
- [ ] Results can be written directly to Parquet, CSV, or DuckDB without loading everything into R memory at once.
- [ ] Existing tests for both functions are updated and pass with the new unified interface.

## Out of Scope
- Unifying other routing endpoints (e.g., one-to-all) in this track.
- Implementing a full-blown "routing manager" with persistence beyond simple checkpointing.
