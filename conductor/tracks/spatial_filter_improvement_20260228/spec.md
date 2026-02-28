# Specification: Spatial Filter Improvement

## Overview
Improve the spatial filtering logic in `rmotis` by replacing the heuristic-based boolean filter with a precise, user-defined numeric filter (`spatial_filter_km`). This change simplifies the API and provides more predictable behavior for large-scale routing queries. Additionally, a rigorous benchmark will compare the custom R implementation against standard {sf} operations to ensure optimal performance.

## Functional Requirements
- **API Change:** 
    - Remove the `spatial_filter` (boolean) and `max_speed_kmh` (numeric) arguments from `motis_one_to_many()` and `motis_one_to_many_intermodal()`.
    - Introduce `spatial_filter_km` (numeric). If provided, it represents the maximum straight-line distance (in kilometers) from the origin to each destination.
- **Filtering Logic:** 
    - Implement a fixed-radius bounding box filter based on `spatial_filter_km`.
    - Use the existing `.km_to_deg` helper for accurate degree-to-km conversion (accounting for latitude).
- **Benchmarking:**
    - Create a dedicated benchmark script (or test file) to compare:
        1. Custom Bounding Box (current).
        2. {sf} Bounding Box (`sf::st_bbox` / `sf::st_intersects`).
        3. {sf} Circular Check (`sf::st_is_within_distance`).
    - Test across different point scales (1k, 10k, 100k).
- **Documentation:**
    - Clearly document that `spatial_filter_km` refers to the **straight-line distance**, not travel distance along the network.
    - Update all related help files and examples.

## Non-Functional Requirements
- **Performance:** The custom filter must remain significantly faster than {sf} circular checks for 100k+ points to justify its existence.
- **Accuracy:** The degree-to-km conversion must remain accurate across different latitudes.

## Acceptance Criteria
- [ ] `spatial_filter_km` replaces `spatial_filter` in all one-to-many functions.
- [ ] The benchmark results are logged (confirming speed/accuracy trade-offs).
- [ ] Unit tests verify that points outside the radius are correctly dropped.
- [ ] Documentation reflects the new argument and its behavior.

## Out of Scope
- Implementing network-based spatial filtering (this is what MOTIS itself does; our filter is a pre-filter).
- Adding complex polygon filtering.
