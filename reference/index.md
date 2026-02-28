# Package index

## Routing API

Main functions for point-to-point and one-to-many routing.

- [`motis_plan()`](http://www.ekotov.pro/rmotis/reference/motis_plan.md)
  : Plan a journey between two points or create a travel time matrix
- [`motis_plan_manual()`](http://www.ekotov.pro/rmotis/reference/motis_plan_manual.md)
  : Plan a journey (MOTIS /api/v4/plan)
- [`motis_one_to_many()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many.md)
  : Calculate one-to-many or many-to-one street-level routes
- [`motis_one_to_many_intermodal()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_intermodal.md)
  : Calculate one-to-many or many-to-one intermodal (public transit)
  routes
- [`motis_one_to_all()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_all.md)
  : Calculate reachable locations from a single point within a given
  travel time

## Batch Routing

Functions for efficient batch processing of routing requests.

- [`motis_one_to_many_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_batch.md)
  **\[deprecated\]** : Run Full One-to-Many Batch Routing Cycle via CLI
- [`motis_one_to_many_generate_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_generate_batch.md)
  : Generate MOTIS Batch Query File for One-to-Many
- [`motis_one_to_many_plan_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_plan_batch.md)
  : Multi-origin Batch Planning for One-to-Many Routing
- [`motis_one_to_many_read_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_read_batch.md)
  : Read MOTIS Batch Response File for One-to-Many
- [`motis_plan_generate_batch()`](http://www.ekotov.pro/rmotis/reference/motis_plan_generate_batch.md)
  : Generate MOTIS Batch Query File
- [`motis_one_to_all_generate_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_all_generate_batch.md)
  : Generate MOTIS Batch Query File for One-to-All

## Server Management

Control local MOTIS instances.

- [`motis_start_server()`](http://www.ekotov.pro/rmotis/reference/motis_start_server.md)
  : Start a MOTIS Server Process
- [`motis_stop_server()`](http://www.ekotov.pro/rmotis/reference/motis_stop_server.md)
  : Stop MOTIS Server
- [`motis_stop_all()`](http://www.ekotov.pro/rmotis/reference/motis_stop_all.md)
  : Stop all running MOTIS servers
- [`motis_servers()`](http://www.ekotov.pro/rmotis/reference/motis_servers.md)
  : List Running MOTIS Servers
- [`motis_set_server_address()`](http://www.ekotov.pro/rmotis/reference/motis_set_server_address.md)
  : Set Server Address in a MOTIS Configuration File
- [`motis_open_ui()`](http://www.ekotov.pro/rmotis/reference/motis_open_ui.md)
  : Open MOTIS Web UI

## Installation & Configuration

Manage MOTIS binaries, data, and configurations.

- [`motis_install()`](http://www.ekotov.pro/rmotis/reference/motis_install.md)
  : Install MOTIS Backend Binaries
- [`motis_uninstall()`](http://www.ekotov.pro/rmotis/reference/motis_uninstall.md)
  : Uninstall MOTIS Backend Binaries
- [`motis_check_available_versions()`](http://www.ekotov.pro/rmotis/reference/motis_check_available_versions.md)
  : Check for Available MOTIS Versions
- [`motis_check_latest_version()`](http://www.ekotov.pro/rmotis/reference/motis_check_latest_version.md)
  : Find the Latest Stable MOTIS Version
- [`motis_config()`](http://www.ekotov.pro/rmotis/reference/motis_config.md)
  : Generate and Customize a MOTIS Configuration File
- [`motis_configure_import()`](http://www.ekotov.pro/rmotis/reference/motis_configure_import.md)
  : Configure MOTIS Import Settings
- [`motis_configure_server()`](http://www.ekotov.pro/rmotis/reference/motis_configure_server.md)
  : Configure a MOTIS Server
- [`motis_prepare_data()`](http://www.ekotov.pro/rmotis/reference/motis_prepare_data.md)
  : Prepare MOTIS Data Directory (Config + Import)
- [`motis_import()`](http://www.ekotov.pro/rmotis/reference/motis_import.md)
  : Import and Preprocess MOTIS Data
- [`motis_add_assets()`](http://www.ekotov.pro/rmotis/reference/motis_add_assets.md)
  : Add MOTIS Assets to a Working Directory
- [`motis_clear_path()`](http://www.ekotov.pro/rmotis/reference/motis_clear_path.md)
  : Clear MOTIS Path from .Rprofile and Restore Session PATH
- [`motis_unlock_limits()`](http://www.ekotov.pro/rmotis/reference/motis_unlock_limits.md)
  : Unlock MOTIS Server Limits

## Diagnostics & Utilities

- [`motis_diagnose()`](http://www.ekotov.pro/rmotis/reference/motis_diagnose.md)
  : Diagnose MOTIS Setup
- [`motis_gui()`](http://www.ekotov.pro/rmotis/reference/motis_gui.md) :
  Launch a GUI to View and Debug MOTIS Routing

## Experimental Text API

- [`motis_one_to_all_txt_1()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_all_txt_1.md)
  : Builds a MOTIS one-to-all request manually and saves it to a text
  file.
- [`motis_plan_txt_1()`](http://www.ekotov.pro/rmotis/reference/motis_plan_txt_1.md)
  : Builds MOTIS plan requests manually and dumps them to a text file.
