# Add MOTIS Assets to a Working Directory

Copies or creates symbolic links for required MOTIS asset directories
(e.g., `tiles-profiles`, `ui`) from a MOTIS installation into a
specified working directory. This is a necessary step before running
`motis_import`.

## Usage

``` r
motis_add_assets(
  work_dir,
  assets_action = c("copy", "symlink", "none"),
  motis_path = NULL
)
```

## Arguments

- work_dir:

  A string. The path to the target directory where the assets will be
  placed.

- assets_action:

  A string specifying how to handle the assets. One of:

  - `"copy"` (default): Copies assets to the `work_dir`. Safe and works
    everywhere.

  - `"symlink"`: Creates a symbolic link to the assets. Saves disk
    space, best for non-Windows systems. Will fall back to copying if it
    fails.

  - `"none"`: Does nothing. For users who manage assets manually.

- motis_path:

  An optional string. The path to the *directory* containing the `motis`
  executable. If `NULL`, the executable is assumed to be on the system
  `PATH`.

## Value

The path to the working directory, invisibly.
