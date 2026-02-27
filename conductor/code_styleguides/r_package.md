# R Package Style Guide: rmotis

This guide follows modern R package development conventions, primarily using the {tidyverse} style and {devtools} ecosystem.

## Syntax & Naming
- **Naming:** Use `snake_case` for functions, arguments, and variables.
- **Assignment:** Always use `<- ` for assignment, never `=`.
- **Booleans:** Always use `TRUE` and `FALSE`, never `T` and `F`.
- **Pipes:** Use the native pipe `|>` (R >= 4.1.0) for new code. Use `%>% ` only if required for compatibility with older code or specific {magrittr} features.
- **Line Length:** Maximum 80 characters.

## Namespace & Dependencies
- **Explicit Namespacing:** Use `pkg::fun()` for functions from other packages to avoid conflicts and make dependencies explicit.
- **Imports:** Use `@importFrom pkg fun` in roxygen headers for frequently used functions.
- **No library():** Never use `library()` or `require()` inside package code.
- **Global State:** Avoid `options()`, `setwd()`, or `<<-`. If global state must be changed, restore it using `on.exit()`.

## Documentation (roxygen2)
- **Exported Functions:** Must have complete documentation (`@param`, `@return`, `@examples`, `@export`).
- **Internal Functions:** Use `@noRd` and prefix with a dot (e.g., `.internal_helper`).
- **Markdown:** Use Markdown in roxygen headers (`Roxygen: list(markdown = TRUE)`).

## Data Masking & rlang
- **Embrace:** Use `{{ var }}` for user-provided column names in data-masking functions.
- **.data pronoun:** Use `.data$col` or `.data[[var]]` to refer to columns in data-masking contexts to pass R CMD check.
- **Dynamic Names:** Use `"{name}" := ...` for dynamic column naming.

## Error Handling (cli)
- **Informative:** Use `{cli}` for error messages (`cli_abort`, `cli_warn`, `cli_inform`).
- **Call Site:** Pass `call = rlang::caller_env()` to `cli_abort` to show the user's call site.

## Testing (testthat 3e)
- **Infrastructure:** Use `testthat` with the 3rd edition.
- **Snapshots:** Use snapshots for complex outputs or error messages.
- **Coverage:** Aim for >80% code coverage.
