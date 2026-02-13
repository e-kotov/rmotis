# Product Guidelines: rmotis

## Documentation & Communication
- **Tone:** Professional and technically accurate for researchers, yet approachable and helpful for new users.
- **Style:** Concise and direct. Avoid unnecessary jargon; when complex MOTIS terms are used, provide brief context or links to the MOTIS documentation.
- **Examples:** Every major function must have a clear, executable example in its documentation. High-level workflows should be covered in vignettes.

## API Design & Code Style
- **Tidyverse Alignment:** Follow the [Tidyverse Style Guide](https://style.tidyverse.org/). Use `snake_case` for functions and arguments. Outputs should be tidy (e.g., `tibble` or `sf` objects) wherever appropriate.
- **MOTIS Consistency:** Use nomenclature consistent with the MOTIS API (e.g., `one_to_many`, `plan`) when it provides clarity for users familiar with the backend.
- **State Management:**
    - **Convenience:** Maintain a global server registry to allow users to run routing functions without explicitly passing a server handle (defaulting to the last started server).
    - **Flexibility:** All server-dependent functions must also accept an optional `server` argument. Functions that start or manage servers must return a `motis_server` object.
- **Dependencies:** Minimize core dependencies. Use `Suggests` for optional visualization or specialized data processing features.

## Error Handling & Feedback
- **Informative Errors:** Use the `{cli}` package for beautiful, actionable error messages. Tell the user *why* it failed and *how* to fix it.
- **Progress Visibility:** Long-running operations (server installation, data import, large batch routing) must provide visual feedback using progress bars or status messages.
- **Graceful Degradation:** Check for optional dependencies before calling features that require them, providing a clear instruction on how to install the missing package.

## Quality Standards
- **Testing:** Aim for high test coverage (>80%). Use `testthat` for unit tests and integration tests with a local MOTIS instance.
- **Manual Verification:** For phase completions or complex features, provide a pre-filled R script in the `private/` directory to facilitate interactive testing in the IDE.
- **Documentation:** Use `roxygen2` for all function documentation. Ensure `NAMESPACE` is automatically managed.
