if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  # Ensure a writable user library directory exists
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path(Sys.getenv("HOME"), "R", "library")
    Sys.setenv(R_LIBS_USER = user_lib)
  }
  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  }
  install.packages("rmarkdown", repos = "https://cloud.r-project.org", lib = user_lib)
}

# Render the report using forward‑slash paths to avoid spaces issues
rmarkdown::render(
  "relatorio_dinamico.Rmd",
  output_format = "html_document",
  envir = new.env(parent = globalenv())
)

