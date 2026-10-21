#!/usr/bin/env Rscript
# JobScout entry point. Run from the project root (the folder containing
# JobScout.Rproj), either in RStudio or via:
#   Rscript main.R [--demo] [--dry-run] [--no-email] [--source usajobs|careers]

suppressPackageStartupMessages({
  library(optparse)
})

source(file.path("R", "utils.R"))
source(file.path("R", "storage.R"))
source(file.path("R", "classify.R"))
source(file.path("R", "usajobs.R"))
source(file.path("R", "careers.R"))
source(file.path("R", "excel_builder.R"))
source(file.path("R", "feedback.R"))
source(file.path("R", "demo.R"))
if (fs::file_exists(file.path("R", "emailer.R"))) source(file.path("R", "emailer.R"))

option_list <- list(
  make_option("--demo", action = "store_true", default = FALSE,
              help = "Run against synthetic demo data (no API keys, no network, no email)"),
  make_option("--dry-run", action = "store_true", default = FALSE, dest = "dry_run",
              help = "Build the workbook but never send email"),
  make_option("--no-email", action = "store_true", default = FALSE, dest = "no_email",
              help = "Same as --dry-run"),
  make_option("--source", type = "character", default = NULL, dest = "source",
              help = "Run only one source: usajobs or careers")
)
opts <- parse_args(OptionParser(option_list = option_list))

SOURCES <- list(usajobs = fetch_usajobs, careers = fetch_careers)

run_live <- function(cfg, opts) {
  run_date <- now_central()
  con <- db_connect(cfg, demo = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sources <- SOURCES
  if (!is.null(opts$source)) {
    if (!opts$source %in% names(sources)) {
      stop("Unknown --source '", opts$source, "'. Choices: ", paste(names(sources), collapse = ", "))
    }
    sources <- sources[opts$source]
  }

  for (name in names(sources)) {
    logger::log_info("Fetching source: {name}")
    tryCatch({
      raw <- sources[[name]](cfg)
      classified <- normalize_and_classify(raw, cfg)
      db_upsert_jobs(con, classified, run_date)
      # Only a fully successful fetch may close jobs by disappearance --
      # that's why this call sits inside the try block, right after a
      # fetch that didn't throw.
      db_close_missing(con, name, classified$job_id, run_date)
      db_set_source_status(con, name, "OK", paste0(nrow(classified), " jobs"), run_date)
      logger::log_info("{name}: {nrow(classified)} jobs stored on {paste(unique(classified$sheet), collapse=', ')}")
    }, error = function(e) {
      logger::log_error("{name} FAILED: {conditionMessage(e)}")
      db_set_source_status(con, name, "FAILED", conditionMessage(e), run_date)
    })
  }

  # Independent of source health: anything past its own closing date closes.
  db_close_past_due(con, run_date)

  path <- build_workbook(con, cfg, run_date, db_get_source_statuses(con), demo = FALSE)
  logger::log_info("Workbook written to {path}")

  send_email <- isTRUE(cfg$email$enabled) && !opts$dry_run && !opts$no_email
  if (send_email) {
    if (exists("send_weekly_email", mode = "function")) {
      send_weekly_email(cfg, con, path, run_date)
    } else {
      logger::log_warn("Email is enabled in config.yaml but emailer.R (Stage 3) hasn't been built yet.")
    }
  } else {
    logger::log_info("Email skipped ({if (opts$dry_run) '--dry-run' else if (opts$no_email) '--no-email' else 'disabled in config.yaml'}).")
  }
}

main <- function() {
  cfg <- load_config()
  setup_logging(cfg, demo = opts$demo)
  logger::log_info("JobScout starting (demo={opts$demo})")

  if (opts$demo) {
    path <- run_demo(cfg)
    logger::log_info("Demo complete -- open {path} to see the formatted workbook. Email is never sent in demo mode.")
    return(invisible())
  }

  run_live(cfg, opts)
}

main()
