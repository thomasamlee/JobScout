# Builds JobScout.xlsx with openxlsx: one sheet per category, a Summary
# sheet, frozen headers, filters, a Decision dropdown with conditional
# formatting, clickable Link cells, and a hidden Job ID column. Also handles
# reading back Decisions before a rebuild, timestamped backups, and the
# "file is open in Excel" fallback.

SHEET_COLUMNS <- c(
  "Job Position", "Company", "Location", "Date Posted", "Closing Date",
  "Salary", "Remote/Hybrid/On-site", "Link", "Decision", "Also Matches", "Job ID"
)
DECISION_COL <- 9
JOB_ID_COL <- 11
DECISION_CHOICES <- c("Approve", "Reject", "Maybe")

workbook_path <- function(cfg, demo = FALSE) {
  dir <- if (demo) cfg$demo$workbook_dir else cfg$paths$workbook_dir
  fs::path(dir, cfg$paths$workbook_name)
}

#' TRUE if the file can't be opened for writing right now (e.g. open in Excel).
is_file_locked <- function(path) {
  if (!fs::file_exists(path)) return(FALSE)
  tryCatch({
    con <- file(path, open = "r+b")
    close(con)
    FALSE
  }, error = function(e) TRUE, warning = function(w) TRUE)
}

#' Read back the Decision column from an existing workbook, keyed by the
#' hidden Job ID column. Returns NULL if no workbook exists yet. Stops the
#' whole run (never overwrites blindly) if the file exists but can't be
#' parsed as a JobScout workbook.
read_existing_decisions <- function(path) {
  if (!fs::file_exists(path)) return(NULL)

  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
    stop("Existing workbook at '", path, "' could not be read (", conditionMessage(e),
         "). Refusing to overwrite it -- check the file manually.")
  })

  job_sheets <- intersect(sheets, c("Government", "Internships", "Cyber", "IT", "Sales", "Out of State"))
  all_decisions <- list()
  for (sh in job_sheets) {
    df <- tryCatch(
      readxl::read_excel(path, sheet = sh, col_types = "text"),
      error = function(e) stop("Sheet '", sh, "' in the existing workbook could not be read (",
                                conditionMessage(e), "). Refusing to overwrite it.")
    )
    if (!all(c("Decision", "Job ID") %in% names(df))) next
    all_decisions[[sh]] <- data.frame(job_id = df[["Job ID"]], decision = df[["Decision"]],
                                       stringsAsFactors = FALSE)
  }
  if (length(all_decisions) == 0) return(data.frame(job_id = character(0), decision = character(0)))
  do.call(rbind, all_decisions)
}

#' Copy the current workbook into backups/ with a timestamp, keeping only
#' the most recent `keep` backups.
backup_workbook <- function(path, backup_dir, keep = 4) {
  if (!fs::file_exists(path)) return(invisible(NULL))
  ensure_dir(backup_dir)
  stamp <- format(now_central(), "%Y%m%d_%H%M%S")
  dest <- fs::path(backup_dir, paste0(fs::path_ext_remove(fs::path_file(path)), "_", stamp, ".xlsx"))
  fs::file_copy(path, dest, overwrite = TRUE)

  existing <- fs::dir_ls(backup_dir, regexp = paste0(fs::path_ext_remove(fs::path_file(path)), "_.*\\.xlsx$"))
  if (length(existing) > keep) {
    ord <- order(fs::file_info(existing)$modification_time, decreasing = TRUE)
    to_remove <- existing[ord][(keep + 1):length(existing)]
    fs::file_delete(to_remove)
  }
  invisible(dest)
}

#' Save a workbook, falling back to a new timestamped file (and returning its
#' path) if the target is locked (open in Excel).
save_workbook_safely <- function(wb, path) {
  if (is_file_locked(path)) {
    alt <- fs::path(fs::path_dir(path),
                     paste0(fs::path_ext_remove(fs::path_file(path)), "_",
                            format(now_central(), "%Y%m%d_%H%M%S"), ".xlsx"))
    openxlsx::saveWorkbook(wb, alt, overwrite = TRUE)
    logger::log_warn("'{path}' is locked (likely open in Excel). Wrote '{alt}' instead.")
    return(alt)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

#' Build the full job-sheet data.frame (display columns, real dates) from the
#' raw DB rows for one sheet.
jobs_to_display <- function(jobs_db) {
  data.frame(
    `Job Position` = jobs_db$title,
    `Company` = jobs_db$company,
    `Location` = jobs_db$location,
    `Date Posted` = as.Date(jobs_db$date_posted),
    `Closing Date` = as.Date(jobs_db$closing_date),
    `Salary` = jobs_db$salary,
    `Remote/Hybrid/On-site` = ifelse(is.na(jobs_db$work_type) | !nzchar(jobs_db$work_type),
                                      "Not specified", jobs_db$work_type),
    `Link` = jobs_db$link,
    `Decision` = jobs_db$decision,
    `Also Matches` = jobs_db$also_matches,
    `Job ID` = jobs_db$job_id,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

write_job_sheet <- function(wb, sheet_name, jobs_db) {
  openxlsx::addWorksheet(wb, sheet_name)
  df <- jobs_to_display(jobs_db)
  n <- nrow(df)

  openxlsx::writeData(wb, sheet_name, df, headerStyle = openxlsx::createStyle(textDecoration = "bold"))

  if (n > 0) {
    link_col <- df$Link
    class(link_col) <- "hyperlink"
    openxlsx::writeData(wb, sheet_name, link_col, startCol = 8, startRow = 2, colNames = FALSE)

    rows <- 2:(n + 1)
    openxlsx::dataValidation(wb, sheet_name, cols = DECISION_COL, rows = rows,
                              type = "list", value = paste0('"', paste(DECISION_CHOICES, collapse = ","), '"'))

    approve_style <- openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = "#006100")
    reject_style <- openxlsx::createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")
    maybe_style <- openxlsx::createStyle(fgFill = "#FFEB9C", fontColour = "#9C6500")
    openxlsx::conditionalFormatting(wb, sheet_name, cols = DECISION_COL, rows = rows,
                                     type = "contains", rule = "Approve", style = approve_style)
    openxlsx::conditionalFormatting(wb, sheet_name, cols = DECISION_COL, rows = rows,
                                     type = "contains", rule = "Reject", style = reject_style)
    openxlsx::conditionalFormatting(wb, sheet_name, cols = DECISION_COL, rows = rows,
                                     type = "contains", rule = "Maybe", style = maybe_style)

    date_style <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")
    openxlsx::addStyle(wb, sheet_name, date_style, rows = rows, cols = 4, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet_name, date_style, rows = rows, cols = 5, gridExpand = TRUE)
  }

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 1)
  openxlsx::addFilter(wb, sheet_name, row = 1, cols = 1:length(SHEET_COLUMNS))
  openxlsx::setColWidths(wb, sheet_name,
                          cols = 1:length(SHEET_COLUMNS),
                          widths = c(32, 20, 18, 12, 12, 16, 16, 28, 12, 20, 14))
  openxlsx::setColWidths(wb, sheet_name, cols = JOB_ID_COL, hidden = TRUE)
}

write_summary_sheet <- function(wb, cfg, stats, source_statuses, run_date) {
  openxlsx::addWorksheet(wb, "Summary")
  bold <- openxlsx::createStyle(textDecoration = "bold")

  row <- 1
  openxlsx::writeData(wb, "Summary", paste("JobScout run:", format(run_date, "%Y-%m-%d %H:%M %Z")),
                       startRow = row, startCol = 1)
  row <- row + 2

  openxlsx::writeData(wb, "Summary", "Open jobs per sheet", startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Summary", bold, rows = row, cols = 1)
  row <- row + 1
  counts_df <- data.frame(Sheet = names(stats$open_counts), `Open Jobs` = as.integer(stats$open_counts),
                           check.names = FALSE)
  openxlsx::writeData(wb, "Summary", counts_df, startRow = row, startCol = 1)
  row <- row + nrow(counts_df) + 2

  openxlsx::writeData(wb, "Summary", "Activity", startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Summary", bold, rows = row, cols = 1)
  row <- row + 1
  activity <- data.frame(
    Metric = c("New this week", "New this run", "Removed this week"),
    Count = c(stats$new_this_week, stats$new_this_run, stats$removed_this_week)
  )
  openxlsx::writeData(wb, "Summary", activity, startRow = row, startCol = 1)
  row <- row + nrow(activity) + 2

  openxlsx::writeData(wb, "Summary", "My decisions (open jobs)", startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Summary", bold, rows = row, cols = 1)
  row <- row + 1
  dec <- stats$decisions
  dec_df <- data.frame(Decision = DECISION_CHOICES,
                        Count = vapply(DECISION_CHOICES, function(d) {
                          if (nrow(dec) == 0) return(0L)
                          v <- dec$n[dec$decision == d]
                          if (length(v) == 0) 0L else as.integer(v)
                        }, integer(1)))
  openxlsx::writeData(wb, "Summary", dec_df, startRow = row, startCol = 1)
  row <- row + nrow(dec_df) + 2

  openxlsx::writeData(wb, "Summary", "Source status", startRow = row, startCol = 1)
  openxlsx::addStyle(wb, "Summary", bold, rows = row, cols = 1)
  row <- row + 1
  if (nrow(source_statuses) == 0) {
    openxlsx::writeData(wb, "Summary", "(no sources run yet)", startRow = row, startCol = 1)
  } else {
    openxlsx::writeData(wb, "Summary", source_statuses[, c("source", "status", "message", "run_at")],
                         startRow = row, startCol = 1)
  }

  openxlsx::setColWidths(wb, "Summary", cols = 1:4, widths = c(24, 40, 40, 20))
}

#' Full rebuild: read back Decisions, save them, back up the old file, write
#' every sheet from current DB state, and save (with lock fallback).
#' Returns the path actually written to.
build_workbook <- function(con, cfg, run_date, source_statuses, demo = FALSE) {
  path <- workbook_path(cfg, demo)
  base_dir <- if (demo) cfg$demo$workbook_dir else cfg$paths$workbook_dir
  backup_dir <- fs::path(base_dir, cfg$paths$backup_subdir)
  ensure_dir(base_dir)

  decisions <- read_existing_decisions(path)
  if (!is.null(decisions) && nrow(decisions) > 0) {
    db_save_decisions(con, decisions)
  }
  backup_workbook(path, backup_dir, cfg$paths$backup_keep)

  sheets <- cfg$categories$priority
  sheets <- c(sheets, "Out of State")

  wb <- openxlsx::createWorkbook()
  stats <- db_get_summary_stats(con, run_date, sheets)
  write_summary_sheet(wb, cfg, stats, source_statuses, run_date)

  for (sh in sheets) {
    write_job_sheet(wb, sh, db_get_open_jobs(con, sh))
  }

  save_workbook_safely(wb, path)
}
