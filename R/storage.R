# SQLite-backed state: open/closed jobs, decisions, source health, and the
# feedback log that a future score.R (Phase 2) will train on.

#' Open (and if needed, create) the SQLite database.
db_connect <- function(cfg, demo = FALSE) {
  db_name <- if (demo) cfg$demo$database_name else cfg$paths$database_name
  base_dir <- if (demo) cfg$demo$workbook_dir else cfg$paths$workbook_dir
  ensure_dir(base_dir)
  path <- fs::path(base_dir, db_name)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  db_init_schema(con)
  con
}

db_init_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS jobs (
      job_id        TEXT PRIMARY KEY,
      source        TEXT NOT NULL,
      sheet         TEXT NOT NULL,
      title         TEXT,
      company       TEXT,
      location      TEXT,
      date_posted   TEXT,
      closing_date  TEXT,
      salary        TEXT,
      work_type     TEXT,
      link          TEXT,
      also_matches  TEXT,
      decision      TEXT,
      status        TEXT NOT NULL DEFAULT 'open',
      first_seen    TEXT NOT NULL,
      last_seen     TEXT NOT NULL,
      closed_date   TEXT
    );
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS feedback (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      job_id      TEXT,
      title       TEXT,
      company     TEXT,
      category    TEXT,
      location    TEXT,
      salary      TEXT,
      decision    TEXT,
      recorded_at TEXT
      -- Hook for Phase 2: score.R will read this table to learn a
      -- Match Score from the user's Approve/Reject/Maybe history.
    );
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS source_status (
      source  TEXT PRIMARY KEY,
      status  TEXT NOT NULL,
      message TEXT,
      run_at  TEXT
    );
  ")
  invisible(con)
}

#' Insert new jobs / refresh existing ones from a fetch. Never touches the
#' `decision` column on jobs that already exist.
db_upsert_jobs <- function(con, jobs, run_date) {
  if (nrow(jobs) == 0) return(invisible(0))
  run_date <- as.character(run_date)

  # Two postings in the same fetch can hash to the same job_id (e.g. a
  # genuine duplicate listing with no distinct posting_id) -- keep the first.
  jobs <- jobs[!duplicated(jobs$job_id), ]

  existing <- DBI::dbGetQuery(con, "SELECT job_id FROM jobs")$job_id

  for (i in seq_len(nrow(jobs))) {
    j <- jobs[i, ]
    if (j$job_id %in% existing) {
      DBI::dbExecute(con, "
        UPDATE jobs SET
          sheet = ?, title = ?, company = ?, location = ?, date_posted = ?,
          closing_date = ?, salary = ?, work_type = ?, link = ?,
          also_matches = ?, status = 'open', last_seen = ?, closed_date = NULL
        WHERE job_id = ?",
        params = list(j$sheet, j$title, j$company, j$location, j$date_posted,
                       j$closing_date, j$salary, j$work_type, j$link,
                       j$also_matches, run_date, j$job_id))
    } else {
      DBI::dbExecute(con, "
        INSERT INTO jobs (job_id, source, sheet, title, company, location,
          date_posted, closing_date, salary, work_type, link, also_matches,
          decision, status, first_seen, last_seen, closed_date)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, 'open', ?, ?, NULL)",
        params = list(j$job_id, j$source, j$sheet, j$title, j$company,
                       j$location, j$date_posted, j$closing_date, j$salary,
                       j$work_type, j$link, j$also_matches, run_date, run_date))
    }
  }
  invisible(nrow(jobs))
}

#' Close jobs from `source` that were open before but weren't in this run's
#' results. Only call this when that source's fetch fully succeeded --
#' callers must gate this themselves (a failed/skipped source must never
#' cause its jobs to be closed just because they weren't re-fetched).
db_close_missing <- function(con, source, seen_job_ids, run_date) {
  open_ids <- DBI::dbGetQuery(con, "SELECT job_id FROM jobs WHERE source = ? AND status = 'open'",
                               params = list(source))$job_id
  missing <- setdiff(open_ids, seen_job_ids)
  if (length(missing) == 0) return(invisible(0))
  placeholders <- paste(rep("?", length(missing)), collapse = ",")
  DBI::dbExecute(con, paste0(
    "UPDATE jobs SET status = 'closed', closed_date = ? WHERE job_id IN (", placeholders, ")"
  ), params = c(list(as.character(run_date)), as.list(missing)))
  invisible(length(missing))
}

#' Close any open job whose closing_date has passed, regardless of source
#' health -- this is based on data already in hand, not on disappearance.
db_close_past_due <- function(con, run_date) {
  DBI::dbExecute(con, "
    UPDATE jobs SET status = 'closed', closed_date = ?
    WHERE status = 'open' AND closing_date IS NOT NULL AND closing_date <> ''
      AND date(closing_date) < date(?)",
    params = list(as.character(run_date), as.character(run_date)))
}

#' Persist Decision values read back from the workbook, and append a
#' feedback row whenever a decision changed (new Approve/Reject/Maybe).
db_save_decisions <- function(con, decisions) {
  if (nrow(decisions) == 0) return(invisible(0))
  changed <- 0
  for (i in seq_len(nrow(decisions))) {
    d <- decisions[i, ]
    if (is.na(d$decision) || !nzchar(trimws(d$decision))) next

    prior <- DBI::dbGetQuery(con, "SELECT decision, title, company, sheet, location, salary FROM jobs WHERE job_id = ?",
                              params = list(d$job_id))
    if (nrow(prior) == 0) next

    DBI::dbExecute(con, "UPDATE jobs SET decision = ? WHERE job_id = ?",
                    params = list(d$decision, d$job_id))

    if (is.na(prior$decision[1]) || prior$decision[1] != d$decision) {
      changed <- changed + 1
      DBI::dbExecute(con, "
        INSERT INTO feedback (job_id, title, company, category, location, salary, decision, recorded_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(d$job_id, prior$title[1], prior$company[1], prior$sheet[1],
                       prior$location[1], prior$salary[1], d$decision,
                       as.character(now_central())))
    }
  }
  invisible(changed)
}

db_get_open_jobs <- function(con, sheet = NULL) {
  if (is.null(sheet)) {
    DBI::dbGetQuery(con, "SELECT * FROM jobs WHERE status = 'open' ORDER BY date_posted DESC")
  } else {
    DBI::dbGetQuery(con, "SELECT * FROM jobs WHERE status = 'open' AND sheet = ? ORDER BY date_posted DESC",
                     params = list(sheet))
  }
}

db_set_source_status <- function(con, source, status, message = NA, run_at = now_central()) {
  DBI::dbExecute(con, "
    INSERT INTO source_status (source, status, message, run_at) VALUES (?, ?, ?, ?)
    ON CONFLICT(source) DO UPDATE SET status = excluded.status, message = excluded.message, run_at = excluded.run_at",
    params = list(source, status, message, as.character(run_at)))
}

db_get_source_statuses <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM source_status")
}

#' Counts used to build the Summary sheet.
db_get_summary_stats <- function(con, run_date, sheets) {
  run_date <- as.character(run_date)
  week_ago <- as.character(as.Date(run_date) - 7)

  open_counts <- DBI::dbGetQuery(con, "SELECT sheet, COUNT(*) AS n FROM jobs WHERE status = 'open' GROUP BY sheet")
  new_week <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM jobs WHERE first_seen >= ?", params = list(week_ago))$n
  new_run <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM jobs WHERE first_seen = ?", params = list(run_date))$n
  removed_week <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM jobs WHERE status = 'closed' AND closed_date >= ?",
                                   params = list(week_ago))$n
  decisions <- DBI::dbGetQuery(con, "SELECT decision, COUNT(*) AS n FROM jobs WHERE status = 'open' AND decision IS NOT NULL GROUP BY decision")

  counts <- setNames(rep(0L, length(sheets)), sheets)
  for (i in seq_len(nrow(open_counts))) counts[[open_counts$sheet[i]]] <- open_counts$n[i]

  list(
    open_counts = counts,
    new_this_week = new_week,
    new_this_run = new_run,
    removed_this_week = removed_week,
    decisions = decisions
  )
}
