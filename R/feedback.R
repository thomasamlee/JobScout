# Phase 2 (design only, not built yet): every Decision is already being
# logged into the `feedback` table by storage.R::db_save_decisions(), with
# the job's title, company, category, location, salary, and timestamp.
#
# HOOK FOR FUTURE WORK: a future R/score.R would read that table, train on
# Approve vs. Reject history, and call something like compute_match_score()
# below for each open job before excel_builder.R writes the sheets, adding a
# "Match Score" column. Do not wire this into the pipeline yet.

#' Placeholder for the future learned match score. Always returns NA until
#' score.R exists -- never fabricate a score in the meantime.
compute_match_score <- function(job, con = NULL) {
  NA_real_
}
