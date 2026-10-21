# Location/category/exclusion classification. Pure functions, no I/O, so they
# are easy to unit test (see tests/testthat/test-classify.R, added in Stage 4).

#' Classify a location string as Texas / Remote / Out-of-state (unknown
#' locations are treated as out-of-state so a job is never silently dropped).
classify_location <- function(location, loc_cfg) {
  if (is.null(location) || is.na(location) || !nzchar(trimws(location))) {
    return(list(out_of_state = TRUE, is_remote = FALSE))
  }
  is_remote <- contains_any_keyword(location, loc_cfg$remote_terms)
  is_texas <- contains_any_keyword(location, loc_cfg$texas_terms)
  list(out_of_state = !(is_remote || is_texas), is_remote = is_remote)
}

#' Derive the Remote/Hybrid/On-site column. An explicit value from the
#' source (e.g. a USAJOBS TelecommutingEligible flag mapped upstream) wins;
#' otherwise it's inferred from the location and full text.
classify_work_type <- function(location, full_text, remote_terms, explicit = NA) {
  if (!is.null(explicit) && !is.na(explicit) && nzchar(trimws(explicit))) return(explicit)
  if (contains_any_keyword(location, remote_terms) || contains_any_keyword(full_text, remote_terms)) {
    return("Remote")
  }
  if (contains_keyword(full_text, "hybrid")) return("Hybrid")
  if (contains_keyword(full_text, "on-site") || contains_keyword(full_text, "onsite") ||
      contains_keyword(full_text, "on site")) {
    return("On-site")
  }
  "Not specified"
}

#' Classify a single job. `job` is a list with: source, title, company,
#' location, description, is_government (logical), work_type (optional,
#' NA if unknown).
#'
#' Returns NULL if the job should be dropped (fails an exclusion rule, or
#' matches none of the tracked categories and isn't a Government posting).
#' Otherwise returns a list with sheet, also_matches, work_type, is_remote,
#' out_of_state.
classify_job <- function(job, cfg) {
  full_text <- paste(job$title, job$description)

  if (contains_any_keyword(job$title, cfg$categories$exclude_title)) return(NULL)
  if (contains_any_keyword(full_text, cfg$categories$exclude_text)) return(NULL)

  categories_matched <- character(0)
  for (cat in names(cfg$categories$keywords)) {
    if (contains_any_keyword(full_text, cfg$categories$keywords[[cat]])) {
      categories_matched <- c(categories_matched, cat)
    }
  }
  if (isTRUE(job$is_government)) categories_matched <- union("Government", categories_matched)

  if (length(categories_matched) == 0) return(NULL)

  loc <- classify_location(job$location, cfg$locations)
  work_type <- classify_work_type(
    job$location, full_text, cfg$locations$remote_terms,
    explicit = job$work_type %||% NA
  )

  if (loc$out_of_state) {
    sheet <- "Out of State"
  } else {
    priority <- cfg$categories$priority
    sheet <- priority[priority %in% categories_matched][1]
  }

  list(
    sheet = sheet,
    also_matches = setdiff(categories_matched, sheet),
    work_type = work_type,
    is_remote = loc$is_remote,
    out_of_state = loc$out_of_state
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

#' Turn a raw source data.frame (source, posting_id, title, company,
#' location, description, date_posted, closing_date, salary, work_type,
#' link, is_government) into a storage-ready data.frame: classified,
#' deduped-ID'd, with dates and salary normalized. Excluded jobs are
#' silently dropped (they fail an exclusion rule or match no tracked
#' category).
normalize_and_classify <- function(raw_df, cfg) {
  if (nrow(raw_df) == 0) return(empty_classified_frame())

  out <- vector("list", nrow(raw_df))
  n <- 0
  for (i in seq_len(nrow(raw_df))) {
    r <- raw_df[i, ]
    job <- list(title = r$title, location = r$location, description = r$description,
                is_government = isTRUE(r$is_government), work_type = r$work_type)
    result <- classify_job(job, cfg)
    if (is.null(result)) next

    date_posted <- parse_posted_date(r$date_posted)
    closing_date <- parse_posted_date(r$closing_date)
    job_id <- make_job_id(r$source, posting_id = r$posting_id, company = r$company,
                           title = r$title, location = r$location)

    n <- n + 1
    out[[n]] <- data.frame(
      job_id = job_id, source = r$source, sheet = result$sheet, title = r$title,
      company = r$company, location = r$location,
      date_posted = if (is.na(date_posted)) NA_character_ else as.character(date_posted),
      closing_date = if (is.na(closing_date)) NA_character_ else as.character(closing_date),
      salary = normalize_salary(r$salary), work_type = result$work_type, link = r$link,
      also_matches = paste(result$also_matches, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  if (n == 0) return(empty_classified_frame())
  do.call(rbind, out[seq_len(n)])
}

empty_classified_frame <- function() {
  data.frame(job_id = character(0), source = character(0), sheet = character(0),
             title = character(0), company = character(0), location = character(0),
             date_posted = character(0), closing_date = character(0), salary = character(0),
             work_type = character(0), link = character(0), also_matches = character(0),
             stringsAsFactors = FALSE)
}
