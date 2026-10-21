# Shared helpers used by every other R/ module: config loading, logging,
# Windows-safe paths, text matching, date/salary parsing, and stable IDs.

#' Read config.yaml from the project root.
#' @param path Path to the config file, relative to the project root.
load_config <- function(path = "config.yaml") {
  if (!fs::file_exists(path)) {
    stop("Config file not found at '", path, "'. Run from the project root.")
  }
  yaml::read_yaml(path)
}

#' Set up logger to write to both console and a dated log file under
#' <workbook_dir>/<log_subdir>/jobscout_YYYY-MM-DD.log.
setup_logging <- function(cfg, demo = FALSE) {
  base_dir <- if (demo) cfg$demo$workbook_dir else cfg$paths$workbook_dir
  log_dir <- fs::path(base_dir, cfg$paths$log_subdir)
  fs::dir_create(log_dir)
  stamp <- format(now_central(), "%Y-%m-%d")
  log_file <- fs::path(log_dir, paste0("jobscout_", stamp, if (demo) "_demo" else "", ".log"))

  logger::log_threshold(logger::INFO)
  logger::log_layout(logger::layout_glue_generator(
    format = "{time} [{level}] {msg}"
  ))
  logger::log_appender(logger::appender_tee(log_file))
  invisible(log_file)
}

#' Current time in America/Chicago, used everywhere instead of Sys.time().
now_central <- function() {
  lubridate::with_tz(lubridate::now(), "America/Chicago")
}

#' Ensure a directory exists (Windows-safe, via fs).
ensure_dir <- function(path) {
  fs::dir_create(path)
  invisible(path)
}

# ---- Text matching --------------------------------------------------------

#' Does `text` contain `keyword` as a whole word (allowing a simple plural
#' "s"), case-insensitively? Keywords with punctuation other than letters,
#' digits, spaces, and hyphens (e.g. "5+ years", "TS-SCI") fall back to a
#' plain case-insensitive substring match since word-boundary regex doesn't
#' apply cleanly to them.
contains_keyword <- function(text, keyword) {
  if (is.na(text) || is.null(text)) return(FALSE)
  if (grepl("^[A-Za-z0-9 \\-]+$", keyword)) {
    pattern <- paste0("\\b", escape_regex(keyword), "s?\\b")
    grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
  } else {
    grepl(escape_regex(keyword), text, ignore.case = TRUE, perl = TRUE)
  }
}

#' TRUE if any of `keywords` is found in `text`.
contains_any_keyword <- function(text, keywords) {
  any(vapply(keywords, function(k) contains_keyword(text, k), logical(1)))
}

#' Which of `keywords` were found in `text`.
matched_keywords <- function(text, keywords) {
  hit <- vapply(keywords, function(k) contains_keyword(text, k), logical(1))
  keywords[hit]
}

escape_regex <- function(x) {
  gsub("([\\\\.|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}

# ---- Dates ------------------------------------------------------------------

#' Parse a posted-date string that may be a real date, an ISO datetime, or a
#' relative phrase like "2 weeks ago", "30+ days ago", "Today", "Yesterday".
#' Returns a Date or NA (never a guessed/invented date).
parse_posted_date <- function(x, reference = now_central()) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(as.Date(NA))
  x <- trimws(x)
  ref_date <- as.Date(reference)

  iso <- tryCatch(suppressWarnings(as.Date(x)), error = function(e) as.Date(NA))
  if (!is.na(iso)) return(iso)

  dt <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
  if (!is.na(dt)) return(as.Date(lubridate::with_tz(dt, "America/Chicago")))

  lx <- tolower(x)
  if (lx == "today") return(ref_date)
  if (lx == "yesterday") return(ref_date - 1)

  m <- regmatches(lx, regexec("^(\\d+)\\+?\\s*(day|week|month)s?\\s*ago$", lx))[[1]]
  if (length(m) == 3) {
    n <- as.integer(m[2])
    unit <- m[3]
    offset <- switch(unit, day = n, week = n * 7, month = n * 30)
    return(ref_date - offset)
  }

  as.Date(NA)
}

# ---- Salary normalization ---------------------------------------------------

#' Normalize a free-text salary string into "$55,000-$70,000/yr" or
#' "$28/hr" style. Returns the original text unchanged if it can't be parsed
#' confidently (never invents a number).
normalize_salary <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return("Not listed")
  x <- trimws(x)

  is_hourly <- grepl("hour|/hr|per hr|\\bhr\\b", x, ignore.case = TRUE)
  is_yearly <- grepl("year|/yr|annual|\\byr\\b", x, ignore.case = TRUE)

  nums <- regmatches(x, gregexpr("[0-9][0-9,]*(\\.[0-9]+)?", x))[[1]]
  if (length(nums) == 0) return(x)
  nums <- as.numeric(gsub(",", "", nums))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0) return(x)

  suffix <- if (is_hourly && !is_yearly) "/hr" else if (is_yearly) "/yr" else {
    # Heuristic: values under 300 with no explicit unit are almost always
    # hourly; larger values are almost always annual salaries.
    if (max(nums) < 300) "/hr" else "/yr"
  }

  fmt_one <- function(n) {
    if (suffix == "/hr") {
      paste0("$", formatC(n, format = "f", digits = 2, big.mark = ","))
    } else {
      paste0("$", formatC(round(n), format = "d", big.mark = ","))
    }
  }

  if (length(nums) >= 2) {
    lo <- min(nums[1:2])
    hi <- max(nums[1:2])
    paste0(fmt_one(lo), "-", fmt_one(hi), suffix)
  } else {
    paste0(fmt_one(nums[1]), suffix)
  }
}

# ---- Stable IDs --------------------------------------------------------------

#' Build a stable job ID: "<source>:<posting_id>" when a posting ID is known,
#' otherwise a hash of normalized company + title + location so the same
#' posting is never added twice, across sources or across weekly runs.
make_job_id <- function(source, posting_id = NA, company = NA, title = NA, location = NA) {
  if (!is.na(posting_id) && nzchar(trimws(posting_id))) {
    return(paste0(source, ":", trimws(posting_id)))
  }
  key <- tolower(paste(trimws(company), trimws(title), trimws(location), collapse = "|"))
  key <- gsub("\\s+", " ", key)
  paste0(source, ":", substr(digest::digest(key, algo = "sha1"), 1, 16))
}
