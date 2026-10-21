# USAJOBS Search API client (https://data.usajobs.gov/api/search), via httr2.
#
# Confirmed against the official docs at developer.usajobs.gov as of this
# writing. Two things differ from common assumptions, noted here because the
# brief asked to flag any substitution:
#   - The occupational-series filter is the `JobCategoryCode` query param,
#     not `Series` (the docs call it "job category," same 4-digit codes).
#   - `HiringPath` only accepts "student" and "graduates" -- there is no
#     "pathways" value. Pathways postings fall under those two paths and are
#     additionally caught by the Internships keyword match in classify.R.
# Every request must send Host: data.usajobs.gov, User-Agent: <your email>,
# and Authorization-Key: <your API key> -- see .Renviron.example.

#' Fetch all entry-level USAJOBS postings matching config, fully paginated.
#' Returns a data.frame of raw rows (one per posting) ready for classify_job(),
#' or throws an error (caught by main.R's per-source tryCatch) on failure.
fetch_usajobs <- function(cfg) {
  api_key <- Sys.getenv("USAJOBS_API_KEY")
  if (!nzchar(api_key)) {
    stop("USAJOBS_API_KEY is not set. Add it to .Renviron (see .Renviron.example).")
  }
  email <- cfg$usajobs$user_agent_email
  if (!nzchar(email) || email == "your_email@example.com") {
    stop("Set usajobs.user_agent_email in config.yaml to your real email (required by USAJOBS).")
  }

  series_param <- paste(cfg$usajobs$series, collapse = ";")
  hiring_path_param <- paste(cfg$usajobs$hiring_paths, collapse = ";")
  per_page <- cfg$usajobs$results_per_page

  base_req <- httr2::request(cfg$usajobs$base_url) |>
    httr2::req_headers(
      Host = "data.usajobs.gov",
      `User-Agent` = email,
      `Authorization-Key` = api_key
    ) |>
    httr2::req_url_query(
      JobCategoryCode = series_param,
      HiringPath = hiring_path_param,
      PayGradeLow = cfg$usajobs$grade_low,
      PayGradeHigh = cfg$usajobs$grade_high,
      ResultsPerPage = per_page,
      SortField = "opendate",
      SortDirection = "desc"
    ) |>
    httr2::req_retry(max_tries = 3, backoff = \(n) 2^n) |>
    httr2::req_throttle(rate = 20 / 60)

  page <- 1
  all_items <- list()
  repeat {
    resp <- base_req |>
      httr2::req_url_query(Page = page) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    items <- body$SearchResult$SearchResultItems
    if (length(items) == 0) break
    all_items <- c(all_items, items)

    total_pages <- tryCatch(as.integer(body$UserArea$NumberOfPages), error = function(e) NA_integer_)
    if (!is.na(total_pages) && page >= total_pages) break
    if (length(items) < per_page) break
    page <- page + 1
  }

  if (length(all_items) == 0) {
    return(empty_usajobs_frame())
  }

  rows <- lapply(all_items, usajobs_item_to_row, cfg = cfg)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty_usajobs_frame())

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

empty_usajobs_frame <- function() {
  data.frame(
    source = character(0), posting_id = character(0), title = character(0),
    company = character(0), location = character(0), description = character(0),
    date_posted = character(0), closing_date = character(0), salary = character(0),
    work_type = character(0), link = character(0), is_government = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Convert one SearchResultItems entry into our internal row shape, applying
#' the GS-04..GS-09 "entry-level ladder" grade check (a ladder like GS-07-11
#' counts because its range overlaps 04-09).
usajobs_item_to_row <- function(item, cfg) {
  d <- item$MatchedObjectDescriptor
  details <- d$UserArea$Details

  low_grade <- suppressWarnings(as.integer(details$LowGrade))
  high_grade <- suppressWarnings(as.integer(details$HighGrade))
  grade_low_cfg <- as.integer(cfg$usajobs$grade_low)
  grade_high_cfg <- as.integer(cfg$usajobs$grade_high)

  grade_ok <- !is.na(low_grade) && !is.na(high_grade) &&
    low_grade <= grade_high_cfg && high_grade >= grade_low_cfg
  if (!is.na(low_grade) && is.na(high_grade)) grade_ok <- low_grade >= grade_low_cfg && low_grade <= grade_high_cfg
  if (is.na(low_grade) && is.na(high_grade)) grade_ok <- TRUE # ungraded (e.g. Pathways interns); keep, hiring-path filter already applied server-side

  if (!isTRUE(grade_ok)) return(NULL)

  locs <- d$PositionLocation
  location <- if (length(locs) > 0) {
    paste(unique(vapply(locs, function(l) l$LocationName %||% "", character(1))), collapse = "; ")
  } else {
    d$PositionLocationDisplay %||% NA_character_
  }

  remun <- d$PositionRemuneration
  salary <- if (length(remun) > 0) {
    r <- remun[[1]]
    lo <- r$MinimumRange %||% ""
    hi <- r$MaximumRange %||% ""
    interval <- r$RateIntervalCode %||% ""
    paste0("$", lo, "-$", hi, " ", interval)
  } else {
    NA_character_
  }

  offering_types <- vapply(d$PositionOfferingType %||% list(), function(x) x$Name %||% "", character(1))
  work_type <- if (any(grepl("intern", offering_types, ignore.case = TRUE))) NA_character_ else NA_character_

  text_blocks <- d$PositionFormattedDescription %||% list()
  description <- paste(vapply(text_blocks, function(b) b$Content %||% "", character(1)), collapse = " ")
  description <- paste(description, details$JobSummary %||% "", details$MajorDuties %||% "")

  list(
    source = "usajobs",
    posting_id = as.character(d$PositionID %||% item$MatchedObjectId %||% NA),
    title = d$PositionTitle %||% NA_character_,
    company = d$OrganizationName %||% NA_character_,
    location = location,
    description = description,
    date_posted = d$PublicationStartDate %||% NA_character_,
    closing_date = d$ApplicationCloseDate %||% NA_character_,
    salary = salary,
    work_type = work_type,
    link = (d$PositionURI %||% (d$ApplyURI %||% list(NA_character_))[[1]]) %||% NA_character_,
    is_government = TRUE
  )
}
