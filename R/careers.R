# Career-page scraper (rvest + polite). STUB for Stage 1 -- replaced with a
# real implementation in Stage 2. Returns no jobs and logs that it hasn't
# been built yet, so main.R's pipeline and per-source error handling can be
# exercised today without crashing on a missing function.

fetch_careers <- function(cfg) {
  logger::log_info("careers.R is a Stage 1 stub -- company career-page scraping arrives in Stage 2.")
  empty_usajobs_frame()[0, ] # same column shape as the USAJOBS source, zero rows
}
