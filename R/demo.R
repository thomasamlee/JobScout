# --demo mode: 15 realistic fake postings spread across every sheet, with no
# API keys and no network access, so the Excel formatting, dropdowns, dedupe,
# and (dry-run) email can all be tested end to end. Demo data and state live
# in cfg$demo$* paths, completely separate from the live workbook/database.

#' Build the raw fake posting set. Two postings carry a closing_date of
#' yesterday, to exercise closed-job removal in the same run; two share the
#' same company/title/location to exercise dedupe (no posting_id, so both
#' hash to the same job_id and collapse into one row).
generate_demo_jobs <- function(run_date) {
  today <- as.Date(run_date)
  d <- function(offset) as.character(today + offset)

  rows <- list(
    list(source = "usajobs", posting_id = "DEMO-GOV-1",
         title = "Cybersecurity Pathways Intern", company = "Dept. of Energy",
         location = "Austin, TX", description = "Pathways Internship Program, entry level, student",
         date_posted = d(-3), closing_date = d(30), salary = "$19.73-$24.69 Per Hour",
         work_type = NA_character_, link = "https://www.usajobs.gov/job/demo-gov-1",
         is_government = TRUE),
    list(source = "usajobs", posting_id = "DEMO-GOV-2",
         title = "IT Specialist", company = "Internal Revenue Service",
         location = "Austin, TX", description = "GS-07/09 entry level IT support help desk",
         date_posted = d(-10), closing_date = d(14), salary = "$52,000-$67,000 Per Year",
         work_type = NA_character_, link = "https://www.usajobs.gov/job/demo-gov-2",
         is_government = TRUE),
    list(source = "usajobs", posting_id = "DEMO-GOV-3",
         title = "Recent Graduate Program Analyst", company = "Dept. of Veterans Affairs",
         location = "Remote", description = "Recent graduates program analyst entry level",
         date_posted = d(-5), closing_date = d(-1), salary = NA_character_,
         work_type = NA_character_, link = "https://www.usajobs.gov/job/demo-gov-3",
         is_government = TRUE),

    list(source = "careers", posting_id = NA_character_,
         title = "Software Engineering Intern - Summer 2027", company = "IBM",
         location = "Austin, TX", description = "intern student summer 2027 program",
         date_posted = d(-2), closing_date = d(60), salary = "$30/hr",
         work_type = NA_character_, link = "https://ibm.com/careers/demo-1",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Data Analyst Co-op", company = "Oracle",
         location = "Austin, TX", description = "co-op student program data analyst",
         date_posted = "2 weeks ago", closing_date = d(5), salary = NA_character_,
         work_type = NA_character_, link = "https://oracle.com/careers/demo-2",
         is_government = FALSE),

    list(source = "careers", posting_id = NA_character_,
         title = "Security Analyst I", company = "Crowe",
         location = "Austin, TX", description = "SOC incident response entry level analyst",
         date_posted = d(-1), closing_date = d(45), salary = "$65,000-$80,000/year",
         work_type = NA_character_, link = "https://crowe.com/careers/demo-3",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Information Security Associate", company = "Vanguard",
         location = "Remote", description = "information security analyst junior incident response",
         date_posted = d(-7), closing_date = d(30), salary = "$70,000/yr",
         work_type = NA_character_, link = "https://vanguard.com/careers/demo-4",
         is_government = FALSE),

    list(source = "careers", posting_id = NA_character_,
         title = "Help Desk Technician", company = "John Deere",
         location = "Austin, TX", description = "IT support help desk entry level technician",
         date_posted = d(-4), closing_date = d(20), salary = "$42,000-$48,000 a year",
         work_type = NA_character_, link = "https://johndeere.com/careers/demo-5",
         is_government = FALSE),
    # Deliberate duplicate of the Help Desk posting above: same source,
    # company, title, and location, so make_job_id() collapses both into
    # a single stored row (no posting_id to disambiguate them).
    list(source = "careers", posting_id = NA_character_,
         title = "Help Desk Technician", company = "John Deere",
         location = "Austin, TX", description = "IT support help desk entry level technician",
         date_posted = d(-4), closing_date = d(20), salary = "$42,000-$48,000 a year",
         work_type = NA_character_, link = "https://johndeere.com/careers/demo-5",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Business Analyst - Entry Level", company = "PROS",
         location = "Austin, TX", description = "business analyst entry level junior",
         date_posted = d(-6), closing_date = d(-1), salary = "$55,000/yr",
         work_type = NA_character_, link = "https://pros.com/careers/demo-6",
         is_government = FALSE),

    list(source = "careers", posting_id = NA_character_,
         title = "Account Representative", company = "Silicon Labs",
         location = "Austin, TX", description = "sales account representative junior",
         date_posted = d(0), closing_date = d(40), salary = NA_character_,
         work_type = NA_character_, link = "https://siliconlabs.com/careers/demo-7",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Business Development Associate", company = "Noble Corporation",
         location = "Houston, TX", description = "business development associate junior sales",
         date_posted = d(-8), closing_date = d(25), salary = "$48,000-$60,000/yr",
         work_type = NA_character_, link = "https://noblecorp.com/careers/demo-8",
         is_government = FALSE),

    list(source = "careers", posting_id = NA_character_,
         title = "Junior Data Analyst", company = "Accenture",
         location = "Chicago, IL", description = "data analyst entry level junior",
         date_posted = d(-3), closing_date = d(30), salary = "$58,000/yr",
         work_type = NA_character_, link = "https://accenture.com/careers/demo-9",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Customer Success Associate", company = "Vanguard",
         location = "Malvern, PA", description = "customer success associate junior",
         date_posted = d(-9), closing_date = d(35), salary = "$50,000/yr",
         work_type = NA_character_, link = "https://vanguard.com/careers/demo-10",
         is_government = FALSE),
    list(source = "careers", posting_id = NA_character_,
         title = "Technical Support Specialist", company = "IBM",
         location = NA_character_, description = "technical support specialist help desk junior",
         date_posted = d(-2), closing_date = d(50), salary = "$45,000/yr",
         work_type = NA_character_, link = "https://ibm.com/careers/demo-11",
         is_government = FALSE)
  )

  cols <- names(rows[[1]])
  do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)[cols]))
}

#' Run the full pipeline against fake data: classify, store, close past-due
#' postings, and rebuild the demo workbook. Returns the path it wrote.
run_demo <- function(cfg) {
  run_date <- now_central()
  con <- db_connect(cfg, demo = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  raw <- generate_demo_jobs(run_date)
  classified <- normalize_and_classify(raw, cfg)
  logger::log_info("Demo: {nrow(raw)} raw postings classified into {nrow(classified)} stored rows (dedupe collapses the duplicate).")

  db_upsert_jobs(con, classified, run_date)
  db_close_missing(con, "usajobs", classified$job_id[classified$source == "usajobs"], run_date)
  db_close_missing(con, "careers", classified$job_id[classified$source == "careers"], run_date)
  db_close_past_due(con, run_date)
  db_set_source_status(con, "demo", "OK", "Synthetic demo data", run_date)

  path <- build_workbook(con, cfg, run_date, db_get_source_statuses(con), demo = TRUE)
  logger::log_info("Demo workbook written to {path}")
  path
}
