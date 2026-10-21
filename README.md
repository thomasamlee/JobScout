# JobScout -- Stage 1

Entry-level job tracker for Austin/Texas. This stage covers: project
structure, config, SQLite state, the USAJOBS source, the Excel workbook
builder, and `--demo` mode. Company career-page scraping (Stage 2), email
(Stage 3), and scheduling/tests/troubleshooting (Stage 4) come next.

## What's here

```
JobScout.Rproj        RStudio project file -- open this in RStudio
main.R                Entry point / CLI (optparse)
config.yaml           All keywords, locations, paths, and settings
.Renviron.example      Template for secrets (copy to .Renviron, never commit it)
.gitignore
renv.lock, renv/       Reproducible package versions
R/
  utils.R              Config/logging/path helpers, text matching, date & salary parsing, stable IDs
  storage.R            SQLite: jobs, feedback, source-status tables
  classify.R           Location/category/exclusion rules + the raw-to-stored pipeline
  usajobs.R            USAJOBS Search API client (httr2)
  careers.R            Stage 1 stub -- real scraper lands in Stage 2
  excel_builder.R      Builds JobScout.xlsx: sheets, dropdown, formatting, backups
  feedback.R           Phase 2 hook only (Match Score) -- not wired in yet
  demo.R               --demo mode: 15 synthetic postings
tests/testthat/        Empty for now -- filled in Stage 4
```

## Try it now (no API key needed)

1. Open `JobScout.Rproj` in RStudio. renv activates automatically.
2. In the R console: `renv::restore()` (installs the exact package versions
   in `renv.lock` into this project only -- doesn't touch your other R
   projects).
3. From the RStudio **Terminal** tab (not the R console):
   ```
   Rscript main.R --demo
   ```
   Or from the R console: `source("main.R")` won't work directly because of
   `optparse`'s command-line parsing -- always run demo mode via `Rscript`,
   either from the Terminal tab or a regular terminal window, with your
   working directory set to this project folder.
4. Open `demo_output/JobScout.xlsx`. You should see:
   - **Summary** sheet with per-sheet counts, new/removed activity, and a
     source-status table.
   - **Government / Internships / Cyber / IT / Sales / Out of State** sheets,
     each with a frozen header row, filters, a green/red/yellow **Decision**
     dropdown (Approve/Reject/Maybe), clickable **Link** cells, real Excel
     dates, and a hidden **Job ID** column.
   - 12 open postings total out of 15 synthetic ones: one duplicate posting
     collapsed into a single row, and two postings with a closing date of
     "yesterday" were automatically closed and removed.
5. Run `Rscript main.R --demo` again -- it reuses `demo_output/jobscout_demo.sqlite`,
   so you can mark a few Decisions in Excel, save, close the file, and rerun
   to confirm your choices survive the rebuild.

Demo output lives entirely under `demo_output/` (workbook, backups, log,
SQLite db), separate from the real `C:\JobScout` workbook folder and
database used by a live run.

## Running a live USAJOBS-only fetch

1. Request a free API key at https://developer.usajobs.gov/apirequest/.
2. Copy `.Renviron.example` to `.Renviron` and fill in `USAJOBS_API_KEY`.
3. Edit `config.yaml`: set `usajobs.user_agent_email` to your real email
   (USAJOBS requires it as the `User-Agent` header on every request).
4. Restart R so `.Renviron` is reloaded, then:
   ```
   Rscript main.R --source usajobs --dry-run
   ```
   `--dry-run` builds the workbook but never emails it (email isn't built
   yet anyway in Stage 1). Output goes to `C:\JobScout\JobScout.xlsx` (or
   whatever `paths.workbook_dir` is set to in `config.yaml`).

## Notes / assumptions made in this stage

- **Occupational series param**: USAJOBS' documented query parameter is
  `JobCategoryCode`, not `Series` -- same 4-digit codes, different name. See
  the comment at the top of `R/usajobs.R`.
- **Hiring paths**: the API only recognizes `student` and `graduates` as
  `HiringPath` values -- there's no separate `pathways` value. Pathways
  postings are still caught because they're almost always tagged `student`
  or `graduates`, and because "pathways" is also an Internships category
  keyword in `config.yaml`.
- **Entry-level grade ladder**: a posting counts as entry-level if its
  `LowGrade`-`HighGrade` range overlaps GS-04..GS-09 at all (so a GS-07-11
  ladder posting counts, per your spec). Ungraded postings (common for
  Pathways internships) are kept rather than dropped, since the hiring-path
  filter already narrows the API call.
- **`digest`** (CRAN, used for the stable-ID hash in `make_job_id()`) was
  added as a dependency -- it's a tiny, dependency-free package that's
  already a transitive dependency of several packages in your requested
  stack, so it adds no real weight.
- A job that matches none of the tracked categories (Internships/Cyber/IT/
  Sales) and isn't a Government posting is dropped rather than placed on a
  seventh catch-all sheet, since none was specified for it.
- Unparseable salary strings are kept as-is (flagged via being left
  unchanged, since there's no separate "couldn't parse" column requested).
- `main.R` assumes it's run with the project folder as the working
  directory (true by default in RStudio, and when Task Scheduler's "Start
  in" field is set to this folder -- covered in Stage 4).

## Package substitutions

None beyond `digest` above. Everything else in your requested stack
(httr2, openxlsx, readxl, DBI, RSQLite, yaml, lubridate, optparse, logger,
fs) is installed and in use as specified. `rvest`, `polite`, `blastula`,
`testthat`, and `taskscheduleR` will be added to `renv.lock` in Stages 2-4
when those modules are built -- run `renv::snapshot()` again after each
stage if you want to lock them in early.
