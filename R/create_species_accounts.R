# Safely create/synchronize species account Rmd files.
# Any content inside USER blocks is preserved verbatim.

trim_account_character <- function(x) {
  if (is.character(x)) trimws(x) else x
}

read_account_species <- function(path = file.path("data", "species.csv")) {
  x <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  x[] <- lapply(x, trim_account_character)

  required <- c(
    "code", "group", "scientific_name",
    "account_name", "result_dir", "sort_order"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "species.csv is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(x$code)) {
    stop("Duplicate account code(s) in species.csv.", call. = FALSE)
  }

  x$sort_order <- as.numeric(x$sort_order)
  x[order(x$sort_order, x$account_name, x$code), , drop = FALSE]
}

account_heading <- function(scientific_name, account_name) {
  scientific_name <- trimws(scientific_name)
  account_name <- trimws(account_name)

  if (identical(scientific_name, account_name)) {
    return(paste0("*", scientific_name, "*"))
  }

  prefix <- paste0(scientific_name, " ")

  if (startsWith(account_name, prefix)) {
    suffix <- substring(account_name, nchar(prefix) + 1L)
    return(paste0("*", scientific_name, "* ", suffix))
  }

  paste0("*", scientific_name, "* — ", account_name)
}

account_id <- function(code) {
  id <- tolower(code)
  id <- gsub("[^a-z0-9_-]+", "-", id)
  gsub("^-+|-+$", "", id)
}

extract_user_blocks <- function(lines, file_label = "<text>") {
  begin_rx <- "^<!-- BEGIN USER:([A-Za-z0-9_.-]+) -->$"
  end_rx <- "^<!-- END USER:([A-Za-z0-9_.-]+) -->$"

  begin_idx <- grep(begin_rx, lines)
  end_idx <- grep(end_rx, lines)

  if (!length(begin_idx) && !length(end_idx)) return(list())

  begin_names <- sub(begin_rx, "\\1", lines[begin_idx])
  end_names <- sub(end_rx, "\\1", lines[end_idx])

  if (anyDuplicated(begin_names)) {
    stop(
      "Duplicate BEGIN USER block(s) in ", file_label, ": ",
      paste(unique(begin_names[duplicated(begin_names)]), collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(end_names)) {
    stop(
      "Duplicate END USER block(s) in ", file_label, ": ",
      paste(unique(end_names[duplicated(end_names)]), collapse = ", "),
      call. = FALSE
    )
  }

  if (!setequal(begin_names, end_names)) {
    stop(
      "Malformed protected USER blocks in ", file_label,
      ": BEGIN and END markers do not match.",
      call. = FALSE
    )
  }

  out <- list()

  for (nm in begin_names) {
    b <- grep(paste0("^<!-- BEGIN USER:", nm, " -->$"), lines)
    e <- grep(paste0("^<!-- END USER:", nm, " -->$"), lines)

    if (length(b) != 1L || length(e) != 1L || e <= b) {
      stop("Malformed USER block `", nm, "` in ", file_label, ".", call. = FALSE)
    }

    out[[nm]] <- if (e == b + 1L) character() else lines[(b + 1L):(e - 1L)]
  }

  out
}

extract_legacy_manual_text <- function(lines) {
  b <- grep("^<!-- BEGIN MANUAL SPECIES TEXT -->$", lines)
  e <- grep("^<!-- END MANUAL SPECIES TEXT -->$", lines)

  if (length(b) == 1L && length(e) == 1L && e > b) {
    if (e == b + 1L) return(character())
    return(lines[(b + 1L):(e - 1L)])
  }

  NULL
}

replace_literal <- function(x, token, value) {
  gsub(token, value, x, fixed = TRUE)
}

render_account_template <- function(
    sp,
    template_path,
    user_blocks = list()) {

  template <- readLines(template_path, warn = FALSE)

  template <- replace_literal(
    template, "{{ACCOUNT_HEADING}}",
    account_heading(sp$scientific_name[[1L]], sp$account_name[[1L]])
  )
  template <- replace_literal(
    template, "{{ACCOUNT_ID}}",
    account_id(sp$code[[1L]])
  )
  template <- replace_literal(
    template, "{{SPECIES_CODE}}",
    sp$code[[1L]]
  )

  # USER blocks are discovered from the template itself.
  user_rx <- "^\\{\\{USER:([A-Za-z0-9_.-]+)\\}\\}$"
  idx <- grep(user_rx, template)
  block_names <- sub(user_rx, "\\1", template[idx])

  if (anyDuplicated(block_names)) {
    dup <- unique(block_names[duplicated(block_names)])
    stop(
      "Duplicate USER placeholder(s) in template: ",
      paste(dup, collapse = ", "),
      call. = FALSE
    )
  }

  defaults <- list(
    overall_assessment = c(
      "",
      "Write the overall expert assessment here.",
      ""
    ),
    statistical_evaluation = c(
      "",
      "Write the expert interpretation of statistical performance here.",
      ""
    ),
    prioritisation_assessment = c(
      "",
      "Write the expert interpretation of conservation prioritization here.",
      ""
    ),
    version_evolution = c(
      "",
      "Describe model-version changes here. R chunks may be included in this block.",
      ""
    )
  )

  for (nm in block_names) {
    token <- paste0("{{USER:", nm, "}}")
    token_idx <- which(template == token)

    content <- user_blocks[[nm]]

    if (is.null(content)) {
      content <- if (nm %in% names(defaults)) defaults[[nm]] else character()
    }

    template <- append(
      template[-token_idx],
      values = content,
      after = token_idx - 1L
    )
  }

  template
}

write_bookdown_yml <- function(
    species,
    path = file.path("book", "_bookdown.yml")) {

  species_files <- file.path("species", paste0(species$code, ".Rmd"))

  rmd_files <- c(
    "index.Rmd",
    "01-TerminologyAcronyms.Rmd",
    "02-Workflow.Rmd",
    "03-SpeciesAccounts.Rmd",
    species_files,
    "04-References.Rmd"
  )

  lines <- c(
    'book_filename: "HiQBioDiv-SDMs"',
    'delete_merged_file: true',
    'output_dir: "../docs"',
    "new_session: false",
    "rmd_files:",
    paste0('  - "', rmd_files, '"')
  )

  writeLines(lines, path)
  message("Updated: ", path)
  invisible(path)
}

sync_species_accounts <- function(
    species_path = file.path("data", "species.csv"),
    template_path = file.path(
      "book", "templates", "species-account-template.Rmd"
    ),
    species_dir = file.path("book", "species"),
    bookdown_path = file.path("book", "_bookdown.yml")) {

  species <- read_account_species(species_path)

  if (!file.exists(template_path)) {
    stop("Species-account template not found: ", template_path, call. = FALSE)
  }

  dir.create(species_dir, recursive = TRUE, showWarnings = FALSE)

  created <- updated <- unchanged <- 0L

  for (i in seq_len(nrow(species))) {
    sp <- species[i, , drop = FALSE]
    out_file <- file.path(species_dir, paste0(sp$code[[1L]], ".Rmd"))

    user_blocks <- list()

    if (file.exists(out_file)) {
      old <- readLines(out_file, warn = FALSE)
      user_blocks <- extract_user_blocks(old, out_file)

      if (!length(user_blocks)) {
        legacy <- extract_legacy_manual_text(old)
        if (!is.null(legacy)) {
          user_blocks$overall_assessment <- legacy
        }
      }
    }

    new <- render_account_template(
      sp = sp,
      template_path = template_path,
      user_blocks = user_blocks
    )

    if (!file.exists(out_file)) {
      writeLines(new, out_file)
      message("Created species account: ", out_file)
      created <- created + 1L
    } else {
      old <- readLines(out_file, warn = FALSE)
      if (!identical(old, new)) {
        writeLines(new, out_file)
        message("Synchronized species account: ", out_file)
        updated <- updated + 1L
      } else {
        unchanged <- unchanged + 1L
      }
    }
  }

  write_bookdown_yml(species, bookdown_path)

  message(
    "Done. Created ", created,
    "; synchronized ", updated,
    "; unchanged ", unchanged, " account(s)."
  )

  invisible(species)
}

create_species_accounts <- sync_species_accounts
