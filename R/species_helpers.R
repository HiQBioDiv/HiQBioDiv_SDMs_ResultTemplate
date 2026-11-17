# Helpers used while rendering HiQBioDiv species accounts.
# Dynamic account information is read directly from data/rezultatu_tabula.xlsx.

# -------------------------------------------------------------------------
# Generic utilities
# -------------------------------------------------------------------------

trim_character_columns <- function(x) {
  char_cols <- vapply(x, is.character, logical(1))
  x[char_cols] <- lapply(x[char_cols], trimws)
  x
}

is_missing_value <- function(x) {
  length(x) == 0L ||
    is.na(x[[1L]]) ||
    (is.character(x[[1L]]) && !nzchar(trimws(x[[1L]])))
}

fmt_num <- function(x, digits = 3, missing = "not available") {
  if (is_missing_value(x)) return(missing)
  value <- x[[1L]]
  if (!is.numeric(value)) {
    value2 <- suppressWarnings(as.numeric(value))
    if (!is.na(value2)) value <- value2
  }
  if (is.numeric(value)) {
    return(format(round(value, digits), trim = TRUE, scientific = FALSE))
  }
  as.character(value)
}

fmt_pct <- function(x, digits = 1, missing = "not available") {
  if (is_missing_value(x)) return(missing)
  value <- suppressWarnings(as.numeric(x[[1L]]))
  if (is.na(value)) return(as.character(x[[1L]]))
  paste0(format(round(100 * value, digits), trim = TRUE, scientific = FALSE), "%")
}

humanise_code <- function(x) {
  if (is_missing_value(x)) return("not specified")
  y <- as.character(x[[1L]])
  y <- gsub("[_-]+", " ", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

# -------------------------------------------------------------------------
# Species metadata used for book order and result folders
# -------------------------------------------------------------------------

read_species_metadata <- function(path = file.path("data", "species.csv")) {
  if (!file.exists(path)) {
    stop("Species metadata file not found: ", path, call. = FALSE)
  }

  x <- suppressWarnings(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
  x <- trim_character_columns(x)

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
    dup <- unique(x$code[duplicated(x$code)])
    stop(
      "Duplicate species/account codes found: ",
      paste(dup, collapse = ", "),
      call. = FALSE
    )
  }

  x$sort_order <- suppressWarnings(as.numeric(x$sort_order))
  if (anyNA(x$sort_order)) {
    stop("`sort_order` must be numeric for every account.", call. = FALSE)
  }

  x[order(x$sort_order, x$account_name, x$code), , drop = FALSE]
}

get_species <- function(
    species_code,
    species_path = file.path("data", "species.csv")) {

  x <- read_species_metadata(species_path)
  hit <- x[x$code == species_code, , drop = FALSE]

  if (nrow(hit) != 1L) {
    stop(
      "Expected exactly one species.csv row for account `",
      species_code, "`; found ", nrow(hit), ".",
      call. = FALSE
    )
  }

  hit[1L, , drop = FALSE]
}

# -------------------------------------------------------------------------
# Near-final results table
# -------------------------------------------------------------------------

make_account_code <- function(code, niche) {
  code <- ifelse(is.na(code), "", trimws(as.character(code)))
  niche <- ifelse(is.na(niche), "", trimws(as.character(niche)))
  niche <- gsub("\\s+", "", niche)
  paste0(code, niche)
}

read_results_table <- function(
    path = file.path("data", "rezultatu_tabula.xlsx")) {

  if (!file.exists(path)) {
    stop(
      "Near-final results table not found: ", path,
      "\nCopy early_gitbook/rezultatu_tabula.xlsx to data/rezultatu_tabula.xlsx.",
      call. = FALSE
    )
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Package `readxl` is required to read rezultatu_tabula.xlsx.",
      call. = FALSE
    )
  }

  x <- as.data.frame(
    readxl::read_excel(path),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  x <- trim_character_columns(x)

  required <- c(
    "Group", "ScientificName", "CODE", "Niche", "Directory",
    "data_split", "bias_type", "LULCfiltering",
    "expert_egv", "expert_map", "expert_applicability",
    "n_pres", "n_presTrain", "n_presTest", "n_bgTrain", "n_bgTest",
    "combAUCtrain", "combAUCtest", "combTSStrain",
    "combTSSthreshold", "indeptestingTSSsensitivity",
    "indeptestingTSSspecificity", "indeptestingTSS"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "rezultatu_tabula.xlsx is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x$account_code <- make_account_code(x$CODE, x$Niche)

  if (anyDuplicated(x$account_code)) {
    dup <- unique(x$account_code[duplicated(x$account_code)])
    stop(
      "Duplicate account codes in rezultatu_tabula.xlsx: ",
      paste(dup, collapse = ", "),
      call. = FALSE
    )
  }

  x
}

get_species_result <- function(
    species_code,
    results_path = file.path("data", "rezultatu_tabula.xlsx")) {

  x <- read_results_table(results_path)
  hit <- x[x$account_code == species_code, , drop = FALSE]

  if (nrow(hit) != 1L) {
    stop(
      "Expected exactly one rezultatu_tabula.xlsx row for account `",
      species_code, "`; found ", nrow(hit), ".",
      call. = FALSE
    )
  }

  hit[1L, , drop = FALSE]
}

# -------------------------------------------------------------------------
# Dynamic Summary text
# -------------------------------------------------------------------------

format_data_split <- function(x) {
  if (is_missing_value(x)) return("Data split was not specified.")
  value <- as.character(x[[1L]])

  if (identical(value, "TrainValTest")) {
    return(
      paste(
        "Occurrence and background data were divided into model-training with",
        "four block spatial cross-validation data and an independent test set."
      )
    )
  }

  if (identical(value, "TrainVal")) {
    return(
      paste(
        "Occurrence and background data were used for model training with",
        "four block spatial cross-validation; no independent test split was used."
      )
    )
  }

  paste0("Data-split scheme: **", humanise_code(value), "**.")
}

format_bias_text <- function(x) {
  if (is_missing_value(x)) {
    return("Sampling-bias treatment was not specified.")
  }
  paste0(
    "Sampling-bias/background treatment: **",
    humanise_code(x),
    "**."
  )
}

format_lulc_component <- function(
    x,
    scale = c("cell", "home_range")) {
  
  scale <- match.arg(scale)
  
  if (is_missing_value(x)) {
    return(NULL)
  }
  
  value <- trimws(
    as.character(x[[1L]])
  )
  
  scale_text <- switch(
    scale,
    
    cell =
      "within the 1-ha analysis cell",
    
    home_range =
      "within the home-range radius around the analysis cell"
  )
  
  
  if (identical(value, "No")) {
    
    return(
      paste0(
        "accumulated LULC change ",
        scale_text,
        " was not evaluated"
      )
    )
  }
  
  
  value_num <- suppressWarnings(
    as.numeric(value)
  )
  
  if (!is.na(value_num)) {
    
    return(
      paste0(
        "observations were retained only where accumulated LULC change ",
        scale_text,
        " from the year of observation to 2024 did not exceed ",
        value_num,
        "%"
      )
    )
  }
  
  
  paste0(
    "LULC-change filtering ",
    scale_text,
    " used the criterion `",
    value,
    "`"
  )
}

format_lulc_filter_text <- function(row) {
  
  cell <- if (
    "LULCcell" %in% names(row)
  ) {
    row$LULCcell
  } else {
    NA
  }
  
  hr <- if (
    "LULChr" %in% names(row)
  ) {
    row$LULChr
  } else {
    NA
  }
  
  
  cell_text <- format_lulc_component(
    cell,
    "cell"
  )
  
  hr_text <- format_lulc_component(
    hr,
    "home_range"
  )
  
  
  parts <- Filter(
    Negate(is.null),
    c(
      cell_text,
      hr_text
    )
  )
  
  
  if (!length(parts)) {
    
    return(
      "LULC-change filtering was not specified."
    )
  }
  
  
  paste0(
    paste0(
      toupper(substr(parts[[1L]], 1, 1)),
      substring(parts[[1L]], 2)
    ),
    if (length(parts) > 1L) {
      paste0(
        "; ",
        parts[[2L]]
      )
    } else {
      ""
    },
    "."
  )
}

render_model_description <- function(row) {
  cat("### Model description {-}\n\n")

  cat(format_data_split(row$data_split), " ")
  cat(format_bias_text(row$bias_type), " ")
  cat(format_lulc_filter_text(row), "\n\n")

  if (!is_missing_value(row$n_presTrain)) {
    cat(
      "The final data set contained **", fmt_num(row$n_pres, 0),
      "** presence records: **", fmt_num(row$n_presTrain, 0),
      "** for training with four spatial block cross-validation",
      if (!is_missing_value(row$n_presTest)) {
        paste0(" and **", fmt_num(row$n_presTest, 0), "** for independent testing")
      } else {
        ""
      },
      ". Training background contained **", fmt_num(row$n_bgTrain, 0),
      "** locations",
      if (!is_missing_value(row$n_bgTest)) {
        paste0(", with **", fmt_num(row$n_bgTest, 0), "** additional independent-test background locations")
      } else {
        ""
      },
      ".\n\n",
      sep = ""
    )
  }

  invisible(NULL)
}


score_description <- function(
    score,
    type = c(
      "response",
      "map",
      "applicability"
    )) {
  
  type <- match.arg(type)
  
  if (is_missing_value(score)) {
    return(NA_character_)
  }
  
  score <- suppressWarnings(
    as.integer(
      round(
        as.numeric(score[[1L]])
      )
    )
  )
  
  if (is.na(score)) {
    return(NA_character_)
  }
  
  
  descriptions <- list(
    
    response = c(
      `1` = "poor ecological interpretability",
      `2` = "limited ecological interpretability",
      `3` = "moderate ecological interpretability",
      `4` = "good ecological interpretability",
      `5` = "excellent ecological interpretability"
    ),
    
    map = c(
      `1` = "poor agreement with known distribution",
      `2` = "limited agreement with known distribution",
      `3` = "moderate agreement with known distribution",
      `4` = "good agreement with known distribution",
      `5` = "excellent agreement with known distribution"
    ),
    
    applicability = c(
      `0` = "not applicable",
      `1` = "very limited applicability",
      `2` = "limited applicability",
      `3` = "moderate applicability",
      `4` = "high applicability",
      `5` = "very high applicability"
    )
  )
  
  
  out <- descriptions[[type]][
    as.character(score)
  ]
  
  
  if (
    length(out) == 0L ||
    is.na(out)
  ) {
    return(NA_character_)
  }
  
  
  unname(out)
}


format_score <- function(
    score,
    type) {
  
  if (is_missing_value(score)) {
    return("not available")
  }
  
  value <- fmt_num(
    score,
    digits = 0
  )
  
  description <- score_description(
    score,
    type
  )
  
  if (is.na(description)) {
    return(value)
  }
  
  paste0(
    value,
    " (",
    description,
    ")"
  )
}

render_expert_scores <- function(row) {
  
  cat(
    "### Expert assessment scores {-}\n\n"
  )
  
  cat(
    "- **Marginal responses and variable importance score:** ",
    format_score(
      row$expert_egv,
      "response"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "- **Map score:** ",
    format_score(
      row$expert_map,
      "map"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "- **Applicability score:** ",
    format_score(
      row$expert_applicability,
      "applicability"
    ),
    "\n\n",
    sep = ""
  )
  
  invisible(NULL)
}

render_independent_metric <- function(
    row,
    label,
    empirical,
    null,
    stat) {
  
  if (
    !empirical %in% names(row) ||
    is_missing_value(
      row[[empirical]]
    )
  ) {
    return(
      invisible(NULL)
    )
  }
  
  
  cat(
    "- **",
    label,
    ":** ",
    fmt_num(
      row[[empirical]],
      digits = 3
    ),
    sep = ""
  )
  
  
  if (
    null %in% names(row) &&
    !is_missing_value(
      row[[null]]
    )
  ) {
    
    cat(
      "; null models: ",
      as.character(
        row[[null]][[1L]]
      ),
      sep = ""
    )
  }
  
  
  if (
    stat %in% names(row) &&
    !is_missing_value(
      row[[stat]]
    )
  ) {
    
    cat(
      "; ",
      as.character(
        row[[stat]][[1L]]
      ),
      sep = ""
    )
  }
  
  
  cat("\n")
  
  
  invisible(NULL)
}

render_statistical_summary <- function(row) {
  
  cat(
    "### Statistical evaluation {-}\n\n"
  )
  
  
  # ---------------------------------------------------------------
  # Overall fitted-model statistics
  # ---------------------------------------------------------------
  
  cat(
    "The final model had a training TSS of **",
    fmt_num(row$combTSStrain),
    "**",
    sep=""
  )
  
  
  if (!is_missing_value(row$combTSSthreshold)) {
    
    cat(
      ", with a training-derived cloglog threshold of **",
      fmt_num(row$combTSSthreshold),
      "**",
      sep=""
    )
  }
  
  
  cat(". ")
  
  
  if (!is_missing_value(row$combAUCtrain)) {
    
    cat(
      "Combined training AUC was **",
      fmt_num(row$combAUCtrain),
      "**",
      sep=""
    )
  }
  
  
  if (!is_missing_value(row$combAUCtest)) {
    
    cat(
      " and combined test AUC was **",
      fmt_num(row$combAUCtest),
      "**",
      sep=""
    )
  }
  
  
  cat(".\n\n")
  
  
  # ---------------------------------------------------------------
  # Spatial cross-validation
  # ---------------------------------------------------------------
  
  metric_specs <- list(
    
    AUC = c(
      "validationAUCemp",
      "validationAUCnulls",
      "validationAUCstat"
    ),
    
    CBI = c(
      "validationCBIemp",
      "validationCBInulls",
      "validationCBIstat"
    ),
    
    OR10 = c(
      "validationOR10emp",
      "validationOR10nulls",
      "validationOR10stat"
    ),
    
    ORmin = c(
      "validationORMINemp",
      "validationORMINnulls",
      "validationORMINstat"
    )
  )
  
  
  available <- vapply(
    metric_specs,
    function(cols) {
      
      all(cols %in% names(row)) &&
        !is_missing_value(
          row[[cols[[1L]]]]
        )
    },
    logical(1)
  )
  
  
  if (any(available)) {
    
    cat(
      "#### Spatial cross-validation {-}\n\n"
    )
    
    
    for (
      metric in
      names(metric_specs)[available]
    ) {
      
      cols <- metric_specs[[metric]]
      
      cat(
        "- **",
        metric,
        ":** ",
        as.character(
          row[[cols[[1L]]]][[1L]]
        ),
        sep=""
      )
      
      
      if (
        !is_missing_value(
          row[[cols[[2L]]]]
        )
      ) {
        
        cat(
          "; null models: ",
          as.character(
            row[[cols[[2L]]]][[1L]]
          ),
          sep=""
        )
      }
      
      
      if (
        !is_missing_value(
          row[[cols[[3L]]]]
        )
      ) {
        
        cat(
          "; ",
          as.character(
            row[[cols[[3L]]]][[1L]]
          ),
          sep=""
        )
      }
      
      
      cat("\n")
    }
    
    
    cat("\n")
  }
  
  
  # ---------------------------------------------------------------
  # Independent testing
  #
  # TrainVal models have no independent testing.
  # ---------------------------------------------------------------
  
  has_independent_test <-
    !is_missing_value(row$data_split) &&
    !identical(
      as.character(row$data_split[[1L]]),
      "TrainVal"
    )
  
  
  if (has_independent_test) {
    
    independent_available <- c(
      
      !is_missing_value(
        row$indeptestingTSS
      ),
      
      "indeptestingAUCemp_mean" %in% names(row) &&
        !is_missing_value(
          row$indeptestingAUCemp_mean
        ),
      
      "indeptestingCBIemp_mean" %in% names(row) &&
        !is_missing_value(
          row$indeptestingCBIemp_mean
        ),
      
      "indeptestingOR10emp_mean" %in% names(row) &&
        !is_missing_value(
          row$indeptestingOR10emp_mean
        ),
      
      "indeptestingORMINemp_mean" %in% names(row) &&
        !is_missing_value(
          row$indeptestingORMINemp_mean
        )
    )
    
    
    if (any(independent_available)) {
      
      cat(
        "#### Independent testing {-}\n\n"
      )
      
      
      # TSS
      if (
        !is_missing_value(
          row$indeptestingTSS
        )
      ) {
        
        cat(
          "- **TSS:** ",
          fmt_num(
            row$indeptestingTSS
          )
        )
        
        
        if (
          !is_missing_value(
            row$indeptestingTSSsensitivity
          )
        ) {
          
          cat(
            "; **sensitivity:** ",
            fmt_num(
              row$indeptestingTSSsensitivity
            )
          )
        }
        
        
        if (
          !is_missing_value(
            row$indeptestingTSSspecificity
          )
        ) {
          
          cat(
            "; **specificity:** ",
            fmt_num(
              row$indeptestingTSSspecificity
            )
          )
        }
        
        
        cat("\n")
      }
      
      
      # AUC
      render_independent_metric(
        row,
        label = "AUC",
        empirical = "indeptestingAUCemp_mean",
        null = "indeptestingAUCnulls",
        stat = "indeptestingAUCstat"
      )
      
      
      # CBI
      render_independent_metric(
        row,
        label = "CBI",
        empirical = "indeptestingCBIemp_mean",
        null = "indeptestingCBInulls",
        stat = "indeptestingCBIstat"
      )
      
      
      # OR10
      render_independent_metric(
        row,
        label = "OR10",
        empirical = "indeptestingOR10emp_mean",
        null = "indeptestingOR10nulls",
        stat = "indeptestingOR10stat"
      )
      
      
      # ORmin
      render_independent_metric(
        row,
        label = "ORmin",
        empirical = "indeptestingORMINemp_mean",
        null = "indeptestingORMINnulls",
        stat = "indeptestingORMINstat"
      )
      
      
      cat("\n")
    }
  }
  
  
  invisible(NULL)
}

render_prioritization_summary <- function(row) {
  
  required <- c(
    "priorPrimProp",
    "priorPrimSuitProp"
  )
  
  
  if (
    !all(
      required %in% names(row)
    )
  ) {
    return(
      invisible(NULL)
    )
  }
  
  
  if (
    is_missing_value(
      row$priorPrimProp
    ) &&
    is_missing_value(
      row$priorPrimSuitProp
    )
  ) {
    return(
      invisible(NULL)
    )
  }
  
  
  cat(
    "### Conservation prioritisation summary {-}\n\n"
  )
  
  
  # ---------------------------------------------------------------
  # Primary priority area
  # ---------------------------------------------------------------
  
  cat(
    "The primary priority area covers **",
    fmt_pct(
      row$priorPrimProp
    ),
    "** of Latvia",
    sep=""
  )
  
  
  if (
    !is_missing_value(
      row$priorPrimSuitProp
    )
  ) {
    
    cat(
      " and contains **",
      fmt_pct(
        row$priorPrimSuitProp
      ),
      "** of the summed habitat suitability (apparent population proxy)",
      sep=""
    )
  }
  
  
  cat(".\n\n")
  
  
  # ---------------------------------------------------------------
  # Secondary / broader high-priority area
  #
  # Only mention when it genuinely extends the primary area.
  # ---------------------------------------------------------------
  
  secondary_available <-
    "priorSecProp" %in% names(row) &&
    "priorSecSuitProp" %in% names(row) &&
    !is_missing_value(
      row$priorSecProp
    )
  
  
  if (secondary_available) {
    
    primary_prop <- suppressWarnings(
      as.numeric(
        row$priorPrimProp[[1L]]
      )
    )
    
    secondary_prop <- suppressWarnings(
      as.numeric(
        row$priorSecProp[[1L]]
      )
    )
    
    
    secondary_is_larger <-
      !is.na(primary_prop) &&
      !is.na(secondary_prop) &&
      secondary_prop > primary_prop
    
    
    if (secondary_is_larger) {
      
      cat(
        "The broader high importance area covers **",
        fmt_pct(
          row$priorSecProp
        ),
        "** of Latvia",
        sep=""
      )
      
      
      if (
        !is_missing_value(
          row$priorSecSuitProp
        )
      ) {
        
        cat(
          " and contains **",
          fmt_pct(
            row$priorSecSuitProp
          ),
          "** of the summed habitat suitability",
          sep=""
        )
      }
      
      
      cat(".\n\n")
    }
  }
  
  
  # ---------------------------------------------------------------
  # Suitable-area reference
  # ---------------------------------------------------------------
  
  if (
    "propHSsuit" %in% names(row) &&
    !is_missing_value(
      row$propHSsuit
    )
  ) {
    
    cat(
      "For comparison, the area classified as suitable using the maximum training sensitivity plus specificity ",
      "habitat-suitability threshold contains **",
      fmt_pct(
        row$propHSsuit
      ),
      "** of the summed continuous habitat suitability.\n\n",
      sep = ""
    )
  }
  
  
  invisible(NULL)
}
# -------------------------------------------------------------------------
# Result manifest and output discovery
# -------------------------------------------------------------------------

read_result_manifest <- function(
    path = file.path("data", "result_manifest.csv")) {

  if (!file.exists(path)) {
    stop("Result manifest not found: ", path, call. = FALSE)
  }

  x <- suppressWarnings(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
  x <- trim_character_columns(x)

  required <- c("type", "key", "title", "pattern", "required")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "result_manifest.csv is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  for (nm in c("description", "caption", "width")) {
    if (!nm %in% names(x)) x[[nm]] <- ""
  }

  required_chr <- toupper(as.character(x$required))
  if (any(!required_chr %in% c("TRUE", "FALSE"))) {
    stop("Manifest `required` must contain TRUE/FALSE.", call. = FALSE)
  }
  x$required <- required_chr == "TRUE"

  x
}

is_navigation_result_dir <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) return(TRUE)
  normalized <- gsub("\\\\", "/", trimws(path))
  normalized <- sub("/+$", "", normalized)
  identical(tolower(normalized), "results/nav")
}

resolve_result_dir <- function(sp, project_root = ".") {
  result_dir <- sp$result_dir[[1L]]

  if (is_navigation_result_dir(result_dir)) return(NA_character_)

  if (grepl("^(/|[A-Za-z]:[/\\\\])", result_dir)) {
    return(result_dir)
  }

  file.path(project_root, result_dir)
}

find_manifest_files <- function(result_dir, pattern) {
  if (is.na(result_dir) || !dir.exists(result_dir)) return(character())

  sort(
    list.files(
      result_dir,
      pattern = pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )
  )
}

manifest_message <- function(message) {
  cat("\n\n*", message, "*\n\n", sep = "")
}

emit_description <- function(x) {
  if (!is_missing_value(x)) {
    cat(trimws(as.character(x[[1L]])), "\n\n", sep = "")
  }
  invisible(NULL)
}

latex_figure_width <- function(width) {
  
  if (is_missing_value(width)) {
    return("\\linewidth")
  }
  
  x <- trimws(as.character(width[[1L]]))
  
  if (grepl("%$", x)) {
    p <- suppressWarnings(
      as.numeric(sub("%$", "", x))
    )
    
    if (!is.na(p)) {
      return(
        paste0(
          format(p / 100, trim = TRUE),
          "\\linewidth"
        )
      )
    }
  }
  
  "\\linewidth"
}

include_figure_files <- function(
    files,
    title = NULL,
    description = NULL,
    caption = NULL,
    width = NULL) {
  
  if (!length(files)) return(invisible(NULL))
  
  if (!is_missing_value(title)) {
    cat(
      "\n\n## ",
      title[[1L]],
      " {-}\n\n",
      sep = ""
    )
  }
  
  emit_description(description)
  
  caption_text <- if (is_missing_value(caption)) {
    ""
  } else {
    trimws(as.character(caption[[1L]]))
  }
  
  width_text <- if (is_missing_value(width)) {
    NULL
  } else {
    trimws(as.character(width[[1L]]))
  }
  
  latex_width <- latex_figure_width(width)
  
  for (file in files) {
    
    file <- gsub("\\\\", "/", file)
    file <- sub("^\\.\\./", "", file)
    
    if (knitr::is_latex_output()) {
      
      cat(
        "\n\\begin{center}\n",
        "\\includegraphics[",
        "width=",
        latex_width,
        ",",
        "height=0.70\\textheight,",
        "keepaspectratio",
        "]{",
        file,
        "}\n",
        sep = ""
      )
      
      if (nzchar(caption_text)) {
        cat(
          "\n\\par\\small\\textit{",
          caption_text,
          "}\\normalsize\n",
          sep = ""
        )
      }
      
      cat(
        "\\end{center}\n\n",
        sep = ""
      )
      
    } else {
      
      cat(
        "![",
        caption_text,
        "](",
        file,
        ")",
        sep = ""
      )
      
      if (!is.null(width_text)) {
        cat(
          "{width=",
          width_text,
          "}",
          sep = ""
        )
      }
      
      cat("\n\n")
    }
  }
  
  invisible(NULL)
}

read_table_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    tsv = utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
    txt = utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package `readxl` is required for Excel tables.", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(path), check.names = FALSE)
    },
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package `readxl` is required for Excel tables.", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(path), check.names = FALSE)
    },
    stop("Unsupported table extension: ", ext, call. = FALSE)
  )
}

include_table_files <- function(
    files,
    title = NULL,
    description = NULL,
    caption = NULL,
    width = NULL) {
  
  if (!length(files)) return(invisible(NULL))
  
  if (!is_missing_value(title)) {
    cat(
      "\n\n## ",
      title[[1L]],
      " {-}\n\n",
      sep = ""
    )
  }
  
  emit_description(description)
  
  caption_text <- if (is_missing_value(caption)) {
    ""
  } else {
    trimws(as.character(caption[[1L]]))
  }
  
  for (file in files) {
    
    tab <- read_table_file(file)
    
    if (knitr::is_latex_output()) {
      
      latex_table <- knitr::kable(
        tab,
        format = "latex",
        booktabs = TRUE,
        caption = NULL,
        escape = TRUE
      )
      
      cat(
        "\n\\begin{center}\n",
        "\\resizebox{\\linewidth}{!}{%\n",
        as.character(latex_table),
        "\n}\n",
        "\\end{center}\n\n",
        sep = ""
      )
      
    } else {
      
      html_table <- knitr::kable(
        tab,
        format = "html",
        caption = NULL
      )
      
      cat(
        "\n",
        as.character(html_table),
        "\n\n",
        sep = ""
      )
    }
    
    if (nzchar(caption_text)) {
      cat(
        "\n*",
        caption_text,
        "*\n\n",
        sep = ""
      )
    }
  }
  
  invisible(NULL)
}

render_manifest_item <- function(item, result_dir, strict = FALSE) {
  files <- find_manifest_files(
    result_dir = result_dir,
    pattern = item$pattern[[1L]]
  )

  if (!length(files)) {
    msg <- paste0(
      if (isTRUE(item$required[[1L]])) "Required" else "Optional",
      " output not found for ", item$title[[1L]], "."
    )

    if (isTRUE(strict) && isTRUE(item$required[[1L]])) {
      stop(msg, call. = FALSE)
    }

    if (isTRUE(item$required[[1L]])) manifest_message(msg)
    return(invisible(FALSE))
  }

  switch(
    item$type[[1L]],
    figure = include_figure_files(
      files,
      title = item$title,
      description = item$description,
      caption = item$caption,
      width = item$width
    ),
    table = include_table_files(
      files,
      title = item$title,
      description = item$description,
      caption = item$caption
    ),
    stop("Unsupported manifest type: ", item$type[[1L]], call. = FALSE)
  )

  invisible(TRUE)
}

render_species_results <- function(
    species_code,
    species_path = file.path("data", "species.csv"),
    manifest_path = file.path("data", "result_manifest.csv"),
    project_root = ".",
    strict = FALSE) {

  sp <- get_species(species_code, species_path)
  result_dir <- resolve_result_dir(sp, project_root)

  if (is.na(result_dir)) {
    manifest_message(
      "Standard model results are not currently available for this account."
    )
    return(invisible(FALSE))
  }

  if (!dir.exists(result_dir)) {
    msg <- paste0(
      "Result directory does not exist for ",
      sp$account_name[[1L]], ": `", result_dir, "`."
    )
    if (isTRUE(strict)) stop(msg, call. = FALSE)
    manifest_message(msg)
    return(invisible(FALSE))
  }

  manifest <- read_result_manifest(manifest_path)

  for (i in seq_len(nrow(manifest))) {
    render_manifest_item(
      manifest[i, , drop = FALSE],
      result_dir = result_dir,
      strict = strict
    )
  }

  invisible(TRUE)
}
