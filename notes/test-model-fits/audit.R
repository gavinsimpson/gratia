# Run from the repository root. This reads R syntax without running tests.
# Inspect source without evaluating test code or fitting models.
constructors <- c("gam", "bam", "gamm", "gamm4", "scam", "scasm", "gamlss", "gjrm",
  "glmmTMB", "sdmTMB", "lm", "glm", "lme", "lmer", "glmer", "glmmPQL", "update",
  "fit", "fitter", "fit_fun", "fit_model", "model_fun", "do.call")
files <- sort(list.files("tests", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE))
extract <- function(lines, node) {
  x <- lines[node$line1:node$line2]
  if (length(x) == 1L) return(substr(x, node$col1, node$col2))
  x[length(x)] <- substr(x[length(x)], 1L, node$col2)
  x[1L] <- substring(x[1L], node$col1)
  paste(x, collapse = "\n")
}
rows <- list()
for (file in files) {
  lines <- readLines(file, warn = FALSE)
  p <- getParseData(parse(file, keep.source = TRUE))
  tokens <- p[p$token == "SYMBOL_FUNCTION_CALL" & p$text %in% constructors, ]
  for (j in seq_len(nrow(tokens))) {
    token <- tokens[j, ]
    head <- p[match(token$parent, p$id), ]
    node <- p[match(head$parent, p$id), ]
    code <- extract(lines, node)
    expr <- tryCatch(parse(text = code)[[1L]], error = function(e) NULL)
    if (!is.call(expr)) next
    args <- as.list(expr)[-1L]
    if (token$text == "do.call" && !grepl("gam|bam|scam|fit|fitter|model", deparse(args[[1L]])[1L])) next
    ancestors <- list()
    a <- node
    while (a$parent != 0L) {
      a <- p[match(a$parent, p$id), ]
      ancestors[[length(ancestors) + 1L]] <- a
    }
    title <- "File setup or helper"
    test_code <- ""
    for (a in ancestors) {
      tt <- p[p$parent == a$id, ]
      first <- tt[order(tt$line1, tt$col1), ][1L, ]
      if (first$token != "expr") next
      head_text <- extract(lines, first)
      if (head_text %in% c("test_that", "testthat::test_that")) {
        test_code <- extract(lines, a)
        test <- parse(text = test_code)[[1L]]
        title <- if (is.character(test[[2L]])) test[[2L]] else paste(deparse(test[[2L]]), collapse = " ")
        break
      }
    }
    arg <- function(name) {
      if (is.null(names(args)) || !name %in% names(args)) return("")
      paste(deparse(args[[name]], width.cutoff = 120L), collapse = " ")
    }
    formula <- arg("formula")
    if (!nzchar(formula) && length(args)) formula <- paste(deparse(args[[1L]], width.cutoff = 120L), collapse = " ")
    rows[[length(rows) + 1L]] <- data.frame(file = file, line = token$line1,
      constructor = extract(lines, head), test = title, formula = formula,
      data = arg("data"), family = arg("family"), method = arg("method"),
      cran_guard = grepl("skip_on_cran\\(|skip_if_not\\(full_test_run", test_code),
      code = gsub("\\s+", " ", code), stringsAsFactors = FALSE)
  }
}
result <- do.call(rbind, rows)
result <- unique(result)
output <- "notes/test-model-fits/inventory.md"
out <- file(output, "w")
writeLines(c("# Test model fit inventory", "",
  "This inventory reads every R file under tests without executing it. Entries are constructor call sites, not runtime fit counts: a call in a loop or helper can fit several models, while a cached call may not run at all.", "",
  "The audit decisions and changes are described in README.md. Dynamic calls and update calls are listed so they are not missed; some update calls change formulas rather than refitting models.", ""), out)
escape <- function(x) gsub("|", "\\|", gsub("`", "", x, fixed = TRUE), fixed = TRUE)
for (file in files) {
  z <- result[result$file == file, ]
  writeLines(c(paste0("## ", file), ""), out)
  if (!nrow(z)) {
    writeLines(c("No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.", ""), out)
    next
  }
  if (basename(file) == "test-hgam-paper.R") {
    disposition <- "Keep the paper workflow opt-in. Its model-fitting steps are part of the workflow being validated."
  } else if (basename(file) == "setup.R") {
    disposition <- "Shared setup or a shared builder. New specialized fits are requested on demand."
  } else if (grepl("fixtures/secondary-models", file, fixed = TRUE)) {
    disposition <- "Already shared on demand behind skip_on_cran(). Keep outside the CRAN workload."
  } else if (grepl("helper-lss-models", file, fixed = TRUE)) {
    disposition <- "Already cached by family. Keep the offsets, weights and parameter definitions needed by the independent references."
  } else if (grepl("integration/", file, fixed = TRUE)) {
    disposition <- "Keep in the separate optional integration workflow; these native families are not setup.R models."
  } else {
    disposition <- "Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute."
  }
  writeLines(c(disposition, "", "| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |",
    "| --- | --- | --- | --- | --- |"), out)
  for (j in seq_len(nrow(z))) {
    a <- z[j, ]
    writeLines(paste0("| ", a$line, " | `", escape(a$constructor), "` | ", escape(a$test),
      " | `", escape(a$formula), "` | `", escape(a$family), "` |"), out)
  }
  writeLines("", out)
}
close(out)
inventory <- readLines(output)
while (length(inventory) && !nzchar(tail(inventory, 1L))) {
  inventory <- head(inventory, -1L)
}
writeLines(inventory, output)
cat(length(files), "R files inspected;", nrow(result), "potential fitting call sites in",
  length(unique(result$file)), "files.\n")
