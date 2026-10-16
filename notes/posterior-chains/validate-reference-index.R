# Run from the repository root after installing this branch and setting R_LIBS.
args <- commandArgs(TRUE)
root <- normalizePath('.')
destination <- if (length(args)) args[1L] else tempfile('gratia-reference-')
pkg <- pkgdown::as_pkgdown(root, override = list(destination = destination))
pkgdown::check_pkgdown(pkg)
pkgdown::build_reference(pkg, examples = FALSE, devel = FALSE, lazy = FALSE)
pkgdown::build_reference_index(pkg)
pkgdown::build_article('sampling-migration', pkg, new_process = FALSE)
reference <- file.path(pkg$dst_path, 'reference')
doc <- xml2::read_html(file.path(reference, 'index.html'))
a <- xml2::xml_find_all(doc, '//main//dl/dt//a')
labels <- trimws(xml2::xml_text(a))
stopifnot(!anyDuplicated(labels))
targets <- unique(xml2::xml_attr(a, 'href'))
stopifnot(all(file.exists(file.path(reference, targets))))
public <- pkg$topics$file_out[!pkg$topics$internal]
stopifnot(all(public %in% targets))
required <- c('posterior_samples', 'fitted_samples', 'predicted_samples',
  'smooth_samples', 'derivative_samples', 'as_mcmc_draws', 'sampling_diagnostics',
  'post_draws', 'mh_draws', 'gaussian_draws', 'user_draws', 'add_fitted_samples')
stopifnot(all(paste0(required, '.html') %in% targets))
# Every public sampling alias must map to the page included in the index.
for (alias in c('generate_draws', 'add_posterior_samples', 'add_predicted_samples', 'add_smooth_samples')) {
  i <- which(vapply(pkg$topics$alias, function(x) alias %in% x, logical(1)))
  stopifnot(length(i) == 1L, pkg$topics$file_out[i] %in% targets)
}
# Check local reference links in descriptions as well as listed topics.
links <- unique(xml2::xml_attr(xml2::xml_find_all(doc, '//main//a'), 'href'))
links <- sub('[#?].*$', '', links)
links <- links[nzchar(links) & !grepl('^([[:alnum:]+.-]+:|/)', links)]
local_reference <- links[!grepl('^../articles/', links)]
stopifnot(all(file.exists(file.path(reference, local_reference))))
migration <- file.path(pkg$dst_path, 'articles', 'sampling-migration.html')
stopifnot(file.exists(migration))
text <- xml2::xml_text(xml2::xml_find_first(xml2::read_html(migration), '//main'))
stopifnot(grepl('Location-scale-shape models', text, fixed = TRUE))
text <- gsub('[[:space:]]+', ' ', xml2::xml_text(xml2::read_html(file.path(reference, 'post_draws.html'))))
stopifnot(grepl('outside the fitted coefficient vector', text, fixed = TRUE),
  grepl('"family"', text, fixed = TRUE))
cat(sprintf('VALIDATED: %d public topics, %d index labels; all topic and local reference links resolve.\n',
  length(public), length(labels)))
cat('Sampling aliases resolve to their canonical indexed pages; migration article built.\n')
