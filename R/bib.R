#' Create crossref.bib
#'
#' Uses package rcrossref to look up DOIs in dois.txt and create crossref.bib.
write_crossref <- function() {
  for (pkg in c("rcrossref", "bib2df")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("write_crossref() requires package '%s'", pkg))
    }
  }
  # Fetch from crossref
  dois <- readLines("analysis/paper/dois.txt")
  refs <- rcrossref::cr_cn(dois = dois, format = "bibtex")
  bib_path <- "analysis/paper/crossref.bib"
  if (file.exists(bib_path)) {
    message("Overwriting crossref.bib")
    file.remove(bib_path)
  }
  file.create(bib_path)
  for (ref in refs) {
    write(paste0(ref, "\n"), file = bib_path, append = TRUE)
  }

  # Fix entry names
  get_first_author <- function(author_list) {
    author_list[1] %>% stringr::str_match(".* ([^ ]+)$") %>% `[`(1, 2)
  }
  append_suffix <- function(keys) {
    if (length(keys) > 1) {
      paste0(keys, letters[seq(length(keys))])
    } else {
      keys
    }
  }
  bibdf <- bib2df::bib2df(bib_path) %>%
    dplyr::mutate(BIBTEXKEY = paste(purrr::map_chr(AUTHOR, get_first_author),
                                    YEAR,
                                    sep = "-")) %>%
    dplyr::group_by(BIBTEXKEY) %>%
    dplyr::mutate(BIBTEXKEY = append_suffix(BIBTEXKEY)) %>%
    dplyr::ungroup()
  bib2df::df2bib(bibdf, bib_path)
}


