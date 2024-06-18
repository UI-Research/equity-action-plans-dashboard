#' Get filtered layout from CSV files
#'
#' This function reads layout data from a directory of CSV files
#' and filters out specified sections.
#'
#' @param doc_type A character string specifying the type of document to read.
#' @param sections_to_filter A character vector specifying the sections to filter out.
#' Default is c("Figure", "Title", "Section header", "Footer").
#'
#' @return A data frame containing the layout data without the filtered sections.
#' @examples

#' Search for keywords using a dictionary
#'
#' Read relevant sections from documents, convert text to a corpus, and construct
#' tokens object using sentences as the unit of analysis.
#' Then, use dictionary to search for keywords using `quanteda::kwic()`. Finally,
#' join back results with layout data
#' @param tokens
#' @param dictionary
#'
#' @return A dataframe containing sentences matching keywords in dictionary
#' as well as layout information
#' @examples
search_keywords <- function(tokens, dictionary, concept = NULL, tokens_type = "word4", window = 100) {
  search_results <-
    kwic(toks, dictionary, window = window) %>%
    as_tibble() %>%
    select(-c(from, to)) %>%
    relocate(pattern, .after = "docname") %>%
    # Join with "tidy" dictionary to append search terms to results
    left_join(make_dict_tidy(dictionary),
      by = c("pattern")
    )

  return(search_results)
}

#' Perform windowed keyword searches (WIP)
#'
#' This function reads a CSV file containing anchor terms, tokenizes the corpus,
#' and then keeps only the tokens that match the anchor terms and their surrounding context.
#'
#' @param corpus (character) The text you want to analyze.
#' @param path (character) The path to the CSV file containing the anchor terms. Default is "data/data-raw/search_terms.csv".
#' @param window (integer) The number of tokens surrounding the anchor terms you want to keep. Default is 10.
#' @return A tokens object containing only the tokens that match the anchor terms and their surrounding context.
#' @examples
#' windowed_search(my_corpus, "data/data-raw/search_terms.csv", 10)
#' @export
windowed_search <- function(corpus, path = "data/data-raw/search_terms.csv", window = 10) {
  # Read in the CSV file containing anchor terms
  terms <- read_csv(here::here(path)) %>%
    rename_with(~ janitor::make_clean_names(.x), everything()) %>%
    pivot_longer(matches("anchor_term"),
      names_to = "anchor_term_number",
      names_prefix = "anchor_term_",
      values_to = "anchor_term"
    ) %>%
    drop_na(anchor_term)

  # Tokenize the corpus
  toks_corpus <- tokens(corpus, remove_punct = TRUE)

  # Keep only the tokens that match the anchor terms and their surrounding context
  toks_windowed <- tokens_keep(toks_corpus, pattern = phrase(terms$anchor_term), window = window)

  return(toks_windowed)
}

#' Get search terms from a CSV file
#'
#' This function reads a CSV file containing search terms and returns a dataframe
#' with a row for each concept and pattern, and a column for the search terms.
#' @param path (character) A string specifying the path to the CSV file.
#' Default is "data/data-raw/search_terms.csv".
#' @param concept An optional character string specifying the concept to filter by.
#' @return A dataframe containing the search terms for each concept and pattern.
#' @examples
#' get_terms("Underserved communities")
get_terms <- function(path = "data/data-raw/search_terms.csv", concept = NULL) {
  terms <-
    read_csv(here::here(path)) %>%
    rename_with(~ janitor::make_clean_names(.x), everything()) %>%
    pivot_longer(matches("search_term"),
      names_to = "search_term_number",
      names_prefix = "search_term_",
      values_to = "search_term"
    ) %>%
    drop_na(search_term) %>%
    summarise(
      search_terms = str_flatten(search_term, collapse = ", ") %>%
        str_trim() |> str_split(", "),
      .by = c("pattern", "concept")
    )

  if (!is.null(concept)) {
    terms <- filter(terms, concept == {{ concept }})
  }

  return(terms)
}

#' Create a dictionary from a dataframe
#'
#' This function takes a dataframe with search terms and patterns and creates a dictionary object.
#' The dictionary keys are the patterns and the values are the search terms.
#' @param data A dataframe containing the search terms and patterns.
#' @return A dictionary object.
#' @examples make_dict(terms)
make_dict <- function(data) {
  terms_pattern <- set_names(data$search_terms, data$pattern)
  # Create dictionary
  quanteda::dictionary(as.list(terms_pattern))
}

#' Convert a quanteda dictionary to a tidy format
#'
#' This function takes a dictionary object and converts it into a tidy dataframe.
#' The dataframe has a row for each category (pattern) and a column for the search terms.
#' @param dictionary A dictionary object to convert.
#' @return A dataframe with columns 'pattern' and 'search_terms', where each row represents a category from the dictionary.
make_dict_tidy <- function(dictionary) {
  dict_tidy <-
    tidy(dictionary) %>%
    mutate(
      search_terms = str_flatten(string = word, collapse = ", "),
      .by = "category"
    ) %>%
    select(pattern = category, search_terms) |>
    distinct(pattern, .keep_all = TRUE)

  return(dict_tidy)
}



tidy_kwic <- function(txt) {
  as_tibble(txt) |>
    separate(docname, into = c("docname", "sentence_number"), sep = "\\.") |>
    separate(docname, into = c("agency", "plan"), extra = "merge") |>
    select(-c(from, to)) |>
    relocate(pattern, .after = "plan")
}

count_matches <- function(txt, ...) {
  txt |>
    group_by(..., pattern, .drop = FALSE) |>
    count() |>
    ungroup() |>
    complete(..., pattern, fill = list(n = 0)) |>
    pivot_wider(names_from = pattern, values_from = n)
}

make_gt_table <- function(data, title = "") {
  data %>%
    gt() %>%
    cols_label_with(
      fn = ~ janitor::make_clean_names(., case = "title")
    ) |>
    opt_interactive(use_pagination = TRUE, use_search = TRUE, use_filters = TRUE, use_highlight = TRUE, use_text_wrapping = TRUE, page_size_default = 5, page_size_values = c(5, 10, 25)) %>%
    tab_header(title = title) %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts()
      )
    ) %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    tab_options(
      column_labels.border.top.width = px(5),
      column_labels.border.top.color = "#FFFFFF",
      table.border.top.color = "#FFFFFF",
      table.border.bottom.color = "#FFFFFF",
      heading.background.color = "#1696d2",
      data_row.padding = px(5),
      source_notes.font.size = 16,
      heading.align = "center",
      row_group.background.color = "#D0D3D4"
    )
}

my_theme <- function(data) {
  data %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts()
      )
    ) %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    # Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    opt_interactive(use_pagination = TRUE, use_search = TRUE, use_filters = TRUE, use_highlight = TRUE, use_text_wrapping = TRUE, page_size_default = 5, page_size_values = c(5, 10, 25)) %>%
    tab_options(
      column_labels.border.top.width = px(5),
      column_labels.border.top.color = "#FFFFFF",
      table.border.top.color = "#FFFFFF",
      table.border.bottom.color = "#FFFFFF",
      heading.background.color = "#1696d2",
      data_row.padding = px(5),
      source_notes.font.size = 16,
      heading.align = "center",
      row_group.background.color = "#D0D3D4"
    )
}


check <- function(plan) {
  if (plan == TRUE) {
    logo_out <- fontawesome::fa("check", fill = "#55b748")
  } else if (plan == FALSE) {
    logo_out <- fontawesome::fa("times", fill = "#db2b27")
  }

  logo_out %>%
    as.character() %>%
    gt::html()
}

library(knitr)

# useful function for options
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

# super-customised table printing ----
knit_print.data.frame <- function(x, options, ...) {
  # get options
  digits <- options$digits %||% getOption("digits")
  rownames <- options$rownames %||% FALSE
  pageLength <- options$pageLength %||% 10
  escape <- options$escape %||% TRUE
  caption <- options$table.cap

  # use DT for longer tables in html
  if (nrow(x) > pageLength & knitr::is_html_output()) {
    numeric_cols <- sapply(x, is.numeric) |>
      which() |>
      names()
    dt <- DT::datatable(x,
      rownames = rownames,
      caption = caption,
      escape = escape,
      width = "100%",
      height = "auto",
      options = list(pageLength = pageLength),
      selection = "none"
    )
    if (length(numeric_cols) > 0) {
      dt <- DT::formatRound(dt,
        columns = numeric_cols,
        digits = digits
      )
    }
    knitr::knit_print(dt, options)
  } else {
    # use kableExtra::kable for PDFs or shorter tables
    k <- kableExtra::kable(x,
      digits = digits,
      row.names = rownames,
      caption = caption,
      escape = escape
    ) |>
      kableExtra::kable_styling(
        full_width = options$full_width,
        bootstrap_options = c("striped", "hover", "condensed")
      )

    if (knitr::is_html_output()) {
      k <- c("<div class=\"kable-table\">", k, "</div>") |>
        paste(collapse = "\n")
    }

    knitr::asis_output(k)
  }
}
registerS3method("knit_print", "data.frame", knit_print.data.frame)
