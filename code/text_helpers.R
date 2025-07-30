#####
###
#     text_helpers.R
#
#       Support tidytext functions.
###
#####

##
#  para2token()
#    input:  a text file whose "lines" are actually paragraphs
#    return: list of tibbles (paragraphs, sentences, words)
##
para2token <- function(
    file,                   # passed to readr::read_lines()
    skip_empty_rows = TRUE, # <lgl> ignore blank rows
    remove_stopword = TRUE, # <lgl> remove stop-words
    strip_punct     = TRUE  # <lgl> remove punctuation
) {

  # vector of input paragraphs
  input_vec <- readr::read_lines(
    file = file,
    skip_empty_rows = skip_empty_rows
  )

  # index input paragraphs in a tibble
  input_tbl <- tibble::tibble(
    pdx  = 1:length(input_vec),
    para = input_vec
  )

  # break each paragraph into sentences
  sentence_tbl <- input_tbl |>
    tidytext::unnest_sentences(
      output      = sntc,
      input       = para,
      strip_punct = strip_punct,
      format      = "text"
    ) |>
    dplyr::mutate(sdx = 1:length(sntc)) |>
    dplyr::select(pdx, sdx, sntc)

  # break each sentence into tokens
  token_tbl <- sentence_tbl |>
    tidytext::unnest_tokens(
      output = word,
      input  = sntc,
      format = "text"
    ) |>
    dplyr::mutate(wdx = 1:length(word)) |>
    dplyr::select(pdx, sdx, wdx, word)

  # remove stop-words
  if (remove_stopword) {
    token_tbl <- token_tbl |>
      dplyr::anti_join(
        tidytext::get_stopwords()
      )
  }

  # construct cleaned paragraphs from token_tbl
  para_tbl <- token_tbl |>
    dplyr::group_by(pdx) |>
    dplyr::summarise(
      para = stringr::str_flatten(word, collapse = " "))


  return(list(
    input_tbl    = input_tbl,
    sentence_tbl = sentence_tbl,
    token_tbl    = token_tbl,
    para_tbl     = para_tbl
  ))
}


##
#  EOF
##
