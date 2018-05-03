
REGEX_GENERAL_COMMENTS <- "overall|general"
GRADE_CUTS <- c(-1,0,15,25,38,42,45,48,52,55,58,62,65,68,77,88,100)
GRADE_LABS <- c("N", "F-", "F", "F+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+")


# Read html text, export to html via pandoc, then return the string
html2md <- function(html) {
  # html <- "<h1>basl</h1>"
  readr::read_file(pander::Pandoc.convert(text=html,
                                   format = "markdown", open=F,
                                   options = "-f html  --atx-headers"))
}

yaml_load_utf8 <- function(string, ...) {
  string <- paste(string, collapse = '\n')
  if (packageVersion('yaml') >= '2.1.14') {
    yaml::yaml.load(string, ...)
  } else {
    mark_utf8(yaml::yaml.load(enc2utf8(string), ...))
  }
}

yaml_load_file_utf8 <- function(input, ...) {
  yaml_load_utf8(readLines(input, encoding = 'UTF-8'), ...)
}

trim_trailing_ws <- function(x) {
  sub("\\s+$", "", x)
}
yaml_front_matter <- function(input,
                              encoding = getOption("encoding")) {
  # read the input file
  input_lines <- read_lines_utf8(input, encoding)

  # parse the yaml front matter
  parse_yaml_front_matter(input_lines)
}


# Parse an RMarkdown file to extract assignment settings
#' @export
parse_yaml_front_matter <- function(input_lines) {
  partitions <- partition_yaml_front_matter(input_lines)
  if (!is.null(partitions$front_matter)) {
    front_matter <- partitions$front_matter
    if (length(front_matter) > 2) {
      front_matter <- front_matter[2:(length(front_matter) - 1)]
      front_matter <- paste(front_matter, collapse = "\n")
      validate_front_matter(front_matter)
      parsed_yaml <- yaml_load_utf8(front_matter)
      if (is.list(parsed_yaml))
        parsed_yaml
      else
        list()
    }
    else
      list()
  }
  else
    list()
}


validate_front_matter <- function(front_matter) {
  front_matter <- trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter))
    stop("Invalid YAML front matter (ends with ':')", call. = FALSE)
}


partition_yaml_front_matter <- function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1] - 1])
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1] - 1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}


##################################
##################################
##################################
# Functions to process the data
##################################
##################################
##################################


# A student is an H1 block, as defined in the markdown
# This processes all feedback and marks within the block
# and returns a list of two dataframes: comments and marks
#' @import dplyr
#' @import magrittr
#' @import stringr
proc.student <- function(student, components, weighting){
  # components <- config$components
  # weighting <- config$weighting
  # student <- all.students[[5]]
  student.name <- rvest::html_text(rvest::html_node(student, 'h1'))
  student.id <- rvest::html_attr(student, 'id')
  participant <- ifelse(str_detect(student.id, "participant"), str_extract(student.id, '\\d{8,12}+'), NA)
  sections <- rvest::html_nodes(student, '.section')
  section.titles <- rvest::html_text(rvest::html_nodes(sections, 'h2'))
  section.contents <-
    purrr::map(sections, ~ as.character(rvest::html_nodes(.x, ':not(h2)')))

  attr.grades <- dplyr::bind_rows(purrr::map2(sections, 1:length(sections),
                            ~rvest::html_attr(.x, 'grade') %>%
                              as_data_frame() %>% mutate(section=.y)))

  extra.grades <- dplyr::bind_rows(
    purrr::map2(sections, 1:length(sections),
                ~stringr::str_match(rvest::html_text(rvest::html_nodes(.x, 'p')), "(grade|mark)=(\\d+)") %>%
                  as_data_frame() %>% mutate(section=.y))
    ) %>% dplyr::mutate(value=V3)

  grades <- dplyr::bind_rows(extra.grades, attr.grades) %>%
    select(section, value) %>%
    arrange(section) %>%
    filter(!is.na(value)) %>%
    rename(grade.raw=value)

  # Add in the components label
  grades <- grades %>%
    mutate(component = components)

  # Don't want to see the inevitable warning when coercing
  suppressWarnings({grades <- grades %>% mutate(grade.numeric = as.numeric(grade.raw))})

  # check we only have one grade per student
  tryCatch({
    grades %>% group_by(section) %>%
      summarise(n=n()) %>% pull(n) %>%
      assertive::assert_all_are_equal_to(1)
    },
    error = function(e) simpleError(paste("Duplicate grades found for student", student.id,
                                          pander::pandoc.table.return(grades, split.tables = Inf)))
  )

  normalised.weights <- weighting/sum(weighting)
  grade.with.total <-
    grades %>%
    mutate(weighted.grade = grade.numeric * normalised.weights) %>%
    dplyr::bind_rows(., summarise(., component="Total", grade.numeric = sum(weighted.grade)))

  grades.clean <-
    grade.with.total %>%
    mutate(grade.letter = cut(grade.numeric,
                              breaks = GRADE_CUTS,
                              labels=GRADE_LABS)) %>%
    # add identifiers back in
    dplyr::mutate(id = student.id, name = student.name, participant = participant)


  feedback.df <- data_frame(section = section.titles,
                            comment = section.contents) %>%
    # pull general comments to the top of the page
    mutate(i = row_number()) %>%
    mutate(showfirst = str_detect(tolower(section), REGEX_GENERAL_COMMENTS)) %>%
    arrange(-showfirst, i) %>% select(-i, -showfirst) %>%
    rowwise() %>%
    mutate(comment.md = html2md(comment)) %>%
    mutate(id = student.id, name = student.name)

  return(list(comments=feedback.df, grades=grades.clean))
}


# Save csv of grades from multiple students (resulting from process_assignment)
#' @import magrittr
save.grades <- function(grades, output_folder='.'){
  # grades <- bind_rows(map(res, function(x) x$grades))
  # process marks and save them
  grades %>%
    dplyr::filter(component=="Total") %>%
    dplyr::select(id, name, participant, grade.numeric) %>%
    readr::write_csv(paste0(output_folder, '/aggregate.marks.csv'))

  grades %>%
    data.table::dcast(id+participant ~ component, value.var="grade.numeric") %>%
    readr::write_csv(paste0(output_folder, '/raw.marks.csv'))
}


# Accepts a processed student <- the result of the proc.student function
#' @import magrittr
make.feedbac.doc <- function(student.res, assignment, output_folder, clean=T, quiet=T){
  # student.res <- res[[2]]
  # assignment <- "test"
  student.id <- student.res$comments$id %>%
    dplyr::first()
  student.name <- student.res$comments$name %>%
    dplyr::first()

  grades <- student.res$grades %>% ungroup() %>% select(component, grade.numeric, grade.letter)

  comments <- student.res$comments %>%
    mutate(section = pander::pandoc.header.return(section, 2)) %>%
    transmute(x = paste(section, comment.md, sep="\n\n")) %>%
    pull(x) %>% paste(., collapse="\n")

  output <- paste(c(
    sprintf('---
title:  "Feedback: %s"
subtitle: %s (%s)
output: pdf_document:
  keep_tex: false
---\n\n', student.name, assignment, student.id),
    pander::pandoc.p.return(""),
    comments,
    pander::pandoc.header.return("Grades", 2),
    pander::pandoc.table.return(grades)), collapse="\n\n")

  # sanitze output filename, save and cleanup
  outpath <- gsub('[^a-zA-Z1-9_-]', '', student.id)
  p <- paste0(output_folder, "/", outpath, '.Rmd')

  readr::write_file(output, p)
  rmarkdown::render(p, clean=clean, quiet = quiet)
  file.remove(p)
}


pre.process.marks <- function(markdown){
  marks.xhtml <- pander::Pandoc.convert(f=markdown, format = "xhtml", open=F,
                                        options="--section-divs")
  marks.xhtml
}



#' Process Markdown files containing student grades and feedback.
#' @export
#' @import magrittr
#' @param markdown Path to an Rmarkdown document
#' @param save_grades Save csv grades to output_folder
#' @param save_feedback Save pdf feedback documents to output folder
#' @param output_folder Folder to save output to, path relative to working directory.
marksback <- function(markdown){
  # markdown <- "test.Rmd"
  # getwd()
  # setwd('~/dev/markback/R')
  assertive::is_readable_file(markdown)

  # read and check the yaml config
  config <- parse_yaml_front_matter(readr::read_lines(markdown))

  if(is.null(config$weighting)){
    config$weighting <- rep(1, length(config$components))
  }

  testthat::expect_false(any(is.na(config$components)))
  testthat::expect_true(is.numeric(config$weighting))
  testthat::expect_equal(length(config$weighting), length(config$components))

  output_folder  <- ifelse(is.null(config$output_folder), "processed", config$output_folder)
  dir.create(file.path(".", output_folder), showWarnings = FALSE)

  save_grades  <- ifelse(is.null(config$save_grades), T, config$save_grades)
  save_feedback  <- ifelse(is.null(config$save_feedback), T, config$save_feedback)



  # pre-process with pandoc into xhtml and then read
  marks.xhtml <- pre.process.marks(markdown)
  doc <- xml2::read_html(marks.xhtml)

  # Make all H1's students. Look for the parent div though because of preproc step
  all.students <- xml2::xml_parent(rvest::html_nodes(doc,  'h1'))

  # proc.student(all.students[[3]], config$components, config$weighting)
  results <- purrr::map(all.students, function(x) proc.student(x, config$components, config$weighting))

  # cleanup xml intermediate step
  if (file.exists(marks.xhtml)) file.remove(marks.xhtml)

  # pull all grades together as a convenience
  grades <- dplyr::bind_rows(purrr::map(results, ~.x$grades))

  return(list(grades=grades, results=results, config=config))
}



#' Save csv grades
#' @export
savegrades <- function(results, output_folder){
  all.grades <- dplyr::bind_rows(purrr::map(results$results, function(x) x$grades))
  try(save.grades(all.grades, output_folder))
  message("Marks saved.")
}

#' Save pdf feedback
#' @export
savefeedback <- function(results, assignment, output_folder){
  subpths <- paste0(output_folder, "/", results$grades$id %>% unique)
  dir.create(file.path(".", output_folder), showWarnings = F)
  purrr::map(subpths, ~ dir.create(file.path(".", .x), showWarnings = F))
  purrr::map2(res$results, subpths, ~ markback::make.feedbac.doc(.x, output_folder = .y, assignment = assignment))
  system(sprintf("find %s -type f -name '*.tex' -delete"), output_folder)
  message("Feedback saved.")
}


#' Match marks to a csv exported by Moodle
#' @export
#' @param results The result of a marksback call
#' @param markbook The path to a csv exported by moodle
#' @param save.markbook Save the results to a new csv ready for re-upload to the DLE with "completed_" as a prefix
#' @param plot.results Create a density plot showing the mark distribution
#' @return A dataframe containing the contents of matched markbook.
matchmarks <- function(results, markbook, save.markbook=T, plot.results=T){
  markbook <-   'Grades-PSY558 - 17SPM-Assignment part 1-459880.csv'
  mbook <- suppressMessages(readr::read_csv(markbook))

  grds <- results$grades %>%
    ungroup() %>%
    filter(component=="Total") %>%
    mutate(Identifier = paste("Participant", participant)) %>%
    select(Identifier, grade.letter) %>%
    rename(Grade = grade.letter)

  mbook.marked <-
    left_join(mbook %>% select(-Grade), grds, by="Identifier") %>%
    # put cols back in original order
    select(names(mbook))

  r <- list(markbook = mbook.marked)

  if(save.markbook){
    newfile <- paste0("completed_", markbook)
    mbook.marked %>%
      readr::write_csv(newfile)
      message(paste("Saved", newfile))
  }

  if(plot.results){
    r$plt <- results$grades %>%
      mutate(t = ifelse(component=="Total", "Total", "Components")) %>%
      ggplot(aes(grade.numeric, color=t)) + geom_density() +
      geom_vline(xintercept = markback:::GRADE_CUTS, linetype="dotted", alpha=.3) +
      xlab('Numeric grade (%)') + ylab("Density") + scale_color_discrete("")
  }

  r
}


# res <- marksback('test.Rmd')
# res$config
# savemarks(res)
