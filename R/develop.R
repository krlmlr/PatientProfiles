# cohortNameReference = "cohort_1"
# cohortNameComparator = c("cohort_2", "cohort_3")
#
# table <- timing1 |>
#   visOmopResults::splitAll() |>
#   dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
#   dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator)
#
# counts <- table |>
#   dplyr::filter(variable_name != "diff_days") |>
#   dplyr::mutate(variable_name = gsub(" ", "_", .data$variable_name)) |>
#   tidyr::pivot_wider(names_from = variable_name, values_from = estimate_value) |>
#   dplyr::select(dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference",
#                                 "cohort_name_comparator", "number_records", "number_subjects")))
#
# table |>
#   dplyr::filter(.data$variable_name == "diff_days") |>
#   dplyr::select(dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator",
#                                 "variable_name", "estimate_name", "estimate_value", "variable_name"))) |>
#   dplyr::mutate(
#     estimate_value = as.numeric(.data$estimate_value),
#     label = paste0(.data$cdm_name, "; ", .data$cohort_name_reference, "; ", .data$cohort_name_comparator),
#     quantile = as.numeric(gsub("q", "", .data$estimate_name))/100
#   ) |>
#   dplyr::left_join(
#     counts
#   ) |>
#   dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
#   dplyr::mutate(density = .data$quantile - dplyr::lag(.data$quantile),
#                 density = dplyr::if_else(is.na(.data$density),
#                                          dplyr::lead(.data$density), .data$density),
#                 density = )
#
#
#
# table <- lapply(split(table,seq(nrow(table))),
#        function(tib){
#          dplyr::tibble(
#            x = seq(from = -10000, to = 10000, length.out = 1000) + tib$mean,
#            y = dnorm(seq(tib$min-10000, tib$max+10000, length.out = 1000), tib$mean, tib$sd)
#          ) |>
#            dplyr::cross_join(tib)
#        }) |>
#   dplyr::bind_rows()
#
# ggData <- dplyr::tibble(
#   cohort_name_reference = cohortNameReference,
#   cohort_name_comparator = cohortNameComparator
# ) |>
#   dplyr::left_join(
#     table,
#     by = c("cohort_name_reference", "cohort_name_comparator")
#   ) |>
#   dplyr::mutate(y_pos = max(y)) |>
#   dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
#   dplyr::mutate(
#     y_pos = .data$y_pos*dplyr::cur_group_id(),
#     y = .data$y + .data$y_pos
#   ) |>
#   dplyr::ungroup()
#
#
# median_data <- ggData |>
#   dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
#   dplyr::mutate(y_points = abs(.data$median - .data$x)) |>
#   dplyr::arrange(.data$y_points) |>
#   dplyr::mutate(id = dplyr::row_number()) |>
#   dplyr::filter(id %in% 1:2)
#   dplyr::filter((.data$median - .data$x) == min(.data$median - .data$x))
#
#
#
# ggData |>
#   ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y, fill = label)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_polygon(alpha = 0.5) +
#   ggplot2::geom_hline(yintercept = unique(ggData$y_pos)) +
#   ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
#   ggplot2::geom_line(ggplot2::aes(x = .data$median, y = .data$y),
#                      data =)
test_that("tableCohortTiming", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=2000),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=30),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5),
                   sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5)),
    cohort_start_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                  rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                  rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5))),
    cohort_end_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5)))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:20,
    person_id = 1:20,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date =  as.Date("2025-01-01"),
    period_type_concept_id = NA
  )

  cdm <- mockPatientProfiles(person = person, observation_period = obs, table = table)

  timing1 <- summariseCohortTiming(cdm$table,
                                   restrictToFirstEntry = TRUE)
  tibble1 <- tableCohortTiming(timing1, type = "tibble")
  expect_true(all(c("Cdm name", "Cohort name reference", "Cohort name comparator", "Variable name", "Estimate name", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(all(unique(tibble1$`Cohort name comparator`) %in%
                    unique(tibble1$`Cohort name reference`)))


  tibble2 <- tableCohortTiming(timing1, type = "tibble", cohortNameReference = "cohort_1")
  expect_true("cohort_1" == unique(tibble2$`Cohort name reference`))

  gt1 <- tableCohortTiming(timing1, type = "gt", cohortNameReference = "cohort_1")
  expect_true("gt_tbl" %in% class(gt1))



  CDMConnector::cdm_disconnect(cdm)

})



#' Format a cohort_timing object into a visual table.
#'
#' @param result A cohort_overlap object.
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param cdmName Name of the databases to include.
#' @param variableName Name of the variable names to include.
#' @param formatEstimateName Whether to include the number of subjects.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", or "tibble".
#' @param minCellCount Counts below which results will be clouded.
#' @param .options named list with additional formatting options.
#' PatientProfiles::optionsTableCohortOverlap() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' cdm_local <- omock::mockCdmReference() |>
#'   omock::mockPerson(100) |>
#'   omock::mockObservationPeriod() |>
#'   omock::mockCohort(numberCohorts = 2)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' cdm <- CDMConnector::copy_cdm_to(con = con,
#'                                  cdm = cdm_local,
#'                                  schema = "main"
#' cdm$cohort |>
#'   summariseCohortOverlap() |>
#'   tableCohortOverlap()
#' }
#'
#' @return A formatted table of the cohort_timing summarised object.
#'
#' @export
#'
tableCohortTiming <- function(result,
                              cohortNameReference = NULL,
                              cohortNameComparator = NULL,
                              cdmName = NULL,
                              variableName = c("number records",
                                               "number subjects",
                                               "diff_days"),
                              formatEstimateName = c(
                                "N" = "<count>",
                                "Mean (SD)" = "<mean> (<sd>)",
                                "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                "Range" = "<min> - <max>"
                              ),
                              type = "gt",
                              minCellCount = 5,
                              .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(type, c("gt", "flextable", "tibble"))
  checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertCharacter(variableName, null.ok = TRUE)
  checkmate::assertCharacter(formatEstimateName, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE)
  checkmate::assertList(.options)

  # split table
  x <- result |>
    visOmopResults::splitAll()

  # add default values
  cohortNameReference <- defaultColumnSelector(
    cohortNameReference,
    x$cohort_name_reference,
    "cohort_name_reference"
  )
  cohortNameComparator <- defaultColumnSelector(
    cohortNameComparator,
    x$cohort_name_comparator,
    "cohort_name_comparator"
  )
  variableName <- defaultColumnSelector(
    variableName,
    x$variable_name,
    "variable_name"
  )
  cdmName <- defaultColumnSelector(cdmName, x$cdm_name, "cdm_name")
  .options <- defaultTimingOptions(.options)

  x <- result |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$variable_name %in% .env$variableName) |>
    omopgenerics::suppress(minCellCount = minCellCount) |>
    visOmopResults::formatEstimateValue(
      decimals = .options$decimals,
      decimalMark = .options$decimalMark,
      bigMark = .options$bigMark
    ) |>
    visOmopResults::formatEstimateName(
      estimateNameFormat = formatEstimateName,
      keepNotFormatted = .options$keepNotFormatted,
      useFormatOrder = .options$useFormatOrder
    ) |>
    visOmopResults::splitAll() |> # to change after new release of visOmopGenerics
    dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    dplyr::select(!c("result_id", "result_type", "package_name", "package_version",
                     "estimate_type", "variable_level"))

  if (type == "gt") {
    x <- x |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
      visOmopResults::gtTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  } else if (type == "flextable") {
    x <- x |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
      visOmopResults::fxTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  } else if (type == "tibble") {
    x <- x |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x)))
  }
  return(x)
}


defaultTimingOptions <- function(userOptions) {
  defaultOpts <- list(
    decimals = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    decimalMark = ".",
    bigMark = ",",
    keepNotFormatted = TRUE,
    useFormatOrder = TRUE,
    style = "default",
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameCol = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}


