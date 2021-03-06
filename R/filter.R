#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are true. Unlike
#' base subsetting with `[`, rows where the condition evaluates to `NA` are
#' dropped.
#'
#' Note that dplyr is not yet smart enough to optimise filtering optimisation
#' on grouped datasets that don't need grouped calculations. For this reason,
#' filtering is often considerably faster on [ungroup()]ed data.
#'
#' @section Useful filter functions:
#'
#' * [`==`], [`>`], [`>=`] etc
#' * [`&`], [`|`], [`!`], [xor()]
#' * [is.na()]
#' * [between()], [near()]
#'
#' @section Grouped tibbles:
#'
#' Because filtering expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped filtering:
#'
#' ```
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' The former keeps rows with `mass` greater than the global average
#' whereas the latter keeps rows with `mass` greater than the gender
#' average.
#'
#' It is valid to use grouping variables in filter expressions.
#'
#' When applied on a grouped tibble, `filter()` automatically [rearranges][arrange]
#' the tibble by groups for performance reasons.
#'
#' @section Tidy data:
#' When applied to a data frame, row names are silently dropped. To preserve,
#' convert to an explicit variable with [tibble::rownames_to_column()].
#'
#' @section Scoped filtering:
#' The three [scoped] variants ([filter_all()], [filter_if()] and
#' [filter_at()]) make it easy to apply a filtering condition to a
#' selection of variables.
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for [tbl_df()], [dtplyr::tbl_dt()] and [dbplyr::tbl_dbi()].
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Logical predicates defined in
#'   terms of the variables in `.data`.
#'   Multiple conditions are combined with `&`. Only rows where the
#'   condition evaluates to `TRUE` are kept.
#' @param .preserve when `FALSE` (the default), the grouping structure
#'   is recalculated based on the resulting data, otherwise it is kept as is.
#' @return An object of the same class as `.data`.
#' @seealso [filter_all()], [filter_if()] and [filter_at()].
#' @export
#' @examples
#' filter(starwars, species == "Human")
#' filter(starwars, mass > 1000)
#'
#' # Multiple criteria
#' filter(starwars, hair_color == "none" & eye_color == "black")
#' filter(starwars, hair_color == "none" | eye_color == "black")
#'
#' # Multiple arguments are equivalent to and
#' filter(starwars, hair_color == "none", eye_color == "black")
#'
#'
#' # The filtering operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' #
#' # The following filters rows where `mass` is greater than the
#' # global average:
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#' # Whereas this keeps rows with `mass` greater than the gender
#' # average:
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' cond <- c(80, 150)
#' starwars %>%
#'   filter(
#'     .data[[vars[[1]]]] > cond[[1]],
#'     .data[[vars[[2]]]] > cond[[2]]
#'   )
#' # Learn more in ?dplyr_tidy_eval
filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}

#' @export
filter.tbl_df <- function(.data, ..., .preserve = FALSE) {
  dots <- enquos(...)
  if (is_empty(dots)) {
    return(.data)
  }
  named <- have_name(dots)
  if (any(named)) {
    for (i in which(named)) {
      quo <- dots[[i]]

      # only allow named logical vectors, anything else
      # is suspicious
      expr <- quo_get_expr(quo)
      if (!is.logical(expr)) {
        stop_filter_named(i, expr, names(dots)[i])
      }

    }
  }

  rows <- group_rows(.data)

  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)
  env_filter <- env()
  tryCatch({
    c(keep, new_rows_sizes, group_indices) %<-% mask$eval_all_filter(dots, env_filter)
  }, simpleError = function(e) {
    stop_filter_eval_tidy(e, env_filter$current_expression)
  })

  out <- vec_slice(.data, keep)

  # regroup
  if (is_grouped_df(.data)) {
    new_groups <- group_data(.data)
    new_groups$.rows <- filter_update_rows(nrow(.data), group_indices, keep, new_rows_sizes)
    attr(out, "groups") <- new_groups

    if (!.preserve) {
      out <- regroup(out)
    }
  }

  # copy back attributes
  # TODO: challenge that with some vctrs theory
  atts <- attributes(.data)
  atts <- atts[! names(atts) %in% c("names", "row.names", "groups", "class")]
  for(name in names(atts)) {
    attr(out, name) <- atts[[name]]
  }

  out
}

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(filter(tbl_df(.data), ..., .preserve = .preserve))
}

regroup <- function(data) {
  # only keep the non empty groups
  non_empty <- map_lgl(group_rows(data), function(.x) length(.x) > 0)
  gdata <- filter(group_data(data), non_empty)

  # then group the grouping data to get expansion if needed
  gdata <- grouped_df(gdata, head(names(gdata), -1L), isTRUE(attr(group_data(data), ".drop")))
  new_groups <- group_data(gdata)
  old_rows  <- gdata$.rows

  new_groups$.rows <- new_list_of(map(new_groups$.rows, function(.x) {
    if (length(.x) == 1L) {
      old_rows[[.x]]
    } else {
      integer()
    }
  }), ptype = integer())

  attr(data, "groups") <- new_groups
  data
}

filter_update_rows <- function(n_rows, group_indices, keep, new_rows_sizes) {
  .Call(`dplyr_filter_update_rows`, n_rows, group_indices, keep, new_rows_sizes)
}
