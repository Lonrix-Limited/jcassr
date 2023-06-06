
.get_data <- function(df) {

  result <- NULL
  df <- as.data.frame(df)
  strats <- .get_strategies(df)

  for (istrat in 1:nrow(strats)) {

    strat <- strats[[istrat, "code"]]
    tmp <- data.frame(period = df$period,
                      objective = df[, paste0(strat, "_obj")],
                      treatment = df[, paste0(strat, "_treat")],
                      cost = df[, paste0(strat, "_cost")])

    tmp$strategy <- strats[[istrat, "label"]]

    if (is.null(result)) {
      result <- tmp
    }
    else
    {
      result <- rbind(result, tmp)
    }
  }

  result$dummy_obj <- ifelse(result$treatment == "-", NA, result$objective)
  result$treatment <- ifelse(result$treatment == "-", NA, result$treatment)
  result$label <- ifelse(is.na(result$treatment), NA,
                         paste0(result$treatment,
                                " ($", round(result$cost/1000,2),"-k)"))
  return(result)
}

.get_strategies <- function(df) {

  result <- NULL
  df <- as.data.frame(df)
  stat_cols <- grep("_obj", names(df), value = TRUE)
  strat_codes <- c(NA, length(stat_cols))
  strat_lbls <- c(NA, length(stat_cols))

  i <- 1
  for (strat in stat_cols) {
    split_parts <- unlist(strsplit(strat, "_"))
    strat_codes[i] <- split_parts[1]
    i <- i + 1
  }

  i <- 1
  for (strat in strat_codes) {

    tmp <- data.frame(treatment = df[, paste0(strat, "_treat")])
    tt <- tmp %>% filter(!is.na(tmp$treatment) & tmp$treatment != "-")
    tt <- tt[, "treatment"]
    lbl <- paste(tt, collapse = " | ")
    if (strat == "dn") {
      strat_lbls[i] <- "Do-Minimum"
    }
    else {
      strat_lbls[i] <- lbl
    }

    i <- i + 1
  }

  result <- data.frame(code = strat_codes, label = strat_lbls)
  return(result)
}

#' Plot the Area-Under-the-Curve for various Strategies
#'
#' \code{jc_get_strategy_plot} Does a nice plot of the area under the curve
#' using data from a jcass Strategy Debug export file. The graph plots the
#' objective values for each strategy and shows when and what treatments are
#' involved.
#'
#' @param strat_detail_data data frame with data loaded from the 'detail' sheet
#' of the Strategy Debug export file
#' @return GGPLOT graph that can be further customised
#' @export
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 aes
#' @importFrom ggrepel geom_label_repel
#'
jc_get_strategy_plot <- function(strat_detail_data) {
  df <- .get_data(strat_detail_data)
  gr <- ggplot2::ggplot(df) +
    geom_point(aes(x = .data$period, y = .data$dummy_obj,
                   shape = .data$strategy,
                   fill = .data$strategy, color = .data$strategy), size = 4) +
    geom_area(aes(x = .data$period, y = .data$objective,
                  fill = .data$strategy),
              alpha = 0.2, position = "identity") +
    geom_label_repel(aes(x = .data$period, y = .data$dummy_obj,
                         label = .data$label),
                     box.padding   = 0.35, point.padding = 0.5,
                     segment.color = 'grey50') +
    theme_classic()
  return(gr)
}

#' Gets a table with strategy summary information for various Strategies
#'
#' \code{jc_get_strategy_table} Gets a nice table with summary information for
#' each treatment strategy using data from a jcass Strategy Debug export file.
#'
#' @param strat_summary_data data frame with data loaded from the 'summary'
#' sheet of the Strategy Debug export file
#' @param strat_details data frame with data loaded from the 'detail'
#' sheet of the Strategy Debug export file
#' @return Data Frame that can be further customised or used for graphs etc
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#'
jc_get_strategy_table <- function(strat_summary_data, strat_details) {

  dt <- as.data.frame(strat_summary_data)
  dt$strategy <- ifelse(dt$strategy == "Do-Nothing", "dn_", dt$strategy)
  # Remove the last character from values in column1 using substr()
  dt$strategy <- substr(dt$strategy, 1, nchar(dt$strategy) - 1)
  strats <- .get_strategies(strat_details)
  dt <- dt %>% left_join(strats, by = c("strategy" = "code"))
  dt <- dt %>% select(-.data$strategy)
  dt$strategy <- dt$label
  dt$label <- NULL
  dt <- dt %>% relocate(.data$strategy) # move the id column to the left
  names(dt) <- c("Strategy", "Total Cost", "Objective Total", "Benefit",
                 "BC-Ratio")
  return(dt)
}

#' Gets a print-ready Table with strategy summary information for various Strategies
#'
#' \code{jc_plot_strategy_table_gt} Gets a nice printable table with summary
#' information for each treatment strategy using data from a jcass Strategy Debug export file.
#'
#' @param header header for the table
#' @param strat_summary_data data frame with data loaded from the 'summary'
#' sheet of the Strategy Debug export file
#' @param strat_details data frame with data loaded from the 'detail'
#' sheet of the Strategy Debug export file
#' @return GT package table object
#' @export
#'
jc_plot_strategy_table_gt <- function(header, strat_summary_data,
                                      strat_details) {

  dt <- as.data.frame(strat_summary_data)
  dt <- jc_get_strategy_table(dt, strat_details)
  tb <- dt |>
    gt::gt() |>
    gt::tab_header(
      title = header,
    ) |>
    gt::fmt_currency(columns = 'Total Cost') |>
    gt::fmt_number(columns = 'Objective Total', suffixing = TRUE)
  return(tb)
}
