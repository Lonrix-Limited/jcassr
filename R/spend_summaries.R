
#' Gets a table summarising spending across several budget codes.
#'
#' \code{jc_get_spend_info_multi_budget} Gets a table that summarises spending across
#' different budget codes. The result contains combined data for all budget
#' codes, and for each budget code provides summary data relating to the
#' following parameters:
#' \enumerate{
#'   \item 'project_count' Report-friendly title for the axis
#'   \item 'min' minimum for the axis scale
#'   \item 'max' maximum for the axis scale
#'   \item 'increm' Increment for the axis scale - this will also determine
#'   the bin size if this is a histogram
#' }
#'
#' This method is similar to \code{jc_get_spend_param_multi_budget}. However,
#' this method returns the summaries for all spending parameters
#' whereas method \code{jc_get_spend_param_multi_budget} returns the data only for one
#' specific spending parameter. See \code{link{jc_get_spend_param_multi_budget}}
#'
#' @param budget_codes vector of budget codes, e.g. 'upper', 'lower', 'base' etc.
#' It is assumed that there will be output files matching these codes and also
#' that the budgets file will have matching sheets containing the budgets for
#' each of these codes.
#' @param budgets_file Relative path to the Excel file containing the budgets.
#' @param network_length Network length from which the percentage of length
#' treatment etc. can be calculated
#' @param max_calendar_period maximum treatment period (in calendar terms) to 
#' consider.
#' @param treats_file_base Relative path to the Excel file containing the
#' treatments exported from jcass. This will not be the full file name, but only
#' the stub from which the full name can be built by appending the budget code
#' at the end.
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#'
jc_get_spend_info_multi_budget <- function(budget_codes, budgets_file,
                                      network_length, max_calendar_period,
                                      treats_file_base = "outputs/treatments_") {

  result <- NULL
  for (budget_code in budget_codes) {

    # Now read the list of treatments triggered/selected by the model
    treats_data <- .jc_get_treatments(budget_code, treats_file_base)
    treats_data <- treats_data %>% 
      filter(.data$period_calendar <= max_calendar_period)

    budgets <- read_xlsx(budgets_file, budget_code,
                         .name_repair = "unique_quiet")
    spend <- jc_get_spending_info(treats_data, budgets, network_length)

    df <- spend %>% dplyr::group_by(.data$treat_category) %>% summarise(
      project_count = sum(.data$number),
      avg_spend = mean(.data$total_cost)/1e6,
      total_spend = sum(.data$total_cost)/1e6,
      avg_perc = mean(.data$total_perc),
      total_perc = sum(.data$total_perc),
      avg_length = mean(.data$total_length),
      total_length = sum(.data$total_length))

    df$budget_code <- budget_code
    if (is.null(result)) {
      result <- df
    } else {
      result <- rbind(result, df)
    }
  }
  result <- result %>% relocate(budget_code) # move the id column to the left
  return(result)
}

#' Gets a table summarising a spending parameter across several budget codes.
#'
#' \code{jc_get_spend_param_multi_budget} Gets a table that summarises spending across
#' different budget codes for a specific spending parameter (e.g. project count).
#' The result contains combined data for all budget codes, and for each budget
#' code provides summary data relating to the specified parameter.
#'
#' This method is similar to \code{link{jc_get_spend_info_multi_budget}}.
#' However, this method returns the summaries for all spending parameters
#' whereas method \code{jc_get_spend_param_multi_budget} returns the data only for one
#' specific spending parameter. See \code{link{jc_get_spend_info_multi_budget}}.
#'
#' @param budget_codes vector of budget codes, e.g. 'upper', 'lower', 'base' etc.
#' It is assumed that there will be output files matching these codes and also
#' that the budgets file will have matching sheets containing the budgets for
#' each of these codes.
#' @param budgets_file Relative path to the Excel file containing the budgets.
#' @param spend_param spending parameter for which to get information. Valid
#' values are (case sensitive!):
#' \enumerate{
#'   \item 'project_count' mean number of projects for category across all periods
#'   \item 'avg_spend' mean spending (in millions) for category across all periods
#'   \item 'total_spend' sum of all spending (in millions) for category across all periods
#'   \item 'avg_perc' mean of percentage of network treated for category
#'   across all periods
#'   \item 'total_perc' sum of annual percentages of network treated for category
#'   across all periods
#'   \item 'avg_length' mean of length treated for category across all periods
#'   \item 'total_length' sum of length treated for category across all periods
#'   \item 'budget_util' Average for percentage budget utilised for category
#'   across all periods
#' }
#' @param network_length Network length from which the percentage of length
#' treatment etc. can be calculated.
#' @param max_calendar_period maximum treatment period (in calendar terms) to 
#' consider.
#' @param treats_file_base Relative path to the Excel file containing the
#' treatments exported from jcass. This will not be the full file name, but only
#' the stub from which the full name can be built by appending the budget code
#' at the end.
#' @export
#' @importFrom dplyr summarise
#'
jc_get_spend_param_multi_budget <- function(budget_codes, budgets_file,
                                            spend_param, network_length,
                                            max_calendar_period, 
                                treats_file_base = "outputs/treatments_") {
  result <- NULL
  lbl <- NULL

  if (spend_param == "project_count") {
    lbl <- "Average Projects per Year"
  } else if (spend_param == "avg_spend") {
    lbl <- "Average Spending ($-million)"
  } else if (spend_param == "total_spend") {
    lbl <- "Total Spending ($-million)"
  } else if (spend_param == "avg_perc") {
    lbl <- "Percentage of Network (Annually)"
  } else if (spend_param == "total_perc") {
    lbl <- "Percentage of Network (Total)"
  } else if (spend_param == "avg_length") {
    lbl <- "Average Annual Length (km)"
  } else if (spend_param == "total_length") {
    lbl <- "Total Treated Length (km)"
  } else if (spend_param == "budget_util") {
    lbl <- "Budget Utilisation (%)"
  } else {
    stop(paste0("Spend paramemeter '", spend_param, "' is not handled"))
  }

  for (budget_code in budget_codes) {

    # Now read the list of treatments triggered/selected by the model
    treats_data <- .jc_get_treatments(budget_code, treats_file_base)
    treats_data <- treats_data %>% 
      filter(.data$period_calendar <= max_calendar_period)

    budgets <- read_xlsx(budgets_file, budget_code,
                         .name_repair = "unique_quiet")
    spend <- jc_get_spending_info(treats_data, budgets, network_length)

    df <- spend %>% group_by(.data$treat_category)

    if (spend_param == "project_count") {
      df <- df %>% summarise(zz = mean(.data$number))
      lbl <- "Average Projects per Year"
    } else if (spend_param == "avg_spend") {
      df <- df %>% summarise(zz = mean(.data$total_cost)/1e6)
    } else if (spend_param == "total_spend") {
      df <- df %>% summarise(zz = sum(.data$total_cost)/1e6)
    } else if (spend_param == "avg_perc") {
      df <- df %>% summarise(zz = mean(.data$total_perc))
    } else if (spend_param == "total_perc") {
      df <- df %>% summarise(zz = sum(.data$total_perc))
    } else if (spend_param == "avg_length") {
      df <- df %>% summarise(zz = mean(.data$total_length))
    } else if (spend_param == "total_length") {
      df <- df %>% summarise(zz = sum(.data$total_length))
    } else if (spend_param == "budget_util") {
      df <- df %>% summarise(zz = mean(.data$budget_util))
    } else {
      stop(paste0("Spend paramemeter '", spend_param, "' is not handled"))
    }

    df[, budget_code] <- df$zz
    df <- df  %>%  select(-c("zz"))
    if (is.null(result)) {
      result <- df
    } else {
      df <- df  %>%  select(-c("treat_category"))
      result <- cbind(result, df)
    }
  }
  result$param <- lbl
  result <- result %>% dplyr::relocate(.data$param) # move 'param' column to the left
  return(result)
}

#' Gets a table summarising spending for a specific budget.
#'
#' \code{jc_get_spending_info} Gets a table that summarises spending grouped by
#' treatment category and period for a specific budget. Provides summary data
#' relating to the following parameters:
#' \enumerate{
#'   \item 'number' number of projects
#'   \item 'total_cost' total cost for category and period
#'   \item 'total_length' total treatment length for category and period
#'   \item 'total_perc' percentage of total length treatment for category
#'   and period
#'   \item 'budget_util' Percentage of budget utilised for category and period. 
#'   NOTE - this column is only added when group_type = 'category'
#' }
#' 
#' 
#' @param treatments Data frame containing the treatments list exported by
#' jcass.
#' @param budget Data frame containing the budget.
#' @param network_length Network length from which the percentage of length
#' treatment etc. can be calculated.
#' @param group_type specifies whether grouping of treatments is by treatment 
#' category or by treatment type (more specific). Pass either 'category' or '
#' treatment'
#' @export
#' @importFrom ggplot2 element_blank
#'
jc_get_spending_info <- function(treatments, budget, network_length,
                                 group_type = "categories") {
  
  if (grepl( "cat", group_type, fixed = TRUE)) {
    
    treat_cats <- unique(treatments$treat_category)
    budgets_long <- budget %>% tidyr::gather(all_of(treat_cats),
                                             key = "treat_category", 
                                             value = "budget")
    spend <- treatments %>% group_by(.data$treat_category, .data$period) %>%
      summarise(
        number = sum(!is.na(.data$treatment_cost)),
        total_cost = sum(.data$treatment_cost),
        total_length = sum(.data$loc_to - .data$loc_from)/1000,
        total_perc = 100 * sum(.data$loc_to - .data$loc_from)/network_length
      )
    spend <- spend %>% dplyr::left_join(budgets_long, 
                                        by = c("treat_category", "period"))
    spend$budget_util <- 100 * spend$total_cost/spend$budget
    
  } else {
    
    spend <- treatments %>% group_by(.data$treatment, .data$period) %>%
      summarise(
        number = sum(!is.na(.data$treatment_cost)),
        total_cost = sum(.data$treatment_cost),
        total_length = sum(.data$loc_to - .data$loc_from)/1000,
        total_perc = 100 * sum(.data$loc_to - .data$loc_from)/network_length
      )
  }
  return(spend)
}




#' Gets a table summarising spending for over multiple budgets.
#'
#' \code{jc_get_spend_summary_multi_budget} Gets a table that summarises spending
#' grouped by treatment category. Provides summary data relating to a chosen set
#' of parameters:
#'
#' @param budget_codes vector of budget codes, e.g. 'upper', 'lower', 'base' etc.
#' It is assumed that there will be output files matching these codes and also
#' that the budgets file will have matching sheets containing the budgets for
#' each of these codes.
#' @param budgets_file Relative path to the Excel file containing the budgets.
#' @param spend_params Vector of spending parameter codes for which to get the
#' summary. Valid values are:
#' \enumerate{
#'   \item 'project_count' mean number of projects for category across all periods
#'   \item 'avg_spend' mean spending (in millions) for category across all periods
#'   \item 'total_spend' sum of all spending (in millions) for category across all periods
#'   \item 'avg_perc' mean of percentage of network treated for category
#'   across all periods
#'   \item 'total_perc' sum of annual percentages of network treated for category
#'   across all periods
#'   \item 'avg_length' mean of length treated for category across all periods
#'   \item 'total_length' sum of length treated for category across all periods
#'   \item 'budget_util' Average for percentage budget utilised for category
#'   across all periods
#' }
#' @param network_length Network length from which the percentage of length
#' treatment etc. can be calculated
#' @param max_calendar_period maximum treatment period (in calendar terms) to 
#' consider.
#' @param treats_file_base Relative path to the Excel file containing the
#' treatments exported from jcass. This will not be the full file name, but only
#' the stub from which the full name can be built by appending the budget code
#' at the end.
#' @export
#' @importFrom ggplot2 element_blank
#'
jc_get_spend_summary_multi_budget <- function(budget_codes,
     budgets_file, network_length, max_calendar_period, 
     spend_params = c("avg_spend", "total_spend", "avg_perc", "total_perc",
                      "avg_length", "total_length"),
    treats_file_base = "outputs/treatments_") {

  result <- NULL
  for (spend_param in spend_params) {
    tmp <- jc_get_spend_param_multi_budget(budget_codes,
                                          budgets_file, spend_param,
                                          network_length, max_calendar_period)
    if (is.null(result)) {
      result <- tmp
    } else {
      result <- rbind(result, tmp)
    }
  }

  return(result)

}

#' Gets a report ready table summarising spending for over multiple budgets.
#'
#' \code{jc_get_spend_summry_multi_budget_gt} Gets a report ready gt() table
#' that summarises spending grouped by treatment category. This method takes
#' the output of method \code{jc_get_spend_summary_multi_budget} and just
#' converts it to a printed image.
#'
#' Also see \code{link{jc_get_spend_summary_multi_budget}}
#'
#' @param summary_table data frame containing the output from method
#' \code{jc_get_spend_summary_multi_budget}.
#' See \code{link{jc_get_spend_summary_multi_budget}}
#' @param budget_codes vector of budget codes, e.g. 'upper', 'lower', 'base' etc.
#' It is assumed that there will be output files matching these codes and also
#' that the budgets file will have matching sheets containing the budgets for
#' each of these codes.
#' @export
#' @importFrom ggplot2 element_blank
#'
jc_get_spend_summry_multi_budget_gt <- function(summary_table, budget_codes) {

  summary_table <- summary_table %>%
    dplyr::rename('Budget Code' = .data$treat_category)  #To put as header after

  tt <- summary_table |>
    group_by(.data$param) |>
    gt::gt() |>
    gt::fmt_integer(
      columns = budget_codes,
      rows = .data$param == "Average Projects per Year",
      use_seps = TRUE,
    ) |>
    gt::fmt_number(
      columns = budget_codes,
      rows = .data$param %in% c("Average Spending ($-million)",
                                "Total Spending ($-million)",
                                "Percentage of Network (Annually)",
                                "Percentage of Network (Total)",
                                "Average Annual Length (km)",
                                "Total Treated Length (km)"),
      use_seps = TRUE,
      decimals = 2,
    ) |>
    gt::fmt_number(
      columns = budget_codes,
      rows = .data$param == "Budget Utilisation (%)",
      use_seps = TRUE,
      decimals = 1,
    )
  return(tt)
}


# worker to extract treatments from jcass output file
.jc_get_treatments <- function(budget_code,
                               treats_file_base = "outputs/treatments_") {

  treats_export_file <- paste0(treats_file_base, budget_code, ".xlsx")
  treats_data <- read_xlsx(treats_export_file,"treatments",
                           .name_repair = "unique_quiet")
  treats_data <- as.data.frame(treats_data)
  return(treats_data)
}
