#' Categorize trends in a GCAM variable
#'
#' This function analyzes trends in an input table of GCAM data values over
#' time.  The time series are categorized into a user-defined number of trend
#' types, and these are returned, along with a table identifying each time
#' series with its trend type.
#'
#' @section Input:
#' Input data should be a data frame in long format, with year and value columns
#' and at least one ID column.  For example, you might have a table of region,
#' year, and population.  The output would then categorize regional population
#' trends.  It is also permissible to have more than one ID column; for example,
#' region, sector, year, and output.  In this case, each combination of region
#' and sector would be a separate data point for categorization purposes.
#'
#' @section Output:
#'
#' The return value is a list containing two elements.  The \code{trends}
#' element contains a table with columns \code{trend.category}, \code{year}, and
#' \code{value}.  It represents a prototype time series for each trend category.
#' The prototype time series is in normalized units, with the largest value in
#' the time series defined to be 1.
#'
#' The \code{categories} element contains all of the columns from the input data
#' set, along with a \code{trend.category} column.  The original value column
#' will be normalized to the maximum value in each time series, just as the
#' prototype trend time series are.  The trend category column indicates which
#' category that time series belonged to.
#'
#' For example, if the input were a table of region, year, and population, then
#' the \code{trends} output will be a table of trend category, year, and
#' population.  If four categories were requested, then the prototype time
#' series might correspond to population time series that are (for example)
#' stable, rising then stable, rising continuously, and rising then declining.
#' The categories aren't labled in any way; an analyst would arrive at these
#' labels by looking at the prototype time series returned.
#'
#' The \code{categories} element would contain a table of region and trend
#' category, with each category being a number from 1-4, corresponding to one of
#' the trend categories returned.  This table tells you which regions have
#' population trends like those in each of the trend categories.
#'
#' @param d The data to analyze.  See details for a description of the input
#'  data format.
#' @param n Number of categories to produce.  For 32-region data, setting n > 8
#'  is not recommended.
#' @param valuecol Name of the column with the data values in it.  The output
#'  trends will also use this name for their value column.
#' @param yearcol Name of the column with the year values in it.  The output
#'  trends will also use this name for their year column.
#' @importFrom magrittr "%>%"
#' @export
trend_analysis <- function(d, n=4, valuecol='value', yearcol='year')
{
    ## silence package notes for NSE
    trend.category <- value <- year <- NULL

    ## standardize the column names for the analysis.  We'll change the names
    ## back at the end.
    if(valuecol != 'value') {
        d <- dplyr::rename_(d, value=valuecol)
    }

    if(yearcol != 'year') {
        d <- dplyr::rename_(d, year=yearcol)
    }

    ## scale each time series to its maximum value.  This is necessary so that
    ## we don't simply group small values with other small values and large
    ## values with other large values.
    idcols <- setdiff(names(d), c('year','value'))
    d <- dplyr::group_by_(d, .dots=idcols) %>%
      dplyr::mutate(value = value / max(value)) %>%
      dplyr::ungroup()

    ## reshape the d to put each time series in a row.  This is the format
    ## needed by most clustering algorithms.
    yearnames <- as.character(unique(d$year))
    dm <- tidyr::spread(d, year, value)
    m <- dplyr::select(dm, dplyr::one_of(yearnames)) %>% as.matrix

    ## run the clustering algorithm
    cl <- stats::kmeans(m, n, nstart = 5)

    ## match cluster ids to id groupings
    dm <- dplyr::select(dm, dplyr::one_of(idcols)) %>%
      dplyr::mutate(trend.category = cl[['cluster']])

    ## format the output data:  rename value column and add the cluster id from
    ## the dm table
    valueout <- paste('normalized',valuecol, sep='.')
    d <- dplyr::rename_(d, .dots = stats::setNames(list('value'), valueout)) %>%
      dplyr::left_join(dm, by=idcols)

    ctr <- cl$centers %>%
      as.data.frame %>%
      dplyr::mutate(trend.category = 1:n) %>%
      tidyr::gather(year, value, -trend.category) %>%
      dplyr::mutate(year=as.integer(year)) %>%
      dplyr::rename_(.dots = stats::setNames(list('value'), valueout))

    list(trends = ctr, categories = d)
}



