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
#' @section Methods:
#'
#' The structure returned by the trend analysis has a summary method that prints
#' a table of which data ponts were assigned to which categories, along with a
#' count of how many data points were assigned to each category.  There is also
#' a plot method that will plot a visualization of the trend categories and
#' their members.
#'
#' @examples
#' data(population)
#' poptrends <- trend_analysis(population, n=4, valuecol='population')
#' summary(poptrends)
#' \dontrun{
#' plot(poptrends)
#' }
#'
#' @param d The data to analyze.  See details for a description of the input
#'  data format.
#' @param n Number of categories to produce.  For 32-region data, setting n > 8
#'  is not recommended.
#' @param valuecol Name of the column with the data values in it.  The output
#'  trends will prepend 'normalized.' to this name for the name of their value
#'  column.
#' @param yearcol Name of the column with the year values in it.  The output
#'  trends will always call their year column "year".
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

    rtn <- list(trends = ctr, categories = d)
    class(rtn) <- 'trendanalysis'
    rtn
}


#' Is an object a tendanalysis object
#'
#' Is method for trendanalysis class
#'
#' @inheritParams methods::is
#' @export
is.trendanalysis <- function(object) {
    inherits(object, 'trendanalysis')
}

#' Summarize a trend analysis
#'
#' Summary method for trendanalysis class.
#'
#' The summary includes the number of trend categories and the table of category
#' identifications.
#'
#' @inheritParams base::summary
#' @export
summary.trendanalysis <- function(object, ...) {
    year <- trend.category <- NULL
    ntype <- length(unique(object$trends$trend.category))

    ## get the first year
    yr1 <- dplyr::filter(object$categories, year==1990)
    ## keep all the character variables; these are the id vars
    catid <- dplyr::select_if(yr1, is.character)
    ## Add back the trend category
    catid$trend.category <- yr1$trend.category
    ## Arrange by trend type
    catid <- dplyr::arrange(catid, trend.category)

    ## package for return
    rtn <- list(ntype=ntype, catid=catid)
    class(rtn) <- 'summary.trendanalysis'
    rtn
}

#' Format a trend analysis summary for pretty-printing
#'
#' Format method for summary.trendanalysis class
#'
#' @inheritParams base::format
#' @export
#' @importFrom utils capture.output
format.summary.trendanalysis <- function(x, ...) {
    ## This is kind of a hacky way of doing this, but it produces a readable
    ## table.  The row numbers on the table are kind of a wart though
    catcount <- capture.output(table(x$catid$trend.category))
    catcount[1] <- 'Number of members for each trend category:'
    c(paste('Number of trend types extracted:', x$ntype),
      capture.output(format(as.data.frame(x$catid))),
      catcount
      )
}

#' Print a trend analysis summary
#'
#' Print method for summary.trendanalysis class
#'
#' @inheritParams base::print
#' @export
print.summary.trendanalysis <- function(x, ...) {
    cat(format(x), sep='\n')
}

#' Plot a trend analysis
#'
#' Plot method for the trendanalysis class. Note that we use ggplot to actually
#' render the plot, not base graphics.
#'
#' @param x The trendanalysis object to plot
#' @param y ignored
#' @param ... ignored
#' @export
plot.trendanalysis <- function(x, y, ...) {
    trends <- x$trends
    cats <- x$categories

    ## first we have to figure out what the name of the column with the data in
    ## it is.  (It will be the same in both the trends and categories.
    valuecol = grep('normalized\\.', names(trends), value=TRUE)


    ## next we have to figure out the id columns and assign each id category a
    ## unique identifier in a single column.  We will need this for grouping in
    ## the plot.
    idcols <- dplyr::setdiff(names(cats), c(valuecol, 'year', 'trend.category'))
    idtbl <- dplyr::select(cats, dplyr::one_of(idcols)) %>% dplyr::distinct()
    idtbl$group <- row(idtbl)
    ## add the group back to the table
    cats <- dplyr::left_join(cats, idtbl, by=idcols)

    ## Awesome.  Now we can make our plot.  We're going to facet by trend type,
    ## plot the prototype trend in a heavy line, and plot the time series from
    ## the data in light lines.

    ggplot2::ggplot() + ggplot2::facet_wrap(facets=~trend.category) +
      ggplot2::geom_line(data=trends,
                         mapping= ggplot2::aes_string(x='year', y=valuecol),
                         group=-1,
                         size=1.5, color="black") +
      ggplot2::geom_line(data=cats,
                         mapping= ggplot2::aes_string(x='year', y=valuecol, group='group'),
                         color='black', size=0.5, alpha=0.5)

}
