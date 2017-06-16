library('GCAManalysis')
library('dplyr', quietly=TRUE, warn.conflicts=FALSE)

context('GCAM trend analysis')

poptrends <- NULL                       # will get the output from the first
                                        # test for use in later tests.

test_that('trend_analysis runs without errors.', {
              set.seed(8675309)         # ensure repeatability
              data('population', package='GCAManalysis')
              expect_silent({pt <- trend_analysis(population, 4,
                                                  'population')})
              poptrends <<- pt
          })

test_that('trend analysis produces the expected kind of output.', {
              expect_true(is.list(poptrends))
              expect_equal(names(poptrends), c('trends', 'categories'))
              expect_true(is.data.frame(poptrends$trends))
              expect_true(is.data.frame(poptrends$categories))
              expect_equal(names(poptrends$trends), c('trend.category', 'year',
                                                      'normalized.population'))
              expect_equal(names(poptrends$categories), c('region', 'year',
                                                          'normalized.population',
                                                          'trend.category'))
          })

## Check for some common features.  Doing this instead of testing for complete
## equality allows us to swap out the clustering algorithm and still pass if we
## get qualitatively similar results.

test_that('high-growth cluster is identified.', {
              ## Observation 1: Three of the African regions form a high-growth
              ## group.  Find this cluster and verify.
              clust <- filter(poptrends$categories, region == 'Africa_Western',
                               year==1990)$trend.category
              crgns <- filter(poptrends$categories, trend.category == clust,
                               year==1990)
              expect_equal(nrow(crgns), 3)
              expect_equal(sort(crgns$region),
                           c('Africa_Eastern', 'Africa_Southern',
                             'Africa_Western'))

              ## Verify that the prototype is steeply increasing in the early
              ## century.
              normpop <- (filter(poptrends$trends, trend.category == clust) %>%
                            arrange(year))[['normalized.population']]
              npstrt <- normpop[1]
              npmid <- normpop[length(normpop)/2]
              npfinal <- normpop[length(normpop)]

              expect_gte(npmid/npstrt, 3.0)
              expect_gte(npfinal/npstrt, 4.0)
          })

test_that('declining cluster is identified.', {
              ## Observation 2: China, Japan, South Korea, and the regions in
              ## eastern europe form a cluster with declining population.
              clust <- filter(poptrends$categories, region == 'China',
                               year==1990)$trend.category
              crgns <- filter(poptrends$categories, trend.category == clust,
                               year==1990)
              expect_equal(nrow(crgns), 6)
              expect_equal(sort(crgns$region),
                           c('China', 'EU-12', 'Europe_Eastern', 'Japan',
                             'Russia', 'South Korea'))
              normpop <- (filter(poptrends$trends, trend.category == clust) %>%
                            arrange(year))[['normalized.population']]
              npstrt <- normpop[1]
              npmid <- normpop[length(normpop)/2]
              npfinal <- normpop[length(normpop)]

              expect_lt(npmid, npstrt)
              expect_lt(npfinal, npmid)
          })

test_that('rising-then-stable cluster is identified.', {
              ## Observation 3: Several regions (see below) end up in a cluster
              ## with moderate growth
              clust <- filter(poptrends$categories, region == 'USA',
                               year==1990)$trend.category
              crgns <- filter(poptrends$categories, trend.category == clust,
                               year==1990)
              expect_equal(nrow(crgns), 7)
              expect_equal(sort(crgns$region),
                           c("Australia_NZ", "Canada",
                             "European Free Trade Association", "Middle East",
                             "Pakistan", "South Asia", "USA"))
              normpop <- (filter(poptrends$trends, trend.category == clust) %>%
                            arrange(year))[['normalized.population']]
              npstrt <- normpop[1]
              npmid <- normpop[length(normpop)/2]
              npfinal <- normpop[length(normpop)]

              expect_gt(npmid, npstrt)
              expect_lte(npmid, 2.0*npstrt)
              expect_lte(npfinal, 1.2*npmid)
          })

test_that('rising-then-falling cluster is identified.', {
              ## By definition this cluster contains everything else, so there
              ## is no need to test membership.
              clust <- filter(poptrends$categories, region == 'EU-15',
                              year==1990)$trend.category
              normpop <- (filter(poptrends$trends, trend.category == clust) %>%
                            arrange(year))[['normalized.population']]
              npstrt <- normpop[1]
              npmid <- normpop[length(normpop)/2]
              npfinal <- normpop[length(normpop)]

              expect_gt(npmid, npstrt)
              expect_gt(npmid, npfinal)
          })

test_that('summary of trend analysis can be displayed.', {
              expect_silent({s <- summary(poptrends)})
              expect_equal(names(s), c('ntype', 'catid'))

              ## print method just cats the output of format, so if format
              ## works, it will too.
              expect_true(is.character(format(s)))
          })
