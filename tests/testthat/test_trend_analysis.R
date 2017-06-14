library('GCAManalysis')

context('GCAM trend analysis')

test_that('trend_analysis works.', {
              set.seed(8675309)
              data('population', package='GCAManalysis')
              expect_silent({poptrends <- trend_analysis(population, 4,
                                                         'population')})
              expect_equal_to_reference(poptrends, 'poptrends.rds')
          })
