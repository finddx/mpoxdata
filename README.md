# MPOXDATA Measures

## WHO

**new_confirmed_cases_calc:** new cases from original data source, NA values are transformed to 0

**cum_confirmed_cases_calc:** cumulative cases calculated from new_confirmed_cases_calc

**new_suspected_cases_calc** suspected cases from original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases calculated from new_suspected_cases_calc

**all_new_confirmed_cases:** smooth new confirmed cases calculated from new_confirmed_cases_calc and cum_confirmed_cases_calc

**all_cum_confirmed_cases:** cumulative cases calculated from all_new_confirmed_cases

**all_new_suspected_cases:** smooth new suspected cases calculated from new_suspected_cases_calc and cum_suspected_cases_calc

**all_cum_suspected_cases:** cumulative cases calculated from all_new_suspected_cases


## Africa CDC

**new_confirmed_cases_calc:** new cases from original data source, NA values are transformed to 0

**cum_confirmed_cases_calc:** cumulative cases calculated from new_confirmed_cases_calc

**new_suspected_cases_calc:** suspected cases from original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases calculated from new_suspected_cases_calc

**all_new_confirmed_cases:** smooth new confirmed cases calculated from new_confirmed_cases_calc and cum_confirmed_cases_calc

**all_cum_confirmed_cases:** cumulative cases calculated from all_new_confirmed_cases

**all_new_suspected_cases:** smooth new suspected cases calculated from new_suspected_cases_calc and cum_suspected_cases_calc

**all_cum_suspected_cases:** cumulative cases calculated from all_new_suspected_cases

**num_tests_pos_orig:** (cum_confirmed_cases_calc / positivity_rate_orig) * 100

**num_tests_pos_calc:** (cum_confirmed_cases_calc / positivity_rate_calc) * 100

**num_tests_test_orig:** (testing_rate_orig * cum_suspected_cases_calc) / 100

**num_tests_test_calc:** (testing_rate_calc * cum_suspected_cases_calc) / 100

## Global Health CDC

**new_confirmed_cases_calc:** new cases from original data source, NA values are transformed to 0

**cum_confirmed_cases_calc:** cumulative cases calculated from new_confirmed_cases_calc

**new_suspected_cases_calc:** suspected cases from original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases calculated from new_suspected_cases_calc

**all_new_confirmed_cases:** smooth new confirmed cases calculated from new_confirmed_cases_calc and cum_confirmed_cases_calc

**all_cum_confirmed_cases:** cumulative cases calculated from all_new_confirmed_cases

**all_new_suspected_cases:** smooth new suspected cases calculated from new_suspected_cases_calcand cum_suspected_cases_calc

**all_cum_suspected_cases:** cumulative cases calculated from all_new_suspected_cases


## Our World in Data

**new_confirmed_cases_calc:** new cases from original data source, NA values are transformed to 0

**cum_confirmed_cases_calc:** cumulative cases calculated from new_confirmed_cases_calc

**new_suspected_cases_calc:** suspected cases calculated from cumulative original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases from original data source

**all_new_confirmed_cases:** smooth new confirmed cases calculated from new_confirmed_cases_calc and cum_confirmed_cases_calc

**all_cum_confirmed_cases:** cumulative cases calculated from all_new_confirmed_cases

**all_new_suspected_cases:** smooth new suspected cases calculated from new_suspected_cases_calc and cum_suspected_cases_calc

**all_cum_suspected_cases:** cumulative cases calculated from all_new_suspected_cases
