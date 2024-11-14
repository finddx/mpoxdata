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



# CALCULATIONS JUSTIFICATION

To calculate Cumulative (CUM) cases we decided to use the Cumulative from New cases for 3 reasons.

1. We observed that for WHO and Our World in Data we were obtained the same numbers, so did not imply any difference.
   
3. Global Health only reports new cases, so it was the only measure we can use to compare the original values.
   
5. The Cumulative cases in Africa CDC present some inconsistencies, which undermines the reliability of this measure.
   
   
For example there are sometimes a reduction in Cumulative confirmed cases in progressive dates:

* Congo 07/01/2024: 21 CUM confirmed cases - Congo 17/03/2024: 19 CUM confirmed cases

* Ghana 16/06/2024: 131 CUM confirmed - Ghana 06/10/2024: 1 CUM confirmed cases
  
* Democratic Republic of the Congo 03/08/2024: 2715 CUM confimred cases - Democratic Republic of the Congo 09/08/2024: 2638 CUM confirmed cases
  

We also compared numbers of CUM confirmed cases and CUM calculated from NEW confirmed cases between Africa CDC and other data sources to check if any of those measurements from Africa CDC have consistent numbers with the reported by others, however numbers were not concise. We list some examples below :

* Republic of the Congo 07/01/2024:  21 CUM confirmed cases Africa CDC - 0 CUM from NEW confirmed cases Africa CDC -  0 CUM confirmed cases other data sources

* Democratic Republic of the Congo 10/02/2024: 247 CUM confirmed cases Africa CDC - 224 CUM from NEW confirmed cases Africa CDC -  247 CUM confirmed cases other data sources

* Kenya 12/10/2024: 12 CUM confirmed cases Africa CDC - 10 CUM from NEW confirmed cases Africa CDC -  8 to 12 CUM confirmed cases other data sources
