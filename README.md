# FIND MPOX Data Collection

The FIND team collects data about the MPOX 2024 outbreak from information found online. It combines it with suspected and confirmed MPOX cases from WHO, Africa CDC, Global Health, and Our World in Data and displays them in an interactive tracker dashboard.


## Data Sources

### WHO

Data is manually downloaded every Monday from https://worldhealthorg.shinyapps.io/mpx_global/#4_Global_situation_update

### Africa CDC

Data is manually extracted every Monday from the PDF report found in https://africacdc.org/resources/?wpv_aux_current_post_id=217&wpv_view_count=549&wpv-resource-type=ebs-weekly-reports

### Global Health

Data is automatically extracted every Monday from https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv

### Our World in Data

Data is automatically extracted every Monday from https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv


## Measurements

We report the following measurements for all data sources (WHO: variables ending with "_who", Africa CDC: variables ending with "_acdc", Global Health: variables ending with "_gh", Our World in Data: variables ending with "_owd")

### Confirmed cases

**new_confirmed_cases_calc:** new cases from original data source, NA values are transformed to 0

**cum_confirmed_cases_calc:** cumulative cases calculated from new_confirmed_cases_calc

### Suspected cases

**new_suspected_cases_calc** suspected cases from original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases calculated from new_suspected_cases_calc

Our Wourld in Data does not report new suspected cases, therefore we calculate new suspected cases as:

**new_suspected_cases_calc:** suspected cases calculated from cumulative original data source, NA values are transformed to 0

**cum_suspected_cases_calc:** cumulative cases from original data source, NA values are transformed to 0

### Smooth confirmed cases

**all_new_confirmed_cases:** smooth new confirmed cases calculated from new_confirmed_cases_calc and cum_confirmed_cases_calc

**all_cum_confirmed_cases:** cumulative cases calculated from all_new_confirmed_cases

### Smooth suspected cases

**all_new_suspected_cases:** smooth new suspected cases calculated from new_suspected_cases_calc and cum_suspected_cases_calc

**all_cum_suspected_cases:** cumulative cases calculated from all_new_suspected_cases

### Per capita measurements

Per capita measurements of all smooth variables are calculated as:

**pop:** smooth_variable / pop

**pop_100k:** smooth_variable / pop_100k

### DX gap
DX gap is calculated on a weekly basis (period column), when grouping by income or continent dxgap is calculated as the mean of means (e.g, mean of countries for that continent).

Dx gap is only calculated when suspected cases are higher or equal (>=) than confirmed cases.

WHO does not report suspected cases.

**dxgap (new cases):** ((all_new_suspected_cases - all_new_confirmed_cases) / all_new_suspected_cases) * 100

**dxgap (cumulative cases):** ((all_cum_suspected_cases - all_cum_confirmed_cases) / all_cum_suspected_cases) * 100

Note that DX Gap is only calculated when suspected cases is higher than confirmed cases

### Testing and Positivity rates

Africa CDC also reports Testing and Positivity rates:

**num_tests_pos_orig:** (cum_confirmed_cases_calc / positivity_rate_orig) * 100

**num_tests_pos_calc:** (cum_confirmed_cases_calc / positivity_rate_calc) * 100

**num_tests_test_orig:** (testing_rate_orig * cum_suspected_cases_calc) / 100

**num_tests_test_calc:** (testing_rate_calc * cum_suspected_cases_calc) / 100


## CALCULATIONS JUSTIFICATION

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


## Aggregation

When aggregating over periods and/or groups, we apply the following principles in turn:

1. Aggregation over period: If data is missing during more than 25% of the most recent observations, the period is considered incomplete, no aggregated value is computed.

2. Aggregation over group: Groups aggregations use all the countries for which data is available. If a ratio is computed (e.g., per capita measures), we only consider observations that have values both for the nominator and the denominator. E.g., to calculate tests per capita for a continent, a country is only used if it reports both test and population data.

When we aggregate both over period and group, we do period aggregation first and group aggregation second.
