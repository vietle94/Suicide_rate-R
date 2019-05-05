Load library
============

    library(tidyverse)
    library(skimr)

Load data
=========

    data <- read_csv("./data/master.csv")

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   year = col_double(),
    ##   sex = col_character(),
    ##   age = col_character(),
    ##   suicides_no = col_double(),
    ##   population = col_double(),
    ##   `suicides/100k pop` = col_double(),
    ##   `country-year` = col_character(),
    ##   `HDI for year` = col_double(),
    ##   `gdp_for_year ($)` = col_number(),
    ##   `gdp_per_capita ($)` = col_double(),
    ##   generation = col_character()
    ## )

    data <- data %>% rename(HDI = `HDI for year`)

Have a look

    summary(data)

    ##    country               year          sex                age           
    ##  Length:27820       Min.   :1985   Length:27820       Length:27820      
    ##  Class :character   1st Qu.:1995   Class :character   Class :character  
    ##  Mode  :character   Median :2002   Mode  :character   Mode  :character  
    ##                     Mean   :2001                                        
    ##                     3rd Qu.:2008                                        
    ##                     Max.   :2016                                        
    ##                                                                         
    ##   suicides_no        population       suicides/100k pop country-year      
    ##  Min.   :    0.0   Min.   :     278   Min.   :  0.00    Length:27820      
    ##  1st Qu.:    3.0   1st Qu.:   97498   1st Qu.:  0.92    Class :character  
    ##  Median :   25.0   Median :  430150   Median :  5.99    Mode  :character  
    ##  Mean   :  242.6   Mean   : 1844794   Mean   : 12.82                      
    ##  3rd Qu.:  131.0   3rd Qu.: 1486143   3rd Qu.: 16.62                      
    ##  Max.   :22338.0   Max.   :43805214   Max.   :224.97                      
    ##                                                                           
    ##       HDI        gdp_for_year ($)    gdp_per_capita ($)  generation       
    ##  Min.   :0.483   Min.   :4.692e+07   Min.   :   251     Length:27820      
    ##  1st Qu.:0.713   1st Qu.:8.985e+09   1st Qu.:  3447     Class :character  
    ##  Median :0.779   Median :4.811e+10   Median :  9372     Mode  :character  
    ##  Mean   :0.777   Mean   :4.456e+11   Mean   : 16866                       
    ##  3rd Qu.:0.855   3rd Qu.:2.602e+11   3rd Qu.: 24874                       
    ##  Max.   :0.944   Max.   :1.812e+13   Max.   :126352                       
    ##  NA's   :19456

-   country-year column is just a concatenation of country and year
    column

-   suicides/100k pop is calculated by suicides / population \* 100000

-   HDI is Human Development Report, seems missing alot, the only
    variable is missing

-   Age is divided into brackets

<!-- -->

    skim(data)%>% skimr::kable()

    ## Skim summary statistics  
    ##  n obs: 27820    
    ##  n variables: 12    
    ## 
    ## Variable type: character
    ## 
    ##    variable      missing    complete      n      min    max    empty    n_unique 
    ## --------------  ---------  ----------  -------  -----  -----  -------  ----------
    ##      age            0        27820      27820     9     11       0         6     
    ##    country          0        27820      27820     4     28       0        101    
    ##  country-year       0        27820      27820     8     32       0        2321   
    ##   generation        0        27820      27820     6     15       0         6     
    ##      sex            0        27820      27820     4      6       0         2     
    ## 
    ## Variable type: numeric
    ## 
    ##       variable         missing    complete      n         mean           sd          p0         p25        p50         p75         p100        hist   
    ## --------------------  ---------  ----------  -------  ------------  ------------  ---------  ---------  ---------  ------------  ---------  ----------
    ##   gdp_for_year ($)        0        27820      27820     4.5e+11       1.5e+12      4.7e+07     9e+09     4.8e+10     2.6e+11      1.8e+13    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##  gdp_per_capita ($)       0        27820      27820     16866.46      18887.58       251       3447       9372        24874       126352     <U+2587><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##         HDI             19456       8364      27820       0.78         0.093        0.48       0.71       0.78         0.85        0.94      <U+2581><U+2581><U+2583><U+2585><U+2587><U+2587><U+2587><U+2586> 
    ##      population           0        27820      27820    1844793.62    3911779.44      278      97498.5    430150     1486143.25    4.4e+07    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##  suicides/100k pop        0        27820      27820      12.82         18.96          0        0.92       5.99        16.62       224.97     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##     suicides_no           0        27820      27820      242.57        902.05         0          3         25          131         22338     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##         year              0        27820      27820     2001.26         8.47        1985       1995       2002         2008        2016      <U+2585><U+2586><U+2587><U+2587><U+2587><U+2587><U+2587><U+2586>

There are:

-   6 Age brackets

-   101 differenet countries in this data set

-   Year from 1985 to 2016. (32 years)

-   2321 combinations of country-year (less than 32 \* 101). Must be
    some implicit missing data

-
