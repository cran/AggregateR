[![CRAN checks](https://cranchecks.info/badges/summary/AggregateR)](https://cran.r-project.org/web/checks/check_results_AggregateR.html)
[![](https://www.r-pkg.org/badges/version/AggregateR?color=orange)](https://cran.r-project.org/package=AggregateR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/AggregateR?color=blue)](https://cran.r-project.org/package=AggregateR)


# AggregateR

The `Aggregate` function (not to be confounded with `aggregate`) prepares a `data.frame`, `tibble` or `data.table` for merging by computing the sum, mean and variance of all continuous (integer and numeric) variables by a given variable. 
For all categorical variabes (character and factor), it creates dummies and subsequently computes the sum and the mode by a given variable. 
For all Date variables, it computes the recency and duration by a given variable with repsect the an end date variable. 
For computational speed, all the calculations are done with `data.table`. This functions aims at maximum information extraction with a minimum amount of code.

The package also contains faster implementations of the `dummy` and `categories` function (comparable to the same functions in the `dummy` package). When using the `AggregateR` package, the `dummy`-package is deprecated and the internal `dummy` and `categories` functions are superior in terms of speed.

# Installation

To install the package from CRAN: 

```
install.packages('AggregateR')
```

To instal the package from github: 

```
devtools::install_github ('MatthBogaert/AggregateR')
```

# Usage

This code blocks shows how the `Aggregate` function works when confronted with a table with numeric, categorical and Date variables. `Aggregate` accepts a `data.frame`, `tibble` or `data.table` and outputs by default a `data.table`.

```
#Create some data
data <- data.frame(V1=sample(as.factor(c('yes','no')), 200000, TRUE),
                   V2=sample(as.character(c(1,2,3,4,5)),200000, TRUE),
                   V3=sample(1:20000,200000, TRUE),
                   V4=sample(300:1000, 200000, TRUE),
                   V5 = sample(as.Date(as.Date('2014-12-09'):Sys.Date()-1, origin = "1970-01-01"),200000,TRUE),
                   ID=sample(x = as.character(1:4), size = 200000, replace = TRUE))

Aggregate(x=data,by='ID')

## Calculating categorical variables ... 
## Calculating numerical variables ... 
## Calculating date variables ...
## ID V1_no_sum V1_no_mode V1_yes_sum V1_yes_mode V2_1_sum V2_1_mode V2_2_sum V2_2_mode V2_3_sum
## 1:  1     24911          0      25080           1    10006         0     9990         0    10170
## 2:  2     24938          0      25160           1     9985         0    10073         0    10030
## 3:  3     25070          1      24933           0     9845         0     9987         0    10108
## 4:  4     24926          0      24982           1     9923         0     9891         0     9901
## V2_3_mode V2_4_sum V2_4_mode V2_5_sum V2_5_mode    V3_sum   V3_mean   V3_var   V4_sum  V4_mean
## 1:         0     9887         0     9938         0 498324620  9968.287 33440187 32426370 648.6442
## 2:         0     9962         0    10048         0 499201602  9964.502 33370364 32606808 650.8605
## 3:         0     9988         0    10075         0 501006529 10019.529 33208428 32535970 650.6804
## 4:         0     9939         0    10254         0 499350872 10005.427 33285590 32461104 650.4189
## V4_var V5_duration V5_recency
## 1: 40972.02        2172          1
## 2: 41186.23        2172          1
## 3: 40789.41        2172          1
## 4: 41224.02        2172          1
```

As mentioned, the user can also output a tibble for nicer printing. 

```
Aggregate(x=data,by='ID', tibble = TRUE)

## Calculating categorical variables ... 
## Calculating numerical variables ... 
## Calculating date variables ... 
##A tibble: 4 x 23
## ID    V1_no_sum V1_no_mode V1_yes_sum V1_yes_mode V2_1_sum V2_1_mode V2_2_sum V2_2_mode V2_3_sum
## <chr>     <dbl>      <dbl>      <dbl>       <dbl>    <dbl>     <dbl>    <dbl>     <dbl>    <dbl>
##1 1         25060          1      24906           0    10046         0     9847         0     9932
##2 2         25056          1      24964           0     9981         0    10010         0     9986
##3 3         24986          0      25068           1     9989         0    10057         0    10076
##4 4         25037          1      24923           0    10086         0     9955         0    10075
## ... with 13 more variables: V2_3_mode <dbl>, V2_4_sum <dbl>, V2_4_mode <dbl>, V2_5_sum <dbl>,
##   V2_5_mode <dbl>, V3_sum <dbl>, V3_mean <dbl>, V3_var <dbl>, V4_sum <dbl>, V4_mean <dbl>,
##   V4_var <dbl>, V5_duration <dbl>, V5_recency <dbl>
```
# Contact

Compose a friendly e-mail to <Matthias.Bogaert@UGent.Be>.

