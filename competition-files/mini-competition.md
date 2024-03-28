Linear Regression Mini-competition
================

# Load necessary libraries

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.1.0 ──

    ## ✔ broom        1.0.5     ✔ recipes      1.0.6
    ## ✔ dials        1.2.0     ✔ rsample      1.1.1
    ## ✔ dplyr        1.1.2     ✔ tibble       3.2.1
    ## ✔ ggplot2      3.4.4     ✔ tidyr        1.3.0
    ## ✔ infer        1.0.4     ✔ tune         1.1.1
    ## ✔ modeldata    1.1.0     ✔ workflows    1.1.3
    ## ✔ parsnip      1.1.0     ✔ workflowsets 1.0.1
    ## ✔ purrr        1.0.1     ✔ yardstick    1.2.0

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ purrr::discard() masks scales::discard()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ✖ recipes::step()  masks stats::step()
    ## • Use tidymodels_prefer() to resolve common conflicts.

``` r
library(readxl)
library(dplyr)
library(ggplot2)
```

``` r
# Load data from the uploaded file
data_2016 <- read_excel("~/Preparations/STA 631/activity08-mini-competition/activity08-mini-competition/competition-files/data/pfi-data.xlsx", sheet = "curated 2016")
data_2019 <- read_excel("~/Preparations/STA 631/activity08-mini-competition/activity08-mini-competition/competition-files/data/pfi-data.xlsx", sheet = "curated 2019")
```

``` r
# Ensure all character variables are treated as factors
data_2016 <- data_2016 %>% mutate_if(is.character, as.factor)
data_2019 <- data_2019 %>% mutate_if(is.character, as.factor)
```

``` r
# Convert ALLGRADEX to a factor in both datasets if it's meant to be treated as categorical
data_2016 <- data_2016 %>% mutate(ALLGRADEX = as.factor(ALLGRADEX))
data_2019 <- data_2019 %>% mutate(ALLGRADEX = as.factor(ALLGRADEX))
```

``` r
# Combine datasets
data_combined <- bind_rows(data_2016, data_2019)
```

``` r
# Initial data exploration
glimpse(data_combined)
```

    ## Rows: 28,963
    ## Columns: 78
    ## $ BASMID       <dbl> 2.0161e+10, 2.0161e+10, 2.0161e+10, 2.0161e+10, 2.0161e+1…
    ## $ EDCPUB       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, …
    ## $ SPUBCHOIX    <dbl> 1, 2, 2, 1, 2, 1, 2, 1, 3, 2, 3, 3, 2, 3, 3, 2, 2, 3, 1, …
    ## $ SCONSIDR     <dbl> 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, …
    ## $ SEENJOY      <dbl> 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 1, 1, 4, 1, 2, 1, …
    ## $ SEGRADES     <dbl> 1, 2, 5, 1, 2, 1, 3, 1, 3, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, …
    ## $ SEABSNT      <dbl> 0, 0, 5, 1, 2, 3, 0, 4, 3, 8, 0, 4, 2, 2, 0, 8, 0, 6, 3, …
    ## $ SEFUTUREX    <dbl> 5, 6, 6, 5, 5, 4, 3, 5, 5, 5, 6, 4, 3, 6, 6, 5, 6, 6, 4, …
    ## $ SEGRADEQ     <dbl> 2, 3, 3, 1, 2, 1, 3, 2, 3, 2, 1, 3, 3, 1, 1, 1, 1, 2, 3, …
    ## $ FSSPORTX     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ FSVOL        <dbl> 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, …
    ## $ FSMTNG       <dbl> 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, …
    ## $ FSPTMTNG     <dbl> 1, 1, 1, 1, 2, 1, 2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 2, …
    ## $ FSATCNFN     <dbl> 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, …
    ## $ FSFUNDRS     <dbl> 1, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, …
    ## $ FSCOMMTE     <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ FSCOUNSLR    <dbl> 2, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2, 2, …
    ## $ FSFREQ       <dbl> 6, 10, 3, 10, 3, 1, 20, 3, 15, 1, 3, 3, 5, 1, 8, 5, 5, 6,…
    ## $ FSNOTESX     <dbl> 1, 1, 1, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 2, …
    ## $ FSMEMO       <dbl> 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, …
    ## $ FCSCHOOL     <dbl> 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 4, 1, 2, 1, …
    ## $ FCTEACHR     <dbl> 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 3, 1, 3, 1, …
    ## $ FCSTDS       <dbl> 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 4, 1, 2, 1, …
    ## $ FCORDER      <dbl> 1, 1, 1, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 4, 1, 2, 1, …
    ## $ FCSUPPRT     <dbl> 1, 1, 1, 2, 3, 3, 1, 3, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, …
    ## $ FHHOME       <dbl> 4, 3, 3, 4, 3, 5, 4, 3, 3, 2, 4, 3, 1, 3, 4, 2, 4, 3, 3, …
    ## $ FHWKHRS      <dbl> 10, 4, 4, 6, 6, -1, 10, 1, 4, 3, 10, 4, 0, 1, 5, 4, 8, 6,…
    ## $ FHAMOUNT     <dbl> 1, 1, 1, 1, 1, -1, 1, 3, 2, 1, 1, 1, 1, 2, 1, 3, 1, 1, 1,…
    ## $ FHCAMT       <dbl> 1, 1, 1, 2, 2, -1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1,…
    ## $ FHPLACE      <dbl> 1, 1, 1, 1, 2, -1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1,…
    ## $ FHCHECKX     <dbl> 4, 4, 4, 3, 1, -1, 4, 3, 4, 3, 4, 4, 2, 3, 4, 4, 4, 4, 4,…
    ## $ FHHELP       <dbl> 2, 2, 3, 1, 5, -1, 1, 1, 3, 2, 5, 3, 1, 1, 4, 2, 1, 4, 3,…
    ## $ FOSTORY2X    <dbl> 2, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, …
    ## $ FOCRAFTS     <dbl> 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, …
    ## $ FOGAMES      <dbl> 2, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, …
    ## $ FOBUILDX     <dbl> 1, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, …
    ## $ FOSPORT      <dbl> 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, …
    ## $ FORESPON     <dbl> 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, …
    ## $ FOHISTX      <dbl> 2, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, …
    ## $ FODINNERX    <dbl> 6, 5, 7, 5, 4, 0, 4, 7, 2, 5, 3, 3, 0, 4, 7, 3, 2, 7, 4, …
    ## $ FOLIBRAYX    <dbl> 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, …
    ## $ FOBOOKSTX    <dbl> 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, …
    ## $ HDHEALTH     <dbl> 2, 2, 1, 2, 1, 1, 2, 2, 4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, …
    ## $ CDOBMM       <dbl> 3, 7, 11, 1, 1, 11, 9, 3, 3, 8, 12, 9, 10, 1, 12, 1, 10, …
    ## $ CDOBYY       <dbl> 2007, 2001, 2008, 2001, 2000, 1999, 2000, 2001, 2007, 200…
    ## $ CSEX         <dbl> 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 2, 1, …
    ## $ CSPEAKX      <dbl> 2, 3, 2, 2, 2, 2, 2, 2, 5, 2, 2, 2, 2, 5, 2, 2, 2, 2, 2, …
    ## $ HHTOTALXX    <dbl> 4, 4, 5, 4, 4, 5, 6, 3, 5, 4, 3, 4, 3, 4, 4, 5, 4, 2, 4, …
    ## $ RELATION     <dbl> 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 6, 5, 1, 1, 1, 1, 2, 2, …
    ## $ P1REL        <dbl> 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 5, 1, 5, 1, 1, 1, 1, 1, 1, …
    ## $ P1SEX        <dbl> 2, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, …
    ## $ P1MRSTA      <dbl> 1, 1, 1, 1, 5, 3, 5, 1, 1, 4, 1, 5, 1, 1, 1, 1, 3, 3, 3, …
    ## $ P1EMPL       <dbl> 1, 1, 5, 1, 1, 1, 1, 1, 5, 1, 6, 5, 1, 3, 5, 1, 1, 1, 1, …
    ## $ P1HRSWK      <dbl> 15, 40, -1, 50, 45, 50, 40, 40, -1, 40, -1, -1, 40, -1, -…
    ## $ P1MTHSWRK    <dbl> 10, 12, 0, 12, 12, 12, 12, 12, 0, 12, 0, 0, 12, 0, 0, 12,…
    ## $ P1AGE        <dbl> 47, 46, 28, 51, 42, 49, 43, 55, 32, 47, 64, 31, 51, 43, 3…
    ## $ P2GUARD      <dbl> 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, …
    ## $ TTLHHINC     <dbl> 10, 6, 6, 9, 8, 4, 5, 9, 3, 7, 8, 3, 5, 2, 10, 8, 5, 8, 9…
    ## $ OWNRNTHB     <dbl> 2, 1, 2, 1, 1, 1, 1, 1, 3, 2, 1, 1, 2, 3, 1, 1, 1, 2, 2, …
    ## $ DSBLTY       <dbl> 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ HHPARN16X    <dbl> 1, 1, 1, 1, 1, 3, 2, 1, 1, 2, 4, 2, 4, 2, 1, 1, 2, 3, 3, …
    ## $ HHPARN16_BRD <dbl> 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, …
    ## $ NUMSIBSX     <dbl> 1, 1, 2, 1, 1, 1, 1, 0, 2, 2, 0, 0, 0, 1, 1, 2, 0, 0, 1, …
    ## $ PARGRADEX    <dbl> 4, 3, 4, 4, 3, 1, 3, 5, 2, 3, 3, 2, 2, 1, 4, 4, 3, 3, 2, …
    ## $ RACEETHN     <dbl> 4, 3, 3, 1, 3, 1, 3, 3, 3, 2, 1, 1, 1, 3, 3, 1, 3, 1, 1, …
    ## $ INTACC       <dbl> 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 2, 1, 1, 3, 1, 1, 1, 1, 1, …
    ## $ ALLGRADEX    <fct> 3, 9, 1, 9, 10, 10, 10, 9, 2, 8, 11, 2, 9, 8, K, 9, 8, 1,…
    ## $ CENREG       <dbl> 4, 3, 2, 3, 2, 2, 4, 4, 2, 2, 3, 4, 3, 4, 1, 3, 4, 4, 3, …
    ## $ ZIPLOCL      <dbl> 21, 13, 21, 21, 11, 41, 31, 12, 41, 21, 42, 21, 32, 11, 2…
    ## $ SCCHOICE     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ SCHLHRSWK    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ EINTNET      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ MOSTIMPT     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ INTNUM       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ CHLDNT       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ HHPARN19X    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ HHPARN19_BRD <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ RACEETH      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
summary(data_combined)
```

    ##      BASMID              EDCPUB        SPUBCHOIX         SCONSIDR     
    ##  Min.   :2.016e+10   Min.   :1.000   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.:2.016e+10   1st Qu.:1.000   1st Qu.: 1.000   1st Qu.: 1.000  
    ##  Median :2.019e+10   Median :1.000   Median : 2.000   Median : 2.000  
    ##  Mean   :2.018e+10   Mean   :1.112   Mean   : 1.922   Mean   : 1.655  
    ##  3rd Qu.:2.019e+10   3rd Qu.:1.000   3rd Qu.: 3.000   3rd Qu.: 2.000  
    ##  Max.   :2.019e+10   Max.   :2.000   Max.   : 3.000   Max.   : 2.000  
    ##                                                                       
    ##     SEENJOY         SEGRADES         SEABSNT         SEFUTUREX   
    ##  Min.   :-1.00   Min.   :-1.000   Min.   : -1.00   Min.   :1.00  
    ##  1st Qu.: 1.00   1st Qu.: 1.000   1st Qu.:  1.00   1st Qu.:4.00  
    ##  Median : 2.00   Median : 2.000   Median :  1.00   Median :5.00  
    ##  Mean   : 1.76   Mean   : 2.027   Mean   :  2.62   Mean   :4.94  
    ##  3rd Qu.: 2.00   3rd Qu.: 2.000   3rd Qu.:  3.00   3rd Qu.:6.00  
    ##  Max.   : 4.00   Max.   : 5.000   Max.   :364.00   Max.   :6.00  
    ##                                                                  
    ##     SEGRADEQ         FSSPORTX          FSVOL            FSMTNG      
    ##  Min.   :-1.000   Min.   :-1.000   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 1.000  
    ##  Median : 2.000   Median : 1.000   Median : 2.000   Median : 1.000  
    ##  Mean   : 2.052   Mean   : 1.191   Mean   : 1.565   Mean   : 1.135  
    ##  3rd Qu.: 3.000   3rd Qu.: 1.000   3rd Qu.: 2.000   3rd Qu.: 1.000  
    ##  Max.   : 5.000   Max.   : 2.000   Max.   : 2.000   Max.   : 2.000  
    ##                                                                     
    ##     FSPTMTNG         FSATCNFN         FSFUNDRS         FSCOMMTE     
    ##  Min.   :-1.000   Min.   :-1.000   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 2.000  
    ##  Median : 2.000   Median : 1.000   Median : 1.000   Median : 2.000  
    ##  Mean   : 1.538   Mean   : 1.255   Mean   : 1.393   Mean   : 1.859  
    ##  3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.: 2.000  
    ##  Max.   : 2.000   Max.   : 2.000   Max.   : 2.000   Max.   : 2.000  
    ##                                                                     
    ##    FSCOUNSLR         FSFREQ          FSNOTESX         FSMEMO      
    ##  Min.   :-1.00   Min.   :-1.000   Min.   :-1.00   Min.   :-1.000  
    ##  1st Qu.: 1.00   1st Qu.: 3.000   1st Qu.: 1.00   1st Qu.: 1.000  
    ##  Median : 2.00   Median : 5.000   Median : 1.00   Median : 1.000  
    ##  Mean   : 1.63   Mean   : 7.406   Mean   : 1.35   Mean   : 1.093  
    ##  3rd Qu.: 2.00   3rd Qu.:10.000   3rd Qu.: 2.00   3rd Qu.: 1.000  
    ##  Max.   : 2.00   Max.   :99.000   Max.   : 2.00   Max.   : 2.000  
    ##                                                                   
    ##     FCSCHOOL         FCTEACHR          FCSTDS         FCORDER      
    ##  Min.   :-1.000   Min.   :-1.000   Min.   :-1.00   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 1.00   1st Qu.: 1.000  
    ##  Median : 1.000   Median : 1.000   Median : 1.00   Median : 1.000  
    ##  Mean   : 1.463   Mean   : 1.478   Mean   : 1.48   Mean   : 1.526  
    ##  3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.: 2.00   3rd Qu.: 2.000  
    ##  Max.   : 4.000   Max.   : 4.000   Max.   : 4.00   Max.   : 4.000  
    ##                                                                    
    ##     FCSUPPRT          FHHOME          FHWKHRS         FHAMOUNT     
    ##  Min.   :-1.000   Min.   :-1.000   Min.   :-1.00   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.: 2.00   1st Qu.: 1.000  
    ##  Median : 1.000   Median : 3.000   Median : 4.00   Median : 1.000  
    ##  Mean   : 1.603   Mean   : 3.186   Mean   : 5.44   Mean   : 1.222  
    ##  3rd Qu.: 2.000   3rd Qu.: 4.000   3rd Qu.: 8.00   3rd Qu.: 1.000  
    ##  Max.   : 4.000   Max.   : 6.000   Max.   :75.00   Max.   : 3.000  
    ##                                                                    
    ##      FHCAMT          FHPLACE          FHCHECKX          FHHELP      
    ##  Min.   :-1.000   Min.   :-1.000   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.: 1.000  
    ##  Median : 1.000   Median : 1.000   Median : 4.000   Median : 2.000  
    ##  Mean   : 1.249   Mean   : 1.027   Mean   : 3.124   Mean   : 2.253  
    ##  3rd Qu.: 2.000   3rd Qu.: 1.000   3rd Qu.: 4.000   3rd Qu.: 3.000  
    ##  Max.   : 3.000   Max.   : 3.000   Max.   : 4.000   Max.   : 5.000  
    ##                                                                     
    ##    FOSTORY2X        FOCRAFTS        FOGAMES         FOBUILDX    
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :1.000   Median :2.000   Median :1.000   Median :1.000  
    ##  Mean   :1.418   Mean   :1.573   Mean   :1.488   Mean   :1.444  
    ##  3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000  
    ##  Max.   :2.000   Max.   :2.000   Max.   :2.000   Max.   :2.000  
    ##                                                                 
    ##     FOSPORT         FORESPON        FOHISTX        FODINNERX    
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :0.000  
    ##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:3.000  
    ##  Median :1.000   Median :1.000   Median :1.000   Median :5.000  
    ##  Mean   :1.298   Mean   :1.294   Mean   :1.461   Mean   :4.765  
    ##  3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:7.000  
    ##  Max.   :2.000   Max.   :2.000   Max.   :2.000   Max.   :7.000  
    ##                                                                 
    ##    FOLIBRAYX       FOBOOKSTX        HDHEALTH         CDOBMM      
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   : 1.000  
    ##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.: 4.000  
    ##  Median :2.000   Median :2.000   Median :1.000   Median : 7.000  
    ##  Mean   :1.675   Mean   :1.656   Mean   :1.572   Mean   : 6.548  
    ##  3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.: 9.000  
    ##  Max.   :2.000   Max.   :2.000   Max.   :5.000   Max.   :12.000  
    ##                                                                  
    ##      CDOBYY          CSEX          CSPEAKX        HHTOTALXX     
    ##  Min.   :1995   Min.   :1.000   Min.   :1.000   Min.   : 2.000  
    ##  1st Qu.:2001   1st Qu.:1.000   1st Qu.:2.000   1st Qu.: 3.000  
    ##  Median :2004   Median :1.000   Median :2.000   Median : 4.000  
    ##  Mean   :2005   Mean   :1.483   Mean   :2.309   Mean   : 4.039  
    ##  3rd Qu.:2008   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.: 5.000  
    ##  Max.   :2015   Max.   :2.000   Max.   :6.000   Max.   :10.000  
    ##                                                                 
    ##     RELATION          P1REL           P1SEX          P1MRSTA     
    ##  Min.   : 1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.: 1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median : 1.000   Median :1.000   Median :2.000   Median :1.000  
    ##  Mean   : 1.718   Mean   :1.289   Mean   :1.675   Mean   :1.801  
    ##  3rd Qu.: 2.000   3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.:3.000  
    ##  Max.   :11.000   Max.   :6.000   Max.   :2.000   Max.   :5.000  
    ##                                                                  
    ##      P1EMPL         P1HRSWK        P1MTHSWRK          P1AGE      
    ##  Min.   :1.000   Min.   :-1.00   Min.   : 0.000   Min.   :15.00  
    ##  1st Qu.:1.000   1st Qu.:20.00   1st Qu.:10.000   1st Qu.:39.00  
    ##  Median :1.000   Median :40.00   Median :12.000   Median :45.00  
    ##  Mean   :1.931   Mean   :31.88   Mean   : 9.536   Mean   :44.84  
    ##  3rd Qu.:2.000   3rd Qu.:42.00   3rd Qu.:12.000   3rd Qu.:50.00  
    ##  Max.   :7.000   Max.   :80.00   Max.   :12.000   Max.   :90.00  
    ##                                                                  
    ##     P2GUARD         TTLHHINC         OWNRNTHB         DSBLTY    
    ##  Min.   :1.000   Min.   : 1.000   Min.   :1.000   Min.   :1.00  
    ##  1st Qu.:1.000   1st Qu.: 4.000   1st Qu.:1.000   1st Qu.:2.00  
    ##  Median :1.000   Median : 8.000   Median :1.000   Median :2.00  
    ##  Mean   :1.261   Mean   : 6.949   Mean   :1.259   Mean   :1.79  
    ##  3rd Qu.:2.000   3rd Qu.: 9.000   3rd Qu.:1.000   3rd Qu.:2.00  
    ##  Max.   :2.000   Max.   :12.000   Max.   :3.000   Max.   :2.00  
    ##                                                                 
    ##    HHPARN16X      HHPARN16_BRD      NUMSIBSX       PARGRADEX    
    ##  Min.   :1.000   Min.   :1.00    Min.   :0.000   Min.   :1.000  
    ##  1st Qu.:1.000   1st Qu.:1.00    1st Qu.:0.000   1st Qu.:3.000  
    ##  Median :1.000   Median :1.00    Median :1.000   Median :4.000  
    ##  Mean   :1.457   Mean   :1.26    Mean   :1.023   Mean   :3.588  
    ##  3rd Qu.:2.000   3rd Qu.:2.00    3rd Qu.:1.000   3rd Qu.:5.000  
    ##  Max.   :4.000   Max.   :2.00    Max.   :7.000   Max.   :5.000  
    ##  NA's   :15500   NA's   :15500                                  
    ##     RACEETHN         INTACC       ALLGRADEX         CENREG         ZIPLOCL     
    ##  Min.   :1.000   Min.   :1.00   12     : 2713   Min.   :1.000   Min.   :11.00  
    ##  1st Qu.:1.000   1st Qu.:1.00   11     : 2604   1st Qu.:2.000   1st Qu.:13.00  
    ##  Median :1.000   Median :1.00   10     : 2490   Median :2.000   Median :21.00  
    ##  Mean   :1.893   Mean   :1.17   9      : 2315   Mean   :2.527   Mean   :22.76  
    ##  3rd Qu.:3.000   3rd Qu.:1.00   8      : 2204   3rd Qu.:3.000   3rd Qu.:31.00  
    ##  Max.   :4.000   Max.   :4.00   7      : 2040   Max.   :4.000   Max.   :43.00  
    ##  NA's   :15500                  (Other):14597                                  
    ##     SCCHOICE        SCHLHRSWK        EINTNET          MOSTIMPT     
    ##  Min.   :-1.000   Min.   :-1.00   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.: 1.000   1st Qu.: 4.00   1st Qu.: 4.000   1st Qu.:-1.000  
    ##  Median : 1.000   Median : 4.00   Median : 4.000   Median :-1.000  
    ##  Mean   : 1.366   Mean   : 3.77   Mean   : 3.921   Mean   :-0.515  
    ##  3rd Qu.: 2.000   3rd Qu.: 4.00   3rd Qu.: 4.000   3rd Qu.:-1.000  
    ##  Max.   : 2.000   Max.   : 4.00   Max.   : 4.000   Max.   :14.000  
    ##  NA's   :13463    NA's   :13463   NA's   :13463    NA's   :13463   
    ##      INTNUM          CHLDNT        HHPARN19X      HHPARN19_BRD  
    ##  Min.   :-1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:-1.00   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :-1.00   Median :1.000   Median :1.000   Median :1.000  
    ##  Mean   :-0.82   Mean   :1.737   Mean   :1.449   Mean   :1.262  
    ##  3rd Qu.:-1.00   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000  
    ##  Max.   :16.00   Max.   :5.000   Max.   :4.000   Max.   :2.000  
    ##  NA's   :13463   NA's   :13463   NA's   :13463   NA's   :13463  
    ##     RACEETH     
    ##  Min.   :1.000  
    ##  1st Qu.:1.000  
    ##  Median :1.000  
    ##  Mean   :1.996  
    ##  3rd Qu.:3.000  
    ##  Max.   :5.000  
    ##  NA's   :13463

``` r
# Filter out 'Valid Skip' responses if they are not relevant to the analysis
data_combined <- data_combined %>%
  filter(SCCHOICE != -1)

# Convert SCCHOICE to a factor, ensuring it accurately represents a categorical variable
data_combined <- data_combined %>%
  mutate(SCCHOICE = factor(SCCHOICE, levels = c(1, 2), labels = c("Yes", "No")))
```

``` r
# Exploratory Data Analysis (EDA) 
# Plotting distributions of numerical variables
numerical_vars <- select_if(data_combined, is.numeric)
for (var in names(numerical_vars)) {
  print(ggplot(data_combined, aes_string(x = var)) + 
          geom_histogram(bins = 30, fill = "skyblue") + 
          labs(title = paste("Distribution of", var)))
}
```

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](mini-competition_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-29.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-30.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-31.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-32.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-33.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-34.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-35.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-36.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-37.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-38.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-39.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-40.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-41.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-42.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-43.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-44.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-45.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-46.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-47.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-48.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-49.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-50.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-51.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-52.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-53.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-54.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-55.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-56.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-57.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-58.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-59.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-60.png)<!-- -->

    ## Warning: Removed 15498 rows containing non-finite values (`stat_bin()`).

![](mini-competition_files/figure-gfm/unnamed-chunk-8-61.png)<!-- -->

    ## Warning: Removed 15498 rows containing non-finite values (`stat_bin()`).

![](mini-competition_files/figure-gfm/unnamed-chunk-8-62.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-63.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-64.png)<!-- -->

    ## Warning: Removed 15498 rows containing non-finite values (`stat_bin()`).

![](mini-competition_files/figure-gfm/unnamed-chunk-8-65.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-66.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-67.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-68.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-69.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-70.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-71.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-72.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-73.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-74.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-75.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-8-76.png)<!-- -->

``` r
# Exploratory analysis of categorical variables
categorical_vars <- select_if(data_combined, is.factor)
for (var in names(categorical_vars)) {
  print(ggplot(data_combined, aes_string(x = var, fill = var)) + 
          geom_bar() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Distribution of", var)))
}
```

![](mini-competition_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](mini-competition_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
# Correlation analysis for numerical variables
# Before performing correlation analysis, ensure there are no columns with only NA values
# and that there is at least one pair of complete observations

# Select numeric variables and remove any columns that are completely NA
clean_numeric_data <- select_if(data_combined, is.numeric) %>%
  select_if(~!all(is.na(.)))
```

``` r
# Perform correlation analysis on cleaned numeric data
correlation_analysis <- cor(clean_numeric_data, use = "pairwise.complete.obs")
```

``` r
# Print the correlation matrix
print(correlation_analysis)
```

    ##                     BASMID        EDCPUB     SPUBCHOIX      SCONSIDR
    ## BASMID        1.000000e+00 -0.0161104259 -0.0039289440 -0.0101924716
    ## EDCPUB       -1.611043e-02  1.0000000000  0.0963288783 -0.1527417835
    ## SPUBCHOIX    -3.928944e-03  0.0963288783  1.0000000000  0.1286104965
    ## SCONSIDR     -1.019247e-02 -0.1527417835  0.1286104965  1.0000000000
    ## SEENJOY       6.454055e-03 -0.0865135092  0.0231706235 -0.0500793217
    ## SEGRADES      6.476128e-03 -0.0427274921  0.0464006286 -0.0175527181
    ## SEABSNT       5.111371e-03 -0.0357511437  0.0076290235 -0.0239871126
    ## SEFUTUREX     9.728239e-03  0.0839484580 -0.0461361446 -0.1024821738
    ## SEGRADEQ     -1.057799e-02 -0.0775113551  0.0576308059  0.0127778531
    ## FSSPORTX      2.253699e-03 -0.0859702895  0.0543632480  0.0164262452
    ## FSVOL         8.405995e-03 -0.1550755416  0.0286033834  0.0465520580
    ## FSMTNG       -4.238747e-03 -0.0549198561  0.0366370751  0.0348156999
    ## FSPTMTNG     -5.542410e-03 -0.0878507141 -0.0148214232  0.0398388441
    ## FSATCNFN     -9.009145e-03 -0.0672288856 -0.0006960165  0.0530653979
    ## FSFUNDRS     -8.018386e-03 -0.1138328954  0.0503163477  0.0233555433
    ## FSCOMMTE      6.703669e-03 -0.1187437556  0.0315868694  0.0143331012
    ## FSCOUNSLR     9.965895e-03 -0.0102655912  0.0233631938  0.0524582807
    ## FSFREQ       -2.246750e-03  0.1398561167 -0.0371879046 -0.0328192210
    ## FSNOTESX     -3.232716e-03 -0.0552715377  0.0245799738  0.0563297215
    ## FSMEMO       -7.953708e-03 -0.0465494098  0.0468706582  0.0401856177
    ## FCSCHOOL      7.607920e-03 -0.1014457851  0.0366050016 -0.1246402602
    ## FCTEACHR     -4.717567e-03 -0.0783002424  0.0147673677 -0.0826790792
    ## FCSTDS        6.820938e-03 -0.1149123537  0.0381797263 -0.0988583451
    ## FCORDER       3.482812e-04 -0.1183307585  0.0221713133 -0.0832242387
    ## FCSUPPRT      1.003030e-02 -0.1050784225  0.0251392769 -0.0718835937
    ## FHHOME       -8.159765e-03  0.0890638129  0.0010871471 -0.0596840570
    ## FHWKHRS      -1.334782e-03  0.0659940888 -0.0297604660 -0.0561133222
    ## FHAMOUNT      8.828652e-03 -0.0462565668  0.0032264282 -0.0391254018
    ## FHCAMT        1.286140e-02 -0.0222209723 -0.0036007619 -0.0305941318
    ## FHPLACE       1.281026e-02  0.0059959001  0.0019898895  0.0181848264
    ## FHCHECKX      9.625760e-03 -0.0337665691  0.0061026030  0.0021512518
    ## FHHELP        3.106529e-03  0.0014235999  0.0235860004 -0.0001294849
    ## FOSTORY2X    -4.991115e-03 -0.0075720810  0.0273906514  0.0694553189
    ## FOCRAFTS     -8.935672e-03 -0.0041785436 -0.0238311377  0.0424454465
    ## FOGAMES      -2.187801e-03 -0.0046811077  0.0005617684  0.0346682019
    ## FOBUILDX      1.307455e-03 -0.0062262378  0.0054200448  0.0364877338
    ## FOSPORT       4.968994e-03 -0.0290929848 -0.0033430940  0.0260706760
    ## FORESPON      4.116791e-03 -0.0012359223  0.0354715447  0.0443352982
    ## FOHISTX       4.505016e-04 -0.0053282417  0.0186145277  0.0534354354
    ## FODINNERX    -3.108321e-03 -0.0173845779 -0.0070138229 -0.0125007931
    ## FOLIBRAYX    -4.329979e-03 -0.0094141934  0.0081516163  0.0513102031
    ## FOBOOKSTX    -6.922754e-03 -0.0596466988  0.0268549667  0.0853655719
    ## HDHEALTH     -2.009431e-03 -0.0640274656  0.0243570938 -0.0025658075
    ## CDOBMM       -5.325161e-03  0.0059941518 -0.0010926092 -0.0050340992
    ## CDOBYY        6.900990e-03  0.0220397203  0.0539025159 -0.0408887437
    ## CSEX         -1.157492e-02  0.0058801478 -0.0185849967 -0.0171504031
    ## CSPEAKX      -3.926785e-05  0.0027888270  0.0592737943  0.0054954614
    ## HHTOTALXX    -5.806150e-03  0.0068304872  0.0170971725  0.0590026393
    ## RELATION      2.739114e-03 -0.0296102954  0.0365983954  0.0528141958
    ## P1REL        -1.645372e-02 -0.0281019058  0.0120186779  0.0449591744
    ## P1SEX         6.888878e-04 -0.0366319773 -0.0146545393  0.0002981851
    ## P1MRSTA       1.420010e-03 -0.0662814606  0.0343432677 -0.0050954160
    ## P1EMPL       -7.231868e-03  0.0032703614  0.0063575397  0.0150567939
    ## P1HRSWK      -1.845027e-03  0.0176797443 -0.0064592318 -0.0292504259
    ## P1MTHSWRK     5.903029e-03  0.0006353014 -0.0110816222 -0.0198410548
    ## P1AGE        -9.821285e-03  0.0519103001 -0.0372701683  0.0264042541
    ## P2GUARD      -3.701978e-03 -0.0547507723  0.0260204890  0.0097779013
    ## TTLHHINC     -1.357744e-03  0.1478221199 -0.0667381518 -0.0681825623
    ## OWNRNTHB     -3.992284e-03 -0.0635391134  0.0391294584 -0.0106137014
    ## DSBLTY       -3.026869e-03  0.0115310557  0.0139697409  0.0545772116
    ## NUMSIBSX     -6.246738e-03  0.0136564317 -0.0018310754  0.0517786444
    ## PARGRADEX    -2.425216e-03  0.1391758238 -0.0968147195 -0.1138377987
    ## INTACC        1.067161e-02 -0.0051048765  0.0422991682  0.0288536441
    ## CENREG       -8.941925e-03 -0.0130317107 -0.1027822294 -0.0268956390
    ## ZIPLOCL       6.343230e-03 -0.0627551626  0.0417699204  0.1180519573
    ## SCHLHRSWK    -2.854224e-03  0.0267652943 -0.0242626692 -0.0163362660
    ## EINTNET       6.965510e-03  0.0040378836  0.0087106212  0.0405515641
    ## MOSTIMPT     -8.528477e-03 -0.0039902222 -0.0182377115 -0.0502352251
    ## INTNUM       -8.079452e-03 -0.0006304665 -0.0134424200 -0.0455400169
    ## CHLDNT        1.183016e-02  0.0137364763  0.0508432483  0.0200931382
    ## HHPARN19X    -5.176147e-03 -0.0542493112  0.0304306429  0.0439889378
    ## HHPARN19_BRD -3.701978e-03 -0.0547507723  0.0260204890  0.0097779013
    ## RACEETH       2.117657e-03 -0.0374313087  0.0268895345 -0.0372235010
    ##                    SEENJOY     SEGRADES      SEABSNT    SEFUTUREX     SEGRADEQ
    ## BASMID        0.0064540547  0.006476128  0.005111371  0.009728239 -0.010577985
    ## EDCPUB       -0.0865135092 -0.042727492 -0.035751144  0.083948458 -0.077511355
    ## SPUBCHOIX     0.0231706235  0.046400629  0.007629024 -0.046136145  0.057630806
    ## SCONSIDR     -0.0500793217 -0.017552718 -0.023987113 -0.102482174  0.012777853
    ## SEENJOY       1.0000000000  0.099191983  0.191046934 -0.224748451  0.371290196
    ## SEGRADES      0.0991919827  1.000000000  0.096473293 -0.206838750  0.413764935
    ## SEABSNT       0.1910469343  0.096473293  1.000000000 -0.141274234  0.152874541
    ## SEFUTUREX    -0.2247484512 -0.206838750 -0.141274234  1.000000000 -0.405618982
    ## SEGRADEQ      0.3712901961  0.413764935  0.152874541 -0.405618982  1.000000000
    ## FSSPORTX      0.1171643683  0.047171982  0.056939333 -0.144266572  0.129083462
    ## FSVOL         0.1185430482  0.002434748  0.046072539 -0.131949609  0.131752399
    ## FSMTNG        0.0858114454  0.011690022  0.051714338 -0.104924397  0.071765748
    ## FSPTMTNG      0.0837374039 -0.019743209  0.026952642 -0.048669004  0.038455381
    ## FSATCNFN      0.0994063767 -0.133525829  0.014956741 -0.012352541 -0.016752183
    ## FSFUNDRS      0.1306700840  0.031653563  0.060087798 -0.113553459  0.123413184
    ## FSCOMMTE      0.0405509206  0.042452611  0.002142335 -0.052481681  0.066769266
    ## FSCOUNSLR    -0.0909278917 -0.005657315 -0.095357764  0.048684687 -0.111193241
    ## FSFREQ       -0.0502450182 -0.051000579 -0.013096643  0.096954475 -0.087771112
    ## FSNOTESX      0.0151705813 -0.093617372 -0.048084135  0.007673247 -0.067388037
    ## FSMEMO        0.0483961861 -0.001358180  0.027160971 -0.107014907  0.050361130
    ## FCSCHOOL      0.3817322283  0.091476618  0.124860960 -0.104948402  0.259255707
    ## FCTEACHR      0.3367971007  0.053737387  0.100487516 -0.076991737  0.234108829
    ## FCSTDS        0.3244478573  0.088695225  0.094849466 -0.098621698  0.239320705
    ## FCORDER       0.3231938389  0.045676888  0.107927573 -0.087015881  0.187237714
    ## FCSUPPRT      0.3200194817  0.026395686  0.090670761 -0.083824225  0.185899072
    ## FHHOME       -0.0813784483  0.018698527 -0.037350805  0.085127042 -0.093707968
    ## FHWKHRS      -0.0220201507 -0.222159013 -0.042124808  0.206123274 -0.174874382
    ## FHAMOUNT      0.0718234568 -0.098434739 -0.011925026  0.095766630  0.010256477
    ## FHCAMT        0.1012125883 -0.108733827  0.001969538  0.095039277  0.009132258
    ## FHPLACE      -0.0001975215 -0.119670849 -0.029705156  0.107588394 -0.062695316
    ## FHCHECKX     -0.0955759567 -0.048036549 -0.087003250  0.129066586 -0.043040335
    ## FHHELP       -0.0404927292 -0.064605663 -0.022481366  0.101729326 -0.059589487
    ## FOSTORY2X     0.1038264659 -0.064197573  0.030943449 -0.090493718  0.062257500
    ## FOCRAFTS      0.1717623701 -0.154208094  0.032316281 -0.050494066  0.051998767
    ## FOGAMES       0.1379712640 -0.078227072  0.043214573 -0.056537687  0.087685639
    ## FOBUILDX      0.1008841311 -0.049831542  0.040430599 -0.030212171  0.048305393
    ## FOSPORT       0.1460819644 -0.019063802  0.080620694 -0.113504867  0.092908241
    ## FORESPON      0.0115586171  0.073699369  0.031479295 -0.065428452 -0.006208376
    ## FOHISTX       0.0658109887  0.058659029  0.007862655 -0.088753089  0.052732850
    ## FODINNERX    -0.1418071994  0.023293652 -0.066605772  0.047991866 -0.076512688
    ## FOLIBRAYX     0.1377415399 -0.053417576  0.044671513 -0.108298235  0.095995500
    ## FOBOOKSTX     0.1069171186  0.028586095  0.039860009 -0.145416674  0.134638055
    ## HDHEALTH      0.2249560699  0.124244257  0.232666309 -0.224903472  0.283057196
    ## CDOBMM       -0.0038998057 -0.016171316 -0.008988130  0.004757805 -0.013483002
    ## CDOBYY       -0.2266207572  0.303848188 -0.090825118  0.097076337 -0.034489554
    ## CSEX         -0.0918688186 -0.088345367  0.018671443  0.117079513 -0.152762288
    ## CSPEAKX      -0.0387455651 -0.016388057 -0.016004388  0.080373853 -0.045612172
    ## HHTOTALXX    -0.0265077447 -0.033301098 -0.018145168 -0.008056141 -0.033217432
    ## RELATION      0.0070524686  0.023405637  0.006396719 -0.101203404  0.049778560
    ## P1REL         0.0260407522  0.043327640  0.028948323 -0.156488388  0.123718933
    ## P1SEX         0.0615222151  0.065966194  0.046945091 -0.050634637  0.108858163
    ## P1MRSTA       0.0429496825  0.087788687  0.064530572 -0.103157754  0.142732854
    ## P1EMPL        0.0198067688  0.046420701  0.087272539 -0.108120149  0.075062135
    ## P1HRSWK      -0.0135864923 -0.062029800 -0.082694313  0.102272065 -0.073281010
    ## P1MTHSWRK    -0.0201053803 -0.060165342 -0.091338511  0.102442304 -0.072115068
    ## P1AGE         0.0530077648 -0.134862072  0.026038699 -0.005721267 -0.018374264
    ## P2GUARD       0.0594628672  0.057144326  0.068123845 -0.095173236  0.126619959
    ## TTLHHINC     -0.0539523450 -0.089504172 -0.093105694  0.261022047 -0.203229980
    ## OWNRNTHB      0.0037330496  0.083230108  0.065465754 -0.084654610  0.100688747
    ## DSBLTY       -0.1588061011 -0.171139628 -0.118773455  0.239805593 -0.308013440
    ## NUMSIBSX     -0.0147692648 -0.051398781 -0.015601009 -0.006757323 -0.030710436
    ## PARGRADEX    -0.0658437004 -0.082671750 -0.090004739  0.358226046 -0.209573584
    ## INTACC        0.0049632471  0.038099048  0.046412053 -0.162857787  0.083025941
    ## CENREG       -0.0166390318  0.044116001  0.019079824 -0.024516076  0.016315708
    ## ZIPLOCL       0.0582215335 -0.030583437  0.017428787 -0.159278244  0.043757612
    ## SCHLHRSWK    -0.0091884986 -0.029757478 -0.030196407  0.072705619 -0.052520028
    ## EINTNET      -0.0391258727  0.022630209 -0.042398002  0.042539442 -0.008822985
    ## MOSTIMPT      0.0381272206 -0.014297645  0.034318211 -0.032295790  0.010425726
    ## INTNUM        0.0438863424 -0.016557184  0.048904058 -0.045190744  0.015816994
    ## CHLDNT        0.0274105172  0.210808362  0.030861798 -0.198519942  0.151081561
    ## HHPARN19X     0.0504749016  0.061276966  0.065673460 -0.157728300  0.141998366
    ## HHPARN19_BRD  0.0594628672  0.057144326  0.068123845 -0.095173236  0.126619959
    ## RACEETH      -0.0572513550  0.010456283 -0.013610239  0.092660245 -0.013517444
    ##                  FSSPORTX        FSVOL       FSMTNG      FSPTMTNG      FSATCNFN
    ## BASMID        0.002253699  0.008405995 -0.004238747 -0.0055424100 -0.0090091447
    ## EDCPUB       -0.085970290 -0.155075542 -0.054919856 -0.0878507141 -0.0672288856
    ## SPUBCHOIX     0.054363248  0.028603383  0.036637075 -0.0148214232 -0.0006960165
    ## SCONSIDR      0.016426245  0.046552058  0.034815700  0.0398388441  0.0530653979
    ## SEENJOY       0.117164368  0.118543048  0.085811445  0.0837374039  0.0994063767
    ## SEGRADES      0.047171982  0.002434748  0.011690022 -0.0197432085 -0.1335258289
    ## SEABSNT       0.056939333  0.046072539  0.051714338  0.0269526421  0.0149567412
    ## SEFUTUREX    -0.144266572 -0.131949609 -0.104924397 -0.0486690038 -0.0123525406
    ## SEGRADEQ      0.129083462  0.131752399  0.071765748  0.0384553809 -0.0167521825
    ## FSSPORTX      1.000000000  0.456777183  0.464712752  0.3263619551  0.3382152848
    ## FSVOL         0.456777183  1.000000000  0.387193745  0.4049581084  0.3552040887
    ## FSMTNG        0.464712752  0.387193745  1.000000000  0.3886468208  0.4321958055
    ## FSPTMTNG      0.326361955  0.404958108  0.388646821  1.0000000000  0.4128983553
    ## FSATCNFN      0.338215285  0.355204089  0.432195805  0.4128983553  1.0000000000
    ## FSFUNDRS      0.443991474  0.481879628  0.406231139  0.3344861188  0.3221077173
    ## FSCOMMTE      0.381081679  0.513685641  0.375034824  0.4453981426  0.3233450053
    ## FSCOUNSLR     0.238803995  0.257670765  0.275937368  0.3078921476  0.2915397595
    ## FSFREQ       -0.173225073 -0.256247193 -0.105409179 -0.1183192996 -0.0538579033
    ## FSNOTESX      0.173109521  0.178207886  0.177674595  0.1599987124  0.2349498464
    ## FSMEMO        0.247361090  0.204693269  0.264812480  0.1412315143  0.2104042671
    ## FCSCHOOL      0.160647301  0.171461211  0.143085185  0.1133171290  0.1189002479
    ## FCTEACHR      0.137280692  0.164425691  0.131844511  0.1196362984  0.1481818735
    ## FCSTDS        0.146745878  0.162174333  0.141178565  0.1091217032  0.1071213039
    ## FCORDER       0.121124500  0.146476805  0.130366008  0.1121996900  0.1275069378
    ## FCSUPPRT      0.147485288  0.177463790  0.153484196  0.1464405121  0.1870593071
    ## FHHOME        0.062577653  0.026854041  0.035409430  0.0213957515  0.0321054600
    ## FHWKHRS      -0.014688942 -0.002463269 -0.013066071 -0.0189365851  0.0932578728
    ## FHAMOUNT      0.015205479  0.058464372  0.045578987  0.0444159877  0.0798806840
    ## FHCAMT       -0.003464505  0.031554333  0.027794059  0.0363119853  0.0685344274
    ## FHPLACE       0.006590202  0.045318360  0.048635430  0.0711664531  0.0822615351
    ## FHCHECKX     -0.045684658 -0.035262557 -0.047480932 -0.0471591045 -0.0721801400
    ## FHHELP        0.019343764  0.014830390  0.057181183  0.0079456404  0.0535284120
    ## FOSTORY2X     0.094201587  0.126141142  0.098765968  0.0850279391  0.1243281539
    ## FOCRAFTS      0.093695475  0.159561932  0.085982937  0.1464786370  0.1939115492
    ## FOGAMES       0.105251305  0.127626721  0.100298991  0.1123269673  0.1650544253
    ## FOBUILDX      0.111116518  0.134937825  0.107462486  0.1193294033  0.1342189729
    ## FOSPORT       0.139425436  0.159530986  0.123011555  0.1349057136  0.1341244696
    ## FORESPON      0.034819548  0.025638946  0.040706672  0.0544320070  0.0075932309
    ## FOHISTX       0.012855323  0.013779895  0.030586875  0.0941472357  0.0213901208
    ## FODINNERX    -0.049865303 -0.060497511 -0.070682961 -0.0674292969 -0.0984571972
    ## FOLIBRAYX     0.063061629  0.133698325  0.068670714  0.0982786726  0.1294076644
    ## FOBOOKSTX     0.092509674  0.119622248  0.087187026  0.0823018372  0.0939630201
    ## HDHEALTH      0.114574878  0.105291552  0.074456767  0.0462414036  0.0266420664
    ## CDOBMM        0.011291250  0.002470139  0.004527002  0.0049946953  0.0083354227
    ## CDOBYY       -0.102850329 -0.158642356 -0.115758809 -0.1107821170 -0.2777970385
    ## CSEX         -0.045521958 -0.041282455 -0.024676198 -0.0046045492  0.0098089896
    ## CSPEAKX       0.088566930  0.049504216  0.067646073 -0.0377633180  0.0199403280
    ## HHTOTALXX    -0.013065315 -0.012316987 -0.016604427 -0.0167902285 -0.0170705663
    ## RELATION      0.042804528  0.061320638  0.064321057 -0.0185261054 -0.0065615212
    ## P1REL         0.039873216  0.059727453  0.026063223 -0.0109118040 -0.0079451283
    ## P1SEX         0.033374167  0.038503627 -0.008472373  0.0507908864  0.0276420538
    ## P1MRSTA       0.074815446  0.095839795  0.048731735 -0.0017100931 -0.0172764716
    ## P1EMPL        0.033177676  0.013618481  0.016456288 -0.0322054235 -0.0069500571
    ## P1HRSWK      -0.053637156 -0.027185809 -0.021704198  0.0071642763 -0.0045595789
    ## P1MTHSWRK    -0.038511300 -0.017077383 -0.014840416  0.0290153202  0.0026074596
    ## P1AGE         0.010218389  0.026481459  0.016759762  0.0080916569  0.0915254166
    ## P2GUARD       0.070730835  0.085177106  0.057656384  0.0034514541  0.0070254310
    ## TTLHHINC     -0.158437740 -0.167985387 -0.105750727  0.0070747798 -0.0125547717
    ## OWNRNTHB      0.090311323  0.083076374  0.049453367 -0.0171231990 -0.0018777878
    ## DSBLTY       -0.034996761 -0.037013776  0.003413318 -0.0039200434  0.0680558297
    ## NUMSIBSX     -0.012221377 -0.009353871 -0.012144088  0.0003902609  0.0026208850
    ## PARGRADEX    -0.155624724 -0.169694757 -0.128818774  0.0037715340 -0.0527185787
    ## INTACC        0.065568553  0.057199945  0.051689878 -0.0212124967 -0.0059435888
    ## CENREG        0.001187694 -0.023070193  0.003417851  0.0349426070 -0.0290107914
    ## ZIPLOCL      -0.036144471  0.003131802 -0.001092676  0.0474815883  0.0228383834
    ## SCHLHRSWK    -0.038441572 -0.025116023 -0.029465202  0.0505900451  0.0079748823
    ## EINTNET       0.014013317  0.037287657  0.001201492  0.0558058608  0.0045543635
    ## MOSTIMPT     -0.011367189 -0.024905131 -0.001423085 -0.0352803156 -0.0087577727
    ## INTNUM       -0.008467623 -0.030213025  0.001149358 -0.0504021882  0.0002988378
    ## CHLDNT        0.044637497  0.025743735  0.037936585  0.0299320398 -0.0529960106
    ## HHPARN19X     0.063150244  0.090118265  0.057719750 -0.0106220782 -0.0037055362
    ## HHPARN19_BRD  0.070730835  0.085177106  0.057656384  0.0034514541  0.0070254310
    ## RACEETH       0.070600384  0.041753905  0.042589270 -0.0540340053  0.0066377949
    ##                  FSFUNDRS      FSCOMMTE     FSCOUNSLR        FSFREQ
    ## BASMID       -0.008018386  0.0067036695  9.965895e-03 -0.0022467497
    ## EDCPUB       -0.113832895 -0.1187437556 -1.026559e-02  0.1398561167
    ## SPUBCHOIX     0.050316348  0.0315868694  2.336319e-02 -0.0371879046
    ## SCONSIDR      0.023355543  0.0143331012  5.245828e-02 -0.0328192210
    ## SEENJOY       0.130670084  0.0405509206 -9.092789e-02 -0.0502450182
    ## SEGRADES      0.031653563  0.0424526109 -5.657315e-03 -0.0510005789
    ## SEABSNT       0.060087798  0.0021423355 -9.535776e-02 -0.0130966431
    ## SEFUTUREX    -0.113553459 -0.0524816806  4.868469e-02  0.0969544750
    ## SEGRADEQ      0.123413184  0.0667692661 -1.111932e-01 -0.0877711120
    ## FSSPORTX      0.443991474  0.3810816788  2.388040e-01 -0.1732250734
    ## FSVOL         0.481879628  0.5136856410  2.576708e-01 -0.2562471928
    ## FSMTNG        0.406231139  0.3750348241  2.759374e-01 -0.1054091795
    ## FSPTMTNG      0.334486119  0.4453981426  3.078921e-01 -0.1183192996
    ## FSATCNFN      0.322107717  0.3233450053  2.915398e-01 -0.0538579033
    ## FSFUNDRS      1.000000000  0.4285643154  2.146775e-01 -0.1957258483
    ## FSCOMMTE      0.428564315  1.0000000000  3.663044e-01 -0.2104910642
    ## FSCOUNSLR     0.214677540  0.3663044402  1.000000e+00 -0.0673441274
    ## FSFREQ       -0.195725848 -0.2104910642 -6.734413e-02  1.0000000000
    ## FSNOTESX      0.165295542  0.1431824506  1.811404e-01 -0.0593044288
    ## FSMEMO        0.221270509  0.1757345808  1.257935e-01 -0.0635229923
    ## FCSCHOOL      0.161989352  0.1345053170  1.271295e-02 -0.0512270224
    ## FCTEACHR      0.144733441  0.1249630576  1.547231e-02 -0.0230559697
    ## FCSTDS        0.153424021  0.1241894169  3.094219e-02 -0.0401687472
    ## FCORDER       0.138318257  0.1154813399  1.010097e-02 -0.0195517092
    ## FCSUPPRT      0.153668904  0.1227337849  3.841846e-02 -0.0495996903
    ## FHHOME        0.045498266  0.0739058143  5.767764e-02  0.0311057079
    ## FHWKHRS      -0.009255583 -0.0119687418 -7.649634e-02  0.0769583163
    ## FHAMOUNT      0.039062603  0.0548484155  2.450658e-02  0.0065368263
    ## FHCAMT        0.015777736  0.0565759177  8.065053e-03  0.0356108876
    ## FHPLACE       0.028486605  0.0649188235  6.590239e-02  0.0349330442
    ## FHCHECKX     -0.048340502  0.0519343141  6.163039e-02  0.0359846374
    ## FHHELP        0.020248656  0.0456069608  3.302726e-02  0.0187586270
    ## FOSTORY2X     0.107828549  0.0272143015  7.174308e-04 -0.0604401996
    ## FOCRAFTS      0.114057637  0.0313535038 -3.982914e-02 -0.0155671574
    ## FOGAMES       0.099973643  0.0222399166 -3.277980e-02 -0.0462007075
    ## FOBUILDX      0.120819075  0.0641079303  2.153825e-02 -0.0528937248
    ## FOSPORT       0.140250516  0.0561004467 -6.399606e-03 -0.1057447947
    ## FORESPON      0.051920150  0.0355655766  9.224591e-02 -0.0413246540
    ## FOHISTX       0.012164210  0.0242060602  9.489176e-02 -0.0057294234
    ## FODINNERX    -0.055902773 -0.0049817294  1.724100e-02  0.0169272611
    ## FOLIBRAYX     0.061404001  0.0323396364  3.371073e-04 -0.0369316361
    ## FOBOOKSTX     0.100950793  0.0473081122  2.814442e-02 -0.0546841878
    ## HDHEALTH      0.107837640  0.0463610222 -7.429885e-02 -0.0927773415
    ## CDOBMM       -0.009529211 -0.0001188423  4.602806e-05 -0.0002707075
    ## CDOBYY       -0.116250874  0.0124325471  1.917114e-01 -0.0289092131
    ## CSEX         -0.032182915 -0.0252111778  2.437760e-02  0.0187218902
    ## CSPEAKX       0.086330427  0.0242561618  1.019215e-02 -0.0857608062
    ## HHTOTALXX     0.005489987 -0.0163870032  1.884424e-02  0.0186471451
    ## RELATION      0.069436525  0.0374452770 -3.517226e-02 -0.0520151564
    ## P1REL         0.040828312  0.0189116680 -6.336190e-02 -0.0513605592
    ## P1SEX        -0.001674703  0.0057934754  2.124535e-02 -0.0220848501
    ## P1MRSTA       0.078977120  0.0454280532 -3.661656e-02 -0.0871811074
    ## P1EMPL        0.033191722 -0.0045908781 -4.817474e-02 -0.0008568916
    ## P1HRSWK      -0.046546472 -0.0086352029  1.350664e-02  0.0285702887
    ## P1MTHSWRK    -0.039392672  0.0013262621  3.217926e-02  0.0096784135
    ## P1AGE         0.019331020 -0.0283058689 -1.189414e-01  0.0345538367
    ## P2GUARD       0.082937452  0.0417660635 -5.150713e-02 -0.0813016639
    ## TTLHHINC     -0.165706832 -0.0791726652  4.890933e-02  0.1452396807
    ## OWNRNTHB      0.103963283  0.0456344747 -3.292634e-02 -0.0892750473
    ## DSBLTY       -0.035626671 -0.0229061999  1.779883e-01  0.0087688396
    ## NUMSIBSX      0.014120756 -0.0159273895  1.879948e-02  0.0268374113
    ## PARGRADEX    -0.148694078 -0.0727574222  1.681665e-02  0.1315334716
    ## INTACC        0.048962029  0.0060159662 -9.277828e-03 -0.0506384472
    ## CENREG       -0.006598477  0.0044959886  3.033636e-02  0.0242302364
    ## ZIPLOCL      -0.043543154 -0.0056737555  1.859292e-02  0.0631635026
    ## SCHLHRSWK    -0.036942137  0.0127510637  5.220639e-02  0.0390104276
    ## EINTNET       0.013922705  0.0668935338  8.663469e-02  0.0034372455
    ## MOSTIMPT     -0.008078897 -0.0436989636 -6.622479e-02 -0.0009701402
    ## INTNUM       -0.008685710 -0.0557559521 -8.177660e-02  0.0053087819
    ## CHLDNT        0.032577313  0.0343853258  6.973927e-02 -0.0545508619
    ## HHPARN19X     0.072610523  0.0399194809 -6.969267e-02 -0.0778678191
    ## HHPARN19_BRD  0.082937452  0.0417660635 -5.150713e-02 -0.0813016639
    ## RACEETH       0.074014916  0.0315342576 -3.483327e-03 -0.0878963949
    ##                  FSNOTESX        FSMEMO     FCSCHOOL      FCTEACHR
    ## BASMID       -0.003232716 -7.953708e-03  0.007607920 -0.0047175674
    ## EDCPUB       -0.055271538 -4.654941e-02 -0.101445785 -0.0783002424
    ## SPUBCHOIX     0.024579974  4.687066e-02  0.036605002  0.0147673677
    ## SCONSIDR      0.056329722  4.018562e-02 -0.124640260 -0.0826790792
    ## SEENJOY       0.015170581  4.839619e-02  0.381732228  0.3367971007
    ## SEGRADES     -0.093617372 -1.358180e-03  0.091476618  0.0537373872
    ## SEABSNT      -0.048084135  2.716097e-02  0.124860960  0.1004875157
    ## SEFUTUREX     0.007673247 -1.070149e-01 -0.104948402 -0.0769917374
    ## SEGRADEQ     -0.067388037  5.036113e-02  0.259255707  0.2341088290
    ## FSSPORTX      0.173109521  2.473611e-01  0.160647301  0.1372806924
    ## FSVOL         0.178207886  2.046933e-01  0.171461211  0.1644256909
    ## FSMTNG        0.177674595  2.648125e-01  0.143085185  0.1318445112
    ## FSPTMTNG      0.159998712  1.412315e-01  0.113317129  0.1196362984
    ## FSATCNFN      0.234949846  2.104043e-01  0.118900248  0.1481818735
    ## FSFUNDRS      0.165295542  2.212705e-01  0.161989352  0.1447334413
    ## FSCOMMTE      0.143182451  1.757346e-01  0.134505317  0.1249630576
    ## FSCOUNSLR     0.181140434  1.257935e-01  0.012712955  0.0154723114
    ## FSFREQ       -0.059304429 -6.352299e-02 -0.051227022 -0.0230559697
    ## FSNOTESX      1.000000000  3.211268e-01  0.116649847  0.1242006647
    ## FSMEMO        0.321126778  1.000000e+00  0.171144349  0.1544716199
    ## FCSCHOOL      0.116649847  1.711443e-01  1.000000000  0.6710520958
    ## FCTEACHR      0.124200665  1.544716e-01  0.671052096  1.0000000000
    ## FCSTDS        0.122369215  1.733717e-01  0.743717150  0.6275937578
    ## FCORDER       0.105391873  1.426607e-01  0.684039159  0.5594013079
    ## FCSUPPRT      0.166245941  1.738281e-01  0.673882680  0.6273163959
    ## FHHOME        0.063177085  7.123968e-02  0.004746069  0.0053855908
    ## FHWKHRS       0.054916852 -1.180555e-02 -0.014262530  0.0258894551
    ## FHAMOUNT      0.051170453  3.519850e-02  0.161970290  0.1718479191
    ## FHCAMT        0.034003681  2.087058e-02  0.097344567  0.1284010432
    ## FHPLACE       0.055774579  1.158149e-02 -0.011277504  0.0184369560
    ## FHCHECKX     -0.024053405 -7.731551e-03 -0.029456223 -0.0257443479
    ## FHHELP        0.036803703  3.447168e-02 -0.007060322 -0.0006777293
    ## FOSTORY2X     0.106393163  8.172190e-02  0.052346744  0.0628754288
    ## FOCRAFTS      0.101415747  3.997372e-02  0.060563226  0.1034406724
    ## FOGAMES       0.069855344  4.877155e-02  0.068193850  0.0897427883
    ## FOBUILDX      0.083032938  5.289004e-02  0.044942024  0.0555022381
    ## FOSPORT       0.062276488  5.552349e-02  0.064495266  0.0543429293
    ## FORESPON      0.044231933  3.913336e-02 -0.003275859 -0.0166894704
    ## FOHISTX       0.027453984 -1.141827e-03  0.002385784  0.0034786228
    ## FODINNERX    -0.034876259 -1.480065e-02 -0.055865549 -0.0741528767
    ## FOLIBRAYX     0.056619582  3.262554e-02  0.053944461  0.0693802422
    ## FOBOOKSTX     0.051767567  5.636736e-02  0.052683655  0.0507783834
    ## HDHEALTH     -0.005080643  6.017782e-02  0.171224742  0.1439212433
    ## CDOBMM        0.001389098  1.368717e-02 -0.007054696 -0.0103926025
    ## CDOBYY       -0.129903359 -7.972815e-02 -0.099491466 -0.1465662582
    ## CSEX          0.044098972 -1.148658e-02 -0.019147948 -0.0111147762
    ## CSPEAKX       0.048768258  3.980251e-02 -0.013403639 -0.0087655207
    ## HHTOTALXX     0.018663967  2.516089e-02 -0.029198118 -0.0366610953
    ## RELATION      0.026352995  8.571760e-02 -0.003325580 -0.0121391464
    ## P1REL         0.009824889  6.218091e-02  0.025099764  0.0057159622
    ## P1SEX         0.032802633  4.367238e-03  0.064918920  0.0446873047
    ## P1MRSTA       0.008282873  6.397759e-02  0.078831852  0.0461717032
    ## P1EMPL        0.033646585  6.047575e-02  0.033767383  0.0057488035
    ## P1HRSWK      -0.048151144 -5.448706e-02 -0.027205637 -0.0010711765
    ## P1MTHSWRK    -0.042641946 -6.486626e-02 -0.035713610 -0.0069941566
    ## P1AGE         0.042454308  1.303466e-02 -0.017114792  0.0120023664
    ## P2GUARD       0.012222015  5.624158e-02  0.065342896  0.0429043325
    ## TTLHHINC     -0.059953645 -1.591123e-01 -0.109580545 -0.0569161873
    ## OWNRNTHB      0.025770664  7.124264e-02  0.046516307  0.0182296108
    ## DSBLTY        0.112325963 -5.876116e-05 -0.095893449 -0.0720698971
    ## NUMSIBSX      0.024725122  1.204718e-02 -0.015682565 -0.0202111310
    ## PARGRADEX    -0.089063376 -1.722118e-01 -0.075011518 -0.0381028836
    ## INTACC        0.046945443  1.195641e-01  0.043762165  0.0168831565
    ## CENREG       -0.002612489 -2.211851e-02 -0.006864429  0.0027444351
    ## ZIPLOCL       0.016908704  4.473074e-02  0.031672155  0.0233902285
    ## SCHLHRSWK    -0.019611399 -5.774481e-02 -0.022679138 -0.0060767003
    ## EINTNET       0.025293819 -2.054882e-02 -0.019630202 -0.0127845638
    ## MOSTIMPT     -0.024890086  1.941118e-02  0.025435084  0.0187132347
    ## INTNUM       -0.018220046  2.247259e-02  0.028412998  0.0242928616
    ## CHLDNT       -0.014060644  5.297659e-02  0.031867418  0.0005524703
    ## HHPARN19X     0.015426144  8.401474e-02  0.048902481  0.0279800141
    ## HHPARN19_BRD  0.012222015  5.624158e-02  0.065342896  0.0429043325
    ## RACEETH       0.033795263  3.754874e-02  0.023222670  0.0196875910
    ##                     FCSTDS       FCORDER      FCSUPPRT        FHHOME
    ## BASMID        0.0068209376  0.0003482812  0.0100303027 -0.0081597653
    ## EDCPUB       -0.1149123537 -0.1183307585 -0.1050784225  0.0890638129
    ## SPUBCHOIX     0.0381797263  0.0221713133  0.0251392769  0.0010871471
    ## SCONSIDR     -0.0988583451 -0.0832242387 -0.0718835937 -0.0596840570
    ## SEENJOY       0.3244478573  0.3231938389  0.3200194817 -0.0813784483
    ## SEGRADES      0.0886952253  0.0456768879  0.0263956863  0.0186985271
    ## SEABSNT       0.0948494661  0.1079275725  0.0906707611 -0.0373508049
    ## SEFUTUREX    -0.0986216985 -0.0870158808 -0.0838242253  0.0851270418
    ## SEGRADEQ      0.2393207048  0.1872377142  0.1858990721 -0.0937079682
    ## FSSPORTX      0.1467458785  0.1211244999  0.1474852879  0.0625776529
    ## FSVOL         0.1621743326  0.1464768054  0.1774637902  0.0268540414
    ## FSMTNG        0.1411785651  0.1303660078  0.1534841964  0.0354094298
    ## FSPTMTNG      0.1091217032  0.1121996900  0.1464405121  0.0213957515
    ## FSATCNFN      0.1071213039  0.1275069378  0.1870593071  0.0321054600
    ## FSFUNDRS      0.1534240209  0.1383182569  0.1536689040  0.0454982658
    ## FSCOMMTE      0.1241894169  0.1154813399  0.1227337849  0.0739058143
    ## FSCOUNSLR     0.0309421917  0.0101009653  0.0384184642  0.0576776427
    ## FSFREQ       -0.0401687472 -0.0195517092 -0.0495996903  0.0311057079
    ## FSNOTESX      0.1223692148  0.1053918728  0.1662459408  0.0631770846
    ## FSMEMO        0.1733716975  0.1426606703  0.1738281419  0.0712396768
    ## FCSCHOOL      0.7437171505  0.6840391585  0.6738826803  0.0047460687
    ## FCTEACHR      0.6275937578  0.5594013079  0.6273163959  0.0053855908
    ## FCSTDS        1.0000000000  0.6654971263  0.6223306362 -0.0078147641
    ## FCORDER       0.6654971263  1.0000000000  0.6470794043 -0.0019331885
    ## FCSUPPRT      0.6223306362  0.6470794043  1.0000000000  0.0015141551
    ## FHHOME       -0.0078147641 -0.0019331885  0.0015141551  1.0000000000
    ## FHWKHRS      -0.0445319790 -0.0089336261  0.0271656409  0.1715305985
    ## FHAMOUNT      0.1673450058  0.1326534440  0.1626723508 -0.4351703384
    ## FHCAMT        0.0826073230  0.0900429919  0.1081435604 -0.3573423906
    ## FHPLACE      -0.0098951162  0.0025572349  0.0103578148 -0.4797584952
    ## FHCHECKX     -0.0374066819 -0.0321462332 -0.0441820350 -0.4234482894
    ## FHHELP       -0.0146298583 -0.0123630632 -0.0110279108 -0.1816821499
    ## FOSTORY2X     0.0430470574  0.0333522106  0.0640345165 -0.0325824853
    ## FOCRAFTS      0.0575297918  0.0636219877  0.1069947270 -0.0371749363
    ## FOGAMES       0.0599976882  0.0689898557  0.0901742405 -0.0223795402
    ## FOBUILDX      0.0456802816  0.0397238000  0.0654060547 -0.0062776193
    ## FOSPORT       0.0535019752  0.0573028591  0.0831691439 -0.0099361246
    ## FORESPON      0.0065595276 -0.0081015313 -0.0043794286  0.0020754192
    ## FOHISTX       0.0002702367 -0.0013389067  0.0080570207 -0.0186624060
    ## FODINNERX    -0.0477386213 -0.0393914649 -0.0709455880  0.0546761117
    ## FOLIBRAYX     0.0508781300  0.0513763499  0.0833409120 -0.0438571663
    ## FOBOOKSTX     0.0504572447  0.0440986836  0.0571852283 -0.0469856138
    ## HDHEALTH      0.1511262616  0.1524987154  0.1455609967 -0.0242841718
    ## CDOBMM       -0.0059508960  0.0024823600 -0.0017256971 -0.0063707960
    ## CDOBYY       -0.0754063900 -0.1098230295 -0.1635830562  0.0309666772
    ## CSEX         -0.0225943144 -0.0056045103 -0.0067944971  0.0502275146
    ## CSPEAKX       0.0035405378 -0.0194505614 -0.0265623822  0.0436144487
    ## HHTOTALXX    -0.0194659962 -0.0100546870 -0.0211833979  0.0107775806
    ## RELATION     -0.0006825413 -0.0032751364  0.0001352134 -0.0105869362
    ## P1REL         0.0068173152  0.0162261744  0.0173573050 -0.0235175500
    ## P1SEX         0.0367485285  0.0580679447  0.0426080278 -0.0099364091
    ## P1MRSTA       0.0555250611  0.0560390503  0.0529620372 -0.0180955702
    ## P1EMPL        0.0230930693  0.0289544133  0.0102293522  0.0081089152
    ## P1HRSWK      -0.0189393840 -0.0240968548 -0.0001111973 -0.0144183090
    ## P1MTHSWRK    -0.0245506551 -0.0323003881 -0.0111966651 -0.0177411590
    ## P1AGE        -0.0278134720 -0.0275641567  0.0026276439  0.0232467508
    ## P2GUARD       0.0427176276  0.0420250048  0.0414748352 -0.0155813778
    ## TTLHHINC     -0.0911142766 -0.0965835271 -0.0730420201  0.0485212371
    ## OWNRNTHB      0.0400471136  0.0376757884  0.0269728632  0.0127804342
    ## DSBLTY       -0.0647168076 -0.0720579098 -0.0466789859  0.0086660018
    ## NUMSIBSX     -0.0083142477  0.0015574997 -0.0078054822  0.0041965302
    ## PARGRADEX    -0.0631154615 -0.0768819483 -0.0617213345  0.0612808731
    ## INTACC        0.0341243425  0.0208715796  0.0211641489 -0.0053073414
    ## CENREG        0.0035122117  0.0117353736 -0.0036861224 -0.0003832861
    ## ZIPLOCL       0.0481880447  0.0558895525  0.0520890258 -0.0876987216
    ## SCHLHRSWK    -0.0131462879  0.0015742887  0.0137299518  0.0112010258
    ## EINTNET      -0.0202280088 -0.0061097689 -0.0074123254  0.0077124447
    ## MOSTIMPT      0.0275438794  0.0122858148  0.0166642779 -0.0051464819
    ## INTNUM        0.0302410215  0.0236476486  0.0201015082  0.0036779222
    ## CHLDNT        0.0420483752  0.0176675617 -0.0047001418 -0.0237715870
    ## HHPARN19X     0.0295443701  0.0283176521  0.0312232907 -0.0219247224
    ## HHPARN19_BRD  0.0427176276  0.0420250048  0.0414748352 -0.0155813778
    ## RACEETH       0.0343962310  0.0060096430 -0.0005393767  0.0469388045
    ##                    FHWKHRS     FHAMOUNT       FHCAMT       FHPLACE
    ## BASMID       -0.0013347822  0.008828652  0.012861401  0.0128102606
    ## EDCPUB        0.0659940888 -0.046256567 -0.022220972  0.0059959001
    ## SPUBCHOIX    -0.0297604660  0.003226428 -0.003600762  0.0019898895
    ## SCONSIDR     -0.0561133222 -0.039125402 -0.030594132  0.0181848264
    ## SEENJOY      -0.0220201507  0.071823457  0.101212588 -0.0001975215
    ## SEGRADES     -0.2221590131 -0.098434739 -0.108733827 -0.1196708491
    ## SEABSNT      -0.0421248081 -0.011925026  0.001969538 -0.0297051556
    ## SEFUTUREX     0.2061232743  0.095766630  0.095039277  0.1075883937
    ## SEGRADEQ     -0.1748743815  0.010256477  0.009132258 -0.0626953158
    ## FSSPORTX     -0.0146889423  0.015205479 -0.003464505  0.0065902017
    ## FSVOL        -0.0024632694  0.058464372  0.031554333  0.0453183598
    ## FSMTNG       -0.0130660713  0.045578987  0.027794059  0.0486354297
    ## FSPTMTNG     -0.0189365851  0.044415988  0.036311985  0.0711664531
    ## FSATCNFN      0.0932578728  0.079880684  0.068534427  0.0822615351
    ## FSFUNDRS     -0.0092555826  0.039062603  0.015777736  0.0284866048
    ## FSCOMMTE     -0.0119687418  0.054848416  0.056575918  0.0649188235
    ## FSCOUNSLR    -0.0764963419  0.024506577  0.008065053  0.0659023932
    ## FSFREQ        0.0769583163  0.006536826  0.035610888  0.0349330442
    ## FSNOTESX      0.0549168515  0.051170453  0.034003681  0.0557745791
    ## FSMEMO       -0.0118055477  0.035198502  0.020870581  0.0115814934
    ## FCSCHOOL     -0.0142625302  0.161970290  0.097344567 -0.0112775035
    ## FCTEACHR      0.0258894551  0.171847919  0.128401043  0.0184369560
    ## FCSTDS       -0.0445319790  0.167345006  0.082607323 -0.0098951162
    ## FCORDER      -0.0089336261  0.132653444  0.090042992  0.0025572349
    ## FCSUPPRT      0.0271656409  0.162672351  0.108143560  0.0103578148
    ## FHHOME        0.1715305985 -0.435170338 -0.357342391 -0.4797584952
    ## FHWKHRS       1.0000000000  0.200102753  0.315125319  0.2088947189
    ## FHAMOUNT      0.2001027533  1.000000000  0.662434453  0.5564004410
    ## FHCAMT        0.3151253189  0.662434453  1.000000000  0.6171137279
    ## FHPLACE       0.2088947189  0.556400441  0.617113728  1.0000000000
    ## FHCHECKX      0.1648671939  0.527468436  0.600248924  0.6089669386
    ## FHHELP        0.2140345773  0.352767273  0.431068618  0.4530533290
    ## FOSTORY2X     0.0579527604  0.032451726  0.018483641  0.0488762364
    ## FOCRAFTS      0.1327941258  0.057727825  0.045078122  0.0653541338
    ## FOGAMES       0.0937132908  0.035887929  0.031414366  0.0283184408
    ## FOBUILDX      0.0413279925  0.021425887  0.013506617  0.0346259597
    ## FOSPORT       0.0536389458  0.007663853  0.009392278  0.0366520703
    ## FORESPON     -0.0838228817 -0.045145708 -0.048316135  0.0141944167
    ## FOHISTX      -0.0846521878 -0.035247734 -0.026682983  0.0158636260
    ## FODINNERX    -0.0051521634 -0.027106225 -0.028161482 -0.0668355313
    ## FOLIBRAYX     0.0131973823  0.004896421  0.011869961  0.0259593390
    ## FOBOOKSTX    -0.0443887269 -0.006077287 -0.007522980  0.0100854025
    ## HDHEALTH     -0.0386802231 -0.002054617  0.001308843 -0.0254450683
    ## CDOBMM       -0.0088200745  0.005397947  0.005460286  0.0012145548
    ## CDOBYY       -0.2823596238 -0.072849880 -0.066656160 -0.0393560155
    ## CSEX          0.1214312600  0.010141330  0.042968022  0.0426903044
    ## CSPEAKX       0.0796346752  0.022459613  0.002206179 -0.0027913263
    ## HHTOTALXX    -0.0010902031 -0.008333561  0.007447070  0.0207969937
    ## RELATION      0.0026985008 -0.008471912 -0.013034181 -0.0115770036
    ## P1REL        -0.0454762788 -0.030113879 -0.040757592 -0.0573465373
    ## P1SEX        -0.0835992111 -0.025764268 -0.014144948 -0.0437688525
    ## P1MRSTA      -0.0629812886 -0.010398410 -0.021289683 -0.0432447578
    ## P1EMPL       -0.0354001745 -0.025567550 -0.023990089 -0.0532695687
    ## P1HRSWK       0.0630758036  0.039831603  0.035090442  0.0578891453
    ## P1MTHSWRK     0.0453303094  0.034901009  0.030836161  0.0623464033
    ## P1AGE         0.1657532619  0.014601347  0.004894879  0.0037516904
    ## P2GUARD      -0.0415681730 -0.009360024 -0.024346574 -0.0420946063
    ## TTLHHINC      0.1091893090  0.040102468  0.048654935  0.0685867243
    ## OWNRNTHB     -0.0365274075 -0.022536566 -0.024984297 -0.0398679073
    ## DSBLTY        0.0603114012  0.030194987  0.012152116  0.0572343129
    ## NUMSIBSX     -0.0078049016 -0.007925537  0.006422536  0.0308165072
    ## PARGRADEX     0.1101474856  0.050983197  0.041565956  0.0590803014
    ## INTACC       -0.0588271007 -0.042459909 -0.044673475 -0.0469573832
    ## CENREG       -0.0003163544 -0.008466338 -0.015725365 -0.0104550230
    ## ZIPLOCL      -0.1041883736 -0.036872329 -0.013895506 -0.0070654261
    ## SCHLHRSWK     0.0381626822  0.039528118  0.041784577  0.0477581944
    ## EINTNET      -0.0488556718 -0.002977422 -0.006077222  0.0190717092
    ## MOSTIMPT      0.0427980364  0.011793943  0.017626384 -0.0141120503
    ## INTNUM        0.0561297237  0.007749460  0.018544758 -0.0095456951
    ## CHLDNT       -0.2482186633 -0.105507549 -0.135275243 -0.0932913636
    ## HHPARN19X    -0.0561266659 -0.027806743 -0.046263389 -0.0626826477
    ## HHPARN19_BRD -0.0415681730 -0.009360024 -0.024346574 -0.0420946063
    ## RACEETH       0.0767700708  0.033505810  0.006077781 -0.0076754362
    ##                   FHCHECKX        FHHELP     FOSTORY2X     FOCRAFTS
    ## BASMID        0.0096257597  0.0031065293 -0.0049911150 -0.008935672
    ## EDCPUB       -0.0337665691  0.0014235999 -0.0075720810 -0.004178544
    ## SPUBCHOIX     0.0061026030  0.0235860004  0.0273906514 -0.023831138
    ## SCONSIDR      0.0021512518 -0.0001294849  0.0694553189  0.042445446
    ## SEENJOY      -0.0955759567 -0.0404927292  0.1038264659  0.171762370
    ## SEGRADES     -0.0480365489 -0.0646056628 -0.0641975729 -0.154208094
    ## SEABSNT      -0.0870032498 -0.0224813656  0.0309434494  0.032316281
    ## SEFUTUREX     0.1290665859  0.1017293256 -0.0904937177 -0.050494066
    ## SEGRADEQ     -0.0430403350 -0.0595894872  0.0622575005  0.051998767
    ## FSSPORTX     -0.0456846578  0.0193437635  0.0942015869  0.093695475
    ## FSVOL        -0.0352625571  0.0148303901  0.1261411419  0.159561932
    ## FSMTNG       -0.0474809321  0.0571811828  0.0987659685  0.085982937
    ## FSPTMTNG     -0.0471591045  0.0079456404  0.0850279391  0.146478637
    ## FSATCNFN     -0.0721801400  0.0535284120  0.1243281539  0.193911549
    ## FSFUNDRS     -0.0483405019  0.0202486562  0.1078285485  0.114057637
    ## FSCOMMTE      0.0519343141  0.0456069608  0.0272143015  0.031353504
    ## FSCOUNSLR     0.0616303917  0.0330272610  0.0007174308 -0.039829138
    ## FSFREQ        0.0359846374  0.0187586270 -0.0604401996 -0.015567157
    ## FSNOTESX     -0.0240534049  0.0368037031  0.1063931626  0.101415747
    ## FSMEMO       -0.0077315513  0.0344716819  0.0817219005  0.039973724
    ## FCSCHOOL     -0.0294562229 -0.0070603222  0.0523467439  0.060563226
    ## FCTEACHR     -0.0257443479 -0.0006777293  0.0628754288  0.103440672
    ## FCSTDS       -0.0374066819 -0.0146298583  0.0430470574  0.057529792
    ## FCORDER      -0.0321462332 -0.0123630632  0.0333522106  0.063621988
    ## FCSUPPRT     -0.0441820350 -0.0110279108  0.0640345165  0.106994727
    ## FHHOME       -0.4234482894 -0.1816821499 -0.0325824853 -0.037174936
    ## FHWKHRS       0.1648671939  0.2140345773  0.0579527604  0.132794126
    ## FHAMOUNT      0.5274684357  0.3527672732  0.0324517256  0.057727825
    ## FHCAMT        0.6002489245  0.4310686182  0.0184836408  0.045078122
    ## FHPLACE       0.6089669386  0.4530533290  0.0488762364  0.065354134
    ## FHCHECKX      1.0000000000  0.4376714836 -0.0901537545 -0.155138698
    ## FHHELP        0.4376714836  1.0000000000  0.0052942107 -0.032772309
    ## FOSTORY2X    -0.0901537545  0.0052942107  1.0000000000  0.292828718
    ## FOCRAFTS     -0.1551386981 -0.0327723088  0.2928287175  1.000000000
    ## FOGAMES      -0.1323376559 -0.0087883250  0.2222598162  0.364338147
    ## FOBUILDX     -0.1198814105  0.0030155893  0.2326731787  0.320178878
    ## FOSPORT      -0.1385274301  0.0028796859  0.1984996662  0.234404092
    ## FORESPON     -0.0815129987  0.0192165946  0.1186519660 -0.006075901
    ## FOHISTX      -0.0603921159 -0.0156588833  0.1982211439  0.084997224
    ## FODINNERX     0.1143899209  0.0026676377 -0.1535088638 -0.215249981
    ## FOLIBRAYX    -0.1006698158 -0.0281697109  0.1571906115  0.232422423
    ## FOBOOKSTX    -0.0717386143 -0.0014516168  0.1489207464  0.188699487
    ## HDHEALTH     -0.0806645895 -0.0291608417  0.0702604126  0.070604473
    ## CDOBMM        0.0017763415 -0.0084489228  0.0152234061  0.007541390
    ## CDOBYY        0.2345963952  0.0502932349 -0.2669475340 -0.503276521
    ## CSEX          0.0026363098  0.0089336204 -0.0236351033 -0.151356230
    ## CSPEAKX       0.0076326601  0.0535509509  0.0176645119 -0.017937649
    ## HHTOTALXX     0.0069402646  0.0055496691  0.0096217074 -0.040353973
    ## RELATION      0.0057861639  0.0096777563  0.0468020178 -0.013637951
    ## P1REL        -0.0344903227 -0.0211660168  0.0295978746 -0.006008497
    ## P1SEX        -0.0214598544  0.0048933247 -0.0296817693  0.001332683
    ## P1MRSTA       0.0007693094  0.0072308894 -0.0047340774 -0.050832751
    ## P1EMPL       -0.0139170364  0.0009305092 -0.0049341422 -0.032853811
    ## P1HRSWK       0.0162457416 -0.0040130495  0.0089457559  0.048636843
    ## P1MTHSWRK     0.0166150094 -0.0042727307  0.0147309141  0.046482923
    ## P1AGE        -0.1099280189 -0.0197618406  0.1405452560  0.247008402
    ## P2GUARD      -0.0281401905 -0.0007912519  0.0205676881 -0.005101797
    ## TTLHHINC     -0.0050844865 -0.0305386187 -0.0380288072  0.076612054
    ## OWNRNTHB      0.0013679484  0.0221433183 -0.0097660360 -0.064746253
    ## DSBLTY        0.0446058042  0.0501772491  0.0100243629 -0.002786464
    ## NUMSIBSX     -0.0160059923 -0.0070620190  0.0091605651 -0.023333938
    ## PARGRADEX     0.0139983369 -0.0369523961 -0.0838560534  0.024401707
    ## INTACC       -0.0266400114 -0.0024834120  0.0048781746 -0.042498347
    ## CENREG       -0.0064172934 -0.0266970527 -0.0146286887 -0.001019413
    ## ZIPLOCL      -0.0099760806 -0.0391243081  0.0102300389  0.015028214
    ## SCHLHRSWK     0.0122065432 -0.0086668352 -0.0193970563  0.036664895
    ## EINTNET       0.0149719915  0.0071795765 -0.0175401606 -0.018376500
    ## MOSTIMPT     -0.0027527492 -0.0001603518  0.0065047310  0.007119682
    ## INTNUM       -0.0137087004 -0.0023461503  0.0225520130  0.014743987
    ## CHLDNT       -0.0588085628 -0.0641943837 -0.0126511804 -0.093915335
    ## HHPARN19X    -0.0369836667 -0.0111776016  0.0333876211 -0.005939132
    ## HHPARN19_BRD -0.0281401905 -0.0007912519  0.0205676881 -0.005101797
    ## RACEETH       0.0331279743  0.0633638060  0.0204185814 -0.037100098
    ##                    FOGAMES      FOBUILDX       FOSPORT      FORESPON
    ## BASMID       -0.0021878010  0.0013074550  4.968994e-03  0.0041167913
    ## EDCPUB       -0.0046811077 -0.0062262378 -2.909298e-02 -0.0012359223
    ## SPUBCHOIX     0.0005617684  0.0054200448 -3.343094e-03  0.0354715447
    ## SCONSIDR      0.0346682019  0.0364877338  2.607068e-02  0.0443352982
    ## SEENJOY       0.1379712640  0.1008841311  1.460820e-01  0.0115586171
    ## SEGRADES     -0.0782270721 -0.0498315423 -1.906380e-02  0.0736993687
    ## SEABSNT       0.0432145734  0.0404305989  8.062069e-02  0.0314792955
    ## SEFUTUREX    -0.0565376872 -0.0302121706 -1.135049e-01 -0.0654284523
    ## SEGRADEQ      0.0876856386  0.0483053932  9.290824e-02 -0.0062083757
    ## FSSPORTX      0.1052513055  0.1111165175  1.394254e-01  0.0348195479
    ## FSVOL         0.1276267209  0.1349378250  1.595310e-01  0.0256389456
    ## FSMTNG        0.1002989905  0.1074624858  1.230116e-01  0.0407066717
    ## FSPTMTNG      0.1123269673  0.1193294033  1.349057e-01  0.0544320070
    ## FSATCNFN      0.1650544253  0.1342189729  1.341245e-01  0.0075932309
    ## FSFUNDRS      0.0999736426  0.1208190753  1.402505e-01  0.0519201496
    ## FSCOMMTE      0.0222399166  0.0641079303  5.610045e-02  0.0355655766
    ## FSCOUNSLR    -0.0327797985  0.0215382491 -6.399606e-03  0.0922459124
    ## FSFREQ       -0.0462007075 -0.0528937248 -1.057448e-01 -0.0413246540
    ## FSNOTESX      0.0698553435  0.0830329383  6.227649e-02  0.0442319328
    ## FSMEMO        0.0487715482  0.0528900378  5.552349e-02  0.0391333626
    ## FCSCHOOL      0.0681938502  0.0449420235  6.449527e-02 -0.0032758586
    ## FCTEACHR      0.0897427883  0.0555022381  5.434293e-02 -0.0166894704
    ## FCSTDS        0.0599976882  0.0456802816  5.350198e-02  0.0065595276
    ## FCORDER       0.0689898557  0.0397238000  5.730286e-02 -0.0081015313
    ## FCSUPPRT      0.0901742405  0.0654060547  8.316914e-02 -0.0043794286
    ## FHHOME       -0.0223795402 -0.0062776193 -9.936125e-03  0.0020754192
    ## FHWKHRS       0.0937132908  0.0413279925  5.363895e-02 -0.0838228817
    ## FHAMOUNT      0.0358879285  0.0214258871  7.663853e-03 -0.0451457075
    ## FHCAMT        0.0314143657  0.0135066166  9.392278e-03 -0.0483161353
    ## FHPLACE       0.0283184408  0.0346259597  3.665207e-02  0.0141944167
    ## FHCHECKX     -0.1323376559 -0.1198814105 -1.385274e-01 -0.0815129987
    ## FHHELP       -0.0087883250  0.0030155893  2.879686e-03  0.0192165946
    ## FOSTORY2X     0.2222598162  0.2326731787  1.984997e-01  0.1186519660
    ## FOCRAFTS      0.3643381470  0.3201788783  2.344041e-01 -0.0060759012
    ## FOGAMES       1.0000000000  0.2361536686  2.618152e-01  0.0172181232
    ## FOBUILDX      0.2361536686  1.0000000000  2.313074e-01  0.0964516924
    ## FOSPORT       0.2618151670  0.2313073542  1.000000e+00  0.0619441256
    ## FORESPON      0.0172181232  0.0964516924  6.194413e-02  1.0000000000
    ## FOHISTX       0.0789812896  0.1394713868  9.438336e-02  0.2762494046
    ## FODINNERX    -0.2149867179 -0.1773157431 -1.890383e-01 -0.0578859307
    ## FOLIBRAYX     0.2226721397  0.1475595555  1.436319e-01  0.0403814333
    ## FOBOOKSTX     0.1647435991  0.1475868454  1.189927e-01  0.0732097270
    ## HDHEALTH      0.0558064280  0.0767516373  1.471863e-01  0.0282483989
    ## CDOBMM       -0.0021254298 -0.0046662696 -4.124036e-03 -0.0064781297
    ## CDOBYY       -0.3467379861 -0.2146116724 -2.578105e-01  0.1126357539
    ## CSEX         -0.0222486197  0.0366819793  5.813122e-02  0.0253828058
    ## CSPEAKX       0.0121579960  0.0103561206 -1.791913e-02 -0.0283689487
    ## HHTOTALXX    -0.0521918413 -0.0108927408 -4.196330e-02  0.0553639826
    ## RELATION      0.0145943267  0.0121859724  5.596795e-02  0.0625398788
    ## P1REL         0.0223373293  0.0115064565  6.298128e-02  0.0106513324
    ## P1SEX        -0.0042937093  0.0389399123  2.268327e-02 -0.0279301243
    ## P1MRSTA       0.0015968401  0.0201982944  2.646250e-02  0.0177091470
    ## P1EMPL       -0.0130046451  0.0023807547  3.411054e-02  0.0115910956
    ## P1HRSWK       0.0297107741 -0.0073534135 -2.750842e-02 -0.0050723384
    ## P1MTHSWRK     0.0194522313 -0.0065006429 -2.726621e-02 -0.0151689971
    ## P1AGE         0.1895699780  0.0952259716  1.473923e-01 -0.0315317063
    ## P2GUARD       0.0336295597  0.0337669214  5.292563e-02  0.0078785880
    ## TTLHHINC      0.0127458383 -0.0065190446 -5.751182e-02 -0.0179737612
    ## OWNRNTHB     -0.0275052671  0.0203819854  1.103573e-02 -0.0146710504
    ## DSBLTY       -0.0178190471  0.0157983620 -6.091905e-02  0.0108160244
    ## NUMSIBSX     -0.0463407105  0.0043896600 -4.348113e-02  0.0503008190
    ## PARGRADEX    -0.0271379122 -0.0140742086 -5.990862e-02 -0.0483372095
    ## INTACC       -0.0211529857 -0.0006587647 -3.052208e-05  0.0105301987
    ## CENREG       -0.0162061186 -0.0106251294 -2.404496e-05 -0.0004811075
    ## ZIPLOCL       0.0037299596 -0.0422738053 -2.000890e-03  0.0223774809
    ## SCHLHRSWK    -0.0052240280  0.0052548778  4.641395e-03  0.0206294245
    ## EINTNET      -0.0278296031  0.0047570156 -1.953496e-02  0.0169098968
    ## MOSTIMPT      0.0185835156 -0.0039629411  1.476980e-02 -0.0085334633
    ## INTNUM        0.0293999266 -0.0006051859  2.001124e-02 -0.0131683131
    ## CHLDNT       -0.0585979098  0.0068921315 -1.258716e-03  0.1073827785
    ## HHPARN19X     0.0356234168  0.0176975687  6.237093e-02  0.0239298728
    ## HHPARN19_BRD  0.0336295597  0.0337669214  5.292563e-02  0.0078785880
    ## RACEETH       0.0127612377  0.0220783440  2.495140e-03 -0.0376339856
    ##                    FOHISTX     FODINNERX     FOLIBRAYX     FOBOOKSTX
    ## BASMID        0.0004505016 -0.0031083208 -4.329979e-03 -6.922754e-03
    ## EDCPUB       -0.0053282417 -0.0173845779 -9.414193e-03 -5.964670e-02
    ## SPUBCHOIX     0.0186145277 -0.0070138229  8.151616e-03  2.685497e-02
    ## SCONSIDR      0.0534354354 -0.0125007931  5.131020e-02  8.536557e-02
    ## SEENJOY       0.0658109887 -0.1418071994  1.377415e-01  1.069171e-01
    ## SEGRADES      0.0586590286  0.0232936524 -5.341758e-02  2.858609e-02
    ## SEABSNT       0.0078626551 -0.0666057720  4.467151e-02  3.986001e-02
    ## SEFUTUREX    -0.0887530891  0.0479918665 -1.082982e-01 -1.454167e-01
    ## SEGRADEQ      0.0527328496 -0.0765126882  9.599550e-02  1.346381e-01
    ## FSSPORTX      0.0128553234 -0.0498653030  6.306163e-02  9.250967e-02
    ## FSVOL         0.0137798945 -0.0604975108  1.336983e-01  1.196222e-01
    ## FSMTNG        0.0305868748 -0.0706829615  6.867071e-02  8.718703e-02
    ## FSPTMTNG      0.0941472357 -0.0674292969  9.827867e-02  8.230184e-02
    ## FSATCNFN      0.0213901208 -0.0984571972  1.294077e-01  9.396302e-02
    ## FSFUNDRS      0.0121642100 -0.0559027731  6.140400e-02  1.009508e-01
    ## FSCOMMTE      0.0242060602 -0.0049817294  3.233964e-02  4.730811e-02
    ## FSCOUNSLR     0.0948917594  0.0172410036  3.371073e-04  2.814442e-02
    ## FSFREQ       -0.0057294234  0.0169272611 -3.693164e-02 -5.468419e-02
    ## FSNOTESX      0.0274539844 -0.0348762585  5.661958e-02  5.176757e-02
    ## FSMEMO       -0.0011418275 -0.0148006461  3.262554e-02  5.636736e-02
    ## FCSCHOOL      0.0023857839 -0.0558655494  5.394446e-02  5.268365e-02
    ## FCTEACHR      0.0034786228 -0.0741528767  6.938024e-02  5.077838e-02
    ## FCSTDS        0.0002702367 -0.0477386213  5.087813e-02  5.045724e-02
    ## FCORDER      -0.0013389067 -0.0393914649  5.137635e-02  4.409868e-02
    ## FCSUPPRT      0.0080570207 -0.0709455880  8.334091e-02  5.718523e-02
    ## FHHOME       -0.0186624060  0.0546761117 -4.385717e-02 -4.698561e-02
    ## FHWKHRS      -0.0846521878 -0.0051521634  1.319738e-02 -4.438873e-02
    ## FHAMOUNT     -0.0352477340 -0.0271062248  4.896421e-03 -6.077287e-03
    ## FHCAMT       -0.0266829830 -0.0281614818  1.186996e-02 -7.522980e-03
    ## FHPLACE       0.0158636260 -0.0668355313  2.595934e-02  1.008540e-02
    ## FHCHECKX     -0.0603921159  0.1143899209 -1.006698e-01 -7.173861e-02
    ## FHHELP       -0.0156588833  0.0026676377 -2.816971e-02 -1.451617e-03
    ## FOSTORY2X     0.1982211439 -0.1535088638  1.571906e-01  1.489207e-01
    ## FOCRAFTS      0.0849972239 -0.2152499814  2.324224e-01  1.886995e-01
    ## FOGAMES       0.0789812896 -0.2149867179  2.226721e-01  1.647436e-01
    ## FOBUILDX      0.1394713868 -0.1773157431  1.475596e-01  1.475868e-01
    ## FOSPORT       0.0943833553 -0.1890382867  1.436319e-01  1.189927e-01
    ## FORESPON      0.2762494046 -0.0578859307  4.038143e-02  7.320973e-02
    ## FOHISTX       1.0000000000 -0.1211345821  9.634075e-02  1.225830e-01
    ## FODINNERX    -0.1211345821  1.0000000000 -1.573927e-01 -1.189036e-01
    ## FOLIBRAYX     0.0963407511 -0.1573926961  1.000000e+00  2.295598e-01
    ## FOBOOKSTX     0.1225830045 -0.1189035625  2.295598e-01  1.000000e+00
    ## HDHEALTH      0.0197864467 -0.1068661800  4.408041e-02  6.982496e-02
    ## CDOBMM       -0.0095725909 -0.0063491504  3.806003e-03 -3.389002e-03
    ## CDOBYY        0.0525834918  0.2055524384 -2.470146e-01 -1.043551e-01
    ## CSEX         -0.0233204195  0.0132946495 -4.549238e-02 -1.011099e-01
    ## CSPEAKX      -0.1110663439  0.0733353654 -3.782424e-02  1.472435e-02
    ## HHTOTALXX    -0.0037170462  0.0311501821 -2.357733e-02  2.382560e-02
    ## RELATION     -0.0136142442 -0.0206493806  1.184563e-02  2.693667e-02
    ## P1REL        -0.0207874548  0.0004262393  2.806942e-02  3.490614e-02
    ## P1SEX        -0.0131944445  0.0095655890  1.538305e-02  2.208594e-02
    ## P1MRSTA      -0.0334139572 -0.0297781581  8.813500e-03  3.123333e-02
    ## P1EMPL       -0.0469031515  0.0590275774 -1.847727e-02  1.974061e-02
    ## P1HRSWK       0.0447235962 -0.0756769621  4.181201e-02 -3.205471e-02
    ## P1MTHSWRK     0.0472363193 -0.0662585028  2.793994e-02 -2.549510e-02
    ## P1AGE        -0.0627827549 -0.1008062038  9.265457e-02  3.595047e-02
    ## P2GUARD      -0.0422604619 -0.0561482873  2.523129e-02  2.287241e-02
    ## TTLHHINC      0.0902669794 -0.0441689076  1.984464e-02 -9.500068e-02
    ## OWNRNTHB     -0.0570547195  0.0284350561 -3.697679e-02  1.228991e-02
    ## DSBLTY       -0.0231139138  0.0109078553 -1.402939e-02 -1.154538e-02
    ## NUMSIBSX      0.0100970785  0.0187106602 -2.359951e-02  3.233873e-02
    ## PARGRADEX     0.0474728467 -0.0201280869 -8.350494e-02 -1.387867e-01
    ## INTACC       -0.0382802261  0.0355837831 -1.657604e-02  3.928863e-02
    ## CENREG       -0.0092563107  0.0273195500 -6.429263e-03  1.033051e-02
    ## ZIPLOCL       0.0847410304  0.0100625919  6.109750e-02  7.094535e-02
    ## SCHLHRSWK     0.0698654105 -0.0024108757  4.482895e-03 -2.729764e-03
    ## EINTNET       0.0393374585  0.0148495060 -4.752286e-03  5.634899e-03
    ## MOSTIMPT     -0.0301094679 -0.0112309138  2.387276e-06 -3.817934e-03
    ## INTNUM       -0.0376276486 -0.0163491770  8.268744e-03 -1.756977e-05
    ## CHLDNT        0.0977899696  0.0299745506 -3.078922e-02  5.778821e-02
    ## HHPARN19X    -0.0468566895 -0.0435312815  3.011300e-02  3.763002e-02
    ## HHPARN19_BRD -0.0422604619 -0.0561482873  2.523129e-02  2.287241e-02
    ## RACEETH      -0.1633552463  0.0477164993 -4.070595e-02 -7.329959e-03
    ##                  HDHEALTH        CDOBMM       CDOBYY          CSEX
    ## BASMID       -0.002009431 -5.325161e-03  0.006900990 -0.0115749237
    ## EDCPUB       -0.064027466  5.994152e-03  0.022039720  0.0058801478
    ## SPUBCHOIX     0.024357094 -1.092609e-03  0.053902516 -0.0185849967
    ## SCONSIDR     -0.002565808 -5.034099e-03 -0.040888744 -0.0171504031
    ## SEENJOY       0.224956070 -3.899806e-03 -0.226620757 -0.0918688186
    ## SEGRADES      0.124244257 -1.617132e-02  0.303848188 -0.0883453672
    ## SEABSNT       0.232666309 -8.988130e-03 -0.090825118  0.0186714429
    ## SEFUTUREX    -0.224903472  4.757805e-03  0.097076337  0.1170795130
    ## SEGRADEQ      0.283057196 -1.348300e-02 -0.034489554 -0.1527622884
    ## FSSPORTX      0.114574878  1.129125e-02 -0.102850329 -0.0455219582
    ## FSVOL         0.105291552  2.470139e-03 -0.158642356 -0.0412824548
    ## FSMTNG        0.074456767  4.527002e-03 -0.115758809 -0.0246761982
    ## FSPTMTNG      0.046241404  4.994695e-03 -0.110782117 -0.0046045492
    ## FSATCNFN      0.026642066  8.335423e-03 -0.277797039  0.0098089896
    ## FSFUNDRS      0.107837640 -9.529211e-03 -0.116250874 -0.0321829152
    ## FSCOMMTE      0.046361022 -1.188423e-04  0.012432547 -0.0252111778
    ## FSCOUNSLR    -0.074298855  4.602806e-05  0.191711375  0.0243775980
    ## FSFREQ       -0.092777342 -2.707075e-04 -0.028909213  0.0187218902
    ## FSNOTESX     -0.005080643  1.389098e-03 -0.129903359  0.0440989719
    ## FSMEMO        0.060177815  1.368717e-02 -0.079728147 -0.0114865819
    ## FCSCHOOL      0.171224742 -7.054696e-03 -0.099491466 -0.0191479481
    ## FCTEACHR      0.143921243 -1.039260e-02 -0.146566258 -0.0111147762
    ## FCSTDS        0.151126262 -5.950896e-03 -0.075406390 -0.0225943144
    ## FCORDER       0.152498715  2.482360e-03 -0.109823029 -0.0056045103
    ## FCSUPPRT      0.145560997 -1.725697e-03 -0.163583056 -0.0067944971
    ## FHHOME       -0.024284172 -6.370796e-03  0.030966677  0.0502275146
    ## FHWKHRS      -0.038680223 -8.820074e-03 -0.282359624  0.1214312600
    ## FHAMOUNT     -0.002054617  5.397947e-03 -0.072849880  0.0101413295
    ## FHCAMT        0.001308843  5.460286e-03 -0.066656160  0.0429680220
    ## FHPLACE      -0.025445068  1.214555e-03 -0.039356016  0.0426903044
    ## FHCHECKX     -0.080664589  1.776341e-03  0.234596395  0.0026363098
    ## FHHELP       -0.029160842 -8.448923e-03  0.050293235  0.0089336204
    ## FOSTORY2X     0.070260413  1.522341e-02 -0.266947534 -0.0236351033
    ## FOCRAFTS      0.070604473  7.541390e-03 -0.503276521 -0.1513562305
    ## FOGAMES       0.055806428 -2.125430e-03 -0.346737986 -0.0222486197
    ## FOBUILDX      0.076751637 -4.666270e-03 -0.214611672  0.0366819793
    ## FOSPORT       0.147186346 -4.124036e-03 -0.257810472  0.0581312221
    ## FORESPON      0.028248399 -6.478130e-03  0.112635754  0.0253828058
    ## FOHISTX       0.019786447 -9.572591e-03  0.052583492 -0.0233204195
    ## FODINNERX    -0.106866180 -6.349150e-03  0.205552438  0.0132946495
    ## FOLIBRAYX     0.044080411  3.806003e-03 -0.247014564 -0.0454923811
    ## FOBOOKSTX     0.069824960 -3.389002e-03 -0.104355137 -0.1011099400
    ## HDHEALTH      1.000000000 -7.336103e-03 -0.086346875 -0.0105831400
    ## CDOBMM       -0.007336103  1.000000e+00 -0.086524240 -0.0052855485
    ## CDOBYY       -0.086346875 -8.652424e-02  1.000000000  0.0177780647
    ## CSEX         -0.010583140 -5.285549e-03  0.017778065  1.0000000000
    ## CSPEAKX       0.020855357 -5.846786e-03  0.015216103  0.0165645518
    ## HHTOTALXX    -0.007042431  5.794401e-04  0.055479669  0.0028523173
    ## RELATION      0.073376194 -3.196585e-03  0.002336093 -0.0006316206
    ## P1REL         0.065836023 -1.349626e-02 -0.031661038  0.0045649664
    ## P1SEX         0.069358157  6.270422e-03  0.026683215  0.0079705093
    ## P1MRSTA       0.096541468  4.048550e-03  0.058894807 -0.0069393549
    ## P1EMPL        0.080252433  1.371810e-03 -0.005914868 -0.0003995442
    ## P1HRSWK      -0.082910146 -2.633721e-03 -0.032017183  0.0029031729
    ## P1MTHSWRK    -0.088398161  2.218169e-03 -0.024394593  0.0045966102
    ## P1AGE         0.006228300 -2.399530e-03 -0.413657570 -0.0067963164
    ## P2GUARD       0.087691539  3.224367e-03 -0.019338227 -0.0124918281
    ## TTLHHINC     -0.183804430 -8.269703e-03 -0.030845729  0.0186801580
    ## OWNRNTHB      0.087493844 -6.862732e-03  0.057717097 -0.0118958511
    ## DSBLTY       -0.238059378  1.226361e-02  0.042413845  0.1129073460
    ## NUMSIBSX     -0.022227503  1.918886e-03  0.013794256  0.0072219105
    ## PARGRADEX    -0.162932722 -1.296925e-02  0.028210626  0.0126011453
    ## INTACC        0.063675608  1.481889e-03  0.037045630 -0.0060907079
    ## CENREG        0.023005710 -5.414163e-03 -0.006358337 -0.0004897889
    ## ZIPLOCL       0.001128032  1.144631e-02 -0.020494749 -0.0167285551
    ## SCHLHRSWK    -0.058471183 -6.864567e-03  0.012270159 -0.0069643495
    ## EINTNET      -0.040744379  5.772990e-04  0.086307934 -0.0031148707
    ## MOSTIMPT      0.022969829  1.482217e-03 -0.059416055  0.0016456788
    ## INTNUM        0.043795952  2.540822e-03 -0.091457986  0.0130807192
    ## CHLDNT        0.064951621  2.795610e-03  0.250866862 -0.0676291267
    ## HHPARN19X     0.096351307 -7.330330e-03 -0.024473304 -0.0153097237
    ## HHPARN19_BRD  0.087691539  3.224367e-03 -0.019338227 -0.0124918281
    ## RACEETH       0.050534046  1.457238e-02  0.041398901  0.0036265142
    ##                    CSPEAKX     HHTOTALXX      RELATION         P1REL
    ## BASMID       -3.926785e-05 -0.0058061500  0.0027391141 -0.0164537178
    ## EDCPUB        2.788827e-03  0.0068304872 -0.0296102954 -0.0281019058
    ## SPUBCHOIX     5.927379e-02  0.0170971725  0.0365983954  0.0120186779
    ## SCONSIDR      5.495461e-03  0.0590026393  0.0528141958  0.0449591744
    ## SEENJOY      -3.874557e-02 -0.0265077447  0.0070524686  0.0260407522
    ## SEGRADES     -1.638806e-02 -0.0333010979  0.0234056374  0.0433276404
    ## SEABSNT      -1.600439e-02 -0.0181451682  0.0063967186  0.0289483232
    ## SEFUTUREX     8.037385e-02 -0.0080561414 -0.1012034038 -0.1564883883
    ## SEGRADEQ     -4.561217e-02 -0.0332174324  0.0497785599  0.1237189331
    ## FSSPORTX      8.856693e-02 -0.0130653152  0.0428045283  0.0398732158
    ## FSVOL         4.950422e-02 -0.0123169870  0.0613206377  0.0597274532
    ## FSMTNG        6.764607e-02 -0.0166044272  0.0643210568  0.0260632229
    ## FSPTMTNG     -3.776332e-02 -0.0167902285 -0.0185261054 -0.0109118040
    ## FSATCNFN      1.994033e-02 -0.0170705663 -0.0065615212 -0.0079451283
    ## FSFUNDRS      8.633043e-02  0.0054899870  0.0694365246  0.0408283122
    ## FSCOMMTE      2.425616e-02 -0.0163870032  0.0374452770  0.0189116680
    ## FSCOUNSLR     1.019215e-02  0.0188442381 -0.0351722614 -0.0633618984
    ## FSFREQ       -8.576081e-02  0.0186471451 -0.0520151564 -0.0513605592
    ## FSNOTESX      4.876826e-02  0.0186639668  0.0263529948  0.0098248893
    ## FSMEMO        3.980251e-02  0.0251608895  0.0857176029  0.0621809075
    ## FCSCHOOL     -1.340364e-02 -0.0291981179 -0.0033255800  0.0250997641
    ## FCTEACHR     -8.765521e-03 -0.0366610953 -0.0121391464  0.0057159622
    ## FCSTDS        3.540538e-03 -0.0194659962 -0.0006825413  0.0068173152
    ## FCORDER      -1.945056e-02 -0.0100546870 -0.0032751364  0.0162261744
    ## FCSUPPRT     -2.656238e-02 -0.0211833979  0.0001352134  0.0173573050
    ## FHHOME        4.361445e-02  0.0107775806 -0.0105869362 -0.0235175500
    ## FHWKHRS       7.963468e-02 -0.0010902031  0.0026985008 -0.0454762788
    ## FHAMOUNT      2.245961e-02 -0.0083335612 -0.0084719121 -0.0301138786
    ## FHCAMT        2.206179e-03  0.0074470700 -0.0130341806 -0.0407575919
    ## FHPLACE      -2.791326e-03  0.0207969937 -0.0115770036 -0.0573465373
    ## FHCHECKX      7.632660e-03  0.0069402646  0.0057861639 -0.0344903227
    ## FHHELP        5.355095e-02  0.0055496691  0.0096777563 -0.0211660168
    ## FOSTORY2X     1.766451e-02  0.0096217074  0.0468020178  0.0295978746
    ## FOCRAFTS     -1.793765e-02 -0.0403539731 -0.0136379507 -0.0060084972
    ## FOGAMES       1.215800e-02 -0.0521918413  0.0145943267  0.0223373293
    ## FOBUILDX      1.035612e-02 -0.0108927408  0.0121859724  0.0115064565
    ## FOSPORT      -1.791913e-02 -0.0419632981  0.0559679480  0.0629812785
    ## FORESPON     -2.836895e-02  0.0553639826  0.0625398788  0.0106513324
    ## FOHISTX      -1.110663e-01 -0.0037170462 -0.0136142442 -0.0207874548
    ## FODINNERX     7.333537e-02  0.0311501821 -0.0206493806  0.0004262393
    ## FOLIBRAYX    -3.782424e-02 -0.0235773340  0.0118456284  0.0280694152
    ## FOBOOKSTX     1.472435e-02  0.0238256035  0.0269366691  0.0349061406
    ## HDHEALTH      2.085536e-02 -0.0070424306  0.0733761944  0.0658360234
    ## CDOBMM       -5.846786e-03  0.0005794401 -0.0031965847 -0.0134962606
    ## CDOBYY        1.521610e-02  0.0554796692  0.0023360928 -0.0316610378
    ## CSEX          1.656455e-02  0.0028523173 -0.0006316206  0.0045649664
    ## CSPEAKX       1.000000e+00  0.0846311154  0.0371984266 -0.0393108834
    ## HHTOTALXX     8.463112e-02  1.0000000000  0.0913243896 -0.0274452347
    ## RELATION      3.719843e-02  0.0913243896  1.0000000000  0.4547774800
    ## P1REL        -3.931088e-02 -0.0274452347  0.4547774800  1.0000000000
    ## P1SEX        -6.367613e-02 -0.0272519975 -0.2529285714  0.0205920594
    ## P1MRSTA      -5.097519e-02 -0.2171954619  0.1173131475  0.0499955169
    ## P1EMPL        1.560271e-03  0.0398627913  0.0867108377  0.2056722867
    ## P1HRSWK      -1.552973e-02 -0.0595806968 -0.0179156701 -0.1474636101
    ## P1MTHSWRK    -2.365649e-02 -0.0654851868 -0.0573994387 -0.1596089331
    ## P1AGE        -1.325463e-02 -0.1216456533  0.1650255793  0.2975711036
    ## P2GUARD      -2.795826e-02 -0.2948203421  0.1049136806  0.0629959039
    ## TTLHHINC     -1.158874e-01  0.0777499448 -0.0796895454 -0.1425838005
    ## OWNRNTHB      8.578693e-02 -0.0780143838 -0.0181674045  0.0189985234
    ## DSBLTY        9.209770e-02  0.0541394451 -0.0081665280 -0.0808030072
    ## NUMSIBSX      3.405457e-02  0.7723663878 -0.0629329958 -0.1012360228
    ## PARGRADEX    -1.008070e-01 -0.0403322185 -0.1622380282 -0.1216421578
    ## INTACC        2.886615e-02 -0.0078287067  0.0456410253  0.0742417902
    ## CENREG        1.817082e-02  0.0351017953  0.0209143738 -0.0045952252
    ## ZIPLOCL      -1.339197e-01  0.0098338947 -0.0089535335  0.0375044459
    ## SCHLHRSWK    -4.933611e-02  0.0087232679 -0.0275643227 -0.0592029568
    ## EINTNET      -2.978053e-02  0.0179316237 -0.0189902628 -0.0191490952
    ## MOSTIMPT     -3.079016e-03 -0.0187196521  0.0153925355  0.0156146918
    ## INTNUM        2.319760e-02 -0.0165039241  0.0234743578  0.0233376848
    ## CHLDNT       -4.471723e-02  0.0087488151  0.0550848094  0.0822480010
    ## HHPARN19X    -4.108380e-02 -0.2255606721  0.4324317179  0.5904604639
    ## HHPARN19_BRD -2.795826e-02 -0.2948203421  0.1049136806  0.0629959039
    ## RACEETH       2.582806e-01  0.0441891276  0.0555825176  0.0093923992
    ##                      P1SEX       P1MRSTA        P1EMPL       P1HRSWK
    ## BASMID        0.0006888878  0.0014200099 -0.0072318675 -0.0018450265
    ## EDCPUB       -0.0366319773 -0.0662814606  0.0032703614  0.0176797443
    ## SPUBCHOIX    -0.0146545393  0.0343432677  0.0063575397 -0.0064592318
    ## SCONSIDR      0.0002981851 -0.0050954160  0.0150567939 -0.0292504259
    ## SEENJOY       0.0615222151  0.0429496825  0.0198067688 -0.0135864923
    ## SEGRADES      0.0659661940  0.0877886873  0.0464207014 -0.0620298003
    ## SEABSNT       0.0469450912  0.0645305718  0.0872725390 -0.0826943134
    ## SEFUTUREX    -0.0506346369 -0.1031577541 -0.1081201486  0.1022720651
    ## SEGRADEQ      0.1088581626  0.1427328544  0.0750621347 -0.0732810097
    ## FSSPORTX      0.0333741670  0.0748154464  0.0331776759 -0.0536371564
    ## FSVOL         0.0385036272  0.0958397949  0.0136184811 -0.0271858091
    ## FSMTNG       -0.0084723734  0.0487317352  0.0164562881 -0.0217041984
    ## FSPTMTNG      0.0507908864 -0.0017100931 -0.0322054235  0.0071642763
    ## FSATCNFN      0.0276420538 -0.0172764716 -0.0069500571 -0.0045595789
    ## FSFUNDRS     -0.0016747032  0.0789771197  0.0331917224 -0.0465464722
    ## FSCOMMTE      0.0057934754  0.0454280532 -0.0045908781 -0.0086352029
    ## FSCOUNSLR     0.0212453533 -0.0366165642 -0.0481747370  0.0135066371
    ## FSFREQ       -0.0220848501 -0.0871811074 -0.0008568916  0.0285702887
    ## FSNOTESX      0.0328026332  0.0082828729  0.0336465854 -0.0481511440
    ## FSMEMO        0.0043672377  0.0639775865  0.0604757483 -0.0544870633
    ## FCSCHOOL      0.0649189201  0.0788318519  0.0337673828 -0.0272056370
    ## FCTEACHR      0.0446873047  0.0461717032  0.0057488035 -0.0010711765
    ## FCSTDS        0.0367485285  0.0555250611  0.0230930693 -0.0189393840
    ## FCORDER       0.0580679447  0.0560390503  0.0289544133 -0.0240968548
    ## FCSUPPRT      0.0426080278  0.0529620372  0.0102293522 -0.0001111973
    ## FHHOME       -0.0099364091 -0.0180955702  0.0081089152 -0.0144183090
    ## FHWKHRS      -0.0835992111 -0.0629812886 -0.0354001745  0.0630758036
    ## FHAMOUNT     -0.0257642683 -0.0103984096 -0.0255675502  0.0398316027
    ## FHCAMT       -0.0141449483 -0.0212896834 -0.0239900889  0.0350904418
    ## FHPLACE      -0.0437688525 -0.0432447578 -0.0532695687  0.0578891453
    ## FHCHECKX     -0.0214598544  0.0007693094 -0.0139170364  0.0162457416
    ## FHHELP        0.0048933247  0.0072308894  0.0009305092 -0.0040130495
    ## FOSTORY2X    -0.0296817693 -0.0047340774 -0.0049341422  0.0089457559
    ## FOCRAFTS      0.0013326827 -0.0508327512 -0.0328538109  0.0486368427
    ## FOGAMES      -0.0042937093  0.0015968401 -0.0130046451  0.0297107741
    ## FOBUILDX      0.0389399123  0.0201982944  0.0023807547 -0.0073534135
    ## FOSPORT       0.0226832685  0.0264624980  0.0341105365 -0.0275084172
    ## FORESPON     -0.0279301243  0.0177091470  0.0115910956 -0.0050723384
    ## FOHISTX      -0.0131944445 -0.0334139572 -0.0469031515  0.0447235962
    ## FODINNERX     0.0095655890 -0.0297781581  0.0590275774 -0.0756769621
    ## FOLIBRAYX     0.0153830515  0.0088134996 -0.0184772676  0.0418120112
    ## FOBOOKSTX     0.0220859404  0.0312333338  0.0197406139 -0.0320547055
    ## HDHEALTH      0.0693581568  0.0965414677  0.0802524327 -0.0829101465
    ## CDOBMM        0.0062704221  0.0040485502  0.0013718102 -0.0026337212
    ## CDOBYY        0.0266832155  0.0588948069 -0.0059148681 -0.0320171833
    ## CSEX          0.0079705093 -0.0069393549 -0.0003995442  0.0029031729
    ## CSPEAKX      -0.0636761262 -0.0509751930  0.0015602709 -0.0155297339
    ## HHTOTALXX    -0.0272519975 -0.2171954619  0.0398627913 -0.0595806968
    ## RELATION     -0.2529285714  0.1173131475  0.0867108377 -0.0179156701
    ## P1REL         0.0205920594  0.0499955169  0.2056722867 -0.1474636101
    ## P1SEX         1.0000000000  0.1849765694  0.1280447668 -0.2818164524
    ## P1MRSTA       0.1849765694  1.0000000000  0.0023099611 -0.0302192162
    ## P1EMPL        0.1280447668  0.0023099611  1.0000000000 -0.8065548244
    ## P1HRSWK      -0.2818164524 -0.0302192162 -0.8065548244  1.0000000000
    ## P1MTHSWRK    -0.1848795400 -0.0172934911 -0.8439791224  0.7808855462
    ## P1AGE        -0.1916088248 -0.1854227710  0.1652615777 -0.0599148501
    ## P2GUARD       0.1633660093  0.6768936951  0.0161292052 -0.0239700334
    ## TTLHHINC     -0.1997162328 -0.4154695341 -0.2337774388  0.2788932538
    ## OWNRNTHB      0.1174439908  0.2895733661  0.0865108919 -0.1182524969
    ## DSBLTY       -0.0510713491 -0.0590038105 -0.0778302284  0.0624380352
    ## NUMSIBSX     -0.0060640424 -0.1330390746  0.0298097273 -0.0468730476
    ## PARGRADEX    -0.1137562106 -0.2681060743 -0.1650257272  0.1820933586
    ## INTACC        0.0565797370  0.1636664841  0.1269546831 -0.1237522352
    ## CENREG       -0.0102432227 -0.0179563635 -0.0002433114 -0.0086992181
    ## ZIPLOCL       0.0249583318 -0.0800633648  0.0143236310  0.0067876373
    ## SCHLHRSWK    -0.0633249801 -0.0959149241 -0.0450093805  0.0718845891
    ## EINTNET       0.0094008230 -0.0203858978 -0.0212796076  0.0030638708
    ## MOSTIMPT     -0.0136802863  0.0193771505  0.0092745961  0.0062907355
    ## INTNUM       -0.0024424820  0.0334012625  0.0174289947 -0.0021329546
    ## CHLDNT        0.0504998255  0.0884348934  0.0700245579 -0.0845854057
    ## HHPARN19X     0.0230595830  0.4966542054  0.1471236482 -0.1061606763
    ## HHPARN19_BRD  0.1633660093  0.6768936951  0.0161292052 -0.0239700334
    ## RACEETH      -0.0084135292  0.0816826269  0.0192082384 -0.0385572553
    ##                  P1MTHSWRK        P1AGE       P2GUARD      TTLHHINC
    ## BASMID        0.0059030287 -0.009821285 -0.0037019784 -1.357744e-03
    ## EDCPUB        0.0006353014  0.051910300 -0.0547507723  1.478221e-01
    ## SPUBCHOIX    -0.0110816222 -0.037270168  0.0260204890 -6.673815e-02
    ## SCONSIDR     -0.0198410548  0.026404254  0.0097779013 -6.818256e-02
    ## SEENJOY      -0.0201053803  0.053007765  0.0594628672 -5.395234e-02
    ## SEGRADES     -0.0601653420 -0.134862072  0.0571443260 -8.950417e-02
    ## SEABSNT      -0.0913385110  0.026038699  0.0681238446 -9.310569e-02
    ## SEFUTUREX     0.1024423043 -0.005721267 -0.0951732357  2.610220e-01
    ## SEGRADEQ     -0.0721150675 -0.018374264  0.1266199593 -2.032300e-01
    ## FSSPORTX     -0.0385113003  0.010218389  0.0707308347 -1.584377e-01
    ## FSVOL        -0.0170773831  0.026481459  0.0851771061 -1.679854e-01
    ## FSMTNG       -0.0148404162  0.016759762  0.0576563842 -1.057507e-01
    ## FSPTMTNG      0.0290153202  0.008091657  0.0034514541  7.074780e-03
    ## FSATCNFN      0.0026074596  0.091525417  0.0070254310 -1.255477e-02
    ## FSFUNDRS     -0.0393926721  0.019331020  0.0829374518 -1.657068e-01
    ## FSCOMMTE      0.0013262621 -0.028305869  0.0417660635 -7.917267e-02
    ## FSCOUNSLR     0.0321792607 -0.118941366 -0.0515071347  4.890933e-02
    ## FSFREQ        0.0096784135  0.034553837 -0.0813016639  1.452397e-01
    ## FSNOTESX     -0.0426419461  0.042454308  0.0122220155 -5.995365e-02
    ## FSMEMO       -0.0648662647  0.013034655  0.0562415800 -1.591123e-01
    ## FCSCHOOL     -0.0357136097 -0.017114792  0.0653428961 -1.095805e-01
    ## FCTEACHR     -0.0069941566  0.012002366  0.0429043325 -5.691619e-02
    ## FCSTDS       -0.0245506551 -0.027813472  0.0427176276 -9.111428e-02
    ## FCORDER      -0.0323003881 -0.027564157  0.0420250048 -9.658353e-02
    ## FCSUPPRT     -0.0111966651  0.002627644  0.0414748352 -7.304202e-02
    ## FHHOME       -0.0177411590  0.023246751 -0.0155813778  4.852124e-02
    ## FHWKHRS       0.0453303094  0.165753262 -0.0415681730  1.091893e-01
    ## FHAMOUNT      0.0349010086  0.014601347 -0.0093600236  4.010247e-02
    ## FHCAMT        0.0308361614  0.004894879 -0.0243465743  4.865493e-02
    ## FHPLACE       0.0623464033  0.003751690 -0.0420946063  6.858672e-02
    ## FHCHECKX      0.0166150094 -0.109928019 -0.0281401905 -5.084486e-03
    ## FHHELP       -0.0042727307 -0.019761841 -0.0007912519 -3.053862e-02
    ## FOSTORY2X     0.0147309141  0.140545256  0.0205676881 -3.802881e-02
    ## FOCRAFTS      0.0464829226  0.247008402 -0.0051017970  7.661205e-02
    ## FOGAMES       0.0194522313  0.189569978  0.0336295597  1.274584e-02
    ## FOBUILDX     -0.0065006429  0.095225972  0.0337669214 -6.519045e-03
    ## FOSPORT      -0.0272662086  0.147392291  0.0529256310 -5.751182e-02
    ## FORESPON     -0.0151689971 -0.031531706  0.0078785880 -1.797376e-02
    ## FOHISTX       0.0472363193 -0.062782755 -0.0422604619  9.026698e-02
    ## FODINNERX    -0.0662585028 -0.100806204 -0.0561482873 -4.416891e-02
    ## FOLIBRAYX     0.0279399374  0.092654567  0.0252312886  1.984464e-02
    ## FOBOOKSTX    -0.0254950988  0.035950474  0.0228724096 -9.500068e-02
    ## HDHEALTH     -0.0883981609  0.006228300  0.0876915385 -1.838044e-01
    ## CDOBMM        0.0022181685 -0.002399530  0.0032243671 -8.269703e-03
    ## CDOBYY       -0.0243945926 -0.413657570 -0.0193382271 -3.084573e-02
    ## CSEX          0.0045966102 -0.006796316 -0.0124918281  1.868016e-02
    ## CSPEAKX      -0.0236564937 -0.013254625 -0.0279582574 -1.158874e-01
    ## HHTOTALXX    -0.0654851868 -0.121645653 -0.2948203421  7.774994e-02
    ## RELATION     -0.0573994387  0.165025579  0.1049136806 -7.968955e-02
    ## P1REL        -0.1596089331  0.297571104  0.0629959039 -1.425838e-01
    ## P1SEX        -0.1848795400 -0.191608825  0.1633660093 -1.997162e-01
    ## P1MRSTA      -0.0172934911 -0.185422771  0.6768936951 -4.154695e-01
    ## P1EMPL       -0.8439791224  0.165261578  0.0161292052 -2.337774e-01
    ## P1HRSWK       0.7808855462 -0.059914850 -0.0239700334  2.788933e-01
    ## P1MTHSWRK     1.0000000000 -0.088113123 -0.0155693407  2.555018e-01
    ## P1AGE        -0.0881131234  1.000000000 -0.0364150921  1.026874e-01
    ## P2GUARD      -0.0155693407 -0.036415092  1.0000000000 -4.038836e-01
    ## TTLHHINC      0.2555018079  0.102687354 -0.4038836361  1.000000e+00
    ## OWNRNTHB     -0.1202142302 -0.148363611  0.2464450811 -4.224357e-01
    ## DSBLTY        0.0669784038 -0.027443316 -0.0482972345  6.020553e-02
    ## NUMSIBSX     -0.0505135724 -0.109053059 -0.1324868348  2.379786e-02
    ## PARGRADEX     0.1738866659  0.091188782 -0.2421978791  5.664493e-01
    ## INTACC       -0.1350426558 -0.038372380  0.1404320836 -2.939359e-01
    ## CENREG       -0.0050409301 -0.012377439 -0.0133806479  9.271258e-03
    ## ZIPLOCL       0.0016482636 -0.023275554 -0.0688456507  4.493153e-05
    ## SCHLHRSWK     0.0602312253 -0.008720712 -0.0927079847  1.795472e-01
    ## EINTNET       0.0224540848 -0.039891234 -0.0289673354  5.557396e-02
    ## MOSTIMPT     -0.0080340573  0.032810428  0.0283157989 -3.580107e-02
    ## INTNUM       -0.0227026375  0.040481201  0.0326459680 -6.019369e-02
    ## CHLDNT       -0.0854490236 -0.128912450  0.0598474030 -1.463648e-01
    ## HHPARN19X    -0.1121496586  0.186475462  0.6945809732 -3.497456e-01
    ## HHPARN19_BRD -0.0155693407 -0.036415092  1.0000000000 -4.038836e-01
    ## RACEETH      -0.0347850001 -0.033545403  0.0611055691 -1.485916e-01
    ##                  OWNRNTHB        DSBLTY      NUMSIBSX    PARGRADEX
    ## BASMID       -0.003992284 -3.026869e-03 -0.0062467380 -0.002425216
    ## EDCPUB       -0.063539113  1.153106e-02  0.0136564317  0.139175824
    ## SPUBCHOIX     0.039129458  1.396974e-02 -0.0018310754 -0.096814719
    ## SCONSIDR     -0.010613701  5.457721e-02  0.0517786444 -0.113837799
    ## SEENJOY       0.003733050 -1.588061e-01 -0.0147692648 -0.065843700
    ## SEGRADES      0.083230108 -1.711396e-01 -0.0513987809 -0.082671750
    ## SEABSNT       0.065465754 -1.187735e-01 -0.0156010089 -0.090004739
    ## SEFUTUREX    -0.084654610  2.398056e-01 -0.0067573227  0.358226046
    ## SEGRADEQ      0.100688747 -3.080134e-01 -0.0307104357 -0.209573584
    ## FSSPORTX      0.090311323 -3.499676e-02 -0.0122213771 -0.155624724
    ## FSVOL         0.083076374 -3.701378e-02 -0.0093538711 -0.169694757
    ## FSMTNG        0.049453367  3.413318e-03 -0.0121440880 -0.128818774
    ## FSPTMTNG     -0.017123199 -3.920043e-03  0.0003902609  0.003771534
    ## FSATCNFN     -0.001877788  6.805583e-02  0.0026208850 -0.052718579
    ## FSFUNDRS      0.103963283 -3.562667e-02  0.0141207555 -0.148694078
    ## FSCOMMTE      0.045634475 -2.290620e-02 -0.0159273895 -0.072757422
    ## FSCOUNSLR    -0.032926343  1.779883e-01  0.0187994816  0.016816654
    ## FSFREQ       -0.089275047  8.768840e-03  0.0268374113  0.131533472
    ## FSNOTESX      0.025770664  1.123260e-01  0.0247251222 -0.089063376
    ## FSMEMO        0.071242645 -5.876116e-05  0.0120471841 -0.172211757
    ## FCSCHOOL      0.046516307 -9.589345e-02 -0.0156825647 -0.075011518
    ## FCTEACHR      0.018229611 -7.206990e-02 -0.0202111310 -0.038102884
    ## FCSTDS        0.040047114 -6.471681e-02 -0.0083142477 -0.063115461
    ## FCORDER       0.037675788 -7.205791e-02  0.0015574997 -0.076881948
    ## FCSUPPRT      0.026972863 -4.667899e-02 -0.0078054822 -0.061721335
    ## FHHOME        0.012780434  8.666002e-03  0.0041965302  0.061280873
    ## FHWKHRS      -0.036527408  6.031140e-02 -0.0078049016  0.110147486
    ## FHAMOUNT     -0.022536566  3.019499e-02 -0.0079255372  0.050983197
    ## FHCAMT       -0.024984297  1.215212e-02  0.0064225362  0.041565956
    ## FHPLACE      -0.039867907  5.723431e-02  0.0308165072  0.059080301
    ## FHCHECKX      0.001367948  4.460580e-02 -0.0160059923  0.013998337
    ## FHHELP        0.022143318  5.017725e-02 -0.0070620190 -0.036952396
    ## FOSTORY2X    -0.009766036  1.002436e-02  0.0091605651 -0.083856053
    ## FOCRAFTS     -0.064746253 -2.786464e-03 -0.0233339379  0.024401707
    ## FOGAMES      -0.027505267 -1.781905e-02 -0.0463407105 -0.027137912
    ## FOBUILDX      0.020381985  1.579836e-02  0.0043896600 -0.014074209
    ## FOSPORT       0.011035734 -6.091905e-02 -0.0434811323 -0.059908622
    ## FORESPON     -0.014671050  1.081602e-02  0.0503008190 -0.048337209
    ## FOHISTX      -0.057054719 -2.311391e-02  0.0100970785  0.047472847
    ## FODINNERX     0.028435056  1.090786e-02  0.0187106602 -0.020128087
    ## FOLIBRAYX    -0.036976789 -1.402939e-02 -0.0235995111 -0.083504938
    ## FOBOOKSTX     0.012289913 -1.154538e-02  0.0323387349 -0.138786750
    ## HDHEALTH      0.087493844 -2.380594e-01 -0.0222275028 -0.162932722
    ## CDOBMM       -0.006862732  1.226361e-02  0.0019188864 -0.012969249
    ## CDOBYY        0.057717097  4.241385e-02  0.0137942563  0.028210626
    ## CSEX         -0.011895851  1.129073e-01  0.0072219105  0.012601145
    ## CSPEAKX       0.085786935  9.209770e-02  0.0340545692 -0.100806984
    ## HHTOTALXX    -0.078014384  5.413945e-02  0.7723663878 -0.040332219
    ## RELATION     -0.018167405 -8.166528e-03 -0.0629329958 -0.162238028
    ## P1REL         0.018998523 -8.080301e-02 -0.1012360228 -0.121642158
    ## P1SEX         0.117443991 -5.107135e-02 -0.0060640424 -0.113756211
    ## P1MRSTA       0.289573366 -5.900381e-02 -0.1330390746 -0.268106074
    ## P1EMPL        0.086510892 -7.783023e-02  0.0298097273 -0.165025727
    ## P1HRSWK      -0.118252497  6.243804e-02 -0.0468730476  0.182093359
    ## P1MTHSWRK    -0.120214230  6.697840e-02 -0.0505135724  0.173886666
    ## P1AGE        -0.148363611 -2.744332e-02 -0.1090530588  0.091188782
    ## P2GUARD       0.246445081 -4.829723e-02 -0.1324868348 -0.242197879
    ## TTLHHINC     -0.422435707  6.020553e-02  0.0237978567  0.566449337
    ## OWNRNTHB      1.000000000 -2.372551e-02 -0.0124071347 -0.249951082
    ## DSBLTY       -0.023725514  1.000000e+00  0.0415527514  0.020128172
    ## NUMSIBSX     -0.012407135  4.155275e-02  1.0000000000 -0.014863315
    ## PARGRADEX    -0.249951082  2.012817e-02 -0.0148633149  1.000000000
    ## INTACC        0.178547065 -3.541901e-02  0.0126323564 -0.249322627
    ## CENREG        0.028255544  2.331250e-02  0.0166824958 -0.046112662
    ## ZIPLOCL      -0.132753269 -3.438213e-02  0.0116051690 -0.048524366
    ## SCHLHRSWK    -0.095103430  2.269262e-02  0.0260788455  0.156379357
    ## EINTNET      -0.017446140  9.802583e-03  0.0179583749  0.039338094
    ## MOSTIMPT     -0.003478045 -1.068400e-02 -0.0201076950 -0.024533537
    ## INTNUM        0.017024965 -1.375297e-02 -0.0168665546 -0.047250136
    ## CHLDNT        0.077772296 -6.362808e-02  0.0158481173 -0.128839133
    ## HHPARN19X     0.165777338 -6.505053e-02 -0.1800605013 -0.253768814
    ## HHPARN19_BRD  0.246445081 -4.829723e-02 -0.1324868348 -0.242197879
    ## RACEETH       0.134910098  5.964004e-02  0.0007232675 -0.104858893
    ##                     INTACC        CENREG       ZIPLOCL    SCHLHRSWK
    ## BASMID        1.067161e-02 -8.941925e-03  6.343230e-03 -0.002854224
    ## EDCPUB       -5.104877e-03 -1.303171e-02 -6.275516e-02  0.026765294
    ## SPUBCHOIX     4.229917e-02 -1.027822e-01  4.176992e-02 -0.024262669
    ## SCONSIDR      2.885364e-02 -2.689564e-02  1.180520e-01 -0.016336266
    ## SEENJOY       4.963247e-03 -1.663903e-02  5.822153e-02 -0.009188499
    ## SEGRADES      3.809905e-02  4.411600e-02 -3.058344e-02 -0.029757478
    ## SEABSNT       4.641205e-02  1.907982e-02  1.742879e-02 -0.030196407
    ## SEFUTUREX    -1.628578e-01 -2.451608e-02 -1.592782e-01  0.072705619
    ## SEGRADEQ      8.302594e-02  1.631571e-02  4.375761e-02 -0.052520028
    ## FSSPORTX      6.556855e-02  1.187694e-03 -3.614447e-02 -0.038441572
    ## FSVOL         5.719994e-02 -2.307019e-02  3.131802e-03 -0.025116023
    ## FSMTNG        5.168988e-02  3.417851e-03 -1.092676e-03 -0.029465202
    ## FSPTMTNG     -2.121250e-02  3.494261e-02  4.748159e-02  0.050590045
    ## FSATCNFN     -5.943589e-03 -2.901079e-02  2.283838e-02  0.007974882
    ## FSFUNDRS      4.896203e-02 -6.598477e-03 -4.354315e-02 -0.036942137
    ## FSCOMMTE      6.015966e-03  4.495989e-03 -5.673755e-03  0.012751064
    ## FSCOUNSLR    -9.277828e-03  3.033636e-02  1.859292e-02  0.052206386
    ## FSFREQ       -5.063845e-02  2.423024e-02  6.316350e-02  0.039010428
    ## FSNOTESX      4.694544e-02 -2.612489e-03  1.690870e-02 -0.019611399
    ## FSMEMO        1.195641e-01 -2.211851e-02  4.473074e-02 -0.057744813
    ## FCSCHOOL      4.376217e-02 -6.864429e-03  3.167215e-02 -0.022679138
    ## FCTEACHR      1.688316e-02  2.744435e-03  2.339023e-02 -0.006076700
    ## FCSTDS        3.412434e-02  3.512212e-03  4.818804e-02 -0.013146288
    ## FCORDER       2.087158e-02  1.173537e-02  5.588955e-02  0.001574289
    ## FCSUPPRT      2.116415e-02 -3.686122e-03  5.208903e-02  0.013729952
    ## FHHOME       -5.307341e-03 -3.832861e-04 -8.769872e-02  0.011201026
    ## FHWKHRS      -5.882710e-02 -3.163544e-04 -1.041884e-01  0.038162682
    ## FHAMOUNT     -4.245991e-02 -8.466338e-03 -3.687233e-02  0.039528118
    ## FHCAMT       -4.467348e-02 -1.572537e-02 -1.389551e-02  0.041784577
    ## FHPLACE      -4.695738e-02 -1.045502e-02 -7.065426e-03  0.047758194
    ## FHCHECKX     -2.664001e-02 -6.417293e-03 -9.976081e-03  0.012206543
    ## FHHELP       -2.483412e-03 -2.669705e-02 -3.912431e-02 -0.008666835
    ## FOSTORY2X     4.878175e-03 -1.462869e-02  1.023004e-02 -0.019397056
    ## FOCRAFTS     -4.249835e-02 -1.019413e-03  1.502821e-02  0.036664895
    ## FOGAMES      -2.115299e-02 -1.620612e-02  3.729960e-03 -0.005224028
    ## FOBUILDX     -6.587647e-04 -1.062513e-02 -4.227381e-02  0.005254878
    ## FOSPORT      -3.052208e-05 -2.404496e-05 -2.000890e-03  0.004641395
    ## FORESPON      1.053020e-02 -4.811075e-04  2.237748e-02  0.020629424
    ## FOHISTX      -3.828023e-02 -9.256311e-03  8.474103e-02  0.069865410
    ## FODINNERX     3.558378e-02  2.731955e-02  1.006259e-02 -0.002410876
    ## FOLIBRAYX    -1.657604e-02 -6.429263e-03  6.109750e-02  0.004482895
    ## FOBOOKSTX     3.928863e-02  1.033051e-02  7.094535e-02 -0.002729764
    ## HDHEALTH      6.367561e-02  2.300571e-02  1.128032e-03 -0.058471183
    ## CDOBMM        1.481889e-03 -5.414163e-03  1.144631e-02 -0.006864567
    ## CDOBYY        3.704563e-02 -6.358337e-03 -2.049475e-02  0.012270159
    ## CSEX         -6.090708e-03 -4.897889e-04 -1.672856e-02 -0.006964349
    ## CSPEAKX       2.886615e-02  1.817082e-02 -1.339197e-01 -0.049336106
    ## HHTOTALXX    -7.828707e-03  3.510180e-02  9.833895e-03  0.008723268
    ## RELATION      4.564103e-02  2.091437e-02 -8.953533e-03 -0.027564323
    ## P1REL         7.424179e-02 -4.595225e-03  3.750445e-02 -0.059202957
    ## P1SEX         5.657974e-02 -1.024322e-02  2.495833e-02 -0.063324980
    ## P1MRSTA       1.636665e-01 -1.795636e-02 -8.006336e-02 -0.095914924
    ## P1EMPL        1.269547e-01 -2.433114e-04  1.432363e-02 -0.045009380
    ## P1HRSWK      -1.237522e-01 -8.699218e-03  6.787637e-03  0.071884589
    ## P1MTHSWRK    -1.350427e-01 -5.040930e-03  1.648264e-03  0.060231225
    ## P1AGE        -3.837238e-02 -1.237744e-02 -2.327555e-02 -0.008720712
    ## P2GUARD       1.404321e-01 -1.338065e-02 -6.884565e-02 -0.092707985
    ## TTLHHINC     -2.939359e-01  9.271258e-03  4.493153e-05  0.179547173
    ## OWNRNTHB      1.785471e-01  2.825554e-02 -1.327533e-01 -0.095103430
    ## DSBLTY       -3.541901e-02  2.331250e-02 -3.438213e-02  0.022692621
    ## NUMSIBSX      1.263236e-02  1.668250e-02  1.160517e-02  0.026078846
    ## PARGRADEX    -2.493226e-01 -4.611266e-02 -4.852437e-02  0.156379357
    ## INTACC        1.000000e+00 -2.011599e-03  5.790047e-02 -0.081039395
    ## CENREG       -2.011599e-03  1.000000e+00 -8.414347e-02 -0.013152339
    ## ZIPLOCL       5.790047e-02 -8.414347e-02  1.000000e+00  0.041374054
    ## SCHLHRSWK    -8.103940e-02 -1.315234e-02  4.137405e-02  1.000000000
    ## EINTNET      -2.762989e-02 -8.504314e-03 -6.554902e-03  0.063979531
    ## MOSTIMPT      1.940191e-02  1.196500e-02  1.586109e-02 -0.039298780
    ## INTNUM        2.017485e-02  1.508761e-02  1.029001e-02 -0.058137095
    ## CHLDNT        3.314316e-01  7.612820e-03  7.859818e-02 -0.021919990
    ## HHPARN19X     1.450775e-01 -8.815663e-03 -1.221977e-02 -0.096839558
    ## HHPARN19_BRD  1.404321e-01 -1.338065e-02 -6.884565e-02 -0.092707985
    ## RACEETH       4.569597e-02  9.388199e-02 -2.116291e-01 -0.069972311
    ##                   EINTNET      MOSTIMPT        INTNUM        CHLDNT
    ## BASMID        0.006965510 -8.528477e-03 -8.079452e-03  0.0118301636
    ## EDCPUB        0.004037884 -3.990222e-03 -6.304665e-04  0.0137364763
    ## SPUBCHOIX     0.008710621 -1.823771e-02 -1.344242e-02  0.0508432483
    ## SCONSIDR      0.040551564 -5.023523e-02 -4.554002e-02  0.0200931382
    ## SEENJOY      -0.039125873  3.812722e-02  4.388634e-02  0.0274105172
    ## SEGRADES      0.022630209 -1.429765e-02 -1.655718e-02  0.2108083620
    ## SEABSNT      -0.042398002  3.431821e-02  4.890406e-02  0.0308617984
    ## SEFUTUREX     0.042539442 -3.229579e-02 -4.519074e-02 -0.1985199415
    ## SEGRADEQ     -0.008822985  1.042573e-02  1.581699e-02  0.1510815613
    ## FSSPORTX      0.014013317 -1.136719e-02 -8.467623e-03  0.0446374966
    ## FSVOL         0.037287657 -2.490513e-02 -3.021302e-02  0.0257437345
    ## FSMTNG        0.001201492 -1.423085e-03  1.149358e-03  0.0379365848
    ## FSPTMTNG      0.055805861 -3.528032e-02 -5.040219e-02  0.0299320398
    ## FSATCNFN      0.004554364 -8.757773e-03  2.988378e-04 -0.0529960106
    ## FSFUNDRS      0.013922705 -8.078897e-03 -8.685710e-03  0.0325773133
    ## FSCOMMTE      0.066893534 -4.369896e-02 -5.575595e-02  0.0343853258
    ## FSCOUNSLR     0.086634686 -6.622479e-02 -8.177660e-02  0.0697392667
    ## FSFREQ        0.003437246 -9.701402e-04  5.308782e-03 -0.0545508619
    ## FSNOTESX      0.025293819 -2.489009e-02 -1.822005e-02 -0.0140606441
    ## FSMEMO       -0.020548816  1.941118e-02  2.247259e-02  0.0529765905
    ## FCSCHOOL     -0.019630202  2.543508e-02  2.841300e-02  0.0318674179
    ## FCTEACHR     -0.012784564  1.871323e-02  2.429286e-02  0.0005524703
    ## FCSTDS       -0.020228009  2.754388e-02  3.024102e-02  0.0420483752
    ## FCORDER      -0.006109769  1.228581e-02  2.364765e-02  0.0176675617
    ## FCSUPPRT     -0.007412325  1.666428e-02  2.010151e-02 -0.0047001418
    ## FHHOME        0.007712445 -5.146482e-03  3.677922e-03 -0.0237715870
    ## FHWKHRS      -0.048855672  4.279804e-02  5.612972e-02 -0.2482186633
    ## FHAMOUNT     -0.002977422  1.179394e-02  7.749460e-03 -0.1055075494
    ## FHCAMT       -0.006077222  1.762638e-02  1.854476e-02 -0.1352752427
    ## FHPLACE       0.019071709 -1.411205e-02 -9.545695e-03 -0.0932913636
    ## FHCHECKX      0.014971991 -2.752749e-03 -1.370870e-02 -0.0588085628
    ## FHHELP        0.007179577 -1.603518e-04 -2.346150e-03 -0.0641943837
    ## FOSTORY2X    -0.017540161  6.504731e-03  2.255201e-02 -0.0126511804
    ## FOCRAFTS     -0.018376500  7.119682e-03  1.474399e-02 -0.0939153352
    ## FOGAMES      -0.027829603  1.858352e-02  2.939993e-02 -0.0585979098
    ## FOBUILDX      0.004757016 -3.962941e-03 -6.051859e-04  0.0068921315
    ## FOSPORT      -0.019534963  1.476980e-02  2.001124e-02 -0.0012587163
    ## FORESPON      0.016909897 -8.533463e-03 -1.316831e-02  0.1073827785
    ## FOHISTX       0.039337458 -3.010947e-02 -3.762765e-02  0.0977899696
    ## FODINNERX     0.014849506 -1.123091e-02 -1.634918e-02  0.0299745506
    ## FOLIBRAYX    -0.004752286  2.387276e-06  8.268744e-03 -0.0307892172
    ## FOBOOKSTX     0.005634899 -3.817934e-03 -1.756977e-05  0.0577882093
    ## HDHEALTH     -0.040744379  2.296983e-02  4.379595e-02  0.0649516211
    ## CDOBMM        0.000577299  1.482217e-03  2.540822e-03  0.0027956097
    ## CDOBYY        0.086307934 -5.941605e-02 -9.145799e-02  0.2508668618
    ## CSEX         -0.003114871  1.645679e-03  1.308072e-02 -0.0676291267
    ## CSPEAKX      -0.029780532 -3.079016e-03  2.319760e-02 -0.0447172303
    ## HHTOTALXX     0.017931624 -1.871965e-02 -1.650392e-02  0.0087488151
    ## RELATION     -0.018990263  1.539254e-02  2.347436e-02  0.0550848094
    ## P1REL        -0.019149095  1.561469e-02  2.333768e-02  0.0822480010
    ## P1SEX         0.009400823 -1.368029e-02 -2.442482e-03  0.0504998255
    ## P1MRSTA      -0.020385898  1.937715e-02  3.340126e-02  0.0884348934
    ## P1EMPL       -0.021279608  9.274596e-03  1.742899e-02  0.0700245579
    ## P1HRSWK       0.003063871  6.290736e-03 -2.132955e-03 -0.0845854057
    ## P1MTHSWRK     0.022454085 -8.034057e-03 -2.270264e-02 -0.0854490236
    ## P1AGE        -0.039891234  3.281043e-02  4.048120e-02 -0.1289124503
    ## P2GUARD      -0.028967335  2.831580e-02  3.264597e-02  0.0598474030
    ## TTLHHINC      0.055573958 -3.580107e-02 -6.019369e-02 -0.1463647598
    ## OWNRNTHB     -0.017446140 -3.478045e-03  1.702496e-02  0.0777722962
    ## DSBLTY        0.009802583 -1.068400e-02 -1.375297e-02 -0.0636280775
    ## NUMSIBSX      0.017958375 -2.010770e-02 -1.686655e-02  0.0158481173
    ## PARGRADEX     0.039338094 -2.453354e-02 -4.725014e-02 -0.1288391328
    ## INTACC       -0.027629888  1.940191e-02  2.017485e-02  0.3314316491
    ## CENREG       -0.008504314  1.196500e-02  1.508761e-02  0.0076128202
    ## ZIPLOCL      -0.006554902  1.586109e-02  1.029001e-02  0.0785981844
    ## SCHLHRSWK     0.063979531 -3.929878e-02 -5.813710e-02 -0.0219199896
    ## EINTNET       1.000000000 -7.896023e-01 -8.580089e-01  0.0396077690
    ## MOSTIMPT     -0.789602346  1.000000e+00  7.394502e-01 -0.0244452109
    ## INTNUM       -0.858008855  7.394502e-01  1.000000e+00 -0.0402066133
    ## CHLDNT        0.039607769 -2.444521e-02 -4.020661e-02  1.0000000000
    ## HHPARN19X    -0.041312991  3.388825e-02  4.722281e-02  0.0871797890
    ## HHPARN19_BRD -0.028967335  2.831580e-02  3.264597e-02  0.0598474030
    ## RACEETH      -0.017194141  3.709690e-03  1.719626e-02 -0.0260424995
    ##                 HHPARN19X  HHPARN19_BRD       RACEETH
    ## BASMID       -0.005176147 -0.0037019784  0.0021176567
    ## EDCPUB       -0.054249311 -0.0547507723 -0.0374313087
    ## SPUBCHOIX     0.030430643  0.0260204890  0.0268895345
    ## SCONSIDR      0.043988938  0.0097779013 -0.0372235010
    ## SEENJOY       0.050474902  0.0594628672 -0.0572513550
    ## SEGRADES      0.061276966  0.0571443260  0.0104562831
    ## SEABSNT       0.065673460  0.0681238446 -0.0136102392
    ## SEFUTUREX    -0.157728300 -0.0951732357  0.0926602454
    ## SEGRADEQ      0.141998366  0.1266199593 -0.0135174443
    ## FSSPORTX      0.063150244  0.0707308347  0.0706003836
    ## FSVOL         0.090118265  0.0851771061  0.0417539047
    ## FSMTNG        0.057719750  0.0576563842  0.0425892704
    ## FSPTMTNG     -0.010622078  0.0034514541 -0.0540340053
    ## FSATCNFN     -0.003705536  0.0070254310  0.0066377949
    ## FSFUNDRS      0.072610523  0.0829374518  0.0740149159
    ## FSCOMMTE      0.039919481  0.0417660635  0.0315342576
    ## FSCOUNSLR    -0.069692670 -0.0515071347 -0.0034833267
    ## FSFREQ       -0.077867819 -0.0813016639 -0.0878963949
    ## FSNOTESX      0.015426144  0.0122220155  0.0337952632
    ## FSMEMO        0.084014737  0.0562415800  0.0375487417
    ## FCSCHOOL      0.048902481  0.0653428961  0.0232226697
    ## FCTEACHR      0.027980014  0.0429043325  0.0196875910
    ## FCSTDS        0.029544370  0.0427176276  0.0343962310
    ## FCORDER       0.028317652  0.0420250048  0.0060096430
    ## FCSUPPRT      0.031223291  0.0414748352 -0.0005393767
    ## FHHOME       -0.021924722 -0.0155813778  0.0469388045
    ## FHWKHRS      -0.056126666 -0.0415681730  0.0767700708
    ## FHAMOUNT     -0.027806743 -0.0093600236  0.0335058101
    ## FHCAMT       -0.046263389 -0.0243465743  0.0060777807
    ## FHPLACE      -0.062682648 -0.0420946063 -0.0076754362
    ## FHCHECKX     -0.036983667 -0.0281401905  0.0331279743
    ## FHHELP       -0.011177602 -0.0007912519  0.0633638060
    ## FOSTORY2X     0.033387621  0.0205676881  0.0204185814
    ## FOCRAFTS     -0.005939132 -0.0051017970 -0.0371000983
    ## FOGAMES       0.035623417  0.0336295597  0.0127612377
    ## FOBUILDX      0.017697569  0.0337669214  0.0220783440
    ## FOSPORT       0.062370932  0.0529256310  0.0024951397
    ## FORESPON      0.023929873  0.0078785880 -0.0376339856
    ## FOHISTX      -0.046856689 -0.0422604619 -0.1633552463
    ## FODINNERX    -0.043531281 -0.0561482873  0.0477164993
    ## FOLIBRAYX     0.030112998  0.0252312886 -0.0407059549
    ## FOBOOKSTX     0.037630021  0.0228724096 -0.0073299591
    ## HDHEALTH      0.096351307  0.0876915385  0.0505340464
    ## CDOBMM       -0.007330330  0.0032243671  0.0145723835
    ## CDOBYY       -0.024473304 -0.0193382271  0.0413989006
    ## CSEX         -0.015309724 -0.0124918281  0.0036265142
    ## CSPEAKX      -0.041083803 -0.0279582574  0.2582806445
    ## HHTOTALXX    -0.225560672 -0.2948203421  0.0441891276
    ## RELATION      0.432431718  0.1049136806  0.0555825176
    ## P1REL         0.590460464  0.0629959039  0.0093923992
    ## P1SEX         0.023059583  0.1633660093 -0.0084135292
    ## P1MRSTA       0.496654205  0.6768936951  0.0816826269
    ## P1EMPL        0.147123648  0.0161292052  0.0192082384
    ## P1HRSWK      -0.106160676 -0.0239700334 -0.0385572553
    ## P1MTHSWRK    -0.112149659 -0.0155693407 -0.0347850001
    ## P1AGE         0.186475462 -0.0364150921 -0.0335454027
    ## P2GUARD       0.694580973  1.0000000000  0.0611055691
    ## TTLHHINC     -0.349745605 -0.4038836361 -0.1485916251
    ## OWNRNTHB      0.165777338  0.2464450811  0.1349100983
    ## DSBLTY       -0.065050531 -0.0482972345  0.0596400431
    ## NUMSIBSX     -0.180060501 -0.1324868348  0.0007232675
    ## PARGRADEX    -0.253768814 -0.2421978791 -0.1048588929
    ## INTACC        0.145077488  0.1404320836  0.0456959724
    ## CENREG       -0.008815663 -0.0133806479  0.0938819913
    ## ZIPLOCL      -0.012219773 -0.0688456507 -0.2116291077
    ## SCHLHRSWK    -0.096839558 -0.0927079847 -0.0699723110
    ## EINTNET      -0.041312991 -0.0289673354 -0.0171941405
    ## MOSTIMPT      0.033888254  0.0283157989  0.0037096897
    ## INTNUM        0.047222806  0.0326459680  0.0171962618
    ## CHLDNT        0.087179789  0.0598474030 -0.0260424995
    ## HHPARN19X     1.000000000  0.6945809732  0.0353894936
    ## HHPARN19_BRD  0.694580973  1.0000000000  0.0611055691
    ## RACEETH       0.035389494  0.0611055691  1.0000000000

``` r
# Splitting the dataset into training and testing sets
set.seed(123) # For reproducibility
data_split <- initial_split(data_combined, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)
```

``` r
# Defining the model using GLM for logistic regression
glm_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")
```

``` r
# Defining the recipe (including factors of interest for research questions)
recipe <- recipe(SCCHOICE ~ SPUBCHOIX + SCONSIDR + SEGRADES + FSSPORTX + FSMTNG + FSPTMTNG + FSATCNFN + FHHOME + FHCAMT + FHPLACE, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())
```

``` r
# Fitting the model
glm_fit <- workflow() %>%
  add_model(glm_model) %>%
  add_recipe(recipe) %>%
  fit(data = train_data)
```
