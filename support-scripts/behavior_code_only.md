
# Cleaning the “behavior” table

### Code only

[**Go to Project Summary**](../readme.md)

``` r
library(readr)
library(dplyr)

cat("libraries loaded\n")

behavior <- read_delim("../original-data/DiverReef_behavior_data.csv",
                       delim = ";")

cat("data loaded")
```

    ## libraries loaded
    ## data loaded

``` r
glimpse(behavior)
```

    ## Rows: 2,312
    ## Columns: 8
    ## $ diver_id        <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    ## $ sex             <chr> "male", "male", "male", "male", "female", "male", "mal…
    ## $ experience      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ camera          <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", …
    ## $ samp_timing_min <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10…
    ## $ damage          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ contact         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ total           <dbl> 2, 5, 2, 8, 12, 11, 3, 12, 10, 18, 2, 3, 10, 15, 7, 10…

``` r
behavior <- behavior %>%
  rename(
    gender = sex,
    samp_time_min = samp_timing_min,
    total_interactions = total
  )

colnames(behavior)
```

    ## [1] "diver_id"           "gender"             "experience"        
    ## [4] "camera"             "samp_time_min"      "damage"            
    ## [7] "contact"            "total_interactions"

``` r
length(unique(behavior$diver_id))  # check number of unique diver_id
class(behavior$diver_id)

# check the range of values in diver_id
min(behavior$diver_id)
max(behavior$diver_id)
```

    ## [1] 2312
    ## [1] "numeric"
    ## [1] 1
    ## [1] 2312

``` r
unique(behavior$gender)
class(behavior$gender)
```

    ## [1] "male"   "female" NA      
    ## [1] "character"

``` r
behavior <- behavior %>%
  mutate(gender = as.factor(gender))  # convert to factor

class(behavior$gender)
```

    ## [1] "factor"

``` r
class(behavior$experience)
min(behavior$experience, na.rm = TRUE)  # ignore NA values
max(behavior$experience, na.rm = TRUE)
```

    ## [1] "numeric"
    ## [1] 1
    ## [1] 10000

``` r
unique(behavior$camera)
class(behavior$camera)
```

    ## [1] "no"  "yes" NA   
    ## [1] "character"

``` r
behavior <- behavior %>%
  mutate(camera = as.factor(camera))

class(behavior$camera)
```

    ## [1] "factor"

``` r
class(behavior$samp_time_min)
min(behavior$samp_time_min, na.rm = TRUE)
max(behavior$samp_time_min, na.rm = TRUE)
```

    ## [1] "numeric"
    ## [1] 5
    ## [1] 72

``` r
cat(" damage:\n")
class(behavior$damage)
min(behavior$damage, na.rm = TRUE)
max(behavior$damage, na.rm = TRUE)

cat("\n contact:\n")
class(behavior$contact)
min(behavior$contact, na.rm = TRUE)
max(behavior$contact, na.rm = TRUE)

cat("\n total_interactions:\n")
class(behavior$total_interactions)
min(behavior$total_interactions, na.rm = TRUE)
max(behavior$total_interactions, na.rm = TRUE)
```

    ##  damage:
    ## [1] "numeric"
    ## [1] 0
    ## [1] 338
    ## 
    ##  contact:
    ## [1] "numeric"
    ## [1] 0
    ## [1] 346
    ## 
    ##  total_interactions:
    ## [1] "numeric"
    ## [1] 0
    ## [1] 684

``` r
# total number of observations
a <- nrow(behavior)

# count records where damage + contact == total_interactions
b <- nrow(behavior %>% filter(damage + contact == total_interactions))

# percentage of observations where damage + contact == total_interactions
cat("percentage of records where damage + contact == total_interactions:\n")
b / a * 100

# check if there are any records where total_interactions is NA
cat("number of records where total_interactions is NA:\n")
nrow(behavior %>% filter(is.na(total_interactions)))
```

    ## percentage of records where damage + contact == total_interactions:
    ## [1] 74.78374
    ## number of records where total_interactions is NA:
    ## [1] 0

``` r
# count records where damage AND contact is NA
c <- nrow(behavior %>% filter(is.na(damage) & is.na(contact)))

# percentage of observations where damage AND contact are NA
c / a * 100
```

    ## [1] 14.35986

``` r
# first scenario
# count records where neither damage or contact is NA
# and they don't add up to total_interactions
d <- nrow(behavior %>% filter(damage + contact != total_interactions))

# second scenario: total
# count records where only one of damage or contact is NA
e <- nrow(behavior %>% filter(xor(is.na(damage), is.na(contact))))

# percentage of observations that have no plausible explanation
# for entries in these fields
cat(" percentage where entries in damage, contact or total_interactions don't make sense:\n")
(d + e) / a * 100


# second scenario: special cases
# count records where only one of damage or contact is NA
# AND that field is equal to total_interactions
cat("\n number of plausible records where only one of damage or contact is NA:\n")
nrow(behavior %>%
       filter(xor(is.na(damage), is.na(contact)) &
                ((damage | contact) == total_interactions)))

# did I get everything?
cat("\n did I account for all of the data?\n")
a == b + c + d + e
```

    ##  percentage where entries in damage, contact or total_interactions don't make sense:
    ## [1] 10.8564
    ## 
    ##  number of plausible records where only one of damage or contact is NA:
    ## [1] 0
    ## 
    ##  did I account for all of the data?
    ## [1] TRUE

``` r
# select only records where the two conditions reasoned before are met
behavior <- behavior %>%
  filter(
    # if damage + contact == total_interactions OR
    (damage + contact == total_interactions) |
      # if damage AND contact are NA
      (is.na(damage) & is.na(contact)))
```

``` r
cat("damage:\n")
max(behavior$damage, na.rm = TRUE)

cat("contact:\n")
max(behavior$contact, na.rm = TRUE)

cat("total interactions:\n")
max(behavior$total_interactions, na.rm = TRUE)
```

    ## damage:
    ## [1] 338
    ## contact:
    ## [1] 346
    ## total interactions:
    ## [1] 684

``` r
# check records with high total_interactions numbers
behavior %>%
  filter(total_interactions > 300) %>%  # 300 is arbitrary
  select(-gender)  # only for total_interactions to fit in the output
```

    ## # A tibble: 5 × 7
    ##   diver_id experience camera samp_time_min damage contact total_interactions
    ##      <dbl>      <dbl> <fct>          <dbl>  <dbl>   <dbl>              <dbl>
    ## 1      232        150 no                10    169     213                382
    ## 2      238       1400 no                10    179     187                366
    ## 3      240         40 no                10    190     208                398
    ## 4      263        240 no                10    195     211                406
    ## 5      360         70 no                10    338     346                684

``` r
behavior <- behavior %>%
  # create interactions_per_min and round to 4 digits
  mutate(interactions_per_min = round(total_interactions / samp_time_min, 4))
```

``` r
# check max interactions_per_min values when sampling time was less than 10 min
behavior %>%
  filter(samp_time_min < 10) %>%
  summarize(max(interactions_per_min)) %>%
  pull()  # show only the result instead of the data frame
```

    ## [1] 7.7778

``` r
# total number of records that will be dropped
nrow(behavior %>% filter(interactions_per_min > 12))

# select only observations with values in interactions_per_min less than 12
behavior <- behavior %>%
  filter(interactions_per_min <= 12)
```

    ## [1] 35

``` r
glimpse(behavior)
```

    ## Rows: 2,026
    ## Columns: 9
    ## $ diver_id             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
    ## $ gender               <fct> male, male, male, male, female, male, male, male,…
    ## $ experience           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ camera               <fct> no, no, no, no, no, no, no, no, no, no, no, no, n…
    ## $ samp_time_min        <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1…
    ## $ damage               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ contact              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ total_interactions   <dbl> 2, 5, 2, 8, 12, 11, 3, 12, 10, 18, 2, 3, 10, 15, …
    ## $ interactions_per_min <dbl> 0.2, 0.5, 0.2, 0.8, 1.2, 1.1, 0.3, 1.2, 1.0, 1.8,…

[**Go to Project Summary**](../readme.md)
