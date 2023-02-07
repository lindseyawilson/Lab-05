Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Lindsey Wilson
2/7/23

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
dn <- dennys
lq <- laquinta
```

### Exercise 1

The code below gives the number of Denny’s locations in Alaska:

``` r
dn_ak <- dn %>%
  filter(state == "AK")
message("There are ", nrow(dn_ak), " Denny's locations in Alaska")
```

    ## There are 3 Denny's locations in Alaska

### Exercise 2

And this tells us how many La Quinta locations are in Alaska:

``` r
lq_ak <- lq %>%
  filter(state == "AK")
message("There are ", nrow(lq_ak), " La Quinta locations in Alaska")
```

    ## There are 2 La Quinta locations in Alaska

### Exercise 3

Since there are 3 Denny’s and 2 La Quinta’s in Alaska, if we want to
calculate the distances between each pair of Denny’s and La Quinta then
in total we’ll need to calculate 6 distances.

### Exercise 4

Below we’ve used `full_join()` to put together the `dn_ak` and `lq_ak`
datasets:

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x      city.x state zip.x longi…¹ latit…² addre…³ city.y zip.y longi…⁴
    ##   <chr>          <chr>  <chr> <chr>   <dbl>   <dbl> <chr>   <chr>  <chr>   <dbl>
    ## 1 2900 Denali    Ancho… AK    99503   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 2 2900 Denali    Ancho… AK    99503   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 3 3850 Debarr R… Ancho… AK    99508   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 4 3850 Debarr R… Ancho… AK    99508   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 5 1929 Airport … Fairb… AK    99701   -148.    64.8 3501 M… "\nAn… 99503   -150.
    ## 6 1929 Airport … Fairb… AK    99701   -148.    64.8 4920 D… "\nFa… 99709   -148.
    ## # … with 1 more variable: latitude.y <dbl>, and abbreviated variable names
    ## #   ¹​longitude.x, ²​latitude.x, ³​address.y, ⁴​longitude.y

This code tells us how many observations are in our newly created
dataset, along with the names of the columns

``` r
message("There are ", nrow(dn_lq_ak), " observations in the dn_lq_ak dataset")
```

    ## There are 6 observations in the dn_lq_ak dataset

``` r
names(dn_lq_ak)
```

    ##  [1] "address.x"   "city.x"      "state"       "zip.x"       "longitude.x"
    ##  [6] "latitude.x"  "address.y"   "city.y"      "zip.y"       "longitude.y"
    ## [11] "latitude.y"

### Exercise 5

It looks like each row represents a Denny’s/La Quinta pair, so to get
the distances between each pair we can just calculate a distance column
for each row. We would do this with the `mutate()` function.

### Exercise 6

This code creates a function to calculate distance for each row:

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

Which allows us to calculate a distance variable for each row in
`dn_lq_ak`:

``` r
dn_lq_ak <- dn_lq_ak %>%
  mutate(distance = haversine(longitude.x,
                              latitude.x,
                              longitude.y,
                              latitude.y
                              ))
```
