Lab 03 - Nobel laureates
================
Barbara Mu
01/23/2026

### Load packages and data

``` r
library(tidyverse) 
```

``` r
nobel <- read_csv("data/nobel.csv")
```

## Exercises

### Exercise 1

There are 935 observations and 26 variables. Each row represents an
individual Nobel Prize winner. A single person can appear more than once
if they won multiple prizes in different years or categories, such as
Marie Curie.

``` r
head(nobel)
```

    ## # A tibble: 6 × 26
    ##      id firstname    surname  year category affiliation city  country born_date 
    ##   <dbl> <chr>        <chr>   <dbl> <chr>    <chr>       <chr> <chr>   <date>    
    ## 1     1 Wilhelm Con… Röntgen  1901 Physics  Munich Uni… Muni… Germany 1845-03-27
    ## 2     2 Hendrik A.   Lorentz  1902 Physics  Leiden Uni… Leid… Nether… 1853-07-18
    ## 3     3 Pieter       Zeeman   1902 Physics  Amsterdam … Amst… Nether… 1865-05-25
    ## 4     4 Henri        Becque…  1903 Physics  École Poly… Paris France  1852-12-15
    ## 5     5 Pierre       Curie    1903 Physics  École muni… Paris France  1859-05-15
    ## 6     6 Marie        Curie    1903 Physics  <NA>        <NA>  <NA>    1867-11-07
    ## # ℹ 17 more variables: died_date <date>, gender <chr>, born_city <chr>,
    ## #   born_country <chr>, born_country_code <chr>, died_city <chr>,
    ## #   died_country <chr>, died_country_code <chr>, overall_motivation <chr>,
    ## #   share <dbl>, motivation <chr>, born_country_original <chr>,
    ## #   born_city_original <chr>, died_country_original <chr>,
    ## #   died_city_original <chr>, city_original <chr>, country_original <chr>

``` r
summary(nobel)
```

    ##        id         firstname           surname               year     
    ##  Min.   :  1.0   Length:935         Length:935         Min.   :1901  
    ##  1st Qu.:234.5   Class :character   Class :character   1st Qu.:1947  
    ##  Median :470.0   Mode  :character   Mode  :character   Median :1976  
    ##  Mean   :475.1                                         Mean   :1970  
    ##  3rd Qu.:716.5                                         3rd Qu.:1999  
    ##  Max.   :969.0                                         Max.   :2018  
    ##                                                                      
    ##    category         affiliation            city             country         
    ##  Length:935         Length:935         Length:935         Length:935        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    born_date            died_date             gender         
    ##  Min.   :1817-11-30   Min.   :1903-11-01   Length:935        
    ##  1st Qu.:1890-12-25   1st Qu.:1955-08-02   Class :character  
    ##  Median :1916-06-28   Median :1983-03-09   Mode  :character  
    ##  Mean   :1910-11-02   Mean   :1977-03-30                     
    ##  3rd Qu.:1935-07-10   3rd Qu.:2004-07-24                     
    ##  Max.   :1997-07-12   Max.   :2019-08-07                     
    ##  NA's   :33           NA's   :308                            
    ##   born_city         born_country       born_country_code   died_city        
    ##  Length:935         Length:935         Length:935         Length:935        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  died_country       died_country_code  overall_motivation     share      
    ##  Length:935         Length:935         Length:935         Min.   :1.000  
    ##  Class :character   Class :character   Class :character   1st Qu.:1.000  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2.000  
    ##                                                           Mean   :1.991  
    ##                                                           3rd Qu.:3.000  
    ##                                                           Max.   :4.000  
    ##                                                                          
    ##   motivation        born_country_original born_city_original
    ##  Length:935         Length:935            Length:935        
    ##  Class :character   Class :character      Class :character  
    ##  Mode  :character   Mode  :character      Mode  :character  
    ##                                                             
    ##                                                             
    ##                                                             
    ##                                                             
    ##  died_country_original died_city_original city_original      country_original  
    ##  Length:935            Length:935         Length:935         Length:935        
    ##  Class :character      Class :character   Class :character   Class :character  
    ##  Mode  :character      Mode  :character   Mode  :character   Mode  :character  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ## 

### Exercise 2

``` r
nobel_living <- nobel %>% 
  filter(is.na(died_date)) %>% 
  filter(!is.na(country)) %>% 
  filter(gender != "org")
```

``` r
nobel_living <- nobel_living %>%
  mutate(
    country_us = if_else(country == "USA", "USA", "Other")
  )
```

``` r
nobel_living_science <- nobel_living %>%
  filter(category %in% c("Physics", "Medicine", "Chemistry", "Economics"))
```

Yes, it has 228 observations.

### Exercise 3

``` r
ggplot(data = nobel_living_science, aes(x = country_us)) + 
  geom_bar() + 
  facet_wrap(~category) + 
  labs(title = "Living Nobel Laureates: US vs Other", 
       y = "Number of Laureates", 
       x = "Location at time of prize") + 
  theme_minimal() +
  coord_flip()
```

![](lab-03_files/figure-gfm/Visualization-1.png)<!-- -->

From the bar plots shown above, the Buzzfeed headline is supported. The
majority of living Nobel laureates were based in the USA at the time
they won their prizes. However, there is no information about whether
these USA winners were origins of the US or immigrants.

### Exercise 4

``` r
nobel_living_science <- nobel_living_science %>% 
  mutate(
    born_country_us = if_else(born_country == "USA", "USA", "Other")
  )
```

``` r
nobel_living_science %>%
  count(born_country_us)
```

    ## # A tibble: 2 × 2
    ##   born_country_us     n
    ##   <chr>           <int>
    ## 1 Other             123
    ## 2 USA               105

There are 105 Nobel Prize winners are born in the US.

### Exercise 5

``` r
ggplot(data = nobel_living_science, aes(x = country_us, fill = born_country_us)) + 
  geom_bar() + 
  facet_wrap(~category) + 
  labs(title = "Living Nobel Laureates: US vs Other", 
       y = "Number of Laureates", 
       x = "Location at time of prize") + 
  theme_minimal() +
  coord_flip()
```

![](lab-03_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> The data
strongly supports the Buzzfeed claim that immigration is vital to
American science. While a high number of living Nobel laureates were
based in the USA at the time of their award, a noticeable portion of
those winners were born in a different country, specially in the areas
of chemistry, medicine, and physics.

### Exercise 6

``` r
nobel_living_science %>%
  filter(country == "USA", born_country != "USA") %>%
  count(born_country) %>%
  arrange(desc(n))
```

    ## # A tibble: 21 × 2
    ##    born_country       n
    ##    <chr>          <int>
    ##  1 Germany            7
    ##  2 United Kingdom     7
    ##  3 China              5
    ##  4 Canada             4
    ##  5 Japan              3
    ##  6 Australia          2
    ##  7 Israel             2
    ##  8 Norway             2
    ##  9 Austria            1
    ## 10 Finland            1
    ## # ℹ 11 more rows

For living Nobel laureates who won their prize in the US but were born
outside of it, Germany and the United Kingdom are tied for the most
common birth country with 7 laureates each
