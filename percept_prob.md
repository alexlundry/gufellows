Perceptions of Probability
================
Alex Lundry
2022-10-03

Read in and review data from the first survey:

``` r
d1 <- read_sheet("https://docs.google.com/spreadsheets/d/1t2i6664OLupX_lTUz01BQ55exkpMmIrsd2b6gopDAqY/edit?usp=sharing", sheet = 1, col_names = T)

d1 <- clean_names(d1)

colnames(d1) <- c("timestamp", "All_But_Certain", "Probable", "Highly_Likely", "Chances_About_Even", "Might", "Highly_Doubtful",
                  "Could", "Unlikely", "Almost_Certainly", "A_Little_Worse_Than_Even", "Almost_Certain", "We_Believe",
                  "We_Doubt", "Almost_Impossible")

glimpse(d1)
```

    ## Rows: 15
    ## Columns: 15
    ## $ timestamp                <dttm> 2022-10-03 19:53:57, 2022-10-03 19:55:23, 20…
    ## $ All_But_Certain          <dbl> 99, 22, 87, 50, 99, 90, 90, 99, 95, 10, 15, 1…
    ## $ Probable                 <dbl> 25, 36, 65, 80, 60, 60, 60, 51, 85, 25, 30, 6…
    ## $ Highly_Likely            <dbl> 26, 87, 35, 90, 75, 75, 80, 80, 75, 40, 20, 5…
    ## $ Chances_About_Even       <dbl> 37, 87, 64, 100, 50, 49, 50, 50, 50, 50, 40, …
    ## $ Might                    <dbl> 78, 24, 87, 100, 45, 40, 40, 25, 40, 65, 60, …
    ## $ Highly_Doubtful          <dbl> 10, 68, 98, 50, 20, 10, 30, 5, 25, 10, 70, 40…
    ## $ Could                    <dbl> 25, 65, 35, 100, 40, 40, 20, 15, 35, 65, 75, …
    ## $ Unlikely                 <dbl> 24, 78, 98, 50, 30, 20, 10, 49, 10, 50, 70, 6…
    ## $ Almost_Certainly         <dbl> 98, 34, 72, 50, 10, 5, 5, 1, 5, 10, 30, 20, 7…
    ## $ A_Little_Worse_Than_Even <dbl> 37, 34, 35, 100, 45, 45, 45, 45, 45, 90, 40, …
    ## $ Almost_Certain           <dbl> 36, 57, 65, 0, 90, 80, 95, 90, 90, 10, 10, 20…
    ## $ We_Believe               <dbl> 48, 68, 78, 0, 35, 40, 40, 40, 10, 50, 40, 40…
    ## $ We_Doubt                 <dbl> 78, 10, 34, 0, 35, 30, 25, 25, 25, 50, 60, 40…
    ## $ Almost_Impossible        <dbl> 92, 90, 98, 0, 5, 20, 5, 1, 15, 1, 25, 10, 5,…

Read in and review data from the second survey:

``` r
d2 <- read_sheet("https://docs.google.com/spreadsheets/d/1_9RFyee91dycB0Pd2LBNcBlpJn1GOsvp_jJRO8vXeqE/edit?usp=sharing", sheet = 1, col_names = T)

d2 <- clean_names(d2)

colnames(d2) <- c("timestamp", "Virtually_Certain", "Odds_Are_Overwhelming", "May", "Perhaps_Will_Win", "Conceivable", "Highly_Probable", "Chances_A_Little_Better_Than_Even", "Impossible", "Some_Slight_Chance", "Certain", "Improbable", "Virtually_Impossible", "Probably_Not", "Estimate_Will_Not", "Doubtful")

glimpse(d2)
```

    ## Rows: 13
    ## Columns: 16
    ## $ timestamp                         <dttm> 2022-10-03 20:30:34, 2022-10-03 20:…
    ## $ Virtually_Certain                 <dbl> 99, 12, 12, NA, 95, 99, 0, 5, 90, 99…
    ## $ Odds_Are_Overwhelming             <dbl> 98, 23, 23, 80, 80, 80, 0, 5, 99, 75…
    ## $ May                               <dbl> 97, 34, 34, 45, 50, 40, 30, 25, 50, …
    ## $ Perhaps_Will_Win                  <dbl> 96, 45, 32, 35, 40, 52, 30, 25, 20, …
    ## $ Conceivable                       <dbl> 95, 56, 32, 40, 30, 47, 80, 30, 70, …
    ## $ Highly_Probable                   <dbl> 76, 67, 21, 65, 65, 60, 10, 10, 80, …
    ## $ Chances_A_Little_Better_Than_Even <dbl> 54, 78, 56, 55, 55, 55, 20, 10, 51, …
    ## $ Impossible                        <dbl> 76, 89, 89, 15, 0, 3, 95, 5, 0, 1, 0…
    ## $ Some_Slight_Chance                <dbl> 54, 90, 90, 35, 15, 25, 100, 30, 15,…
    ## $ Certain                           <dbl> 43, 98, 97, 100, 100, 70, 0, 5, 100,…
    ## $ Improbable                        <dbl> 78, 87, 65, 40, 15, 30, 95, 70, 20, …
    ## $ Virtually_Impossible              <dbl> 54, 76, 86, 20, 5, 5, 80, 70, 10, 1,…
    ## $ Probably_Not                      <dbl> 79, 65, 98, 30, 40, 38, 90, 80, 30, …
    ## $ Estimate_Will_Not                 <dbl> 32, 54, 76, 45, 25, 33, 80, 70, 60, …
    ## $ Doubtful                          <dbl> 21, 43, 65, 30, 30, 39, 70, 70, 45, …

Filter out test data:

``` r
d1 <- d1 %>% 
   filter(lubridate::now() - timestamp < lubridate::hours(24))

d2 <- d2 %>% 
   filter(lubridate::now() - timestamp < lubridate::hours(24))
```

Reformat data for viz by pivoting longer, cleaning up names:

``` r
d1b <- d1 %>% 
   select(-timestamp) %>% 
   pivot_longer(cols = 1:14, names_to = "phrase", values_to = "estimate") %>% 
   mutate(phrase = str_replace_all(phrase, "_", " "))

d2b <- d2 %>% 
   select(-timestamp) %>% 
   pivot_longer(cols = 1:15, names_to = "phrase", values_to = "estimate") %>% 
   mutate(phrase = str_replace_all(phrase, "_", " "))

d3 <- bind_rows(d1b, d2b)
```

Reordering the factors by the mean estimate:

``` r
phrase_fct <- d3 %>% 
   group_by(phrase) %>% 
   summarize(mean = mean(estimate, na.rm = T)) %>% 
   arrange(desc(mean)) %>% 
   pull(phrase)

d3 <- d3 %>% 
   mutate(phrase = factor(phrase, levels = phrase_fct) %>% fct_rev,
          estimate = estimate / 100)

d3 %>% 
   group_by(phrase) %>% 
   summarize(mean = mean(estimate, na.rm = T)) %>% 
   arrange(desc(mean)) %>% 
   print(n = 29)
```

    ## # A tibble: 29 × 2
    ##    phrase                             mean
    ##    <fct>                             <dbl>
    ##  1 Highly Likely                     0.6  
    ##  2 All But Certain                   0.587
    ##  3 Probable                          0.555
    ##  4 Virtually Certain                 0.537
    ##  5 Certain                           0.534
    ##  6 Might                             0.525
    ##  7 A Little Worse Than Even          0.521
    ##  8 Almost Certain                    0.52 
    ##  9 Could                             0.512
    ## 10 Chances About Even                0.512
    ## 11 Odds Are Overwhelming             0.494
    ## 12 Highly Probable                   0.468
    ## 13 Estimate Will Not                 0.443
    ## 14 Conceivable                       0.437
    ## 15 Chances A Little Better Than Even 0.426
    ## 16 Probably Not                      0.425
    ## 17 Unlikely                          0.412
    ## 18 Improbable                        0.406
    ## 19 May                               0.405
    ## 20 We Doubt                          0.383
    ## 21 We Believe                        0.369
    ## 22 Doubtful                          0.359
    ## 23 Perhaps Will Win                  0.347
    ## 24 Highly Doubtful                   0.329
    ## 25 Some Slight Chance                0.32 
    ## 26 Virtually Impossible              0.216
    ## 27 Almost Certainly                  0.178
    ## 28 Almost Impossible                 0.168
    ## 29 Impossible                        0.119

Create the Density Ridgelines:

``` r
library(ggridges)

ggplot(d3,aes(y=phrase,x=estimate)) +
   geom_density_ridges(aes(fill=phrase), alpha=0.75, scale = 4, size = 0.2) +
   scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent) +
   guides(fill=FALSE) +
   labs(title="Perceptions of Probability",
        x="Estimated Probability",
        y="",
        caption="inpsired by /u/zonination")
```

![](percept_prob_files/figure-gfm/Create%20viz-1.png)<!-- -->
