Perceptions of Probability
================
Alex Lundry
2022-10-03

**PLACEHOLDER PAGE WITH DEMO DATA - TO BE UPDATED WITH LIVE DATA**

Read in and review data from the first survey:

``` r
d1 <- read_sheet("https://docs.google.com/spreadsheets/d/1t2i6664OLupX_lTUz01BQ55exkpMmIrsd2b6gopDAqY/edit?usp=sharing", sheet = 1, col_names = T)

d1 <- clean_names(d1)

colnames(d1) <- c("timestamp", "All_But_Certain", "Probable", "Highly_Likely", "Chances_About_Even", "Might", "Highly_Doubtful",
                  "Could", "Unlikely", "Almost_Certainly", "A_Little_Worse_Than_Even", "Almost_Certain", "We_Believe",
                  "We_Doubt", "Almost_Impossible")

glimpse(d1)
```

    ## Rows: 3
    ## Columns: 15
    ## $ timestamp                <dttm> 2022-10-03 19:53:57, 2022-10-03 19:55:23, 20…
    ## $ All_But_Certain          <dbl> 99, 22, 87
    ## $ Probable                 <dbl> 25, 36, 65
    ## $ Highly_Likely            <dbl> 26, 87, 35
    ## $ Chances_About_Even       <dbl> 37, 87, 64
    ## $ Might                    <dbl> 78, 24, 87
    ## $ Highly_Doubtful          <dbl> 10, 68, 98
    ## $ Could                    <dbl> 25, 65, 35
    ## $ Unlikely                 <dbl> 24, 78, 98
    ## $ Almost_Certainly         <dbl> 98, 34, 72
    ## $ A_Little_Worse_Than_Even <dbl> 37, 34, 35
    ## $ Almost_Certain           <dbl> 36, 57, 65
    ## $ We_Believe               <dbl> 48, 68, 78
    ## $ We_Doubt                 <dbl> 78, 10, 34
    ## $ Almost_Impossible        <dbl> 92, 90, 98

Read in and review data from the second survey:

``` r
d2 <- read_sheet("https://docs.google.com/spreadsheets/d/1_9RFyee91dycB0Pd2LBNcBlpJn1GOsvp_jJRO8vXeqE/edit?usp=sharing", sheet = 1, col_names = T)

d2 <- clean_names(d2)

colnames(d2) <- c("timestamp", "Virtually_Certain", "Odds_Are_Overwhelming", "May", "Perhaps_Will_Win", "Conceivable", "Highly_Probable", "Chances_A_Little_Better_Than_Even", "Impossible", "Some_Slight_Chance", "Certain", "Improbable", "Virtually_Impossible", "Probably_Not", "Estimate_Will_Not", "Doubtful")

glimpse(d2)
```

    ## Rows: 3
    ## Columns: 16
    ## $ timestamp                         <dttm> 2022-10-03 20:30:34, 2022-10-03 20:…
    ## $ Virtually_Certain                 <dbl> 99, 12, 12
    ## $ Odds_Are_Overwhelming             <dbl> 98, 23, 23
    ## $ May                               <dbl> 97, 34, 34
    ## $ Perhaps_Will_Win                  <dbl> 96, 45, 32
    ## $ Conceivable                       <dbl> 95, 56, 32
    ## $ Highly_Probable                   <dbl> 76, 67, 21
    ## $ Chances_A_Little_Better_Than_Even <dbl> 54, 78, 56
    ## $ Impossible                        <dbl> 76, 89, 89
    ## $ Some_Slight_Chance                <dbl> 54, 90, 90
    ## $ Certain                           <dbl> 43, 98, 97
    ## $ Improbable                        <dbl> 78, 87, 65
    ## $ Virtually_Impossible              <dbl> 54, 76, 86
    ## $ Probably_Not                      <dbl> 79, 65, 98
    ## $ Estimate_Will_Not                 <dbl> 32, 54, 76
    ## $ Doubtful                          <dbl> 21, 43, 65

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
```

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
