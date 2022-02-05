HW\_Module\_1
================
Jill Walton
2/5/2022

``` r
# Loading tidyverse to complete the HW assignment

library("tidyverse")
```

# 1. Loading Data

I am using data from a synthetic community experiment that I ran a while
ago. This data includes the synthetic community mix, timepoint that the
samples were taken, carbon source added, replicate number, strain, cell
counts for each strain, total counts for each mix replicate, and any
treatment added to the synthetic community.

``` r
syn_com_data <- read_csv("syncom_both_carbon_HW.csv")
```

    ## Rows: 1650 Columns: 8

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): mix, carbon, bio_rep, strain, treatment
    ## dbl (3): time_point, count, total

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(syn_com_data)
```

    ## Rows: 1,650
    ## Columns: 8
    ## $ mix        <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",~
    ## $ time_point <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ carbon     <chr> "coum", "coum", "coum", "coum", "coum", "coum", "coum", "co~
    ## $ bio_rep    <chr> "1A", "1A", "1A", "1A", "1A", "1B", "1B", "1B", "1B", "1B",~
    ## $ strain     <chr> "Y4I", "SE45", "E37", "ISM", "EE36", "Y4I", "SE45", "E37", ~
    ## $ count      <dbl> 21000, 52000, 8000, 49000, 43000, 61000, 102000, 21000, 510~
    ## $ total      <dbl> 173000, 173000, 173000, 173000, 173000, 281000, 281000, 281~
    ## $ treatment  <chr> "none", "none", "none", "none", "none", "none", "none", "no~

# 2. Analyze Data

I want to look at the Mix A synthetic community that was cultured in
YTSS and calculate the average viable cell counts of all the strains in
this mix over each timepoint.

## The question I want to answer is: Does Y4I have a competitive advantage over the other strains?

``` r
syn_com_data_summary <- syn_com_data %>%
  filter(mix == "A", carbon == "YTSS") %>% # filtering out data that is not for Mix A or YTSS carbon source
  group_by(time_point, strain) %>% # grouping replicates together to give on average viable cell count per strain per timepoint
  summarise(average.strain.counts = mean(count, na.rm = TRUE)) # taking the average of the cell counts for each strain so I can look at changes in the cell count over time
```

    ## `summarise()` has grouped output by 'time_point'. You can override using the `.groups` argument.

``` r
head(syn_com_data_summary)
```

    ## # A tibble: 6 x 3
    ## # Groups:   time_point [2]
    ##   time_point strain average.strain.counts
    ##        <dbl> <chr>                  <dbl>
    ## 1          0 E37                    2444.
    ## 2          0 EE36                   2667.
    ## 3          0 ISM                    3778.
    ## 4          0 SE45                    556.
    ## 5          0 Y4I                     556.
    ## 6         20 E37                    1111.

Now that we have the average viable cell counts for each strain at each
timepoint, we can graph the viable cell counts for each strain over
time.

``` r
ave_viable_cell_count_graph <- ggplot(data = syn_com_data_summary)+
geom_line(aes(x = time_point, y = average.strain.counts, color = strain),size = 1.5)+ # each strain wil be identified by its own color
scale_color_brewer(palette ="Dark2")+
theme_classic()+
labs(y ="average viable cell counts", x= "time(hours)")

print(ave_viable_cell_count_graph)
```

![](HW_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Let’s split up the graphs since it is a bit hard to see strain EE36.

``` r
ave_viable_cell_count_graph_split <- ggplot(data = syn_com_data_summary)+
geom_line(aes(x = time_point, y = average.strain.counts, color = strain),size = 1.5)+
scale_color_brewer(palette ="Dark2")+
facet_wrap(facets = "strain", nrow =1)+ # splitting up each strain line
theme_classic()+
labs(y ="average viable cell counts", x= "time(hours)")

print(ave_viable_cell_count_graph_split)
```

![](HW_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## From this data, we can see that Y4I appears to dominate the community at all timepoints. The viable cell counts of the other strains do not seem to vary much over the timepoints compared to Y4I. From this data we can show that Y4I does have a competitive advantage over the other strains.
