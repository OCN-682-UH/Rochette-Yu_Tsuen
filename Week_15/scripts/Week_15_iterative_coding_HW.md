Week 15 - Iterative Coding HW
================
Keanu Rochette
2024-12-08

- [Load Libraries](#load-libraries)
- [Iterative Coding in Base R](#iterative-coding-in-base-r)
  - [Setting up the elements of the
    loop](#setting-up-the-elements-of-the-loop)
  - [Setup the for loop](#setup-the-for-loop)
  - [Returning a Table Summary](#returning-a-table-summary)
- [Iterative Coding with Tidyverse](#iterative-coding-with-tidyverse)
  - [Setup to initiate the “looping”](#setup-to-initiate-the-looping)
  - [Calculating the values with
    tidyerse](#calculating-the-values-with-tidyerse)

## Load Libraries

``` r
library(tidyverse)
library(here)
library(gt)
```

## Iterative Coding in Base R

### Setting up the elements of the loop

Set the path to the files

``` r
TPpath<- here("Week_15", "data", "homework")
```

Create a vector containing the files

``` r
files <- dir(path = TPpath, pattern = ".csv")
files
```

    ## [1] "TP1.csv" "TP2.csv" "TP3.csv" "TP4.csv"

Create an empty tibble with the varibles that we want to extract:

- filename: which CSV is the data coming from
- temp_mean: average temperature of the file
- temp_sd: standard deviation of temperature of the file
- light_mean: average light intensity of the file
- light_sd: standard deviation of light intensity of the file

``` r
TPdata <- tibble(filename = rep(NA, length(files)),
                 temp_mean = rep(NA, length(files)),
                 temp_sd = rep(NA, length(files)),
                 light_mean = rep(NA, length(files)),
                 light_sd = rep(NA, length(files))
                 )
```

Testing if we can read a file using the paths and the directory set up
above

``` r
datatest<- read_csv(paste0(TPpath, "/", files[1]))
head(datatest, 10)
```

    ## # A tibble: 10 × 7
    ##    PoolID Foundation_spp Removal_Control Date.Time    Temp.C Intensity.lux
    ##     <dbl> <chr>          <chr>           <chr>         <dbl>         <dbl>
    ##  1      1 Phyllospadix   Control         6/16/19 0:01  10.2              0
    ##  2      1 Phyllospadix   Control         6/16/19 0:16  10.1              0
    ##  3      1 Phyllospadix   Control         6/16/19 0:31  10.2              0
    ##  4      1 Phyllospadix   Control         6/16/19 0:46  10.1              0
    ##  5      1 Phyllospadix   Control         6/16/19 1:01  10.1              0
    ##  6      1 Phyllospadix   Control         6/16/19 1:16  10.1              0
    ##  7      1 Phyllospadix   Control         6/16/19 1:31  10.0              0
    ##  8      1 Phyllospadix   Control         6/16/19 1:46  10.0              0
    ##  9      1 Phyllospadix   Control         6/16/19 2:01   9.95             0
    ## 10      1 Phyllospadix   Control         6/16/19 2:16   9.56             0
    ## # ℹ 1 more variable: LoggerDepth <dbl>

Making sure that we can calculate the mean based on a file

``` r
mean(datatest$Temp.C, na.rm = T)
```

    ## [1] 13.27092

### Setup the for loop

Making sure that loop works to read files

``` r
for (i in 1:length(files)){
  rawdata <- read_csv(paste0(TPpath, "/", files[i] ))
  glimpse(rawdata)
}
```

    ## Rows: 5,981
    ## Columns: 7
    ## $ PoolID          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Control", "Control", "Control", "Control", "Control",…
    ## $ Date.Time       <chr> "6/16/19 0:01", "6/16/19 0:16", "6/16/19 0:31", "6/16/…
    ## $ Temp.C          <dbl> 10.21, 10.08, 10.16, 10.12, 10.08, 10.08, 10.04, 10.04…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, …

    ## Rows: 5,974
    ## Columns: 7
    ## $ PoolID          <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Removal", "Removal", "Removal", "Removal", "Removal",…
    ## $ Date.Time       <chr> "6/16/19 0:14", "6/16/19 0:29", "6/16/19 0:44", "6/16/…
    ## $ Temp.C          <dbl> 10.12, 10.12, 10.08, 9.91, 9.95, 9.78, 9.56, 9.56, 9.3…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295…

    ## Rows: 5,972
    ## Columns: 7
    ## $ PoolID          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Removal", "Removal", "Removal", "Removal", "Removal",…
    ## $ Date.Time       <chr> "6/16/19 0:14", "6/16/19 0:29", "6/16/19 0:44", "6/16/…
    ## $ Temp.C          <dbl> 10.12, 10.08, 10.04, 9.95, 9.78, 9.69, 9.48, 9.39, 9.2…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.265…

    ## Rows: 5,995
    ## Columns: 7
    ## $ PoolID          <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Control", "Control", "Control", "Control", "Control",…
    ## $ Date.Time       <chr> "6/16/19 0:10", "6/16/19 0:25", "6/16/19 0:40", "6/16/…
    ## $ Temp.C          <dbl> 10.25, 10.21, 10.12, 9.99, 9.99, 9.82, 9.65, 9.44, 9.3…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095…

Setup up the full for loop

``` r
for (i in 1:length(files)){ # i in 1 to 4
  rawdata <- read_csv(paste0(TPpath, "/", files[i] )) # where to read the data
  
  TPdata$filename[i] <- files[i] # filename i in TP data is the file i
  
  TPdata$temp_mean[i] <- mean(rawdata$Temp.C, na.rm = TRUE) # calculate the quantities desired
  TPdata$temp_sd[i] <- sd(rawdata$Temp.C, na.rm = TRUE)
  TPdata$light_mean[i] <- mean(rawdata$Intensity.lux, na.rm = TRUE)
  TPdata$light_sd[i] <- sd(rawdata$Intensity.lux, na.rm = TRUE)
}

TPdata #read the populated tibble
```

    ## # A tibble: 4 × 5
    ##   filename temp_mean temp_sd light_mean light_sd
    ##   <chr>        <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 TP1.csv       13.3    2.32       427.    1661.
    ## 2 TP2.csv       13.2    2.31      5603.   11929.
    ## 3 TP3.csv       13.1    2.32      5605.   12101.
    ## 4 TP4.csv       13.2    2.27       655.    2089.

### Returning a Table Summary

``` r
TPdata %>% gt(rowname_col = "filename") %>% 
  cols_label(temp_mean = "Avg Temperature (°C)",
             temp_sd = "Std Dev Temperature",
             light_mean = "Avg Light (Lux)",
             light_sd = "Std Dev Light")
```

<div id="uyndrmaxxs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uyndrmaxxs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#uyndrmaxxs thead, #uyndrmaxxs tbody, #uyndrmaxxs tfoot, #uyndrmaxxs tr, #uyndrmaxxs td, #uyndrmaxxs th {
  border-style: none;
}
&#10;#uyndrmaxxs p {
  margin: 0;
  padding: 0;
}
&#10;#uyndrmaxxs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#uyndrmaxxs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#uyndrmaxxs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#uyndrmaxxs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#uyndrmaxxs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#uyndrmaxxs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#uyndrmaxxs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#uyndrmaxxs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#uyndrmaxxs .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#uyndrmaxxs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#uyndrmaxxs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#uyndrmaxxs .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#uyndrmaxxs .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#uyndrmaxxs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#uyndrmaxxs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uyndrmaxxs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#uyndrmaxxs .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#uyndrmaxxs .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#uyndrmaxxs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uyndrmaxxs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#uyndrmaxxs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uyndrmaxxs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#uyndrmaxxs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uyndrmaxxs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uyndrmaxxs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uyndrmaxxs .gt_left {
  text-align: left;
}
&#10;#uyndrmaxxs .gt_center {
  text-align: center;
}
&#10;#uyndrmaxxs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#uyndrmaxxs .gt_font_normal {
  font-weight: normal;
}
&#10;#uyndrmaxxs .gt_font_bold {
  font-weight: bold;
}
&#10;#uyndrmaxxs .gt_font_italic {
  font-style: italic;
}
&#10;#uyndrmaxxs .gt_super {
  font-size: 65%;
}
&#10;#uyndrmaxxs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#uyndrmaxxs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#uyndrmaxxs .gt_indent_1 {
  text-indent: 5px;
}
&#10;#uyndrmaxxs .gt_indent_2 {
  text-indent: 10px;
}
&#10;#uyndrmaxxs .gt_indent_3 {
  text-indent: 15px;
}
&#10;#uyndrmaxxs .gt_indent_4 {
  text-indent: 20px;
}
&#10;#uyndrmaxxs .gt_indent_5 {
  text-indent: 25px;
}
&#10;#uyndrmaxxs .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#uyndrmaxxs div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temp_mean">Avg Temperature (°C)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temp_sd">Std Dev Temperature</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="light_mean">Avg Light (Lux)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="light_sd">Std Dev Light</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">TP1.csv</th>
<td headers="stub_1_1 temp_mean" class="gt_row gt_right">13.27092</td>
<td headers="stub_1_1 temp_sd" class="gt_row gt_right">2.324037</td>
<td headers="stub_1_1 light_mean" class="gt_row gt_right">426.8262</td>
<td headers="stub_1_1 light_sd" class="gt_row gt_right">1660.553</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">TP2.csv</th>
<td headers="stub_1_2 temp_mean" class="gt_row gt_right">13.17498</td>
<td headers="stub_1_2 temp_sd" class="gt_row gt_right">2.308312</td>
<td headers="stub_1_2 light_mean" class="gt_row gt_right">5603.1554</td>
<td headers="stub_1_2 light_sd" class="gt_row gt_right">11928.919</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">TP3.csv</th>
<td headers="stub_1_3 temp_mean" class="gt_row gt_right">13.10386</td>
<td headers="stub_1_3 temp_sd" class="gt_row gt_right">2.317297</td>
<td headers="stub_1_3 light_mean" class="gt_row gt_right">5605.0705</td>
<td headers="stub_1_3 light_sd" class="gt_row gt_right">12101.099</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">TP4.csv</th>
<td headers="stub_1_4 temp_mean" class="gt_row gt_right">13.22150</td>
<td headers="stub_1_4 temp_sd" class="gt_row gt_right">2.269209</td>
<td headers="stub_1_4 light_mean" class="gt_row gt_right">655.1037</td>
<td headers="stub_1_4 light_sd" class="gt_row gt_right">2088.839</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## Iterative Coding with Tidyverse

### Setup to initiate the “looping”

Set the path to files and directory to access the files

``` r
TPpath <- here("Week_15", "data", "homework")
files <- dir(path = TPpath, pattern = ".csv", full.names =  TRUE)
```

### Calculating the values with tidyerse

``` r
files %>% 
  set_names() %>% 
  map_df(read_csv, .id = "filename") %>% # create a df with the first column
  group_by(filename) %>% # group the next values based on filename
  
  #calculating the desired quantities with tidyverse
  summarize(temp_mean = mean(Temp.C, na.rm = T),
            temp_sd = sd(Temp.C, na.rm = T),
            light_mean = mean(Intensity.lux, na.rm = T),
            light_sd = sd(Intensity.lux, na.rm = T)
            ) %>% 
  
  # change the filename to only keep the file name.csv and not the whole path directory
  mutate(filename = str_extract(filename, "[^/]+$")) %>% 
  
  #return a clean table summary with the values wanted
  gt(rowname_col = "filename") %>% 
  cols_label(temp_mean = "Avg Temperature (°C)",
             temp_sd = "Std Dev Temperature",
             light_mean = "Avg Light (Lux)",
             light_sd = "Std Dev Light")
```

<div id="yfopelqsom" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#yfopelqsom table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#yfopelqsom thead, #yfopelqsom tbody, #yfopelqsom tfoot, #yfopelqsom tr, #yfopelqsom td, #yfopelqsom th {
  border-style: none;
}
&#10;#yfopelqsom p {
  margin: 0;
  padding: 0;
}
&#10;#yfopelqsom .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#yfopelqsom .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#yfopelqsom .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#yfopelqsom .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#yfopelqsom .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#yfopelqsom .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#yfopelqsom .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#yfopelqsom .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#yfopelqsom .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#yfopelqsom .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#yfopelqsom .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#yfopelqsom .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#yfopelqsom .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#yfopelqsom .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#yfopelqsom .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#yfopelqsom .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#yfopelqsom .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#yfopelqsom .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#yfopelqsom .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#yfopelqsom .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#yfopelqsom .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#yfopelqsom .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#yfopelqsom .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#yfopelqsom .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#yfopelqsom .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#yfopelqsom .gt_left {
  text-align: left;
}
&#10;#yfopelqsom .gt_center {
  text-align: center;
}
&#10;#yfopelqsom .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#yfopelqsom .gt_font_normal {
  font-weight: normal;
}
&#10;#yfopelqsom .gt_font_bold {
  font-weight: bold;
}
&#10;#yfopelqsom .gt_font_italic {
  font-style: italic;
}
&#10;#yfopelqsom .gt_super {
  font-size: 65%;
}
&#10;#yfopelqsom .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#yfopelqsom .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#yfopelqsom .gt_indent_1 {
  text-indent: 5px;
}
&#10;#yfopelqsom .gt_indent_2 {
  text-indent: 10px;
}
&#10;#yfopelqsom .gt_indent_3 {
  text-indent: 15px;
}
&#10;#yfopelqsom .gt_indent_4 {
  text-indent: 20px;
}
&#10;#yfopelqsom .gt_indent_5 {
  text-indent: 25px;
}
&#10;#yfopelqsom .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#yfopelqsom div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temp_mean">Avg Temperature (°C)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temp_sd">Std Dev Temperature</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="light_mean">Avg Light (Lux)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="light_sd">Std Dev Light</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">TP1.csv</th>
<td headers="stub_1_1 temp_mean" class="gt_row gt_right">13.27092</td>
<td headers="stub_1_1 temp_sd" class="gt_row gt_right">2.324037</td>
<td headers="stub_1_1 light_mean" class="gt_row gt_right">426.8262</td>
<td headers="stub_1_1 light_sd" class="gt_row gt_right">1660.553</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">TP2.csv</th>
<td headers="stub_1_2 temp_mean" class="gt_row gt_right">13.17498</td>
<td headers="stub_1_2 temp_sd" class="gt_row gt_right">2.308312</td>
<td headers="stub_1_2 light_mean" class="gt_row gt_right">5603.1554</td>
<td headers="stub_1_2 light_sd" class="gt_row gt_right">11928.919</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">TP3.csv</th>
<td headers="stub_1_3 temp_mean" class="gt_row gt_right">13.10386</td>
<td headers="stub_1_3 temp_sd" class="gt_row gt_right">2.317297</td>
<td headers="stub_1_3 light_mean" class="gt_row gt_right">5605.0705</td>
<td headers="stub_1_3 light_sd" class="gt_row gt_right">12101.099</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">TP4.csv</th>
<td headers="stub_1_4 temp_mean" class="gt_row gt_right">13.22150</td>
<td headers="stub_1_4 temp_sd" class="gt_row gt_right">2.269209</td>
<td headers="stub_1_4 light_mean" class="gt_row gt_right">655.1037</td>
<td headers="stub_1_4 light_sd" class="gt_row gt_right">2088.839</td></tr>
  </tbody>
  &#10;  
</table>
</div>
