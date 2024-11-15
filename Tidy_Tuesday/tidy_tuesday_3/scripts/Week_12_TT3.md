Week 12 - Tidy Tuesday 3
================
Keanu Rochette
2024-11-14

- [Disclaimer : Strings are annoying](#disclaimer--strings-are-annoying)
- [Load the Libraries](#load-the-libraries)
- [Load the Data](#load-the-data)
- [Introduction: A New Secret Code](#introduction-a-new-secret-code)
- [Load the Genetic Code](#load-the-genetic-code)
- [Creating a Function that Will Convert our Country Names to “DNA
  Sequences”](#creating-a-function-that-will-convert-our-country-names-to-dna-sequences)
  - [Examples !](#examples-)
- [Tidy Tuesday 3 Learnings](#tidy-tuesday-3-learnings)
  - [**Conclusion:** I wish my research never involves having to deal
    with strings. (One can only
    dream!)](#conclusion-i-wish-my-research-never-involves-having-to-deal-with-strings-one-can-only-dream)

# Disclaimer : Strings are annoying

<figure>
<img src="https://i.redd.it/72lxlzwn81k61.jpg"
alt="Me Dealing with Strings" />
<figcaption aria-hidden="true">Me Dealing with Strings</figcaption>
</figure>

# Load the Libraries

``` r
library(tidyverse)
library(here)
library(random)
library(gt)
```

# Load the Data

This week we have a data set about country codes. Not very inspired
unfortunately…

``` r
tuesdata <- tidytuesdayR::tt_load('2024-11-12')

countries <- tuesdata$countries
countries
```

    ## # A tibble: 249 × 6
    ##    alpha_2 alpha_3 numeric name                 official_name        common_name
    ##    <chr>   <chr>     <dbl> <chr>                <chr>                <chr>      
    ##  1 AW      ABW         533 Aruba                <NA>                 <NA>       
    ##  2 AF      AFG           4 Afghanistan          Islamic Republic of… <NA>       
    ##  3 AO      AGO          24 Angola               Republic of Angola   <NA>       
    ##  4 AI      AIA         660 Anguilla             <NA>                 <NA>       
    ##  5 AX      ALA         248 Åland Islands        <NA>                 <NA>       
    ##  6 AL      ALB           8 Albania              Republic of Albania  <NA>       
    ##  7 AD      AND          20 Andorra              Principality of And… <NA>       
    ##  8 AE      ARE         784 United Arab Emirates <NA>                 <NA>       
    ##  9 AR      ARG          32 Argentina            Argentine Republic   <NA>       
    ## 10 AM      ARM          51 Armenia              Republic of Armenia  <NA>       
    ## # ℹ 239 more rows

``` r
#country_subdivisions <- tuesdata$country_subdivisions
#former_countries <- tuesdata$former_countries
```

# Introduction: A New Secret Code

This week I wanted to use the name of the countries and convert them
into a sequence of coded letters just like DNA is.

To do that, I decided to use the letters K, R, Y and T (my initials) as
my “nucleotides”. The nucleotides will grouped by 3s, i.e. the codons,
and each codons will be assigned a letter.

    #Creating 28 sequences of 3 letters with the nucleotides. 
    codon <- replicate(28, paste0(sample(c("K", "R", "Y", "T"), 3, replace = TRUE), collapse=""))

    # Creating a vector with the letters of the alphabet, 
    # an apostrophy and an underscore (as a space)
    alpha <- c(letters, "_", "'")

    # Assigning the letters a codon. 
    gen_code <- tibble(alpha, codon)

    # Because the codons where generated random and we want to have a consistant 
    # genetic code, I save the matched letters and codons as a csv.
    write_csv(gen_code,here("Tidy_Tuesday", "tidy_tuesday_3", "data", "gen_code.csv"))

# Load the Genetic Code

``` r
gen_code <- tibble(read_csv(here("Tidy_Tuesday", "tidy_tuesday_3", "data", "gen_code.csv")))

gen_code
```

    ## # A tibble: 28 × 2
    ##    alpha codon
    ##    <chr> <chr>
    ##  1 a     TYR  
    ##  2 b     RRR  
    ##  3 c     RTT  
    ##  4 d     YKK  
    ##  5 e     YYY  
    ##  6 f     TYR  
    ##  7 g     KKY  
    ##  8 h     RTT  
    ##  9 i     RTY  
    ## 10 j     TTK  
    ## # ℹ 18 more rows

# Creating a Function that Will Convert our Country Names to “DNA Sequences”

``` r
KRYT_DNA <- function(data, name = name ){
  data %>% tibble() %>%
    
    #the country names must be cleaned up before they can be encoded
    mutate(name_trans = tolower({{name}})) %>% # transform to lower case
    mutate(
      #spaces are replaced by a "_"
         name_trans = str_replace_all(name_trans, " ", "_"),
      # because this is a simple code, we will transform
      # any punctuation symbol as a "_" for the sake of simplicity 
         name_trans = str_replace_all(name_trans, ",", "_"),
         name_trans = str_replace_all(name_trans, "\\(", "_"),
         name_trans = str_replace_all(name_trans, "\\)", "_"),
      # we create a new column that splits the name of the countries into
      # their letter components. This returns lists.
         alpha = str_split(name_trans, "", simplify = FALSE)) %>%
    # we undo the list the letters can be manipulated
    unnest(alpha) %>% 
    # we can match the letter to the genetic code
    left_join(gen_code, by = "alpha") %>% 
    # and group the letters and codons by country names so they are kept tidy
    group_by(name_trans) %>% 
    # in order to group the codons together, we make a list with them
    mutate(DNA_seq = list(codon)) %>% 
    # we use map_chr(), which is a hidden for loop that applies a function to an object.
    #that way we can have the codon read one after the other to make a single string
    mutate(DNA_seq = map_chr(DNA_seq, ~ str_c(.x, collapse = ""))) %>% 
    #we only keep the relevant columns
    select(name, name_trans, DNA_seq) %>% distinct() %>%  
    # I had to ungroup or my later table gets messy
    ungroup()
}
```

## Examples !

We can make random lists of countries from this week’s data set to test
the function

``` r
test_1 <- countries %>% select(name) %>% slice_sample(n= 20)


KRYT_DNA(test_1) %>% 
  # create the gt table with a title and subtitle
  gt() %>% 
  tab_header(title = md("**Converting Country Names into Secret Messages**"))  %>%
  
  # change the column names into prettier names
  cols_label(
    name = "Country Name",
    name_trans = "Transformed Name",
    DNA_seq = "KRYT DNA Code") %>% 
  tab_source_note("Source: Tidy Tuesday November 12, 2024") %>%
  
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_column_labels(columns = everything())) %>% 
  
  # center text within the columns 
  cols_align(
    align = "left",
    columns = everything())
```

<div id="zklviqfrmh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zklviqfrmh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zklviqfrmh thead, #zklviqfrmh tbody, #zklviqfrmh tfoot, #zklviqfrmh tr, #zklviqfrmh td, #zklviqfrmh th {
  border-style: none;
}
&#10;#zklviqfrmh p {
  margin: 0;
  padding: 0;
}
&#10;#zklviqfrmh .gt_table {
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
&#10;#zklviqfrmh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zklviqfrmh .gt_title {
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
&#10;#zklviqfrmh .gt_subtitle {
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
&#10;#zklviqfrmh .gt_heading {
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
&#10;#zklviqfrmh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_col_headings {
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
&#10;#zklviqfrmh .gt_col_heading {
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
&#10;#zklviqfrmh .gt_column_spanner_outer {
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
&#10;#zklviqfrmh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zklviqfrmh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zklviqfrmh .gt_column_spanner {
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
&#10;#zklviqfrmh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zklviqfrmh .gt_group_heading {
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
&#10;#zklviqfrmh .gt_empty_group_heading {
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
&#10;#zklviqfrmh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zklviqfrmh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zklviqfrmh .gt_row {
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
&#10;#zklviqfrmh .gt_stub {
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
&#10;#zklviqfrmh .gt_stub_row_group {
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
&#10;#zklviqfrmh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zklviqfrmh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zklviqfrmh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zklviqfrmh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zklviqfrmh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zklviqfrmh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zklviqfrmh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zklviqfrmh .gt_footnotes {
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
&#10;#zklviqfrmh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zklviqfrmh .gt_sourcenotes {
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
&#10;#zklviqfrmh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zklviqfrmh .gt_left {
  text-align: left;
}
&#10;#zklviqfrmh .gt_center {
  text-align: center;
}
&#10;#zklviqfrmh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zklviqfrmh .gt_font_normal {
  font-weight: normal;
}
&#10;#zklviqfrmh .gt_font_bold {
  font-weight: bold;
}
&#10;#zklviqfrmh .gt_font_italic {
  font-style: italic;
}
&#10;#zklviqfrmh .gt_super {
  font-size: 65%;
}
&#10;#zklviqfrmh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zklviqfrmh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zklviqfrmh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zklviqfrmh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zklviqfrmh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zklviqfrmh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zklviqfrmh .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zklviqfrmh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zklviqfrmh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Converting Country Names into Secret Messages</strong></span></td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Country Name">Country Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Transformed Name">Transformed Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="KRYT DNA Code">KRYT DNA Code</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="name" class="gt_row gt_left">Albania</td>
<td headers="name_trans" class="gt_row gt_left">albania</td>
<td headers="DNA_seq" class="gt_row gt_left">TYRTKKRRRTYRRRTRTYTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Morocco</td>
<td headers="name_trans" class="gt_row gt_left">morocco</td>
<td headers="DNA_seq" class="gt_row gt_left">YYTYRYKRTYRYRTTRTTYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Puerto Rico</td>
<td headers="name_trans" class="gt_row gt_left">puerto_rico</td>
<td headers="DNA_seq" class="gt_row gt_left">YRYTRKYYYKRTYRTYRYRRTKRTRTYRTTYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Virgin Islands, U.S.</td>
<td headers="name_trans" class="gt_row gt_left">virgin_islands__u.s.</td>
<td headers="DNA_seq" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Bahamas</td>
<td headers="name_trans" class="gt_row gt_left">bahamas</td>
<td headers="DNA_seq" class="gt_row gt_left">RRRTYRRTTTYRYYTTYRTKY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Brazil</td>
<td headers="name_trans" class="gt_row gt_left">brazil</td>
<td headers="DNA_seq" class="gt_row gt_left">RRRKRTTYRYKRRTYTKK</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Sint Maarten (Dutch part)</td>
<td headers="name_trans" class="gt_row gt_left">sint_maarten__dutch_part_</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYRTYRRTYRTRRTYYTTYRTYRKRTYRTYYYRRTRRTRRTYKKTRKYRTRTTRTTRRTYRYTYRKRTYRTRRT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Lesotho</td>
<td headers="name_trans" class="gt_row gt_left">lesotho</td>
<td headers="DNA_seq" class="gt_row gt_left">TKKYYYTKYYRYYRTRTTYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Åland Islands</td>
<td headers="name_trans" class="gt_row gt_left">åland_islands</td>
<td headers="DNA_seq" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Burkina Faso</td>
<td headers="name_trans" class="gt_row gt_left">burkina_faso</td>
<td headers="DNA_seq" class="gt_row gt_left">RRRTRKKRTYTYRTYRRTTYRRRTTYRTYRTKYYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Guernsey</td>
<td headers="name_trans" class="gt_row gt_left">guernsey</td>
<td headers="DNA_seq" class="gt_row gt_left">KKYTRKYYYKRTRRTTKYYYYRYT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Taiwan, Province of China</td>
<td headers="name_trans" class="gt_row gt_left">taiwan__province_of_china</td>
<td headers="DNA_seq" class="gt_row gt_left">YRTTYRRTYYRKTYRRRTRRTRRTYRYKRTYRYRYYRTYRRTRTTYYYRRTYRYTYRRRTRTTRTTRTYRRTTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Czechia</td>
<td headers="name_trans" class="gt_row gt_left">czechia</td>
<td headers="DNA_seq" class="gt_row gt_left">RTTYKRYYYRTTRTTRTYTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Haiti</td>
<td headers="name_trans" class="gt_row gt_left">haiti</td>
<td headers="DNA_seq" class="gt_row gt_left">RTTTYRRTYYRTRTY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Saint Pierre and Miquelon</td>
<td headers="name_trans" class="gt_row gt_left">saint_pierre_and_miquelon</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYTYRRTYRRTYRTRRTYRYRTYYYYKRTKRTYYYRRTTYRRRTYKKRRTYYTRTYYKYTRKYYYTKKYRYRRT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Malta</td>
<td headers="name_trans" class="gt_row gt_left">malta</td>
<td headers="DNA_seq" class="gt_row gt_left">YYTTYRTKKYRTTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Wallis and Futuna</td>
<td headers="name_trans" class="gt_row gt_left">wallis_and_futuna</td>
<td headers="DNA_seq" class="gt_row gt_left">YRKTYRTKKTKKRTYTKYRRTTYRRRTYKKRRTTYRTRKYRTTRKRRTTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Central African Republic</td>
<td headers="name_trans" class="gt_row gt_left">central_african_republic</td>
<td headers="DNA_seq" class="gt_row gt_left">RTTYYYRRTYRTKRTTYRTKKRRTTYRTYRKRTRTYRTTTYRRRTRRTKRTYYYYRYTRKRRRTKKRTYRTT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Spain</td>
<td headers="name_trans" class="gt_row gt_left">spain</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYYRYTYRRTYRRT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Equatorial Guinea</td>
<td headers="name_trans" class="gt_row gt_left">equatorial_guinea</td>
<td headers="DNA_seq" class="gt_row gt_left">YYYYKYTRKTYRYRTYRYKRTRTYTYRTKKRRTKKYTRKRTYRRTYYYTYR</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source: Tidy Tuesday November 12, 2024</td>
    </tr>
  </tfoot>
  &#10;</table>
</div>

``` r
test_2 <- countries %>% select(name) %>% slice_sample(n= 20)


KRYT_DNA(test_2) %>% 
  # create the gt table with a title and subtitle
  gt() %>% 
  tab_header(title = md("**Converting Country Names into Secret Messages**"))  %>%
  
  # change the column names into prettier names
  cols_label(
    name = "Country Name",
    name_trans = "Transformed Name",
    DNA_seq = "KRYT DNA Code") %>% 
  tab_source_note("Source: Tidy Tuesday November 12, 2024") %>%
  
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_column_labels(columns = everything())) %>% 
  
  # center text within the columns 
  cols_align(
    align = "left",
    columns = everything())
```

<div id="ylmwdrucxr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ylmwdrucxr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ylmwdrucxr thead, #ylmwdrucxr tbody, #ylmwdrucxr tfoot, #ylmwdrucxr tr, #ylmwdrucxr td, #ylmwdrucxr th {
  border-style: none;
}
&#10;#ylmwdrucxr p {
  margin: 0;
  padding: 0;
}
&#10;#ylmwdrucxr .gt_table {
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
&#10;#ylmwdrucxr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ylmwdrucxr .gt_title {
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
&#10;#ylmwdrucxr .gt_subtitle {
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
&#10;#ylmwdrucxr .gt_heading {
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
&#10;#ylmwdrucxr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_col_headings {
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
&#10;#ylmwdrucxr .gt_col_heading {
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
&#10;#ylmwdrucxr .gt_column_spanner_outer {
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
&#10;#ylmwdrucxr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ylmwdrucxr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ylmwdrucxr .gt_column_spanner {
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
&#10;#ylmwdrucxr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ylmwdrucxr .gt_group_heading {
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
&#10;#ylmwdrucxr .gt_empty_group_heading {
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
&#10;#ylmwdrucxr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ylmwdrucxr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ylmwdrucxr .gt_row {
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
&#10;#ylmwdrucxr .gt_stub {
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
&#10;#ylmwdrucxr .gt_stub_row_group {
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
&#10;#ylmwdrucxr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ylmwdrucxr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ylmwdrucxr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylmwdrucxr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ylmwdrucxr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylmwdrucxr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ylmwdrucxr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylmwdrucxr .gt_footnotes {
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
&#10;#ylmwdrucxr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylmwdrucxr .gt_sourcenotes {
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
&#10;#ylmwdrucxr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylmwdrucxr .gt_left {
  text-align: left;
}
&#10;#ylmwdrucxr .gt_center {
  text-align: center;
}
&#10;#ylmwdrucxr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ylmwdrucxr .gt_font_normal {
  font-weight: normal;
}
&#10;#ylmwdrucxr .gt_font_bold {
  font-weight: bold;
}
&#10;#ylmwdrucxr .gt_font_italic {
  font-style: italic;
}
&#10;#ylmwdrucxr .gt_super {
  font-size: 65%;
}
&#10;#ylmwdrucxr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ylmwdrucxr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ylmwdrucxr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ylmwdrucxr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ylmwdrucxr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ylmwdrucxr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ylmwdrucxr .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ylmwdrucxr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ylmwdrucxr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Converting Country Names into Secret Messages</strong></span></td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Country Name">Country Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Transformed Name">Transformed Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="KRYT DNA Code">KRYT DNA Code</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="name" class="gt_row gt_left">United States</td>
<td headers="name_trans" class="gt_row gt_left">united_states</td>
<td headers="DNA_seq" class="gt_row gt_left">TRKRRTRTYYRTYYYYKKRRTTKYYRTTYRYRTYYYTKY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Guadeloupe</td>
<td headers="name_trans" class="gt_row gt_left">guadeloupe</td>
<td headers="DNA_seq" class="gt_row gt_left">KKYTRKTYRYKKYYYTKKYRYTRKYRYYYY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Singapore</td>
<td headers="name_trans" class="gt_row gt_left">singapore</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYRTYRRTKKYTYRYRYYRYKRTYYY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Seychelles</td>
<td headers="name_trans" class="gt_row gt_left">seychelles</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYYYYRYTRTTRTTYYYTKKTKKYYYTKY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Lebanon</td>
<td headers="name_trans" class="gt_row gt_left">lebanon</td>
<td headers="DNA_seq" class="gt_row gt_left">TKKYYYRRRTYRRRTYRYRRT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Poland</td>
<td headers="name_trans" class="gt_row gt_left">poland</td>
<td headers="DNA_seq" class="gt_row gt_left">YRYYRYTKKTYRRRTYKK</td></tr>
    <tr><td headers="name" class="gt_row gt_left">San Marino</td>
<td headers="name_trans" class="gt_row gt_left">san_marino</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYTYRRRTRRTYYTTYRKRTRTYRRTYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Botswana</td>
<td headers="name_trans" class="gt_row gt_left">botswana</td>
<td headers="DNA_seq" class="gt_row gt_left">RRRYRYYRTTKYYRKTYRRRTTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Solomon Islands</td>
<td headers="name_trans" class="gt_row gt_left">solomon_islands</td>
<td headers="DNA_seq" class="gt_row gt_left">TKYYRYTKKYRYYYTYRYRRTRRTRTYTKYTKKTYRRRTYKKTKY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Viet Nam</td>
<td headers="name_trans" class="gt_row gt_left">viet_nam</td>
<td headers="DNA_seq" class="gt_row gt_left">RYYRTYYYYYRTRRTRRTTYRYYT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Montenegro</td>
<td headers="name_trans" class="gt_row gt_left">montenegro</td>
<td headers="DNA_seq" class="gt_row gt_left">YYTYRYRRTYRTYYYRRTYYYKKYKRTYRY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Peru</td>
<td headers="name_trans" class="gt_row gt_left">peru</td>
<td headers="DNA_seq" class="gt_row gt_left">YRYYYYKRTTRK</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Yemen</td>
<td headers="name_trans" class="gt_row gt_left">yemen</td>
<td headers="DNA_seq" class="gt_row gt_left">RYTYYYYYTYYYRRT</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Grenada</td>
<td headers="name_trans" class="gt_row gt_left">grenada</td>
<td headers="DNA_seq" class="gt_row gt_left">KKYKRTYYYRRTTYRYKKTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">North Macedonia</td>
<td headers="name_trans" class="gt_row gt_left">north_macedonia</td>
<td headers="DNA_seq" class="gt_row gt_left">RRTYRYKRTYRTRTTRRTYYTTYRRTTYYYYKKYRYRRTRTYTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Greenland</td>
<td headers="name_trans" class="gt_row gt_left">greenland</td>
<td headers="DNA_seq" class="gt_row gt_left">KKYKRTYYYYYYRRTTKKTYRRRTYKK</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Iran, Islamic Republic of</td>
<td headers="name_trans" class="gt_row gt_left">iran__islamic_republic_of</td>
<td headers="DNA_seq" class="gt_row gt_left">RTYKRTTYRRRTRRTRRTRTYTKYTKKTYRYYTRTYRTTRRTKRTYYYYRYTRKRRRTKKRTYRTTRRTYRYTYR</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Heard Island and McDonald Islands</td>
<td headers="name_trans" class="gt_row gt_left">heard_island_and_mcdonald_islands</td>
<td headers="DNA_seq" class="gt_row gt_left">RTTYYYTYRKRTYKKRRTRTYTKYTKKTYRRRTYKKRRTTYRRRTYKKRRTYYTRTTYKKYRYRRTTYRTKKYKKRRTRTYTKYTKKTYRRRTYKKTKY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Congo, The Democratic Republic of the</td>
<td headers="name_trans" class="gt_row gt_left">congo__the_democratic_republic_of_the</td>
<td headers="DNA_seq" class="gt_row gt_left">RTTYRYRRTKKYYRYRRTRRTYRTRTTYYYRRTYKKYYYYYTYRYRTTKRTTYRYRTRTYRTTRRTKRTYYYYRYTRKRRRTKKRTYRTTRRTYRYTYRRRTYRTRTTYYY</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Bahrain</td>
<td headers="name_trans" class="gt_row gt_left">bahrain</td>
<td headers="DNA_seq" class="gt_row gt_left">RRRTYRRTTKRTTYRRTYRRT</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source: Tidy Tuesday November 12, 2024</td>
    </tr>
  </tfoot>
  &#10;</table>
</div>

# Tidy Tuesday 3 Learnings

This week I wanted to play more with strings and getting to manipulate
them. It was pretty difficult to read a string letter by letter
especially if those are part of a data set.  
There was a lot of googling and inquiries with chatgpt to understand the
underlining priciples behind manipulating strings.  
One caveat of the function is that the new genetic code does not
encompass every type of characters. If a country name has special
characters such as “é”, “ê” or “-”, the DNA sequence cannot be
generated.

## **Conclusion:** I wish my research never involves having to deal with strings. (One can only dream!)
