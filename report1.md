Report
================
\[Will Halliday\] \[042343\]
\[09/04/2019\]

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.4.4

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.4

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ## Warning: package 'tibble' was built under R version 3.4.4

    ## Warning: package 'tidyr' was built under R version 3.4.4

    ## Warning: package 'readr' was built under R version 3.4.4

    ## Warning: package 'purrr' was built under R version 3.4.4

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## Warning: package 'forcats' was built under R version 3.4.4

``` r
setwd("C:/Users/Will Halliday/Documents/tab")

files <- dir("C:/Users/Will Halliday/Documents/tab", pattern = "indresp", recursive = TRUE, full.names = TRUE)

files <- files[stringr::str_detect(files, "ukhls")]
variables <- c("memorig", "sex_dv", "doby_dv", "vote4", "finfut", "racel_dv", "age_dv", "gor_dv")

for (i in 1:7) {
  selected.variables <- paste(letters[i], variables, sep = "_")
  selected.variables <- c("pidp", selected.variables)
  data <- fread(files[i], select = selected.variables)
  if (i == 1) {
    waves.7 <- data  
  }
  else {
    waves.7 <- full_join(waves.7, data, by = "pidp")
  }
  rm(data)
} 
```

``` r
## Data

# Party
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w1 = ifelse(
    a_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

``` r
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w2 = ifelse(
      b_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w3 = ifelse(
      c_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w4 = ifelse(
      d_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w5 = ifelse(
      e_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w6 = ifelse(
      f_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))
  waves.7 <- waves.7 %>% 
    mutate(party.binary.w7 = ifelse(
      g_vote4 == 1, "Conservative", ifelse(a_vote4 == 2, "Labour", NA_character_)))

# Support Switch
  waves.7 <- waves.7 %>% 
    mutate(support.change.w1.w2 = ifelse(
      a_vote4 != b_vote4, 1, ifelse(a_vote4 == b_vote4, 0 , NA_real_))) %>% 
    mutate(support.change.w2.w3 = ifelse(
      b_vote4 != c_vote4, 1, ifelse(b_vote4 == c_vote4, 0 , NA_real_))) %>% 
    mutate(support.change.w3.w4 = ifelse(
      c_vote4 != d_vote4, 1, ifelse(c_vote4 == d_vote4, 0 , NA_real_))) %>% 
    mutate(support.change.w4.w5 = ifelse(
      d_vote4 != e_vote4, 1, ifelse(d_vote4 == e_vote4, 0 , NA_real_))) %>% 
    mutate(support.change.w5.w6 = ifelse(
      e_vote4 != f_vote4, 1, ifelse(e_vote4 == f_vote4, 0 , NA_real_))) %>% 
    mutate(support.change.w6.w7 = ifelse(
      f_vote4 != g_vote4, 1, ifelse(f_vote4 == g_vote4, 0 , NA_real_)))
  

#Total switch  
waves.7 <- waves.7 %>%
  filter(!is.na(support.change.w1.w2), !is.na(support.change.w2.w3),
         !is.na(support.change.w3.w4), !is.na(support.change.w4.w5),
         !is.na(support.change.w5.w6), !is.na(support.change.w6.w7))

waves.7 <- waves.7 %>%
  mutate(total.switch = (support.change.w1.w2 + support.change.w2.w3 +
           support.change.w3.w4 + support.change.w4.w5 +
           support.change.w5.w6 + support.change.w6.w7))
######

actual <- waves.7 %>%
  gather(a_memorig:g_gor_dv, key = "variable", value = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  spread(key = variable, value = value)
```

``` r
#Recoding sex_dv
actual<- actual %>%  
  mutate(sex.binary = ifelse(
    sex_dv == 1, "male", ifelse(sex_dv == 2, "female", NA_character_)))
# recoding racel_dv
actual <- actual %>% 
  mutate(race = case_when(
    racel_dv == 1 ~ "white British",
    racel_dv == 2 ~ "Irish",
    racel_dv == 9 ~ "Indian",
    racel_dv == 10 ~ "Pakistani",
    racel_dv == 11 ~ "Bangladeshi",
    racel_dv == 12 ~ "Chinese",
    racel_dv == 14 ~ "Black Caribbean",
    racel_dv == 15 ~ "Black African",
    between(racel_dv, 2, 4) ~ " other white background",
    (racel_dv >= 5 & racel_dv <= 8) | racel_dv > 10 ~ "other",
    racel_dv == -9 ~ NA_character_))
actual <- actual %>% 
  rename("age" = age_dv)


## Creating generation variable 

actual <- actual %>%
  mutate(generation = case_when(
    between(doby_dv, 0, 1924) ~ "GI Generation",
    between(doby_dv, 1925, 1942) ~ "Silent Generation",
    between(doby_dv, 1943, 1965) ~ "Baby Boomers",
    between(doby_dv, 1966, 1980) ~ "Generation X",
    between(doby_dv, 1981, 1999) ~ "Millennials",
    doby_dv >= 2000 ~ "Generation Z"
  ))

# Recoding finfut

actual <- actual %>% 
  mutate(financial.future = case_when(
    finfut == 1 ~ "better",
    finfut == 2 ~ "worse",
    finfut == 3 ~ "no change",
    finfut < 1 ~ NA_character_))
#Recoding gor_dv
actual <- actual %>% 
    mutate(region = recode(gor_dv,
                       `-9` = NA_character_,
                       `1` = "North East",
                       `2` = "North West",
                       `3` = "Yorkshire",
                       `4` = "East Midlands",
                       `5` = "West Midlands",
                       `6` = "East of England",
                       `7` = "London",
                       `8` = "South East",
                       `9` = "Souh West",
                       `10` = "Wales",
                       `11` = "Scotland",
                       `12` = "Northern Ireland"))
#Creating Main Dataset
data <- actual %>% 
  select(race, sex.binary, financial.future, region, generation, pidp, wave, memorig, total.switch, age, support.change.w1.w2, support.change.w2.w3, support.change.w3.w4, support.change.w4.w5, support.change.w5.w6, support.change.w6.w7, party.binary.w1, party.binary.w2,
         party.binary.w3,party.binary.w4, party.binary.w5, party.binary.w6, party.binary.w7) 
```

``` r
##### Introduction 
#Britain’s political system is regularly cited as a “two-party political system”. This is a result of the fact that, in every general election since the Second World War, the national voting results have seen two specific political parties (the Labour Party and the Conservative Party) obtain over 60% of the vote share. In the past, many have the emergence of Britain’s two-party political system has as being intentional. The institutional causes of Britain’s two-party system are therefore broadly due to the process of British politics. Specifically, Britain’s use of a First-Past-The-Post voting system, singe-member constituencies and plurality victories all promote the two party-system within Britain. However, this essay will argue that there is a more underlying and psychological reason amongst voters which preserves the current two-party political system. This reason is best described as political partisanship amongst voters, in the sense of the principal that “Labour supporters rarely become Conservative supporters and Conservative supporters rarely become Labour supporters”. To this extent, this essay will use data to explore the characteristics of voters in the context of changing political support. Initially, this essay will examine and analyse the rate of change in support for political parties as a whole, before examining the rate of changes in support by different socio-economic factors. 

#In particular, this essay will identify and examine differences in the rate of changes in political support between sexes, as well examine differences in the rate of changes in public support between races and regions. In addition, this essay will use self-described estimates of individual financial outlooks, regional location and generational identifiers to examine differences in the rate of changes in public support amongst respondents. To do this, this essay will use data derived from waves 1 to 7 from the original Understanding Society survey. For the purposes of this question, the analysis of political support switching between the Conservative Party and the Labour Party will not extent to the smaller parties that exist and compete within the British political system. 
```

``` r
#Mean Number of Respondents Who Change Political Support Across Waves
data  %>%
    summarise((average.switch.support = (mean(support.change.w1.w2, na.rm = TRUE)) + (mean(support.change.w2.w3, na.rm = TRUE)) + (mean(support.change.w3.w4, na.rm = TRUE)) +
                (mean(support.change.w4.w5, na.rm = TRUE)) + (mean(support.change.w5.w6, na.rm = TRUE)) + (mean(support.change.w6.w7, na.rm = TRUE))) / 6)
```

    ##    `/`(...)
    ## 1 0.2791896

``` r
####### Number of Changes in Political Support by Percentage of Respondents
data%>% 
  filter(!is.na(financial.future)) %>% 
    filter(!is.na(total.switch)) %>% 
    count(total.switch) %>%
    mutate(perc = n/ sum(n) * 100)
```

    ## # A tibble: 7 x 3
    ##   total.switch     n   perc
    ##          <dbl> <int>  <dbl>
    ## 1            0 43209 34.5  
    ## 2            1 18605 14.9  
    ## 3            2 26200 20.9  
    ## 4            3 18585 14.9  
    ## 5            4 12302  9.83 
    ## 6            5  4977  3.98 
    ## 7            6  1251  1.000

``` r
#Number of Changes in Political Support by Percentage and Sex of Respondents
data %>%
    filter(!is.na(sex.binary)) %>% 
    filter(!is.na(total.switch)) %>% 
    group_by(sex.binary) %>% 
    count(total.switch) %>%
    mutate(perc = n/ sum(n) * 100)
```

    ## # A tibble: 14 x 4
    ## # Groups:   sex.binary [2]
    ##    sex.binary total.switch     n   perc
    ##    <chr>             <dbl> <int>  <dbl>
    ##  1 female                0 25766 34.9  
    ##  2 female                1 10913 14.8  
    ##  3 female                2 15169 20.6  
    ##  4 female                3 10962 14.9  
    ##  5 female                4  7280  9.87 
    ##  6 female                5  2821  3.83 
    ##  7 female                6   819  1.11 
    ##  8 male                  0 18886 32.9  
    ##  9 male                  1  8897 15.5  
    ## 10 male                  2 12355 21.5  
    ## 11 male                  3  8638 15.1  
    ## 12 male                  4  5698  9.93 
    ## 13 male                  5  2422  4.22 
    ## 14 male                  6   490  0.854

``` r
# When viewing the entire dataset (as opposed to between-wave changes), it is clear that the largest percentage of respondents were those who had not changed their political support across all 7 waves (34.53%). 14.87% of respondents had switched their political support once, whilst 20.94% of respondents had switched their political supported twice. The percentage of respondents who had switched their political support then becomes inverse to the number of times political support had been switched, and thus decreases. Only 1% of respondents selected 6; the highest rate of switching political support between the Conservative Party and the Labour Party or vice versa. This data varies slightly when viewed in the context of the sex of individual respondents. For example, the highest rate of political support switching amongst men was the rate of having never switched political support (32.91%) whilst the same rate of political support switching amongst female respondents was 34.95%. This is a 2.04% difference between sex amongst those who have never switched political support, and demonstrates the variation between sex in the context of changing political support.
```

``` r
data %>% 
  filter(!is.na(sex.binary), !is.na(total.switch)) %>%
     summarise(mean.support.change = mean(total.switch, na.rm = TRUE))
```

    ##   mean.support.change
    ## 1             1.67515

``` r
data %>%
  filter(!is.na(sex.binary)) %>% 
  ggplot(aes(x = total.switch)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 1.67515 , colour = "red") +
  xlab("Number of Times Party Support Has Changed") +
  facet_wrap(~ sex.binary)
```

![](report1_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#The two graphs below reflect the number of times each respective respondent changed their political support. The graph on the left reflects female respondents whilst the graph on the right reflects male respondents. The broad trend on both graphs is that, as the number of times party support has changed increases, the respondent count decreases. The average number of changes in political support across both sexes is 1.68, as reflected by the red line in each graph.
```

``` r
###### Average Change in Political Support by Financial Outlook

 data %>% 
  filter(!is.na(financial.future)) %>% 
         group_by(financial.future)%>%
     summarise(mean.support.change = mean(total.switch, na.rm = TRUE))
```

    ## # A tibble: 3 x 2
    ##   financial.future mean.support.change
    ##   <chr>                          <dbl>
    ## 1 better                          1.67
    ## 2 no change                       1.65
    ## 3 worse                           1.72

``` r
 data %>% 
  filter(!is.na(financial.future), !is.na(sex.binary)) %>% 
     group_by(financial.future, sex.binary) %>%
     summarise(mean.support.change = mean(total.switch, na.rm = TRUE))
```

    ## # A tibble: 6 x 3
    ## # Groups:   financial.future [?]
    ##   financial.future sex.binary mean.support.change
    ##   <chr>            <chr>                    <dbl>
    ## 1 better           female                    1.65
    ## 2 better           male                      1.71
    ## 3 no change        female                    1.63
    ## 4 no change        male                      1.67
    ## 5 worse            female                    1.70
    ## 6 worse            male                      1.74

``` r
#It is also important to analyse the respondents’ perspective on their own individual economic outlook when examining switching of political support between parties. This is on the basis that individuals’ financial situations as well as the state of the wider economy has long been an important factor in political support, as demonstrated by Bill Clinton’s famous quote that “it’s the economy, stupid”. 
 
#In this sense, when examining switching of political support between voters, it is clear that respondents with a more negative future outlook on their own personal financial situation have a tendency to switch support between political parties at a slightly higher rate. This is demonstrated by the analysis of the “financial.future” variable, whereby those who saw their own personal financial situation to get “worse” in the future had an average rate of switching support between political parties of 1.72%. In comparison, those who saw their own personal financial situation to get “better” in the future had an average rate of switching support between political parties of 1.67%. This demonstrates how a negative financial outlook caused individuals to switch political support at an average rate of 4% more than those with a positive negative outlook. 

#It is worth mentioning that a 4% discrepancy could be accounted for my errors in the data collection and collation. Specifically, both measurement error and sampling error could both potentially be a significant contributor to the 4% margin.
```

``` r
###### Average Change in Political Support by Generation
 
  (data %>% 
  filter(!is.na(generation), !is.na(total.switch)) %>% 
     group_by(generation) %>%
     summarise(mean.support.change = mean(total.switch, na.rm = TRUE)))
```

    ## # A tibble: 5 x 2
    ##   generation        mean.support.change
    ##   <chr>                           <dbl>
    ## 1 Baby Boomers                     1.65
    ## 2 Generation X                     1.68
    ## 3 GI Generation                    2.04
    ## 4 Millennials                      1.72
    ## 5 Silent Generation                1.70

``` r
  data%>% 
  filter(!is.na(generation)) %>% 
    count(generation) %>%
    mutate(perc = n/ sum(n) * 100)
```

    ## # A tibble: 5 x 3
    ##   generation            n   perc
    ##   <chr>             <int>  <dbl>
    ## 1 Baby Boomers      58191 44.4  
    ## 2 Generation X      36980 28.2  
    ## 3 GI Generation       714  0.545
    ## 4 Millennials       16569 12.6  
    ## 5 Silent Generation 18662 14.2

``` r
#Furthermore, it is important to analyse generational differences in the rate of switching of political support between the Conservative Party and the Labour Party. For example, the oldest generation (the GI Generation) had the highest average total changes in political support at 2.04, whilst the youngest generation (Millennials) had a significantly lower average number of changes in political support  at 1.71. This is a 16.18% difference between generations. However, it is worth noting that, with respondents from the GI Generation only comprising 0.54% of the dataset, the average number of changes in political support for the GI Generation is likely to be susceptible to outliers.
```

``` r
data %>%
  filter(!is.na(generation)) %>% 
  ggplot(aes(x = total.switch, fill = generation)) +
  geom_histogram(bins = 10, position = "dodge") +
  xlab("Number of Times Party Support Has Changed")
```

![](report1_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# The graph below reflects the number of times respondents from different generations have switched their political support between either of the two main political parties across the 7 waves within the dataset. As reflected by the previous analysis, the majority of respondents have not changed their political support between parties more than twice.
```

``` r
data %>%
     filter(!is.na(race)) %>% 
     filter(!is.na(total.switch)) %>% 
     filter(!is.na(sex.binary)) %>% 
     group_by(race, sex.binary) %>%
     summarise(average.switch.support = mean(total.switch, na.rm = TRUE)) %>% 
       ggplot(aes(x = reorder(race, average.switch.support), y = average.switch.support, fill = sex.binary)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("red", "blue")) +
    coord_flip() +
    ylab("Mean Number of Changes in Party Support") +
    xlab("Racial Grouping") +
    theme(legend.position="top") +
    guides(fill=guide_legend(reverse=TRUE)) +
    labs(fill="") 
```

![](report1_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#Furthermore, racial differences often entail differences in social outlook and political perspective, meaning it is important to analyse variation between racial groups when analysis changing political support. In this case, the analysis has also factored in differences in sex.

#The category with the lowest average number of changes in political support are white British females with a mean of 1.60 changes in political support across 7 waves. In comparison, the category with the lowest average number of changes in political support are Pakistani males with a mean of 2.42 changes in political support across 7 waves.

#This is demonstrated by the graph below, which represents the mean number of changes in party support in a comparative context of both racial grouping and sex.
```

``` r
#The analysis and graph below examine the average number of changes in the political support of respondents when categorised by region. In this context, the region with the lowest average number of changes in political support per respondent was the North East, with an average number of changes of 1.54. In comparison, the region with the highest average number of changes in political support per respondent was Northern Ireland, with an average number of changes of 1.90.
  
#One reason for Northern Ireland being the region in the United Kingdom with the highest mean number of changes in political support across 7 waves could be the history of political instability in Northern Ireland. Specifically, the history of sectarian violence could lead to volatility in party loyalty and mean that voters switch support between the Labour and Conservative Party more frequently than other regions.
data %>% 
  filter(!is.na(region), !is.na(total.switch)) %>% 
     group_by(region) %>%
     summarise(mean.support.change = mean(total.switch, na.rm = TRUE))
```

    ## # A tibble: 12 x 2
    ##    region           mean.support.change
    ##    <chr>                          <dbl>
    ##  1 East Midlands                   1.57
    ##  2 East of England                 1.67
    ##  3 London                          1.83
    ##  4 North East                      1.54
    ##  5 North West                      1.61
    ##  6 Northern Ireland                1.90
    ##  7 Scotland                        1.72
    ##  8 Souh West                       1.67
    ##  9 South East                      1.61
    ## 10 Wales                           1.77
    ## 11 West Midlands                   1.59
    ## 12 Yorkshire                       1.74

``` r
  data %>%
    filter(!is.na(region)) %>% 
    ggplot(aes(x = total.switch)) +
    geom_density(fill = "black") +
    xlab("Number of Times Political Support Changes") +
    facet_wrap( ~ region)
```

![](report1_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
#The respective density charts for each region demonstrate the highly uneven distribution of data across the seven waves, with the number of times individuals change their political support being concentrated in the lower values.
```

``` r
##### Summary
#Overall, it is clear that the majority of respondents within the original understanding society rarely change their political support, with this trend holding true despite variations in sex, financial outlook,  region, generation and more. Furthermore, it is clear that a negative financial outlook increases the rate of switching political support, whilst a positive financial outlook decreases the rate of political support, although the difference is small. In addition, the race and sex grouping least likely to change political support is White British women, whilst the race and sex grouping most likely to change their political support is Pakistani Men. In the context of regions, Northern Ireland has the highest average number of changes in political support whilst the North East has the lowest.
```
