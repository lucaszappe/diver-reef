# DiverReef Data Exploration in R

#### April 2024.

### This personal project aimed to solidify and enhance my proficiency in R programming through practical data exploration.

This document provides a summary of the cleaning processes, analysis, and findings derived from the exploration of a diver behavior dataset. For a detailed account and step-by-step reasoning, refer to the full report in the [report.md](/report.md) file.

Code only file versions of these processes can be found in the [support-scripts](/support-scripts) directory.

## Overview

This project explored the DiverReef dataset, focusing on physical diver interactions with reef environments. Using R for data cleaning and analysis, I investigated various aspects of diver behavior, including differences between scuba diving and snorkeling, behavior in Marine Protected Areas (MPAs) versus non-MPAs, correlation between diver experience and number of interactions, the impact of diving with cameras, visibility conditions, and profiling the group with the highest interaction rates.

The exploration of the DiverReef dataset stemmed from a desire to apply and enhance skills in R programming, and to explore a subject area of personal interest - diving and environmental impact.

Note that **interactions** refers to instances where the diver made contact with the reef, whether intentionally or unintentionally. This analysis cannot distinguish between deliberate reef touching and accidental contact.

## Data Description

The DiverReef dataset, sourced from Giglio (2023), comprises 20 years of data collected from 2312 divers across 9 countries and 179 diving sites globally. It includes information on diver profile, environmental interactions, dive locations, and more. The dataset consists of two tables: `DiverReef_behavior_data` and `DiverReef_location_information`.

While the diversity in source studies presented in this dataset provides a comprehensive overview of diving practices worldwide, it could also imply variations in data collection methodologies and observation techniques. Understanding and addressing these potential biases is important important for ensuring reliable and valid analysis results.

***Giglio, V. (2023). DiverReef: A global database of the behavior of recreational divers and their interactions with the reef [Data set]. In Ecology. Zenodo.*** [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10154560.svg)](https://doi.org/10.5281/zenodo.10154560)

## Cleaning
I performed data cleaning on both the "location" and "behavior" tables to ensure data integrity and consistency. Tasks included formatting data types, correcting spellings, and addressing inconsistencies in data entry. Sample code representing these actions is shown below, along with a brief description of the main challenges faced in each table. The identified discrepancies highlighted the need for further investigation and caution when interpreting the data.

Handling inconsistencies and formats:

```r
location2 <- location2 %>%
  mutate(
    activity = activity %>%  # in the activity column, do the following:
      case_match(
        "Scuba" ~ "scuba",   # rename Scuba as scuba
        .default = activity  # don't change any of the others
      ) %>%
      as.factor()            # format as factor
  )
```

```r
location2 <- location2 %>%
  mutate(
    dive_site = dive_site %>%
      str_trim() %>%                # trim trailing whitespaces
      str_to_lower() %>%            # convert to lowercase
      str_replace_all("[-.]", "_")  # replace "-" and "." with underscore
  )
```


### Location table main challenges:

- The longitude and latitude fields contained some entries with two decimal points, which is not standard for coordinate data. I created a function to remove the second decimal point and applied it to those columns.

```r
# define function to replace 2nd instance of ".", if it exists
replace_second_period <- function(string) {
  # store all positions of "." from the input string
  positions <- str_locate_all(string, fixed("."))[[1]][, "start"]
  # check if string has more than one "."
  if (length(positions) >= 2) {
    # concatenate part before the second "." with part after it
    string <- str_sub(string, 1, positions[2] - 1) %>%
      str_c(str_sub(string, positions[2] + 1))
  }
  return(string)
}

# apply function to longitude and latitude columns
location2 <- location %>%
  mutate(
    longitude = map_chr(longitude, replace_second_period),
    latitude = map_chr(latitude, replace_second_period)
  )
```

- Some records had coordinates in a different standard (Degrees and Decimal Minutes) and needed to be converted to Decimal Degrees. Another function was implemented for this purpose.

```r
# define function to convert coordinates
convert_coord <- function(coord) {
  # split the coordinate into parts
  parts <- str_split(coord, " ")[[1]]

  # extract degrees
  degrees <- as.numeric(parts[1])
  # extract minutes and remove letter
  minutes <- as.numeric(str_replace(parts[2], "[NSEW]", ""))

  # determine the sign based on the direction (last character of the string)
  direction <- str_sub(parts[2], -1, -1)
  sign <- if_else(direction %in% c("N", "E"), 1, -1)

  # calculate decimal degrees
  decimal_degrees <- sign * (degrees + minutes / 60)

  return(decimal_degrees)
}
```

### Behavior table main challenges:

- Some entries in the fields representing the number of interactions showed inconsistencies. After exploring different scenarios to understand these entries, I decided to drop rows containing observations with no plausible explanations.

```r
# select only records where the reasoned conditions are met
behavior <- behavior %>%
  filter(
    # if damage + contact == total_interactions OR
    (damage + contact == total_interactions) |
      # if damage AND contact are NA
      (is.na(damage) & is.na(contact)))
```

- To assess observation credibility, I derived a new metric - interactions per minute - from the total number of interactions and sampling time and established a threshold to filter out records with unrealistically high values.

```r
behavior <- behavior %>%
  # create interactions_per_min and round to 4 digits
  mutate(interactions_per_min = round(total_interactions / samp_time_min, 4)) %>%
  # select only observations with interactions_per_min below threshold
  filter(interactions_per_min <= 12)
```

## Analysis and Findings

After cleaning the data, the next step involved joining the two tables to enable exploration. The resulting joined table ensured that only corresponding observations were retained.

Due to potential methodological variances among researchers, variables related to locations were not investigated. Instead, the focus shifted to variables less likely to be affected by such disparities, such as diver experience levels. An examination revealed a consistent distribution of experience levels across different locations, as seen on the figure below. Similarly, I decided not to conduct any analysis on time-related variables, as this dataset’s source studies were not designed for such inquiries. 

![](report-figures/xp%20by%20ctry%20hist-1.png)

Figures were created using the `ggplot2` library and only the results are shown here. For the code, refer to the [full report](/report.md) or the [support-scripts](/support-scripts) directory. 

The analysis approach focused on exploring diver behavior and its correlates while considering potential biases and limitations. Key findings are as follows.

### Scuba Diving vs Snorkeling

Divers interact with the environment more frequently than snorkelers on average. Since the data was shown to be heavily skewed, I evaluated the median as well which reaffirmed this tendency. In fact, at least 50% of snorkelers had no recorded interactions with the reef, compared to 30% of divers.

Summary statistics for scuba divers is shown below and from this point on, only this group was evaluated.

```r
# calculate desired statistics for interactions_per_min
reef_scuba %>% summarize(
  # mean interactions per minute
  mean_int = mean(interactions_per_min),
  # percentage below mean:
  # creates a vector with TRUE for every value below the mean and
  # mean() calculates the ratio of TRUE to FALSE
  int_below_mean = mean(interactions_per_min < mean_int) * 100,
  # first quartile
  first_quartile = quantile(interactions_per_min, probs = 0.25),
  # median
  median = median(interactions_per_min),
  # second quartile
  third_quartile = quantile(interactions_per_min, probs = 0.75),
  # percentage of interactions_per_min that are zero
  int_zero = sum(interactions_per_min == 0) /
    sum(!is.na(interactions_per_min)) * 100
)
```

    ## # A tibble: 1 × 6
    ##   mean_int int_below_mean first_quartile median third_quartile int_zero
    ##      <dbl>          <dbl>          <dbl>  <dbl>          <dbl>    <dbl>
    ## 1    0.682           75.3              0   0.15          0.655     30.8

This type of research and its findings are essential for designing effective resource management plans in protected areas. Depending on the sensitivity of the environment, such results may be deemed acceptable or warrant further action.

### Marine Protected Areas vs non-MPAs

Contrary to initial hypotheses, non-MPAs exhibited lower interaction rates with the reef, as indicated by the box plot in the figure below. Since these areas usually have processes in place to prevent interactions between divers and the environment, there could be other factors at play that warrant further investigation to ensure the credibility of the data and identify any potential biases.

![](report-figures/mpa%20plot-1.png)

### Experience and Number of Interactions Correlation

Analysis indicated no correlation between diver experience and the number of physical interactions, challenging usual assumptions about this relationship. Despite the mechanical skills associated with diving improving over time, this did not translate into more responsible interactions with the environment.

This is supported by a low calculated linear correlation between these variables, as well as scatter plot visualizations created during this part of the exploration. An example of such a visualization is shown below.

![](report-figures/xp%20100-1.png)

### Impact of Diving with Cameras

Following a similar approach to the MPAs section, I calculated summary statistics and created a box plot of interactions per minute for each group of divers: with and without cameras. The results suggested that cameras have minimal impact on the extent of physical interaction between divers and the reef. Interestingly, photographers appear to engage slightly less with the environment. This was evidenced by a lower median value and a higher percentage of divers in this category not touching the reef at all.


### Impact of Visibility Conditions

According to linear correlation and a scatter plot visualization, visibility conditions did not appear to correlate with the frequency of diver interactions with the reef. Subjective visibility estimates provided by researchers introduced complexities to the analysis, emphasizing the need for nuanced interpretation. The tendency of observers to enter visibility values as convenient numbers, such as 5, 10 and 15, can easily be spotted in the bar chart below.

![](report-figures/viz-2.png)

### Top 10% of Interactions Rates Group

The group with the highest recorded interaction rates, defined as observations in the 90th percentile of interactions per minute, supported the previous results:

- This group consisted of divers from all experience levels, suggesting that experience level has no bearing on the number of interactions.
- There was a higher proportion of "no-camera" divers compared to "yes-camera" divers in this group, relative to the entire dataset, indicating that divers without cameras are overrepresented in this group.
- Additionally, a larger proportion of observations took place outside Marine Protected Areas (MPAs) in this group, compared to the entire dataset, which corroborates the earlier finding.

## Conclusion

I successfully achieved this project’s objective of enhancing my proficiency in R programming and data analysis. Despite dataset limitations and potential biases, the analysis provided insights into diver behavior and interactions with the environment. These findings serve as a foundation for further exploration and highlight areas for future research.

I am confident in my ability to use R to explore various datasets. However, this project underscores the importance of data quality, statistical knowledge, and analytical skills in deriving meaningful insights from datasets. Moving forward, part of my efforts will focus on refining my understanding of statistics, so I am able to ask more insightful questions and validate results effectively.
