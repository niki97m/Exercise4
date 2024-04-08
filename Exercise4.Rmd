---
title: "Exercise 4"
author: "Niki Mahmoodzadeh"
date: "2024-04-08"
output:
  word_document: default
  html_document: default
---

# Introduction

This task aims to explore how a patent examiner's network centrality influences the duration of patent application processes.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)
library(gender)
library(wru)
library(igraph)
library(ggraph)
library(gridExtra)


```

# Data loading and preprocessing

Initially, I import the dataset and prepare it for subsequent analysis.


```{r data}
applications = read_csv("/Users/nikimahmoodzadeh/Downloads/672_project_data-2/app_data_sample.csv", show_col_types = FALSE)
edges = read_csv("/Users/nikimahmoodzadeh/Downloads/672_project_data-2/edges_sample.csv",show_col_types = FALSE)

```

# Data Preprocessing

## Estimating examiner demographics


```{r examiner-demographic-estimation, echo=FALSE}

# Gender estimation

examiner_names=applications %>% distinct(examiner_name_first)

# Predict gender based on first names
examiner_names_gender=examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(examiner_name_first = name, gender, proportion_female)

# Join gender data back to the main applications dataset
applications=applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# Race estimation

examiner_surnames=applications %>% select(surname = examiner_name_last) %>% distinct()

# Predict race based on last names
examiner_race=predict_race(voter.file = examiner_surnames, surname.only = T) %>% as_tibble()

# Select the race with the highest probability for each last name
examiner_race=examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

# Join race data back to the main applications dataset
applications=applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))


# Tenure calculation

examiner_dates=applications %>% 
  select(examiner_id, filing_date, appl_status_date)

# Convert dates to a consistent format
examiner_dates=examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

# Calculate the earliest and latest dates for each examiner and their tenure in days
examiner_dates=examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018)

# Join tenure data back to the main applications dataset
applications=applications %>% left_join(examiner_dates, by = "examiner_id")


```

## Creating processing time variable

```{r processing-time-variable, echo=FALSE}
applications=applications %>%
  mutate(
    final_decision_date = coalesce(patent_issue_date, abandon_date),
    app_proc_time = as.numeric(difftime(final_decision_date, filing_date, units = "days"))
  )

```


# Centrality measures

Next, I generate a distinct list of examiner IDs as a preparatory step before diving into the main analysis.

```{r unique-examiner-ids, echo=FALSE}
unique_examiner_ids=unique(c(edges$ego_examiner_id, edges$alter_examiner_id))

g=graph_from_data_frame(edges[, c("ego_examiner_id", "alter_examiner_id")], directed = TRUE, vertices = data.frame(name = unique_examiner_ids))
```

Following this, I begin to calculate various centrality metrics for further examination.

```{r centrality-measures, echo=FALSE}
centrality_entire=data.frame(
  examiner_id = V(g)$name,
  degree_centrality = degree(g, mode = "out"),
  betweenness_centrality = betweenness(g, directed = TRUE),
  closeness_centrality = closeness(g, mode = "out")
)

centrality_entire$examiner_id=as.numeric(centrality_entire$examiner_id)

applications=applications %>%
  left_join(centrality_entire, by = "examiner_id")

centrality_entire=data.frame(
  examiner_id = V(g)$name,
  degree_centrality = degree(g, mode = "out"),
  betweenness_centrality = betweenness(g, directed = TRUE),
  closeness_centrality = closeness(g, mode = "out")
)

centrality_entire$examiner_id=as.numeric(centrality_entire$examiner_id)

applications=applications %>%
  left_join(centrality_entire, by = "examiner_id")

```


# Exploratory Data Analysis
```{r column-names, echo=FALSE}
# column names to confirm the presence of 'gender'
colnames(applications)
```


```{r eda, echo=FALSE}
# Histogram of processing time
ggplot(applications, aes(x = app_proc_time)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Application Processing Time", x = "Processing Time (days)")

```

# Regression Analysis

First, I will remove the missing values in degree, betweenness, and closeness centrality.
```{r missing-value-remove, echo=FALSE}
applications_clean=applications %>%
  filter(!is.na(degree_centrality.x),
         !is.na(betweenness_centrality.x),
         !is.na(closeness_centrality.x))

```

## Degree centrality linear regression model

I conduct an analysis to construct a linear regression model, using degree centrality as the predictor variable.

```{r regression-analysis-degree-centrality, echo=FALSE}
# Estimate the linear regression model with degree_centrality as the independent variable
degree_model=lm(
  app_proc_time ~ degree_centrality.x + gender.x + race.x + tenure_days.x,
  data = applications_clean
)

summary(degree_model)

```
### Explanation on degree centrality linear regression model

The model constructed includes variables such as degree centrality, gender, race, and tenure days, predicting application processing time. The model's adjusted R-squared value is 0.003339, indicating a mere 0.33% variance in processing time can be accounted for by these variables, suggesting a poor model fit.

## Betweenness centrality linear regression model

I proceed to estimate a linear regression model, this time with betweenness centrality as the predictor.

```{r regression-analysis-betweenness-centrality, echo=FALSE}
# Betweenness centrality linear regression model
betweenness_model=lm(
  app_proc_time ~ betweenness_centrality.x + gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(betweenness_model)

```

### Explanation on betweenness centrality linear regression model

This model, incorporating betweenness centrality, gender, race, and tenure days to predict processing time, achieves an adjusted R-squared of 0.003478. This further implies that betweenness centrality is an ineffective predictor of processing time.

## Closeness centrality linear regression model

Next, I estimate a linear regression model with closeness centrality as the predictor.

```{r closeness_model}
# Closeness centrality linear regression model
closeness_model=lm(
  app_proc_time ~ closeness_centrality.x + gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(closeness_model)
```


### Explanation on closeness centrality linear regression model
The closeness centrality model, including the same set of variables, yields an adjusted R-squared of 0.008616, showing a slight improvement over the previous models but still indicating weak predictive capability.


## Combined model of linear regression

Finally, I estimate a combined linear regression model that includes all centrality measures.

```{r combined_model, echo=FALSE}
# Combined centrality linear regression model
combined_model=lm(
  app_proc_time ~ degree_centrality.x + betweenness_centrality.x + closeness_centrality.x + gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(combined_model)
```

### Explanation on combined model

The combined model exhibits an adjusted R-squared of 0.008712, a minor improvement over the closeness model's 0.008616, underscoring the marginal enhancement achieved by combining these centrality measures.


# Analysis to see if this relationship differ by examiner gender

## Degree-Gender interaction

```{r degree_gender_interaction}
# Degree centrality model with interaction
degree_gender_interaction=lm(
  app_proc_time ~ degree_centrality.x * gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(degree_gender_interaction)
```

### Explanation on Degree-Gender interaction

In the model analyzing the interaction between degree centrality and gender, a significant interaction indicates varying effects of degree centrality on processing time by gender, with a mitigated effect observed for male examiners.


## Betweenness-Gender interaction

```{r betweenness_gender_interaction}
# Betweenness centrality model with interaction
betweenness_gender_interaction=lm(
  app_proc_time ~ betweenness_centrality.x * gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(betweenness_gender_interaction)
```
### Explanation on Betweenness-Gender interaction

The model examining betweenness centrality and gender interaction demonstrates that higher betweenness centrality may lengthen processing times, especially for male examiners, though its overall explanatory power is minimal.



## Closeness-Gender interaction:

```{r closeness_gender_interaction}
# Closeness centrality model with interaction
closeness_gender_interaction=lm(
  app_proc_time ~ closeness_centrality.x * gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(closeness_gender_interaction)
```

### Explanation on Closeness-Gender interaction

The closeness centrality and gender interaction model show that closeness centrality typically reduces processing times, but this effect is less pronounced for male examiners.


## Combined-Gender interaction:

```{r combined_gender_interaction}
# Combined model with interaction
combined_gender_interaction=lm(
  app_proc_time ~ (degree_centrality.x + betweenness_centrality.x + closeness_centrality.x) * gender.x + race.x + tenure_days.x,
  data = applications_clean
)
summary(combined_gender_interaction)
```

### Explanation on Combined-Gender interaction

The model that combines all centrality measures and their interactions with gender reveals complex effects, with gender moderating these impacts, yet it still fails to significantly enhance explanatory power.


# Conclusion

In summary, although gender modifies the influence of centrality on processing times, the low adjusted R-squared values across all models indicate that these variables alone do not effectively predict processing times, highlighting the need for a more comprehensive model to fully grasp the dynamics affecting processing times at the USPTO.
