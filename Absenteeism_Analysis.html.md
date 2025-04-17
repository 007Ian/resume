---
title: "HR Analytics"
execute:
  echo: true
  keep-md: true
format:
  html:
    theme: darkly
    code-link: true
    code-fold: true
    code-line-numbers: true
date: "April 05, 2025"
---




## Absenteeism

Our project is to find which variables play into absenteeism at work the most.



::: {.cell}

```{.r .cell-code}
library(readxl)
library(dplyr)
library(mosaic)
library(broom)
library(kableExtra)
Absenteeism <- read_excel("Absenteeism_at_work_Project.xls")

#View(Absenteeism)
```
:::



We can see that each employee has several rows/absence reports. We need to find out if we should find the average or do a longitudinal study.

First lets look at the averages for each employee, see the risk factors, and build a regression model to understand the absence rates. Then, we will look at the longitudinal data to see if we can find any patterns over time.



::: {.cell}

```{.r .cell-code}
summary_HR <- Absenteeism %>%
  group_by(ID) %>%
  summarise(
    total_absences = n(),
    Age = mean(Age, na.rm = TRUE),
    Distance_from_Residence_to_Work = mean(`Distance from Residence to Work`, na.rm = TRUE),
    Reason = as.factor(`Reason for absence`),
    Month_of_absence = as.factor(`Month of absence`),
    Day_of_the_week = as.factor(`Day of the week`),
    Seasons = as.factor(Seasons),
    Work_load_Average_day = mean(`Work load Average/day`, na.rm = TRUE),
    Absenteeism_time_in_hours = mean(`Absenteeism time in hours`, na.rm = TRUE),
    Travel_Expense = mean(`Transportation expense`, na.rm = TRUE),
    Service_time = mean(`Service time`, na.rm = TRUE),
    Disciplinary_failure = sum(`Disciplinary failure`, na.rm = TRUE),
    Education = mean(Education, na.rm = TRUE),
    Son = mean(Son, na.rm = TRUE),
    Social_drinker = mean(`Social drinker`, na.rm = TRUE),  
    Social_smoker = mean(`Social smoker`, na.rm = TRUE),  
    Pet = mean(Pet, na.rm = TRUE),
    Hit_score = mean(`Hit target`, na.rm = TRUE),
    BMI = mean(`Body mass index`, na.rm = TRUE),
    Total = sum(Absenteeism_time_in_hours) #sum one
  )


#print(summary_HR)

#redo models with transportation costs
    
    
# absent.lm <- lm(total_absences ~ Age + Absenteeism_time_in_hours+Distance_from_Residence_to_Work + Work_load_Average_day + Service_time + Disciplinary_failure + Education + Son + Social_drinker + Social_smoker + Pet + Hit_score + BMI, data = summary_HR)
# summary(absent.lm)

# absent.lm <- lm(Absenteeism_time_in_hours ~ Age + Distance_from_Residence_to_Work + Work_load_Average_day + Service_time + Disciplinary_failure + Education + Son + Social_drinker + Social_smoker + Pet + Hit_score + BMI, data = summary_HR)
# summary(absent.lm)

# Create pairs plots for smaller groups of variables
# Create pairs plots for smaller groups of variables

# Subset 1: Basic Demographics and Job Details
pairs(summary_HR[, c("Age", "Distance_from_Residence_to_Work", "Education", "Son")],
      main = "Pairs Plot: Age, Distance from Residence to Work, Education, Son")
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::

```{.r .cell-code}
# Subset 2: Work-related Metrics
pairs(summary_HR[, c("Work_load_Average_day", "Travel_Expense", "Service_time", "Disciplinary_failure")],
      main = "Pairs Plot: Work Load, Travel Expense, Service Time, Disciplinary Failure")
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-2-2.png){width=672}
:::

```{.r .cell-code}
# Subset 3: Social Metrics and Performance
pairs(summary_HR[, c("Social_drinker", "Social_smoker", "Pet", "Hit_score", "BMI")],
      main = "Pairs Plot: Social Drinker, Social Smoker, Pet, Hit Score, BMI")
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-2-3.png){width=672}
:::

```{.r .cell-code}
# Subset 4: Absenteeism Metrics
pairs(summary_HR[, c("total_absences", "Absenteeism_time_in_hours", "Total")],
      main = "Pairs Plot: Total Absences, Absenteeism Time in Hours, Total Absenteeism")
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-2-4.png){width=672}
:::
:::



To find the best variables to predict absenteeism we did a stepwise regression. We found the following variables to be the most significant:



::: {.cell}

```{.r .cell-code}
#make graphs to show correlations with total 

# Initial full model
#total.lm <- lm(Total ~ . -Absenteeism_time_in_hours, data = summary_HR)

# # Stepwise regression to remove non-significant terms
# step_model <- step(total.lm, direction = "both")  # or use "backward"
# summary(step_model)


# total.lm <- lm(Total ~ Age + Distance_from_Residence_to_Work +Transportation_ Education + Son + BMI, data = summary_HR)
# summary(total.lm)

# total.lm <- lm(Total ~ . -Absenteeism_time_in_hours ,data = summary_HR)
# summary(total.lm)


# total2.lm <- lm(Total ~ Work_load_Average_day + Month_of_absence + Service_time + Disciplinary_failure + Education + Son + Social_drinker + Social_smoker + Pet + Hit_score + BMI, data = summary_HR)
# summary(total2.lm)


#plot(total.lm, which = 4)  # or use cooks.distance(model)

#boxCox(total.lm)

summary_HR <- summary_HR %>%
  mutate(Significant_reason = ifelse(Reason %in% c(6, 12, 18, 24), 1, 0))

lm_final <- lm(
  Total ~ Age + Distance_from_Residence_to_Work +
    Significant_reason +
    Work_load_Average_day + Travel_Expense + Service_time +
    Education + Son + Social_drinker + Social_smoker +
    Hit_score + BMI,
  data = summary_HR
)


# Tidy the linear model output
lm_tidy <- tidy(lm_final)

# Create a HTML table from the tidy output
lm_tidy %>%
  kable("html", caption = "Linear Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

::: {.cell-output-display}

`````{=html}
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Linear Model Summary</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> -41.0051453 </td>
   <td style="text-align:right;"> 11.3122650 </td>
   <td style="text-align:right;"> -3.624840 </td>
   <td style="text-align:right;"> 0.0003093 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age </td>
   <td style="text-align:right;"> 0.0920467 </td>
   <td style="text-align:right;"> 0.0246358 </td>
   <td style="text-align:right;"> 3.736293 </td>
   <td style="text-align:right;"> 0.0002014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Distance_from_Residence_to_Work </td>
   <td style="text-align:right;"> -0.1902782 </td>
   <td style="text-align:right;"> 0.0093922 </td>
   <td style="text-align:right;"> -20.259179 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Significant_reason </td>
   <td style="text-align:right;"> 3.0467328 </td>
   <td style="text-align:right;"> 0.4449288 </td>
   <td style="text-align:right;"> 6.847686 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Work_load_Average_day </td>
   <td style="text-align:right;"> -0.0000708 </td>
   <td style="text-align:right;"> 0.0000102 </td>
   <td style="text-align:right;"> -6.919850 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Travel_Expense </td>
   <td style="text-align:right;"> 0.0189498 </td>
   <td style="text-align:right;"> 0.0019832 </td>
   <td style="text-align:right;"> 9.555373 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Service_time </td>
   <td style="text-align:right;"> 0.1763905 </td>
   <td style="text-align:right;"> 0.0406936 </td>
   <td style="text-align:right;"> 4.334606 </td>
   <td style="text-align:right;"> 0.0000167 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Education </td>
   <td style="text-align:right;"> -2.0344281 </td>
   <td style="text-align:right;"> 0.2090350 </td>
   <td style="text-align:right;"> -9.732477 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Son </td>
   <td style="text-align:right;"> 0.4757634 </td>
   <td style="text-align:right;"> 0.1078085 </td>
   <td style="text-align:right;"> 4.413041 </td>
   <td style="text-align:right;"> 0.0000117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Social_drinker </td>
   <td style="text-align:right;"> 3.4573575 </td>
   <td style="text-align:right;"> 0.2780789 </td>
   <td style="text-align:right;"> 12.433008 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Social_smoker </td>
   <td style="text-align:right;"> -0.6267099 </td>
   <td style="text-align:right;"> 0.4167712 </td>
   <td style="text-align:right;"> -1.503726 </td>
   <td style="text-align:right;"> 0.1330861 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hit_score </td>
   <td style="text-align:right;"> 0.7878794 </td>
   <td style="text-align:right;"> 0.1093993 </td>
   <td style="text-align:right;"> 7.201866 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BMI </td>
   <td style="text-align:right;"> -0.4276038 </td>
   <td style="text-align:right;"> 0.0348316 </td>
   <td style="text-align:right;"> -12.276307 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
</tbody>
</table>

`````

:::

```{.r .cell-code}
summary(lm_final)
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = Total ~ Age + Distance_from_Residence_to_Work + 
    Significant_reason + Work_load_Average_day + Travel_Expense + 
    Service_time + Education + Son + Social_drinker + Social_smoker + 
    Hit_score + BMI, data = summary_HR)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.1204 -1.2539 -0.0791  0.9972 16.5179 

Coefficients:
                                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                     -4.101e+01  1.131e+01  -3.625 0.000309 ***
Age                              9.205e-02  2.464e-02   3.736 0.000201 ***
Distance_from_Residence_to_Work -1.903e-01  9.392e-03 -20.259  < 2e-16 ***
Significant_reason               3.047e+00  4.449e-01   6.848 1.60e-11 ***
Work_load_Average_day           -7.078e-05  1.023e-05  -6.920 9.93e-12 ***
Travel_Expense                   1.895e-02  1.983e-03   9.555  < 2e-16 ***
Service_time                     1.764e-01  4.069e-02   4.335 1.67e-05 ***
Education                       -2.034e+00  2.090e-01  -9.732  < 2e-16 ***
Son                              4.758e-01  1.078e-01   4.413 1.17e-05 ***
Social_drinker                   3.457e+00  2.781e-01  12.433  < 2e-16 ***
Social_smoker                   -6.267e-01  4.168e-01  -1.504 0.133086    
Hit_score                        7.879e-01  1.094e-01   7.202 1.49e-12 ***
BMI                             -4.276e-01  3.483e-02 -12.276  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.715 on 727 degrees of freedom
Multiple R-squared:  0.586,	Adjusted R-squared:  0.5791 
F-statistic: 85.74 on 12 and 727 DF,  p-value: < 2.2e-16
```


:::
:::



You will notice a new column called Significant_reason. This is a binary variable that indicates whether the reason for absence is significant or not. We can see that the most significant reasons are 6, 12, 18, and 24. Here is a bar chart that helped me find this out.



::: {.cell}

```{.r .cell-code}
summary_HR %>%
  filter(!is.na(Reason)) %>%
  group_by(Reason) %>%
  summarise(Avg_Total = mean(Total, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Reason, Avg_Total), y = Avg_Total, fill = Avg_Total)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Total, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Average Absence Hours by Reason",
    x = "Reason for Absence",
    y = "Average Absence Hours",
    fill = "Avg Hours"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::



As we can see from this chart, some of the reasons for absence are more significant than others.We can check the average hours for each reason and see what reasons cause the most absence on average so that we can predict it better.

Turns out that 6 is Mental and behavioural disorders 15 is Pregnancy, childbirth and the puerperium, which makes sense, and 24 is blood donation, so apperantly those take the longest and should be taken into account when hiring someone.



::: {.cell}

```{.r .cell-code}
ggplot(summary_HR, aes(x = Age, y = Total, color= BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Absence Hours vs Age", x = "Age", y = "Total Absence Hours") +
  theme_minimal()
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
ggplot(summary_HR, aes(x = Distance_from_Residence_to_Work, y = Total, color= Son)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Absence Hours vs Distance from Residence to Work", x = "Distance from Residence to Work", y = "Total Absence Hours") +
  theme_minimal()
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::



These summaries show that the most significant variables are Age, Distance from Residence to Work, BMI, and the reasons they have to be absent from work. They display how they are correlated with the total absence hours.




::: {.cell}

```{.r .cell-code}
#Create lines for everything

monthly_summary <- Absenteeism %>%
  group_by(ID, Month = `Month of absence`) %>%
  summarise(
    Avg_Absence_Hours = mean(`Absenteeism time in hours`, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(monthly_summary, aes(x = Month, y = Avg_Absence_Hours, group = ID, color = as.factor(ID))) +
  geom_line(alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", size = 1.2, se = FALSE) +
  scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
  labs(title = "Monthly Average Absence Hours per Employee",
       x = "Month",
       y = "Avg Absence Hours") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
```

::: {.cell-output-display}
![](Absenteeism_Analysis_files/figure-html/unnamed-chunk-7-1.png){width=672}
:::
:::



There does not seem to be a correlation with peoples absences and the month of the year. If there was any uptick in absences for a month, than we would see it in our regression models or in this graph. This graph clearly shows that time of month has no effect on absences in general. 

Our analysis concludes that there are a few things that can predict whether an employee will spend a lot of time absent. The biggest variables in our dataset were Age, Distance from Residence to Work, BMI, the reasons they have to be absent from work, their work load on an average day, travel expenses, service time, education, whether they have children or not, and whether they are a social drinker or smoker.

These variables can be used to predict absenteeism in the future. We can use this information to help us make better hiring decisions and to help us understand why people are absent from work. You can see the magnitude in which each of the variables effects abenteeism in the regression model summary. 
