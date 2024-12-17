# Modelling physical activity in Melbourne


This Quarto markdown document is intended to contain code to predict
physical activity for sport and recreation excluding walking and
cycling, given explanatory variables (age, sex, SES, education, has car,
etc) that align with the JIBE synthetic population data. This will be
conducted to predict physical activity for residents of Melbourne,
Australia, using the Australian Bureau of Statistics Australian National
Health Survey data (2017-18).

The analysis draws on Belen Zapata-Diomedi’s code for cleaning the NHS
dataset (../document.qmd), and Belen Zapata-Diomedi, Qin Zhang and
Marina Berdokhova’s code for a predictive model of marginal metabolic
equivalent hours per week (mMET house/week) for Manchester, UK.

To allow this code to be run on different computers and operating
systems easily, rather than hardcode data paths for inputs there is a
file chooser for the three key inputs used:

1.  ABS NHS households (NHS17HHB.csv)
2.  ABS NHS persons (NHS17PSB.csv)
3.  Melbourne synthetic population (population_final.rds)

The user will be asked to provide paths for these files in this order.
If using RStudio or Visual Studio code, there should be a graphical file
picker. If using Posit or commandline on linux, you may have to enter
the file path string.

## Dependencies

Analysis was conducted using R 4.4.1 using a Quarto markdown document
(Quarto 1.5.55) in Positron IDE (2024.10.0), with renv 1.0.11 for
package management.

The following packages have been installed using renv:

    renv::install(c('dplyr','data.table','ggplot2','vtable','rmarkdown','pscl'))

In principle, the R environment for this notebook should be able to be
restored by running

    renv::restore()

Load libaries

``` r
library(dplyr)
library(vtable)
library(ggplot2)
library(data.table)
```

## Data

[National Health
Survey](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/national-health-survey)
(NHS) data for 2017-18 were retrieved from the [Microdata
Downloads](https://microdatadownload.abs.gov.au/MicrodataDownload/login.xhtml)
section of the Australian Bureau of Statistics website on 3 December
2024. ABS Microdata were accessed under the [ABS/Universities Australia
Agreement
(2024)](https://www.abs.gov.au/statistics/microdata-tablebuilder/absuniversities-australia-agreement)
by Carl Higgs (RMIT University). The NHS Microdata data descriptions are
available for download
[here](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/national-health-survey#data-item-lists).

NHS microdata are provided in CSV, SAS, SPSS, or Stata formats. The CSV
data do not have labels, hence the `haven` package could be installed to
read the labelled data in .dta (Stata) format. However, perhaps labels
are not required — for now, CSV will be used to keep things simple.

| File (csv, dta, etc) | Description                |
|----------------------|----------------------------|
| NHS17HHB             | Household level data       |
| NHS17SPB             | Person level data          |
| NHS17A3B             | Alcohol day level data     |
| NHS17A4B             | Alcohol type level data    |
| NHS17CNB             | Conditions level data      |
| NHS17MDB             | Medications level data     |
| NHS17HLB             | Health Literacy level data |

ABS NHS 2017-18 Microdata files

The household data contain geographic attributes and could potentially
be used to restrict the sample, e.g. to residents of urban areas within
Greater Melbourne. Sensitivity analysis could be conducted to evaluate
the impact of this decision, e.g. relative to all persons and all
persons living in Australian urban regions.

Household variables of interest include:

| Variable | Description | Comment |
|----|----|----|
| ABSHIDB | Household identifier | Link with persons |
| STATE16 | State or Territory (ASGS 2016) | 2 == Victoria |
| NUMPERBC | Number |  |
| of persons  in household | 0, 1, 2, 3, 4, 5, 6==6+ |  |
| SA1SF2DN | SEIFA - Index of Relative Socio-economic Disadvantage - 2016 - SA1 - Deciles - National |  |

Person-level variables of interest (see data dictionaries for detailed
codes) include:

| Variable | Description | Comment |
|----|----|----|
| ABSPID | Person identifier | person number within household |
| ABSHIDB | Household identifier | Link with households, but actually does not match for CURF records |
| AGEB | Age of person | 1== 0-4 years  … 19 == 85 years+ |
| SEX | Sex of person | 1==Male, 2==Female |
| LFSBC | Labour force status | 0==NA, 1==Employed, 2==Unemployed, 3==Not in the labour force |
| HYSCHCBC | Highest year of school completed | 0==NA, 1==Postgraduate, … 13==Never attended school |
| HIGHLVLBC | Level of highest educational attainment | 0==NA, 1==Postgraduate, … 13==Never attended school |
| EMPSTAT | Labour force full-time/part-time status | 0==NA, 1==Employed full time … 6 Not in labour force |
| Walk for recreation (mins) | EXFSRMIN | Total minutes walked for fitness, recreation or sport in last week (for at least 10 minutes) |
| Walk for transport (mins) | EXTRAMIN | Total minutes spent walking for transport in last week (for at least 10 minutes) |
| Moderate exercise (mins) | EXLWMMIN | Total minutes undertaken moderate exercise last week (for example, a light jog, strenght and toning exercises, lifting small boxes and sweeping) |
| Vigorous exercise (mins) | EXLWVMIN | Total minutes undertaken vigorous exercise last week (for example, playing basketball, running, lifting heavy boxes, and strength and toning exercises) |

## Methods

### Read and join NHS data

Adults aged 18 years and over from the persons dataset are left joined
to the household data, with only the relevant variables retained. The
core exposure variables are renamed to enhance readability of the
subsequent code.

``` r
# choose household file (may require GUI IDE like RStudio/PositStudio/VSCode)
NHS17HHB.csv <- file.choose()
# choose person file (may require GUI IDE like RStudio/PositStudio/VSCode)
NHS17SPB.csv <- file.choose()
data <- list(
    households = read.csv(NHS17HHB.csv),
    persons = read.csv(NHS17SPB.csv)
)
```

``` r
nhs <- dplyr::left_join(
            data$households %>% 
                select(
                    c( 
                        "ABSHIDB",
                        "STATE16", 
                        "NUMPERBC",
                        "SA1SF2DN"
                    )
                ),
            data$persons %>% 
                select(
                    c(
                        "ABSHIDB",
                        "ABSPID",
                        "AGEB",
                        "SEX",
                        "LFSBC",
                        "STDYFTPT",
                        "HYSCHCBC",
                        "HIGHLVBC",
                        "EXFSRMIN",
                        "EXTRAMIN",
                        "EXLWMMIN",
                        "EXLWVMIN"
                    )
                ) %>% rename(
                    walk_recreation_min=EXFSRMIN,
                    walk_transport_min=EXTRAMIN,
                    mod_excercise_min=EXLWMMIN,
                    vig_excercise_min=EXLWVMIN
                )%>% 
                filter(
                    AGEB > 4
                ), 
        by = c("ABSHIDB")
        ) 
nhs %>% st(out='kable')
```

| Variable            | N     | Mean | Std. Dev. | Min | Pctl. 25 | Pctl. 75 | Max   |
|:--------------------|:------|:-----|:----------|:----|:---------|:---------|:------|
| STATE16             | 16376 | 3.6  | 2.2       | 1   | 2        | 5        | 8     |
| NUMPERBC            | 16376 | 2.4  | 1.3       | 1   | 1        | 3        | 6     |
| SA1SF2DN            | 16376 | 5.4  | 2.8       | 1   | 3        | 8        | 10    |
| ABSPID              | 16370 | 1    | 0         | 1   | 1        | 1        | 1     |
| AGEB                | 16370 | 12   | 3.6       | 5   | 9        | 15       | 19    |
| SEX                 | 16370 | 1.5  | 0.5       | 1   | 1        | 2        | 2     |
| LFSBC               | 16370 | 1.8  | 0.96      | 1   | 1        | 3        | 3     |
| STDYFTPT            | 16370 | 2.9  | 0.45      | 1   | 3        | 3        | 3     |
| HYSCHCBC            | 16370 | 2    | 1.3       | 1   | 1        | 3        | 5     |
| HIGHLVBC            | 16370 | 4.5  | 2.6       | 1   | 2        | 6        | 12    |
| walk_recreation_min | 16370 | 153  | 2474      | 0   | 0        | 120      | 99998 |
| walk_transport_min  | 16370 | 352  | 5236      | 0   | 0        | 90       | 99998 |
| mod_excercise_min   | 16370 | 67   | 1359      | 0   | 0        | 40       | 99998 |
| vig_excercise_min   | 16370 | 32   | 104       | 0   | 0        | 0        | 2400  |

Summary Statistics

### Read and consider the synthetic population data

To predict mMETs for the synthetic population, we need to understand how
the variables are structured and ensure that our NHS derived data that
we will use in modelling has a comparable structure. We’ll load up the
data and consider a summary of variables to better understand this.

I am using
[data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)
as it is meant to be optimised for handling large datasets, like this
synthetic population. This performs subsequent operations deriving new
variables much faster.

``` r
population_final.rds <- file.choose()
pp <- readRDS(population_final.rds) %>% as.data.table()
pp %>% st(out='kable')
```

| Variable | N | Mean | Std. Dev. | Min | Pctl. 25 | Pctl. 75 | Max |
|:---|:---|:---|:---|:---|:---|:---|:---|
| AgentId | 4174097 | 2087049 | 1204958 | 1 | 1043525 | 3130573 | 4174097 |
| Age | 4174097 | 37 | 22 | -1 | 19 | 53 | 104 |
| Gender | 4174097 |  |  |  |  |  |  |
| … Female | 2128562 | 51% |  |  |  |  |  |
| … Male | 2045535 | 49% |  |  |  |  |  |
| RelationshipStatus | 4174097 |  |  |  |  |  |  |
| … GROUP_HOUSEHOLD | 202422 | 5% |  |  |  |  |  |
| … LONE_PARENT | 176081 | 4% |  |  |  |  |  |
| … LONE_PERSON | 366024 | 9% |  |  |  |  |  |
| … MARRIED | 1918556 | 46% |  |  |  |  |  |
| … O15_CHILD | 285971 | 7% |  |  |  |  |  |
| … RELATIVE | 133914 | 3% |  |  |  |  |  |
| … STUDENT | 264158 | 6% |  |  |  |  |  |
| … U15_CHILD | 826971 | 20% |  |  |  |  |  |
| HouseholdId | 4174097 | 1057931 | 432171 | 1 | 797012 | 1370237 | 1837719 |
| PartnerId | 1918556 | 2097046 | 1206587 | 2469 | 1061171 | 3141437 | 4174097 |
| MotherId | 1323492 | 2095777 | 1204385 | 4563 | 1056389 | 3131546 | 4174092 |
| FatherId | 1125454 | 2096707 | 1205114 | 4567 | 1056012 | 3128035 | 4174091 |
| SA2_MAINCODE | 4174097 | 210185213 | 2543207 | 206011105 | 208021180 | 212051322 | 214021385 |
| SA1_7DIGCODE | 4174097 | 2127554 | 10350 | 2110501 | 2118307 | 2135504 | 2146828 |
| SA1_MAINCODE_2016 | 4174097 | 21018521345 | 254320731 | 20601110501 | 20802118008 | 21205132223 | 21402138547 |
| age_cat | 4174097 | 7.9 | 4.4 | 1 | 4 | 11 | 21 |
| is_employed | 4174097 |  |  |  |  |  |  |
| … No | 2209464 | 53% |  |  |  |  |  |
| … Yes | 1964633 | 47% |  |  |  |  |  |
| education | 4174097 |  |  |  |  |  |  |
| … high | 1036035 | 25% |  |  |  |  |  |
| … low | 1148285 | 28% |  |  |  |  |  |
| … medium | 1989777 | 48% |  |  |  |  |  |
| IRSAD | 4174097 | 6.2 | 2.7 | -1 | 4 | 8 | 11 |
| hhSize | 4174097 | 3.1 | 1.5 | 1 | 2 | 4 | 8 |
| hhCar | 4174097 | 1.8 | 1 | 0 | 1 | 2 | 4 |

Summary Statistics

Considering the above, I think the following points are worth
considering as model refinements:

- Represent age in years (e.g. using bracket mid-point) as “Age”. This
  would better model age as a continuous variable, for direct prediction
  using the synthetic population. A caveat to that would be
  consideration of whether age is better modelled as linearly (a more
  parsimonious approach) or non-linearly (which its current treatment as
  a factor variable allows for, but complicates things and doesn’t
  directly translate to the synthetic population variable that is
  continuous age in years).
  - on the other hand, there is an `age_cat` variable, however I think
    the model would have more power if age could be modelled as
    continuous. Having said that it will be good to consult with Belen
    and Qin to get their recommendations and plans for usage.
- Represent sex as a binary indicator ‘female’ having 0 (male) and 1
  (female)
- Simplify employment as a binary variable “is_employed” with values of
  0 (no) and 1 (yes).
- Simplify education as a three level variable education, having values
  ‘low’, ‘medium’, ‘high’. While it might be that this could be
  represented as a pseudo continuous variable (0, 1, 2), I think its
  best to not assume its linear and leave it as a categorical factor
  variable. Break points for low, medium and high may be subjective;
  need to consult what this represent in synthetic population. For now,
  have assumed any tertiary education (Bachelor and higher) is high;
  Year 10, 11, 12, and Certificates higher than III are medium; and
  certificates I/II, or Year 9 or lower is low.
- There is a variable ‘IRSAD’ that might be disadvantage but it ranges
  from -1 to 11, so is not clear what this represents (not simply
  deciles, and not quintiles)
- other considerations:
  - The synthetic population seems to have ‘student’ as a category in
    RelationshipStatus – if that does identify someone as a student,
    then perhaps we can consider ‘is_student’ in the model for mMETs.
  - hhSize is present in synthetic population. It may not be
    conceptually relevant, but is present in household data for NHS, so
    we could consider its appropriateness for the model.
  - hhCar is in synethic population, but there is no data to represent
    this in the NHS data, that I can see.

### Data preparation

Missing data or NA values (e.g 99997 and 99998) are replaced as missing,
while maximum values of walking time variables are truncated at 840
minutes to constrain influence of extreme outliers.

SA1 Index of Relative Socio-economic Disadvantage (IRSD; `SA1SF2DN`) is
rescaled to use quintiles rather than deciles, for consistency with the
synthetic population, with ‘1’ being most deprived and ‘5’ being least
deprived.

Two age variables will be created for consideration, first as a factor
variable and second in years, using the first age bracket year.

To match the synthetic population data for Melbourne a binary indicator
‘is_employed’ will be created, along with a possible supplementary
indicator ‘is_student’ that could be derived for the synthetic
population.

Education will be summarised using categories of low, medium and high
for direct comparison with the synthetic population.

Marginal metabolic equivalent house per week (mMET hours/week; `mmet`)
are calculated as the sum of hours spent walking for recreation, walking
for transport, doing moderate exercise and doing vigorous exercise, with
each respectively multiplied by the metabolic equivalent of these tasks
(METs).

When deriving factor variables I have set ordered to False, as the
alternative setting needlessly over-complicates the modelling
(i.e. derives polynomial functions) and limits our capacity to transfer
predictions for the synthetic population (see
https://stackoverflow.com/questions/57297771/interpretation-of-l-q-c-4-for-logistic-regression).

Remember that SA1 IRSD ranges from 1 (most deprived) to 5 (least
deprived).

``` r
MMET_MOD <- 3.5 # As in meta analysis GArcia et al. 
MMET_VIG <- 7 # As in meta analysis GArcia et al. 
MMET_CYCLING <- 5.8 # From ithimr (check)
MMET_WALKING <- 2.5 # From ithimr (check)

pa_data <- nhs  %>%
    mutate_all(~ ifelse( . %in% c(99997, 99998), NA, .)) %>%
    mutate(
        walk_recreation_min = case_when(
                walk_recreation_min > 840 ~ 840, 
                TRUE ~  walk_recreation_min # handles unexpected values using default
            ),
        walk_transport_min = case_when(
                walk_transport_min > 840 ~ 840, 
                TRUE ~ walk_transport_min
            ),
        mod_excercise_min = case_when(
                mod_excercise_min > 840 ~ 840, 
                TRUE ~ mod_excercise_min
            ),
        vig_excercise_min = case_when(
                vig_excercise_min > 840 ~ 840, 
                TRUE ~ vig_excercise_min
            )
    ) %>%
    mutate(
        irsd_sa1 = case_when(
            SA1SF2DN %in% c(1, 2) ~ 1,
            SA1SF2DN %in% c(3, 4) ~ 2,
            SA1SF2DN %in% c(5, 6) ~ 3,
            SA1SF2DN %in% c(7, 8) ~ 4,
            SA1SF2DN %in% c(9, 10) ~ 5
        )
    ) %>% 
  mutate(
    age_years = case_when(
        AGEB == 5 ~ 18,
        AGEB == 6 ~ 20,
        AGEB == 7 ~ 25,
        AGEB == 8 ~ 30,
        AGEB == 9 ~ 35,
        AGEB == 10 ~ 40,
        AGEB == 11 ~ 45,
        AGEB == 12 ~ 50,
        AGEB == 13 ~ 55,
        AGEB == 14 ~ 60,
        AGEB == 15 ~ 65,
        AGEB == 16 ~ 70,
        AGEB == 17 ~ 75,
        AGEB == 18 ~ 80,
        AGEB == 19 ~ 85,
        TRUE ~ NA_real_  # Handle unexpected values
    ),
        AGEB = factor(
            AGEB, 
            levels = 5:19, 
            labels = c("18-19","20-24", "25-29", "30-34", "35-39", 
                        "40-44", "45-49", "50-54", "55-59", 
                        "60-64", "65-69", "70-74", "75-79", 
                        "80-84", "≥85"),
            ordered = FALSE
        ),
    female=ifelse(SEX==2,1,0),
    is_employed=case_when(
            LFSBC==1 ~ 1,
            LFSBC %in% c(0,2,3) ~ 0
        ),
    is_student=ifelse(STDYFTPT %in% c(1,2),1,0),
    education = case_when(
        HIGHLVBC %in% c(1,2) ~ 2,
        HIGHLVBC %in% c(3,7) ~ 1,
        HIGHLVBC %in% c(8,12) ~ 0,
        TRUE ~ NA_real_  # Handle unexpected values
        ),
    education = factor(
        education,
        levels=0:2,
        labels=c('low','medium','high'),
        ordered=FALSE
    )
    )  %>%
    mutate(
        mmet_hours_per_week=walk_recreation_min/60* MMET_WALKING +
             walk_transport_min/60 * MMET_WALKING + 
             mod_excercise_min/60 * MMET_MOD + 
             vig_excercise_min/60 * MMET_VIG
    )  %>%
    mutate(
        mmet_hours_per_week_zero = ifelse(mmet_hours_per_week == 0, 1, 0)
    ) 

pa_data %>% st(out='kable')
```

| Variable                 | N     | Mean  | Std. Dev. | Min | Pctl. 25 | Pctl. 75 | Max |
|:-------------------------|:------|:------|:----------|:----|:---------|:---------|:----|
| STATE16                  | 16376 | 3.6   | 2.2       | 1   | 2        | 5        | 8   |
| NUMPERBC                 | 16376 | 2.4   | 1.3       | 1   | 1        | 3        | 6   |
| SA1SF2DN                 | 16376 | 5.4   | 2.8       | 1   | 3        | 8        | 10  |
| ABSPID                   | 16370 | 1     | 0         | 1   | 1        | 1        | 1   |
| AGEB                     | 16370 |       |           |     |          |          |     |
| … 18-19                  | 321   | 2%    |           |     |          |          |     |
| … 20-24                  | 853   | 5%    |           |     |          |          |     |
| … 25-29                  | 1145  | 7%    |           |     |          |          |     |
| … 30-34                  | 1431  | 9%    |           |     |          |          |     |
| … 35-39                  | 1467  | 9%    |           |     |          |          |     |
| … 40-44                  | 1377  | 8%    |           |     |          |          |     |
| … 45-49                  | 1452  | 9%    |           |     |          |          |     |
| … 50-54                  | 1348  | 8%    |           |     |          |          |     |
| … 55-59                  | 1403  | 9%    |           |     |          |          |     |
| … 60-64                  | 1386  | 8%    |           |     |          |          |     |
| … 65-69                  | 1293  | 8%    |           |     |          |          |     |
| … 70-74                  | 1163  | 7%    |           |     |          |          |     |
| … 75-79                  | 760   | 5%    |           |     |          |          |     |
| … 80-84                  | 546   | 3%    |           |     |          |          |     |
| … ≥85                    | 425   | 3%    |           |     |          |          |     |
| SEX                      | 16370 | 1.5   | 0.5       | 1   | 1        | 2        | 2   |
| LFSBC                    | 16370 | 1.8   | 0.96      | 1   | 1        | 3        | 3   |
| STDYFTPT                 | 16370 | 2.9   | 0.45      | 1   | 3        | 3        | 3   |
| HYSCHCBC                 | 16370 | 2     | 1.3       | 1   | 1        | 3        | 5   |
| HIGHLVBC                 | 16370 | 4.5   | 2.6       | 1   | 2        | 6        | 12  |
| walk_recreation_min      | 16360 | 89    | 146       | 0   | 0        | 120      | 840 |
| walk_transport_min       | 16325 | 69    | 134       | 0   | 0        | 80       | 840 |
| mod_excercise_min        | 16367 | 47    | 112       | 0   | 0        | 40       | 840 |
| vig_excercise_min        | 16370 | 31    | 88        | 0   | 0        | 0        | 840 |
| irsd_sa1                 | 16376 | 3     | 1.4       | 1   | 2        | 4        | 5   |
| age_years                | 16370 | 49    | 18        | 18  | 35       | 65       | 85  |
| female                   | 16370 | 0.54  | 0.5       | 0   | 0        | 1        | 1   |
| is_employed              | 16370 | 0.61  | 0.49      | 0   | 0        | 1        | 1   |
| is_student               | 16376 | 0.095 | 0.29      | 0   | 0        | 0        | 1   |
| education                | 8165  |       |           |     |          |          |     |
| … low                    | 127   | 2%    |           |     |          |          |     |
| … medium                 | 3518  | 43%   |           |     |          |          |     |
| … high                   | 4520  | 55%   |           |     |          |          |     |
| mmet_hours_per_week      | 16318 | 13    | 17        | 0   | 1.2      | 18       | 207 |
| mmet_hours_per_week_zero | 16318 | 0.21  | 0.41      | 0   | 0        | 0        | 1   |

Summary Statistics

### Exploratory data analysis

#### Age

mMET hours per week by age backet – there may be some non-linearity
(e.g. 18-19 year olds median METS are lower, but perhaps not
meaningfully so), but broadly, younger people have higher mMETs:

``` r
ggplot(pa_data,aes(x=AGEB, y=mmet_hours_per_week)) + geom_boxplot() + coord_flip()
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/visualise-age-distribution-1.png)

``` r
qplot(x=pa_data$age_years,y=pa_data$mmet_hours_per_week, geom='smooth', span =0.5)
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/visualise-age-distribution-2.png)

Looking at the qplot, to me it seems reasonable on grounds of parsimony
to model the relationship between age and mMET hours/week as a linear
function. There is a drop off after 70, however our data is relatively
sparse beyond there and our last category does capture persons older
than 85. Its easier to interpret the story from the qplot than boxplots
too.

#### Correlations

For now I have included both education variables in the below
exploratory data analysis, although most likely a derived combined
variable for education will be used, pending discussion with the team.
The State variable is also included to get a sense of how variables
differ across Australian states; Victorian-specific results could be
expected to be mostly drawn from Melbourne, and this could be explored
in a sensitivity analysis.

Many of our variables are factor variables for which direct numeric
correlations could be misleading (i.e. they aren’t necessarily ordinal
or linear, e.g. labour force status, or ‘NA’ values in educational
attainment).

``` r
cor=as.data.frame(lapply(pa_data[, c("irsd_sa1", "age_years", "female", "is_employed", "is_student", "education", "NUMPERBC", "mmet_hours_per_week")], as.numeric))
cor=na.omit(cor)
correlation_matrix <- cor(cor) %>% as.data.frame()
# corrplot(correlation_matrix %>% as.double(), method = "number",order = "FPC",type="lower") 
correlation_matrix[order(correlation_matrix$mmet_hours_per_week),] %>% round(2)
##                     irsd_sa1 age_years female is_employed is_student education
## age_years              -0.02      1.00   0.02       -0.52      -0.26     -0.24
## female                 -0.03      0.02   1.00       -0.10       0.01     -0.04
## NUMPERBC                0.12     -0.38  -0.03        0.23       0.06      0.12
## is_student              0.00     -0.26   0.01        0.08       1.00      0.10
## is_employed             0.13     -0.52  -0.10        1.00       0.08      0.26
## irsd_sa1                1.00     -0.02  -0.03        0.13       0.00      0.25
## education               0.25     -0.24  -0.04        0.26       0.10      1.00
## mmet_hours_per_week     0.13     -0.12  -0.11        0.12       0.03      0.16
##                     NUMPERBC mmet_hours_per_week
## age_years              -0.38               -0.12
## female                 -0.03               -0.11
## NUMPERBC                1.00                0.01
## is_student              0.06                0.03
## is_employed             0.23                0.12
## irsd_sa1                0.12                0.13
## education               0.12                0.16
## mmet_hours_per_week     0.01                1.00
```

None of the variables in themselves are strongly associated with mMET
hours/week. As indicated above, age in years has a negative association.
Being female is similarly associated with lower mMET hours/week. Number
of persons in household and being a student were not associated with
mMETs and need not be included in the model. Employment, lower
socio-economic deprivation, and higher degree of education had weak
positive associations with mMET hours/week.

#### Clustering

Persons are theoretically clustered within households. If there is more
than one person within each household, as this data structure implies,
this clustering should be accounted for in the model. The following
checks the maximum number of persons within households, and confirms
that only one person is associated with each household and so clustering
within households will not be required in the model.

``` r
pa_data %>%
  group_by(ABSHIDB) %>%
  summarise(num_persons = n()) %>%
  summarise(max_persons = max(num_persons))
## # A tibble: 1 × 1
##   max_persons
##         <int>
## 1           1
```

#### Set up data for modelling

Select relevant variables and only retain records with full data.

``` r
pa_data =pa_data%>%
  select(ABSPID, age_years, female, is_employed, education, irsd_sa1, mmet_hours_per_week, mmet_hours_per_week_zero)%>%
  na.omit()

pa_data %>% st(out='kable')
```

| Variable                 | N    | Mean | Std. Dev. | Min | Pctl. 25 | Pctl. 75 | Max |
|:-------------------------|:-----|:-----|:----------|:----|:---------|:---------|:----|
| ABSPID                   | 8139 | 1    | 0         | 1   | 1        | 1        | 1   |
| age_years                | 8139 | 48   | 17        | 18  | 35       | 60       | 85  |
| female                   | 8139 | 0.59 | 0.49      | 0   | 0        | 1        | 1   |
| is_employed              | 8139 | 0.66 | 0.47      | 0   | 0        | 1        | 1   |
| education                | 8139 |      |           |     |          |          |     |
| … low                    | 126  | 2%   |           |     |          |          |     |
| … medium                 | 3503 | 43%  |           |     |          |          |     |
| … high                   | 4510 | 55%  |           |     |          |          |     |
| irsd_sa1                 | 8139 | 3.2  | 1.4       | 1   | 2        | 4        | 5   |
| mmet_hours_per_week      | 8139 | 15   | 18        | 0   | 2.5      | 20       | 168 |
| mmet_hours_per_week_zero | 8139 | 0.16 | 0.37      | 0   | 0        | 0        | 1   |

Summary Statistics

Copy a subset of data for persons who record at least some mMET
hours/week

``` r
pa_data_over0=pa_data[pa_data$mmet_hours_per_week>0,]
pa_data_over0 %>% st(out='kable')
```

| Variable                 | N    | Mean | Std. Dev. | Min   | Pctl. 25 | Pctl. 75 | Max |
|:-------------------------|:-----|:-----|:----------|:------|:---------|:---------|:----|
| ABSPID                   | 6838 | 1    | 0         | 1     | 1        | 1        | 1   |
| age_years                | 6838 | 47   | 16        | 18    | 35       | 60       | 85  |
| female                   | 6838 | 0.58 | 0.49      | 0     | 0        | 1        | 1   |
| is_employed              | 6838 | 0.68 | 0.46      | 0     | 0        | 1        | 1   |
| education                | 6838 |      |           |       |          |          |     |
| … low                    | 80   | 1%   |           |       |          |          |     |
| … medium                 | 2676 | 39%  |           |       |          |          |     |
| … high                   | 4082 | 60%  |           |       |          |          |     |
| irsd_sa1                 | 6838 | 3.3  | 1.4       | 1     | 2        | 4        | 5   |
| mmet_hours_per_week      | 6838 | 17   | 18        | 0.058 | 5        | 24       | 168 |
| mmet_hours_per_week_zero | 6838 | 0    | 0         | 0     | 0        | 0        | 0   |

Summary Statistics

### Modelling

The modelling approach (and earlier data preparation) draws on code from
the Manchester physical activity modelling R code file
`otherSportPA_hurdle_v3.R` authored by Qin Zhang, Belen Zapata-Diomedi
and Marina Berdokhova.

#### Modelling mMETS hours/week

``` r
m.total_mMETs <- list()

m.total_mMETs$linear <- lm(
    mmet_hours_per_week ~ female+age_years+is_employed+education+irsd_sa1,
    data = pa_data_over0)
summary(m.total_mMETs$linear)
## 
## Call:
## lm(formula = mmet_hours_per_week ~ female + age_years + is_employed + 
##     education + irsd_sa1, data = pa_data_over0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.221 -11.712  -5.642   6.319 151.079 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     16.15068    2.28834   7.058 1.86e-12 ***
## female          -4.23488    0.44093  -9.604  < 2e-16 ***
## age_years       -0.05182    0.01566  -3.310 0.000938 ***
## is_employed      1.10728    0.55212   2.005 0.044950 *  
## educationmedium  0.69181    2.04164   0.339 0.734731    
## educationhigh    3.29679    2.05315   1.606 0.108380    
## irsd_sa1         0.92744    0.16381   5.662 1.56e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.87 on 6831 degrees of freedom
## Multiple R-squared:  0.03433,    Adjusted R-squared:  0.03348 
## F-statistic: 40.47 on 6 and 6831 DF,  p-value: < 2.2e-16
```

In this model, most predicted results were slightly under-estimated
compared with the observed values (median -5.6 mMET hours/week; IQR
-11.7 to 6.3).

#### Modelling zero mMETS hours/week

``` r
m.total_mMETs$zeroModel <- glm(
    mmet_hours_per_week_zero ~ female+age_years+is_employed+education+irsd_sa1,
    family = "binomial",
    data = pa_data
)

summary(m.total_mMETs$zeroModel)
## 
## Call:
## glm(formula = mmet_hours_per_week_zero ~ female + age_years + 
##     is_employed + education + irsd_sa1, family = "binomial", 
##     data = pa_data)
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.310901   0.249581  -5.252 1.50e-07 ***
## female           0.030371   0.064484   0.471    0.638    
## age_years        0.019088   0.002254   8.470  < 2e-16 ***
## is_employed     -0.024421   0.077091  -0.317    0.751    
## educationmedium -0.285015   0.193820  -1.471    0.141    
## educationhigh   -1.096310   0.199793  -5.487 4.08e-08 ***
## irsd_sa1        -0.220375   0.023529  -9.366  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7152.8  on 8138  degrees of freedom
## Residual deviance: 6636.9  on 8132  degrees of freedom
## AIC: 6650.9
## 
## Number of Fisher Scoring iterations: 5
```

Considering the non-normal nature of mMET hours/week, which is a
positively skewed rate variable (a non-negative count of hours per
week), it is worth considering whether a Poisson model might be more
appropriate and provide a better fit.

``` r
hist(pa_data$mmet_hours_per_week) 
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/consider_model_for_rate_data-1.png)

``` r
hist(log(pa_data$mmet_hours_per_week))
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/consider_model_for_rate_data-2.png)

``` r
summary(pa_data$mmet_hours_per_week)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.500   8.167  14.517  20.500 168.000
mean_mmet <- mean(pa_data$mmet_hours_per_week)
var_mmet <- var(pa_data$mmet_hours_per_week)

print(paste("Mean:", mean_mmet))
## [1] "Mean: 14.5173096613016"
print(paste("Variance:", var_mmet))
## [1] "Variance: 317.808067651961"
if (var_mmet > 2 * mean_mmet) {
    print("var_mmet > 2 * mean_mmet (Consider using a Negative Binomial or Zero-Inflated Negative Binomial model due to overdispersion)")
}
## [1] "var_mmet > 2 * mean_mmet (Consider using a Negative Binomial or Zero-Inflated Negative Binomial model due to overdispersion)"
```

The model may appear appoximately normally distributed on a log scale,
it also has considerably over-dispersion so a negative binomial model
may be appropriate, potentially accounting for the large number of
zeros.

``` r
library(MASS)
library(pscl)

m.total_mMETs$neg_binom <- glm.nb(
    mmet_hours_per_week ~ female + age_years + is_employed + education + irsd_sa1,
    data = pa_data
)

summary(m.total_mMETs$neg_binom)
## 
## Call:
## glm.nb(formula = mmet_hours_per_week ~ female + age_years + is_employed + 
##     education + irsd_sa1, data = pa_data, init.theta = 0.6573741891, 
##     link = log)
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      2.391444   0.135944  17.591  < 2e-16 ***
## female          -0.240994   0.028600  -8.426  < 2e-16 ***
## age_years       -0.006630   0.001002  -6.615 3.71e-11 ***
## is_employed      0.100407   0.035505   2.828  0.00468 ** 
## educationmedium  0.184990   0.117561   1.574  0.11559    
## educationhigh    0.466551   0.118683   3.931 8.46e-05 ***
## irsd_sa1         0.092533   0.010592   8.736  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for Negative Binomial(0.6574) family taken to be 1)
## 
##     Null deviance: 10129.2  on 8138  degrees of freedom
## Residual deviance:  9668.8  on 8132  degrees of freedom
## AIC: 59030
## 
## Number of Fisher Scoring iterations: 1
## 
## 
##               Theta:  0.6574 
##           Std. Err.:  0.0109 
## 
##  2 x log-likelihood:  -59013.8600

# Zero-Inflated Negative Binomial model
pa_data$mmet_hours_per_week_round <- round(pa_data$mmet_hours_per_week)

m.total_mMETs$zinb <- zeroinfl(
    mmet_hours_per_week_round ~ female + age_years + is_employed + education + irsd_sa1 | female + age_years + is_employed + education + irsd_sa1,
    data = pa_data,
    dist = "negbin"
)

# Summary of the Zero-Inflated Negative Binomial model
summary(m.total_mMETs$zinb)
## 
## Call:
## zeroinfl(formula = mmet_hours_per_week_round ~ female + age_years + is_employed + 
##     education + irsd_sa1 | female + age_years + is_employed + education + 
##     irsd_sa1, data = pa_data, dist = "negbin")
## 
## Pearson residuals:
##     Min      1Q  Median      3Q     Max 
## -0.9235 -0.6954 -0.3613  0.3358  9.0739 
## 
## Count model coefficients (negbin with log link):
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      2.6466925  0.1389950  19.042  < 2e-16 ***
## female          -0.2562271  0.0263709  -9.716  < 2e-16 ***
## age_years       -0.0034920  0.0009545  -3.659 0.000254 ***
## is_employed      0.0954131  0.0326992   2.918 0.003524 ** 
## educationmedium  0.0611648  0.1254226   0.488 0.625784    
## educationhigh    0.2282646  0.1260724   1.811 0.070205 .  
## irsd_sa1         0.0630390  0.0098692   6.387 1.69e-10 ***
## Log(theta)      -0.0879755  0.0237487  -3.704 0.000212 ***
## 
## Zero-inflation model coefficients (binomial with logit link):
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.963402   0.373560  -5.256 1.47e-07 ***
## female          -0.161139   0.101562  -1.587    0.113    
## age_years        0.028999   0.004053   7.156 8.33e-13 ***
## is_employed      0.060906   0.125485   0.485    0.627    
## educationmedium -0.346316   0.243478  -1.422    0.155    
## educationhigh   -1.589976   0.274404  -5.794 6.86e-09 ***
## irsd_sa1        -0.276149   0.037765  -7.312 2.63e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Theta = 0.9158 
## Number of iterations in BFGS optimization: 22 
## Log-likelihood: -2.923e+04 on 15 Df
```

``` r
# Compare AIC values
aic_values <- data.frame(
    Model = c("Linear", "Binomial", "neg_binom", "zinb"),
    AIC = c(
        AIC(m.total_mMETs$linear), 
        AIC(m.total_mMETs$zeroModel), 
        AIC(m.total_mMETs$neg_binom),
        AIC(m.total_mMETs$zinb)
    )
)
print(aic_values)
##       Model       AIC
## 1    Linear 58847.097
## 2  Binomial  6650.869
## 3 neg_binom 59029.860
## 4      zinb 58482.976
```

The zero-inflated negative binomial model has a better fit of mMET
hours/week to the observed values than the linear model (median Pearson
residual of -0.4, IQR -0.7, 0.3, range -0.9 to 9), with a lower AIC
reflecting the better fit.

### Predictions

``` r
MonteCarlo <- function(model, data,facetVar = NA) {
  probability.matrix <- as.vector(predict(model,data,type = "response"))
  MC.prediction <- rep(NA,nrow(data))
  for(n in c(1:nrow(data))) {
    MC.prediction[n] <- runif(1)<=probability.matrix[n]
  }
  data=data%>%mutate(zeroPrediction=MC.prediction)
  return(data)
}
```

``` r
predicted_mmet <- predict(m.total_mMETs$zinb, type = "response")
predicted_binary <- ifelse(predicted_mmet > 0, 1, 0)
actual_binary <- ifelse(pa_data$mmet_hours_per_week > 0, 1, 0)
confusion_matrix <- table(Predicted = predicted_binary, Actual = actual_binary)
print(confusion_matrix)
##          Actual
## Predicted    0    1
##         1 1301 6838

results <- data.frame(
    predicted_mmet = predicted_mmet,
    actual_binary = pa_data$actual_binary <- ifelse(pa_data$mmet_hours_per_week > 0, "Greater than 0", "0")
)
# Generate box plots
ggplot(results, aes(x = actual_binary, y = predicted_mmet)) +
    geom_boxplot() +
    labs(
        title = "Predicted mMET Hours/Week by Actual mMET Hours/Week > 0 Status",
        x = "Actual mMET Hours/Week > 0 Status",
        y = "Predicted mMET Hours/Week"
    ) +
    theme_minimal()
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/predict-total-mMETs-nhs-1.png)

While the model overestimates mMET hours for those who were estimated to
have accrued none, those with none predicted were significantly lower
which may be a more realistic estimate. (i.e. in the real world, it may
be unlikely that zero physical activity is conducted, some physical
activity does occur incidentally as a result of being alive, so the
over-estimation of the ‘zero’ observed mMET hours/week may not be such a
problem, necessarily).

But … the original zero model still did by far the best at predicting
zeros, perhaps using the zero model with negative binomial regression
for the over zeros would be the best model.

``` r
m.total_mMETs$neg_binom_over0 <- glm.nb(
    mmet_hours_per_week ~ female + age_years + is_employed + education + irsd_sa1,
    data = pa_data_over0
)

summary(m.total_mMETs$neg_binom_over0)
## 
## Call:
## glm.nb(formula = mmet_hours_per_week ~ female + age_years + is_employed + 
##     education + irsd_sa1, data = pa_data_over0, init.theta = 1.196222182, 
##     link = log)
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      2.7190470  0.1223949  22.215  < 2e-16 ***
## female          -0.2385074  0.0233173 -10.229  < 2e-16 ***
## age_years       -0.0033029  0.0008297  -3.981 6.86e-05 ***
## is_employed      0.0849283  0.0292899   2.900  0.00374 ** 
## educationmedium  0.0718325  0.1095197   0.656  0.51190    
## educationhigh    0.2274413  0.1100983   2.066  0.03885 *  
## irsd_sa1         0.0558365  0.0086826   6.431 1.27e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for Negative Binomial(1.1962) family taken to be 1)
## 
##     Null deviance: 7904.0  on 6837  degrees of freedom
## Residual deviance: 7599.8  on 6831  degrees of freedom
## AIC: 52686
## 
## Number of Fisher Scoring iterations: 1
## 
## 
##               Theta:  1.1962 
##           Std. Err.:  0.0205 
## 
##  2 x log-likelihood:  -52670.2490

# Compare AIC values
aic_values <- data.frame(
    Model = c("Linear", "Binomial", "neg_binom", "zinb","neg_binom_over0"),
    AIC = c(
        AIC(m.total_mMETs$linear), 
        AIC(m.total_mMETs$zeroModel), 
        AIC(m.total_mMETs$neg_binom),
        AIC(m.total_mMETs$zinb),
        AIC(m.total_mMETs$neg_binom_over0)
    )
)
print(aic_values)
##             Model       AIC
## 1          Linear 58847.097
## 2        Binomial  6650.869
## 3       neg_binom 59029.860
## 4            zinb 58482.976
## 5 neg_binom_over0 52686.249
```

The combination of negative binomial model with the zero model appears
to outperform the linear model.

``` r
# Make predictions using the zero model
predicted_probabilities <- predict(m.total_mMETs$zeroModel, type = "response")

# Convert predicted probabilities to binary outcomes using a threshold of 0.5
predicted_binary <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual binary outcomes
actual_binary <- pa_data$mmet_hours_per_week_zero

# Generate the confusion matrix
confusion_matrix <- table(Predicted = predicted_binary, Actual = actual_binary)
print(confusion_matrix)
##          Actual
## Predicted    0    1
##         0 6833 1299
##         1    5    2
```

Review the combined predictions

``` r
nonzero_predictions <- predict(
    m.total_mMETs$neg_binom_over0, 
    type = "response", 
    newdata = pa_data)
combined_predictions <- ifelse(predicted_probabilities > 0.5, 0, nonzero_predictions)

# Evaluate the model
actual_values <- pa_data$mmet_hours_per_week
results <- data.frame(
    actual_values = actual_values,
    combined_predictions = combined_predictions
)
ggplot(results, aes(x = actual_values, y = combined_predictions)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
        title = "Scatter Plot of Actual vs. Predicted mMET Hours/Week",
        x = "Actual mMET Hours/Week",
        y = "Predicted mMET Hours/Week"
    ) +
    theme_minimal()
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/review_combined_predictions-1.png)

Save the model outputs for usage later

``` r
# Get today's date
today_date <- format(Sys.Date(), "%d%m%Y")

# Save the RDS file with today's date in the filename
saveRDS(m.total_mMETs, paste0("model_total_mMETs_", today_date, ".rds"))
```

#### Predicting mMET hours/week for synthetic population

Now we need to get our synthetic population data into the equivalent
format to our model, e.g. by converting string categorical variables to
binary indicators and adapting the SEIFA IRSD to quintiles. Then we can
use our predictions and apply them to the synthetic population.

``` r
# Perform the transformations
pp[, `:=`(
    irsd_sa1 = fifelse(IRSAD %in% c(1, 2), 1,
                fifelse(IRSAD %in% c(3, 4), 2,
                fifelse(IRSAD %in% c(5, 6), 3,
                fifelse(IRSAD %in% c(7, 8), 4,
                fifelse(IRSAD %in% c(9, 10), 5, NA_real_))))),
    age_years = Age,
    female = fifelse(Gender == "Female", 1, 0),
    is_employed = fifelse(is_employed == "Yes", 1, 0),
    education = fifelse(education == 'high', 2,
                fifelse(education == 'medium', 1,
                fifelse(education == 'low', 0, NA_real_)))
)]

# Convert education to a factor
pp[, education := factor(education, levels = 0:2, labels = c('low', 'medium', 'high'), ordered = FALSE)]

# Select the relevant columns
data <- pp[, .(AgentId, age_years, female, is_employed, education, irsd_sa1)]
  

data %>% st(out='kable')
```

| Variable    | N       | Mean    | Std. Dev. | Min | Pctl. 25 | Pctl. 75 | Max     |
|:------------|:--------|:--------|:----------|:----|:---------|:---------|:--------|
| AgentId     | 4174097 | 2087049 | 1204958   | 1   | 1043525  | 3130573  | 4174097 |
| age_years   | 4174097 | 37      | 22        | -1  | 19       | 53       | 104     |
| female      | 4174097 | 0.51    | 0.5       | 0   | 0        | 1        | 1       |
| is_employed | 4174097 | 0       | 0         | 0   | 0        | 0        | 0       |
| education   | 4174097 |         |           |     |          |          |         |
| … low       | 1148285 | 28%     |           |     |          |          |         |
| … medium    | 1989777 | 48%     |           |     |          |          |         |
| … high      | 1036035 | 25%     |           |     |          |          |         |
| irsd_sa1    | 4167715 | 3.3     | 1.3       | 1   | 2        | 4        | 5       |

Summary Statistics

#### Prediction of mMET hours/week for synethic population

``` r
prediction=MonteCarlo(m.total_mMETs$zeroModel,data)
table(prediction$zeroPrediction)
## 
##   FALSE    TRUE 
## 3478671  689044
```

#### Join estimates back onto synthetic population

``` r
nonzeroPP <- prediction %>% filter(!zeroPrediction)

# the non-zero mMET hours/week negative binomial regression output is on a log scale 
# and must be exponentiated for the correct units
nonzeroPP_predict <- nonzeroPP %>% mutate(
    predicted_log_mmet_hours_per_week = predict(m.total_mMETs$neg_binom_over0, nonzeroPP),
    mmetHr_total = exp(predicted_log_mmet_hours_per_week)
)

pp <- pp %>% 
      left_join(
        nonzeroPP_predict %>% dplyr::select('AgentId','mmetHr_total')
      )
pp <- pp %>% mutate(
    mmetHr_total = ifelse(is.na(mmetHr_total), 0, mmetHr_total)
)
summary(pp$mmetHr_total)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   12.63   15.27   13.46   17.62   23.95
ggplot(pp)+stat_ecdf(aes(x=mmetHr_total))
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/synpop-prediction-outputs-1.png)

``` r
fwrite(pp,"pp_health_2021_withTotalMMets.csv")
```

``` r
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
 
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(pp$mmetHr_total, horizontal=TRUE , ylim=c(0,30), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(pp$mmetHr_total, breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Predicted mMET hours/week", xlim=c(0,30))
```

![](jibe-melbourne-physical-activity_files/figure-commonmark/boxplot-mmetHr_total-1.png)
