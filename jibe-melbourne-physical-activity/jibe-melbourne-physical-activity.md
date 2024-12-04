# Modelling physical activity in Melbourne


This Quarto markdown document is intended to contain code to predict
physical activity for sport and recreation excluding walking and
cycling, given explanatory variables (age, sex, SES, education, has car,
etc) that align with the JIBE synthetic population data. This will be
conducted to predict physical activity for residents of Melbourne,
Australia, using the Australian Bureau of Statistics Australian National
Health Survey data (2017-18).

## Dependencies

Analysis was conducted using R 4.4.1 using a Quarto markdown document
(Quarto 1.5.55) in Positron IDE (2024.10.0), with renv 1.0.11 for
package management.

The following packages have been installed using renv:

    renv::install(c('dplyr','haven','rmarkdown'))

In principle, the R environment for this notebook should be able to be
restored by running

    renv::restore()

Load libaries

``` r
library(dplyr)
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
| GCCSA16 | Greater Capital City Statistical Area (ASGS 2016) | 1 == capital city |
| SOS16 | Sections of State (ASGS 2016) | 0 == Major urban, 1 == Other urban (filter in c(0,1)) |
| STATE16 | State or Territory (ASGS 2016) | 2 == Victoria |
| DWSTQ02 | Dwelling structure | in case of use later |
| HSTENURE | Tenure type of households | in case of use later |
| SA1SF2DN | SEIFA - Index of Relative Socio-economic Disadvantage - 2016 - SA1 - Deciles - National |  |
| GCDISTME | Median commuting distance (kms) |  |

Note that there are also household geospatial indicators for
supermarkets, amenities, fast food an public open space (any, and larger
than 1 hecatare) within buffer distances of 400, 1000 and 1500 metres.

Person variables of interest (see data dictionaries for detailed codes)
include:

| Variable | Description | Comment |
|----|----|----|
| ABSPID | Person identifier | Need to verify that this is unique within households |
| ABSHIDB | Household identifier | Link with households |
| AGE99 | Age of person |  |
| SEX | Sex of person | 1==Male, 2==Female |
| HIGHLVL | Level of highest educational attainment | 0==NA, 1==Postgraduate, … 13==Never attended school |
| EMPSTAT | Labour force full-time/part-time status | 0==NA, 1==Employed full time … 6 Not in labour force |
| … | other variables | more to add |

``` r
data_dir <- '/Users/E33390/Library/CloudStorage/OneDrive-RMITUniversity/projects/abs/microdata/NHS2017-18_CSV/NHS2017-18_CSV/'
data <- list(
    households = read.csv(paste0(data_dir,'NHS17HHB.csv')),
    persons = read.csv(paste0(data_dir,'NHS17SPB.csv'))
)
```

``` r
nhs <- left_join(
            data$households,
                # %>% as.data.frame(), 
                # select(
                #     c( 
                #         "GCCSA16", 
                #         "SOS16",
                #         "STATE16", 
                #         "DWSTQ02",
                #         "HSTENURE",
                #         "SA1SF2DN", 
                #         "GCDISTME"
                #     )
                # ) %>% 
                # filter(
                #     STATE16==2 &
                #     GCCSA16==1 &
                #     SOS16 %in% c(0,1)
                # ), # for Victoria GCCSA (i.e. Melbourne) urban regions 
            data$persons %>% 
                select(
                    c(
                        "ABSPID",
                        "ABSHIDB",
                        "AGEB",
                        "SEX",
                        "HIGHLVBC",
                        "EMPTYPE"
                    )
                ), 
        by = "ABSHIDB"
        ) 

nhs %>% summary()
##      LEVEL1    ABSHIDB             ABSPID.x     ABSDID      ABSTID      ABSCID 
##  Min.   :1   Length:21315       Min.   :0   Min.   :0   Min.   :0   Min.   :0  
##  1st Qu.:1   Class :character   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
##  Median :1   Mode  :character   Median :0   Median :0   Median :0   Median :0  
##  Mean   :1                      Mean   :0   Mean   :0   Mean   :0   Mean   :0  
##  3rd Qu.:1                      3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
##  Max.   :1                      Max.   :0   Max.   :0   Max.   :0   Max.   :0  
##      ABSMID      ABSLID     HHADULBC        HHCO14BC         NUMKHHBC    
##  Min.   :0   Min.   :0   Min.   :1.000   Min.   :0.0000   Min.   :0.000  
##  1st Qu.:0   1st Qu.:0   1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0.000  
##  Median :0   Median :0   Median :2.000   Median :0.0000   Median :0.000  
##  Mean   :0   Mean   :0   Mean   :1.846   Mean   :0.7115   Mean   :0.834  
##  3rd Qu.:0   3rd Qu.:0   3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:2.000  
##  Max.   :0   Max.   :0   Max.   :3.000   Max.   :3.0000   Max.   :3.000  
##     NUMPERBC        HH15OVBC        STHHTYC         SMKHSQ4      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :3.000   Median :2.000   Median :1.000   Median :0.0000  
##  Mean   :2.751   Mean   :1.927   Mean   :1.778   Mean   :0.9095  
##  3rd Qu.:4.000   3rd Qu.:2.000   3rd Qu.:3.000   3rd Qu.:0.0000  
##  Max.   :6.000   Max.   :3.000   Max.   :5.000   Max.   :8.0000  
##     DSMKHHRB          DSMKHH         INCDECU1        TENUREC     
##  Min.   :0.0000   Min.   :1.000   Min.   : 1.00   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.:5.000   1st Qu.: 4.00   1st Qu.:1.000  
##  Median :0.0000   Median :5.000   Median : 6.00   Median :2.000  
##  Mean   :0.3713   Mean   :4.199   Mean   :18.02   Mean   :2.142  
##  3rd Qu.:0.0000   3rd Qu.:5.000   3rd Qu.: 9.00   3rd Qu.:3.000  
##  Max.   :8.0000   Max.   :8.000   Max.   :99.00   Max.   :5.000  
##     LNDLORDB         DWLSTRBC        NUMRMSBC        LANDLINE    
##  Min.   :0.0000   Min.   :1.000   Min.   :0.000   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:3.000   1st Qu.:1.000  
##  Median :0.0000   Median :1.000   Median :3.000   Median :3.000  
##  Mean   :0.3836   Mean   :1.515   Mean   :3.282   Mean   :2.756  
##  3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:4.000   3rd Qu.:4.000  
##  Max.   :3.0000   Max.   :8.000   Max.   :8.000   Max.   :8.000  
##     HHMOBILE        SA1SF2DN         STATE16        ARIABC16    
##  Min.   :1.000   Min.   : 1.000   Min.   :1.00   Min.   :1.000  
##  1st Qu.:2.000   1st Qu.: 3.000   1st Qu.:2.00   1st Qu.:1.000  
##  Median :2.000   Median : 6.000   Median :3.00   Median :1.000  
##  Mean   :2.245   Mean   : 5.499   Mean   :3.65   Mean   :1.564  
##  3rd Qu.:2.000   3rd Qu.: 8.000   3rd Qu.:5.00   3rd Qu.:2.000  
##  Max.   :8.000   Max.   :10.000   Max.   :8.00   Max.   :3.000  
##     NHSHHWT           WHHOR01          WHHOR02          WHHOR03      
##  Min.   :  20.57   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 289.25   1st Qu.: 195.3   1st Qu.: 203.8   1st Qu.: 197.1  
##  Median : 558.25   Median : 548.2   Median : 551.6   Median : 558.4  
##  Mean   : 574.47   Mean   : 574.2   Mean   : 574.4   Mean   : 574.4  
##  3rd Qu.: 839.16   3rd Qu.: 843.8   3rd Qu.: 832.3   3rd Qu.: 843.6  
##  Max.   :2280.67   Max.   :2280.7   Max.   :2280.7   Max.   :2460.2  
##     WHHOR04          WHHOR05          WHHOR06          WHHOR07      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 195.2   1st Qu.: 206.8   1st Qu.: 207.2   1st Qu.: 206.7  
##  Median : 550.3   Median : 554.2   Median : 558.3   Median : 555.0  
##  Mean   : 574.4   Mean   : 574.5   Mean   : 574.5   Mean   : 574.4  
##  3rd Qu.: 839.1   3rd Qu.: 836.9   3rd Qu.: 846.9   3rd Qu.: 847.5  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2333.2   Max.   :2280.7  
##     WHHOR08          WHHOR09          WHHOR10          WHHOR11      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 203.8   1st Qu.: 197.3   1st Qu.: 197.3   1st Qu.: 202.6  
##  Median : 547.7   Median : 551.6   Median : 554.4   Median : 562.0  
##  Mean   : 574.5   Mean   : 574.6   Mean   : 574.4   Mean   : 574.4  
##  3rd Qu.: 838.6   3rd Qu.: 844.9   3rd Qu.: 842.7   3rd Qu.: 844.6  
##  Max.   :2388.2   Max.   :2280.7   Max.   :2388.2   Max.   :2280.7  
##     WHHOR12          WHHOR13          WHHOR14          WHHOR15      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 204.5   1st Qu.: 206.9   1st Qu.: 197.3   1st Qu.: 204.6  
##  Median : 554.4   Median : 558.2   Median : 554.2   Median : 551.5  
##  Mean   : 574.4   Mean   : 574.4   Mean   : 574.6   Mean   : 574.3  
##  3rd Qu.: 842.4   3rd Qu.: 835.2   3rd Qu.: 851.1   3rd Qu.: 841.8  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2460.2   Max.   :2280.7  
##     WHHOR16          WHHOR17          WHHOR18          WHHOR19      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 195.0   1st Qu.: 203.8   1st Qu.: 195.2   1st Qu.: 195.3  
##  Median : 554.5   Median : 552.9   Median : 558.0   Median : 556.5  
##  Mean   : 574.3   Mean   : 574.6   Mean   : 574.5   Mean   : 574.5  
##  3rd Qu.: 838.3   3rd Qu.: 842.0   3rd Qu.: 835.1   3rd Qu.: 838.1  
##  Max.   :2333.2   Max.   :2280.7   Max.   :2280.7   Max.   :2280.7  
##     WHHOR20          WHHOR21          WHHOR22          WHHOR23      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 195.4   1st Qu.: 200.0   1st Qu.: 204.5   1st Qu.: 196.8  
##  Median : 555.5   Median : 559.7   Median : 560.3   Median : 564.2  
##  Mean   : 574.2   Mean   : 574.5   Mean   : 574.6   Mean   : 574.5  
##  3rd Qu.: 845.0   3rd Qu.: 846.8   3rd Qu.: 839.5   3rd Qu.: 835.8  
##  Max.   :2332.9   Max.   :2332.9   Max.   :2280.7   Max.   :2280.7  
##     WHHOR24          WHHOR25          WHHOR26          WHHOR27      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 201.9   1st Qu.: 201.9   1st Qu.: 206.8   1st Qu.: 197.7  
##  Median : 557.6   Median : 553.1   Median : 552.9   Median : 555.5  
##  Mean   : 574.6   Mean   : 574.4   Mean   : 574.5   Mean   : 574.4  
##  3rd Qu.: 842.5   3rd Qu.: 841.8   3rd Qu.: 842.2   3rd Qu.: 832.1  
##  Max.   :2280.7   Max.   :2280.7   Max.   :2401.8   Max.   :2280.7  
##     WHHOR28          WHHOR29          WHHOR30          WHHOR31      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 199.5   1st Qu.: 193.8   1st Qu.: 199.7   1st Qu.: 205.0  
##  Median : 554.4   Median : 558.4   Median : 551.7   Median : 558.5  
##  Mean   : 574.3   Mean   : 574.7   Mean   : 574.5   Mean   : 574.4  
##  3rd Qu.: 839.9   3rd Qu.: 836.0   3rd Qu.: 842.3   3rd Qu.: 828.5  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2280.7   Max.   :2280.7  
##     WHHOR32          WHHOR33          WHHOR34          WHHOR35      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 195.4   1st Qu.: 211.7   1st Qu.: 205.5   1st Qu.: 202.5  
##  Median : 554.3   Median : 553.0   Median : 553.0   Median : 558.8  
##  Mean   : 574.7   Mean   : 574.5   Mean   : 574.5   Mean   : 574.6  
##  3rd Qu.: 831.7   3rd Qu.: 841.0   3rd Qu.: 837.1   3rd Qu.: 853.6  
##  Max.   :2333.2   Max.   :2280.7   Max.   :2333.2   Max.   :2388.2  
##     WHHOR36          WHHOR37          WHHOR38          WHHOR39      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 207.6   1st Qu.: 202.3   1st Qu.: 206.0   1st Qu.: 198.0  
##  Median : 555.0   Median : 551.7   Median : 553.3   Median : 551.5  
##  Mean   : 574.5   Mean   : 574.6   Mean   : 574.5   Mean   : 574.3  
##  3rd Qu.: 839.5   3rd Qu.: 842.1   3rd Qu.: 840.1   3rd Qu.: 837.0  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2346.2   Max.   :2280.7  
##     WHHOR40          WHHOR41          WHHOR42          WHHOR43      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 206.7   1st Qu.: 204.5   1st Qu.: 204.6   1st Qu.: 203.8  
##  Median : 550.5   Median : 554.2   Median : 561.6   Median : 557.2  
##  Mean   : 574.6   Mean   : 574.5   Mean   : 574.5   Mean   : 574.3  
##  3rd Qu.: 845.8   3rd Qu.: 838.3   3rd Qu.: 850.0   3rd Qu.: 846.3  
##  Max.   :2445.9   Max.   :2387.9   Max.   :2280.7   Max.   :2280.7  
##     WHHOR44          WHHOR45          WHHOR46          WHHOR47      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 205.2   1st Qu.: 197.1   1st Qu.: 206.4   1st Qu.: 203.1  
##  Median : 551.8   Median : 553.0   Median : 558.2   Median : 557.2  
##  Mean   : 574.6   Mean   : 574.3   Mean   : 574.3   Mean   : 574.6  
##  3rd Qu.: 850.6   3rd Qu.: 847.7   3rd Qu.: 837.8   3rd Qu.: 838.1  
##  Max.   :2280.7   Max.   :2401.5   Max.   :2280.7   Max.   :2333.2  
##     WHHOR48          WHHOR49          WHHOR50          WHHOR51      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 206.6   1st Qu.: 207.7   1st Qu.: 203.7   1st Qu.: 206.8  
##  Median : 555.2   Median : 551.7   Median : 551.6   Median : 554.1  
##  Mean   : 574.4   Mean   : 574.6   Mean   : 574.6   Mean   : 574.3  
##  3rd Qu.: 836.9   3rd Qu.: 841.4   3rd Qu.: 841.2   3rd Qu.: 837.1  
##  Max.   :2280.7   Max.   :2280.7   Max.   :2388.2   Max.   :2388.2  
##     WHHOR52          WHHOR53          WHHOR54          WHHOR55      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 204.4   1st Qu.: 216.3   1st Qu.: 205.7   1st Qu.: 195.2  
##  Median : 554.5   Median : 555.2   Median : 551.6   Median : 558.7  
##  Mean   : 574.5   Mean   : 574.6   Mean   : 574.5   Mean   : 574.4  
##  3rd Qu.: 842.7   3rd Qu.: 833.0   3rd Qu.: 835.1   3rd Qu.: 842.7  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2280.7   Max.   :2401.5  
##     WHHOR56          WHHOR57          WHHOR58          WHHOR59      
##  Min.   :   0.0   Min.   :   0.0   Min.   :   0.0   Min.   :   0.0  
##  1st Qu.: 210.1   1st Qu.: 204.6   1st Qu.: 198.0   1st Qu.: 202.4  
##  Median : 555.7   Median : 558.5   Median : 554.5   Median : 556.2  
##  Mean   : 574.6   Mean   : 574.2   Mean   : 574.6   Mean   : 574.6  
##  3rd Qu.: 841.1   3rd Qu.: 851.1   3rd Qu.: 843.4   3rd Qu.: 844.4  
##  Max.   :2280.7   Max.   :2333.2   Max.   :2280.7   Max.   :2333.2  
##     WHHOR60          ABSPID.y          AGEB             SEX       
##  Min.   :   0.0   Min.   :1.000   Min.   : 1.000   Min.   :1.000  
##  1st Qu.: 203.6   1st Qu.:1.000   1st Qu.: 6.000   1st Qu.:1.000  
##  Median : 555.5   Median :1.000   Median :10.000   Median :2.000  
##  Mean   : 574.3   Mean   :1.232   Mean   : 9.532   Mean   :1.527  
##  3rd Qu.: 847.2   3rd Qu.:1.000   3rd Qu.:14.000   3rd Qu.:2.000  
##  Max.   :2333.2   Max.   :2.000   Max.   :19.000   Max.   :2.000  
##     HIGHLVBC         EMPTYPE      
##  Min.   : 0.000   Min.   :0.0000  
##  1st Qu.: 1.000   1st Qu.:0.0000  
##  Median : 4.000   Median :0.0000  
##  Mean   : 3.768   Mean   :0.7188  
##  3rd Qu.: 6.000   3rd Qu.:1.0000  
##  Max.   :12.000   Max.   :6.0000
```
