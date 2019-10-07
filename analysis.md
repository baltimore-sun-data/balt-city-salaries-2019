Analysis: Baltimore city salaries, fiscal year 2019
---------------------------------------------------

The Baltimore Sun analyzed data from [Open
Baltimore](https://data.baltimorecity.gov/City-Government/Baltimore-City-Employee-Salaries-FY2019/6xv6-e66h)
on salaries for city employees employed on June 30, 2019. The dataset
captures salary from July 1, 2018 through June 30, 2019.

Here are the key statistics reported in the story:

-   Sgt. Ethan Newberg, a 24-year veteran of the city’s police force,
    earned $260,775, more than double his base salary of $107,807
-   In the previous fiscal year, Newberg ranked second on the list of
    best-paid city employees, making $243,132
-   Police Commissioner Michael Harrison and his two deputy
    commissioners, Danny Murphy and Michael Sullivan, were the only
    police employees whose base salary ranked in the top 10
-   Seven of the 10 highest-paid city employees were police officers
-   Harrison makes a base salary of $275,000. Former police commissioner
    Darryl DeSousa made an annual base salary of $210,000
-   Total police department pay exceeded base salary by more than $40
    million, down from 2018, when the difference was $51 million
-   The median pay for an employee in the police department was $93,278
-   The fire department had the second-highest total pay, with total
    compensation exceeding base salaries by more than $16 million
-   The median pay for a fire department employee was $82,097
-   The second-highest paid employee was Frank Johnson, who earned
    $251,922 on a salary of $250,000. The next-highest paid was William
    Harris Jr., a police sergeant who made $249,356 on a salary of
    $107,364. Harris was followed by police Lt. Thomas Mistysyn Jr., who
    earned $247,182 on a base salary of $122,049 and fire department
    employee Anthony Smith, who earned $243,747 on a base salary of
    $105,000. State’s Attorney Marilyn Mosby was the sixth-highest paid,
    with a salary of $238,772
-   Harris was the city’s highest paid employee in fiscal year 2018
-   Mayor Bernard C. “Jack” Young earned $127,984 on a salary of
    $184,832, nearly $10,000 more than what he earned as City Council
    President during the last fiscal year

### Load R libraries

``` r
library('tidyverse')
library('janitor')
library('lubridate')
```

### Read salaries data

``` r
salaries.19 <- read_csv('input/Baltimore_City_Employee_Salaries_FY2019.csv') %>% clean_names()
salaries.18 <- read_csv('input/Baltimore_City_Employee_Salaries_FY2018.csv') %>% clean_names()
```

### Finding: Sgt. Ethan Newberg, a 24-year veteran of the city’s police force, earned $260,775, more than double his base salary of $107,807

Arrange the data by `gross` — the total compensation in fiscal year 2019
— and use `head()` to see the top salaries. Newberg is first.

``` r
salaries.19 %>% arrange(desc(gross)) %>% head()
```

    ## # A tibble: 6 x 7
    ##   name      jobtitle       deptid descr        hire_dt     annual_rt  gross
    ##   <chr>     <chr>          <chr>  <chr>        <chr>           <dbl>  <dbl>
    ## 1 Newberg,… Police Sergea… A99220 Police Depa… 06/26/1995…    107807 2.61e5
    ## 2 Johnson,… Executive Dir… A40001 M-R Info Te… 09/01/2017…    250000 2.52e5
    ## 3 Harris J… Police Sergea… A99304 Police Depa… 10/24/2000…    107364 2.49e5
    ## 4 Mistysyn… Police Lieute… A99225 Police Depa… 05/22/1990…    122049 2.47e5
    ## 5 Smith,An… Contract Srvc… A64001 Fire Depart… 09/28/1994…    105000 2.44e5
    ## 6 Mosby,Ma… State's Attor… A29001 States Atto… 08/01/2005…    238772 2.39e5

``` r
print(paste(salaries.19[order(-salaries.19$gross), ][1,]$name, "was the highest earner in fiscal year 2019, making",
      salaries.19[order(-salaries.19$gross), ][1,]$gross, "or", 
      round(salaries.19[order(-salaries.19$gross), ][1,]$gross/salaries.19[order(-salaries.19$gross), ][1,]$annual_rt, 1), "times base pay."))
```

    ## [1] "Newberg,Ethan R was the highest earner in fiscal year 2019, making 260775.26 or 2.4 times base pay."

``` r
print(paste("The highest earner has worked in the city for", round(as.numeric(difftime(today(), 
                                                                                  as.Date(mdy_hms(salaries.19[order(-salaries.19$gross), ][1,]$hire_dt)), unit="weeks")/52.25), 1), "years."))
```

    ## [1] "The highest earner has worked in the city for 24.2 years."

### Finding: In the previous fiscal year, Newberg ranked second on the list of best-paid city employees, making $243,132

``` r
salaries.18 %>% arrange(desc(gross)) %>% head()
```

    ## # A tibble: 6 x 7
    ##   name      jobtitle      deptid descr         hire_dt     annual_rt  gross
    ##   <chr>     <chr>         <chr>  <chr>         <chr>           <dbl>  <dbl>
    ## 1 Harris J… Police Serge… A99304 Police Depar… 10/24/2000…    100228 2.50e5
    ## 2 Newberg,… Police Serge… A99341 Police Depar… 06/26/1995…     99860 2.43e5
    ## 3 Mosby,Ma… State's Atto… A29001 States Attor… 08/01/2005…    238772 2.39e5
    ## 4 Green,Er… Police Offic… A99335 Police Depar… 05/07/1998…     84796 2.27e5
    ## 5 Merrick,… Police Lieut… A99425 Police Depar… 05/10/1994…    114228 2.25e5
    ## 6 Johnson,… Executive Di… A40001 M-R Info Tec… 09/01/2017…    250000 2.17e5

### Finding: Police Commissioner Michael Harrison and his two deputy commissioners, Danny Murphy and Michael Sullivan, were the only police employees whose base salary ranked in the top 10

Arrange the data by `annual_rt` — the base salary in fiscal year 2019 —
and use `head(10)` to see the top 10 salaries.

``` r
salaries.19 %>% arrange(desc(annual_rt)) %>% head(10)
```

    ## # A tibble: 10 x 7
    ##    name      jobtitle       deptid descr       hire_dt     annual_rt  gross
    ##    <chr>     <chr>          <chr>  <chr>       <chr>           <dbl>  <dbl>
    ##  1 Harrison… Police Commis… A99390 Police Dep… 02/11/2019…    275000 1.01e5
    ##  2 Johnson,… Executive Dir… A40001 M-R Info T… 09/01/2017…    250000 2.52e5
    ##  3 Mosby,Ma… State's Attor… A29001 States Att… 08/01/2005…    238772 2.39e5
    ##  4 Raymond,… Executive Dir… A23001 FIN-Admin … 05/08/2008…    214514 2.17e5
    ##  5 Ford,Nil… Executive Dir… A64006 Fire Depar… 01/15/2014…    210000 2.00e5
    ##  6 Braverma… Executive Dir… A06001 Housing & … 12/15/1986…    199716 2.02e5
    ##  7 Sullivan… Deputy Police… A99389 Police Dep… 06/17/2019…    195000 3.24e3
    ##  8 Murphy,D… Deputy Police… A99399 Police Dep… 04/08/2019…    195000 4.12e4
    ##  9 Calhoun,… Executive Dir… A54001 FPR Admin … 07/08/2013…    192725 1.93e5
    ## 10 Davis,An… City Solicitor A30001 Law Depart… 09/01/2017…    188000 1.90e5

### Finding: Seven of the 10 highest-paid city employees were police officers

Create a column, `police`, that = 1 if the employee works in the police
department; 0 if not. Arrange the top 10 by gross and sum the number of
police.

``` r
salaries.19 %>% 
  mutate(police = ifelse(grepl("Police", descr), 1, 0)) %>% 
  arrange(desc(gross)) %>% 
  head(10) %>% 
  summarise(police_in_top_10 = sum(police))
```

    ## # A tibble: 1 x 1
    ##   police_in_top_10
    ##              <dbl>
    ## 1                7

### Finding: Harrison’s base salary is $275,000 a year. Former police commissioner Darryl DeSousa made an annual base salary of $210,000

Use the 2019 and 2018 salary database to compare Harrison and DeSousa’s
pay

``` r
print(salaries.19 %>% filter(jobtitle == 'Police Commissioner'))
```

    ## # A tibble: 1 x 7
    ##   name       jobtitle     deptid descr        hire_dt      annual_rt  gross
    ##   <chr>      <chr>        <chr>  <chr>        <chr>            <dbl>  <dbl>
    ## 1 Harrison,… Police Comm… A99390 Police Depa… 02/11/2019 …    275000 1.01e5

``` r
print(salaries.18 %>% filter(jobtitle == 'Police Commissioner'))
```

    ## # A tibble: 1 x 7
    ##   name       jobtitle      deptid descr       hire_dt      annual_rt  gross
    ##   <chr>      <chr>         <chr>  <chr>       <chr>            <dbl>  <dbl>
    ## 1 De Sousa,… Police Commi… A99390 Police Dep… 12/14/1988 …    210000 1.77e5

### Finding: Total police department pay exceeded base salary by more than $40 million, down from 2018, when the difference was $52 million

Calculate the difference betwewen total base salary and actual pay for
the police department in 2019 verus 2018

``` r
salaries.19 %>% 
  filter(grepl("Police Department", descr)) %>% 
  summarise(base = sum(annual_rt, na.rm = T),
            actual = sum(gross, na.rm = T)) %>%
  mutate(difference = actual - base)
```

    ## # A tibble: 1 x 3
    ##        base     actual difference
    ##       <dbl>      <dbl>      <dbl>
    ## 1 234064370 274393528.  40329158.

``` r
salaries.18 %>% 
  filter(grepl("Police", descr)) %>% 
  summarise(base = sum(annual_rt, na.rm = T),
            actual = sum(gross, na.rm = T)) %>%
  mutate(difference = actual - base)
```

    ## # A tibble: 1 x 3
    ##        base     actual difference
    ##       <dbl>      <dbl>      <dbl>
    ## 1 221875622 273276087.  51400465.

### Finding: The median pay for an employee in the police department was $93,278

Filter and summarize to calculate the median police department
(including overtime) pay in 2019

``` r
salaries.19 %>% 
  filter(grepl("Police", descr)) %>% 
  summarise(median = median(gross, na.rm = T)) 
```

    ## # A tibble: 1 x 1
    ##   median
    ##    <dbl>
    ## 1 93278.

### Finding: The fire department had the second-highest total pay, with salaries exceeding base salaries by more than $16 million

Group by department (remove the parenthetical numbers in the department
`descr` column to get an accurate tally), and arrange by total gross
pay. Use `head()` to see the top departments.

``` r
salaries.19.dept <- separate(salaries.19,
                           descr,
                           into = c('dept', 'number'),
                           sep = "\\(",
                           remove = F) %>%
  select(-number) %>% 
  mutate(dept = trimws(dept)) %>%
  group_by(dept) %>%
  summarise(actual = sum(gross, na.rm = T)) %>%
  arrange(desc(actual)) %>%
  head(10)

salaries.19.dept
```

    ## # A tibble: 10 x 2
    ##    dept                         actual
    ##    <chr>                         <dbl>
    ##  1 Police Department        274393528.
    ##  2 Fire Department          138760643.
    ##  3 DPW-Water & Waste Water   79210435.
    ##  4 TRANS-Highways            36685507.
    ##  5 HLTH-Health Department    32039843.
    ##  6 DPW-Solid Waste           26419567.
    ##  7 States Attorneys Office   23497941 
    ##  8 Housing & Community Dev   21652618.
    ##  9 Enoch Pratt Free Library  20773938.
    ## 10 General Services          19392899.

The fire department is \#2 after the police department.

Calculate total pay plus the difference between base and actual for the
fire department.

``` r
salaries.19 %>% 
  filter(grepl("Fire", descr)) %>% 
  summarise(base = sum(annual_rt, na.rm = T),
            actual = sum(gross, na.rm = T)) %>%
  mutate(difference = actual - base)
```

    ## # A tibble: 1 x 3
    ##        base     actual difference
    ##       <dbl>      <dbl>      <dbl>
    ## 1 122136478 138760643.  16624165.

The difference is more than 16 million.

### Finding: The median pay for a fire department employee was $82,097

``` r
salaries.19 %>% 
  filter(grepl("Fire", descr)) %>% 
  summarise(median = median(gross, na.rm = T)) 
```

    ## # A tibble: 1 x 1
    ##   median
    ##    <dbl>
    ## 1 82098.

### Finding: The second-highest paid employee was Frank Johnson, who earned $251,922 on a salary of $250,000. The next-highest paid was William Harris Jr., a police sergeant who made $249,356 on a salary of $107,364. Harris was followed by police Lt. Thomas Mistysyn Jr., who earned $247,182 on a base salary of $122,049 and fire department employee Anthony Smith, who earned $243,747 on a base salary of $105,000

Arrange the data by `gross` and use `head()` to see the top salaries

``` r
salaries.19 %>% arrange(desc(gross)) %>% head()
```

    ## # A tibble: 6 x 7
    ##   name      jobtitle       deptid descr        hire_dt     annual_rt  gross
    ##   <chr>     <chr>          <chr>  <chr>        <chr>           <dbl>  <dbl>
    ## 1 Newberg,… Police Sergea… A99220 Police Depa… 06/26/1995…    107807 2.61e5
    ## 2 Johnson,… Executive Dir… A40001 M-R Info Te… 09/01/2017…    250000 2.52e5
    ## 3 Harris J… Police Sergea… A99304 Police Depa… 10/24/2000…    107364 2.49e5
    ## 4 Mistysyn… Police Lieute… A99225 Police Depa… 05/22/1990…    122049 2.47e5
    ## 5 Smith,An… Contract Srvc… A64001 Fire Depart… 09/28/1994…    105000 2.44e5
    ## 6 Mosby,Ma… State's Attor… A29001 States Atto… 08/01/2005…    238772 2.39e5

### Finding: Harris was the city’s highest paid employee in fiscal year 2018

Arrange the 2018 data by `gross` and use `head(1)` to see the top paid
employee

``` r
salaries.18 %>% arrange(desc(gross)) %>% head(1)
```

    ## # A tibble: 1 x 7
    ##   name       jobtitle    deptid descr         hire_dt      annual_rt  gross
    ##   <chr>      <chr>       <chr>  <chr>         <chr>            <dbl>  <dbl>
    ## 1 Harris Jr… Police Ser… A99304 Police Depar… 10/24/2000 …    100228 2.50e5

### Finding: Mayor Bernard C. “Jack” Young earned $127,984 on a salary of $184,832, nearly $10,000 more than what he earned as City Council President during the last fiscal year

Filter the 2018 and 2019 data to find Young’s salaries in those fiscal
years

``` r
print(salaries.18 %>% filter(name =='Young,Bernard C'))
```

    ## # A tibble: 1 x 7
    ##   name      jobtitle       deptid descr       hire_dt      annual_rt  gross
    ##   <chr>     <chr>          <chr>  <chr>       <chr>            <dbl>  <dbl>
    ## 1 Young,Be… President Cit… A02001 City Counc… 10/21/1996 …    119402 1.18e5

``` r
print(salaries.19 %>% filter(name =='Young,Bernard C'))
```

    ## # A tibble: 1 x 7
    ##   name        jobtitle deptid descr         hire_dt        annual_rt  gross
    ##   <chr>       <chr>    <chr>  <chr>         <chr>              <dbl>  <dbl>
    ## 1 Young,Bern… Mayor    A01001 Mayors Offic… 10/21/1996 12…    184832 1.28e5

``` r
print(paste("Mayor Young earned $", 
      salaries.19[salaries.19$name =='Young,Bernard C', ]$gross - salaries.18[salaries.18$name =='Young,Bernard C',]$gross,
      "more in fiscal year 2019 than in fiscal year 2018."))
```

    ## [1] "Mayor Young earned $ 9927 more in fiscal year 2019 than in fiscal year 2018."
