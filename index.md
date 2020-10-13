<head>
<style>
.page-header {color: #ffff;text-align: center; padding: 5rem 6rem; background-color: #0473aa; background-image: linear-gradient(180deg, #1d1599, #045d23);}
.page-header h2{font-size: 24px;
body{width: 1200px; margin: 0;}

.tmbcomment, .tmbalert, .tmbinfo, .tmbqns, .tmbcheck1, .tmbcheck2, .tmbshare, .tmbarrowF, .tmb2arrowsF {
  padding: 1em 1em 1em 4em;
  margin-bottom: 10px;
  background: #f5f5f5;
  position:relative;
}

.tmbcomment:before {
    content: "\f14d";
  font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }

.tmbalert:before {
    content: "\f06a";
  font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }

.tmbinfo:before {
    content: "\f05a";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }

.tmbinfo2:before {
    content: "\10f129";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }
 
.tmbarrowF:before {
    content: "\f0a9";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }
 
.tmb2arrowsF:before {
    content: "\f101";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }
 
.tmbshare:before {
    content: "\f064";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 35px;
 }
 
.tmbqns:before {
    content: "\f059";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }

.tmbcheck1:before {
    content: "\f14a";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }
 
.tmbcheck2:before {
    content: "\f058";
    font-family: FontAwesome;
    left:10px;
    position:absolute;
    font-size: 45px;
 }

</style>
<link rel="stylesheet" type="text/css" href="./style.css">
<link rel="stylesheet" type="text/css" href="./toc.css">
</head>

# Empowerment in Agriculture

```{}
# Load required packages
load("data/packages.RData")
load("data/globalSetup.RData")

source("_common.R")

library(tidyverse)
library(viridis)

htmltools::tagList(rmarkdown::html_dependency_font_awesome())
```

# Focus: Five Dimensions of Empowerment


## Methodology 

Here we adopt the model develoved previously [@Alkire2011]

For more details refer to the [R-Model workflow](https://complexdatainsights/indexes/fiveDE.html)

Model: [@Alkire2011]

```{}
# Load required packages
load("data/packages.RData")
load("data/globalSetup.RData")

source("_common.R")

library(tidyverse)
library(viridis)

htmltools::tagList(rmarkdown::html_dependency_font_awesome())
```

## 5DE Methodology 

[R-Model workflow](https://tmbuza.github.io/indexes/)


1. Method 1 focuses on the percentage of empowered women and adequacies among the disempowered. 
2. Method 2 focuses on the percentage of disempowered women and the percentage of domains in which they lack adequate achievements. 

> Method 2 is commonly used as it is consistent with the M0 measurement (Alkire and Foster 2011).
 the matrix has new column `ciwk` that contains censored inadequacy score.


<br><br>

## Uncensored Headcount Ratios
```{}
library(dplyr)
inadequacies <- readRDS("data/uncensored.rds")

uncensored <- inadequacies 
ciM0 <- round(mean(uncensored$ci), 3)
d01Hn <- round(mean(uncensored$d01), 3)
d02Hn <- round(mean(uncensored$d02), 3)
d03Hn <- round(mean(uncensored$d03), 3)
d04Hn <- round(mean(uncensored$d04), 3)
d05Hn <- round(mean(uncensored$d05), 3)
d06Hn <- round(mean(uncensored$d06), 3)
d07Hn <- round(mean(uncensored$d07), 3)
d08Hn <- round(mean(uncensored$d08), 3)
d09Hn <- round(mean(uncensored$d09), 3)
d10Hn <- round(mean(uncensored$d10), 3)

uncensoredM0Index <- sum(d01Hn, d02Hn, d03Hn, d04Hn, d05Hn, d06Hn, d07Hn, d08Hn, d09Hn, d10Hn)
uncensoredFiveDE <- 1-uncensoredM0Index

Inadequate <- c(d01Hn, d02Hn, d03Hn, d04Hn, d05Hn, d06Hn, d07Hn, d08Hn, d09Hn, d10Hn, uncensoredM0Index, uncensoredFiveDE)

cat("\nVerify: Sum of Indicators M0s equals the overall M0\n")
paste("IndicatorSum =", round(uncensoredM0Index, digits = 3), "and overall M0 =", round(ciM0, digits = 3))
cat("Status:", ifelse(round(uncensoredM0Index, digits = 3) == round(ciM0, digits = 3), "PASSED", "FAILED"))

```

<br>
<br>

## Censoring Inadequate Achievements
> Note: A respondent is identified as disempowered if ci > k and empowered if ci ≤ k. 
> Censoring: If ci ≤ k the score is replaced by 0, otherwise cik = ci, d01k = d01, d02k = d02, and so forth.

```{}
library(dplyr)

# Set the cutoff(k) for empowerment among inadequacies
k = 0.2

# Create columns containing censored achievements.
censored <- inadequacies %>% 
  mutate(
    cik = ifelse(ci <= k, 0, ci),
    d01k = ifelse(ci <= k, 0, d01), 
    d02k = ifelse(ci <= k, 0, d02), 
    d03k = ifelse(ci <= k, 0, d03), 
    d04k = ifelse(ci <= k, 0, d04),
    d05k = ifelse(ci <= k, 0, d05),
    d06k = ifelse(ci <= k, 0, d06),
    d07k = ifelse(ci <= k, 0, d07),
    d08k = ifelse(ci <= k, 0, d08),
    d09k = ifelse(ci <= k, 0, d09),
    d10k = ifelse(ci <= k, 0, d10)
    )

saveRDS(censored, ("data/censored.rds"))
```

<br>
<br>

### Censored Haedcount Ratios
```{r}
censored <- readRDS("data/censored.rds")

cikM0 <- round(mean(censored$cik), 3)
d01kHn <- round(mean(censored$d01k), 3)
d02kHn <- round(mean(censored$d02k), 3)
d03kHn <- round(mean(censored$d03k), 3)
d04kHn <- round(mean(censored$d04k), 3)
d05kHn <- round(mean(censored$d05k), 3)
d06kHn <- round(mean(censored$d06k), 3)
d07kHn <- round(mean(censored$d07k), 3)
d08kHn <- round(mean(censored$d08k), 3)
d09kHn <- round(mean(censored$d09k), 3)
d10kHn <- round(mean(censored$d10k), 3)

censoredM0Index <- d01kHn + d02kHn + d03kHn + d04kHn + d05kHn + d06kHn + d07kHn + d08kHn + d09kHn + d10kHn
censoredFiveDE <- 1-censoredM0Index
  
Disempowered <- c(d01kHn, d02kHn, d03kHn, d04kHn, d05kHn, d06kHn, d07kHn, d08kHn, d09kHn, d10kHn, censoredM0Index, censoredFiveDE)

cat("Verify: Sum of Indicators M0s equals the overall M0\n")
paste("IndicatorSum =", round(censoredM0Index, digits = 2), "and overall M0 =", round(cikM0, digits = 2))
cat("Status:", ifelse(round(censoredM0Index, digits = 2) == round(cikM0, digits = 2), "PASSED", "FAILED"))
```


## Create a Dataframe
```{}
Indicator <- c("Indicator01", "Indicator02", "Indicator03", "Indicator04", "Indicator05", "Indicator06","Indicator07", "Indicator08", "Indicator09", "Indicator10", "M0", "FiveDE" )

HCRatio <- data.frame(Indicator, Inadequate, Disempowered)
head(HCRatio, 20)
```


## Plot the Results
```{}
library(tidyverse)
library(viridis)

p1 <- HCRatio[1:10,] %>% 
  gather(key = "variable", value = value, -Indicator) %>% 
  ggplot(aes(x = variable, y = value, fill = Indicator)) +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat = "identity", position = "stack", width = .5, color = "#f1f1f1") + 
  labs(x = "", y = "Absolute Contribution", title = "")

p2 <- HCRatio[1:10,] %>% 
  gather(key = "variable", value = value, -Indicator) %>% 
  ggplot(aes(x = variable, y = value, fill = Indicator)) +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat = "identity", position = "fill", width = .5, color = "#f1f1f1") +
  labs(x = "", y = "Relative Contribution", title = "") +
  scale_y_continuous(labels = scales::percent)

ggpubr::ggarrange(p1, p2, common.legend = TRUE, legend = "right")
```





