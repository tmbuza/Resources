set.seed(110912)
options(digits = 3)

htmltools::tagList(rmarkdown::html_dependency_font_awesome())

knitr::opts_chunk$set(
  echo = TRUE,
  # echo = FALSE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  comment = NA,
  fig.path='./_figs/IDX-',
  fig.show='asis',
  dev = 'png',
  fig.align='center',
  out.width = "70%",
  out.height = "80%",
  # fig.width = 7,
  # fig.asp = 0.618,  # 1 / phi
  fig.show = "asis"
)

load("_data/packages.RData")
load("_data/globalSetup.RData")

library(ggplot2)
library(readr)
library(dplyr)
library(kableExtra)
library(funModeling)
library(ggplot2)
library(ggpubr)
library(stringr)
library(stringi)
library(tools)
library(data.table)
library(magrittr)
library(tree)


centertitle <- theme(plot.title = element_text(hjust = 0.5))

nonzero <- function(x) sum(x != 0)
