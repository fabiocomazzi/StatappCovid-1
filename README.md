# StatappCovid

## Table of contents
* [Introduction](#introduction "Goto introduction")
* [Technologies](#technologies "Goto technologies")

## Introduction
This project consists in the analysis of data regarding mortality rates in Italy during 2020. In particular, the main focus of the analysis are the effects of the spread of COVID-19 on such rates. The main points of the analysis are the following:
1. **COVID Surplus Mortality Estimation**: An estimate of the mortality surplus caused by COVID is obtained by forecasting the data regarding the previous years (2011-2019) through an ARIMA model and comparing such forecast with the actual data collected during 2020. [Results](https://fabiocomazzi.wixsite.com/mortalityprovince). 
2. **Cluster Analysis**: Hierarchical clustering is used to cluster Italian regions according to how they were affected by COVID throughout 2020. [Results (final clusters)](https://github.com/SnoopKilla/StatappCovid/blob/main/Output/Plot/Semester%20-%20Clusters.pdf)
3. **Correlation with Google Search Trends**: A functional data analysis technique (dynamical correlation) is used to investigate the correlation between mortality data and (daily) google search trends for specific keywords ("polmonite" and "febbre"). [Results (correlation for "febbre")](https://github.com/SnoopKilla/StatappCovid/blob/main/Output/Plot/Correlation%20-%20Febbre.pdf).

**I personally contributed to this project by developing step 1 of the analysis.**

## Technologies
Project is created with **R**.
