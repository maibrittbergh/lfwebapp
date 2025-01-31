# lfwebapp
The idea of this shiny web app (Chang, W. et al., 2021)  is to develop an environment that offers an understandable, easy and compact approach for a low flow analysis. All the functions within this web app are taken from the R-package "lfanalyse" and the discharge data is from the GRDC-dataset (GRDC; 2020).

## How to choose a station 
You can choose a station of interest on the map or in the table. While you can select a location of interest on the map, the table can help you to filter the different stations by location, length of time series, catchment size, start and end day of measurements or river. Currently 330 stations are listed on the Webapp. 



<img width="1000" alt="Bildschirmfoto 2025-01-31 um 11 59 13" src="https://github.com/user-attachments/assets/c1545d7c-c367-4b16-ba57-351c25e510a4" />

## Analysing tool 

In a next step one can decide on the Analysing tool (right hand side of the map). For a first overview the WebApp allows to use descriptive statistics (saved under the option Analysing tool). Depending on the research question one can perform a seasonal or annual analysis of the discharge data. 

<img width="501" alt="Bildschirmfoto 2025-01-31 um 12 04 20" src="https://github.com/user-attachments/assets/29c7f7a8-8432-4e93-9a2a-f353f278c033" />

## Low flow analysis 
To perform a low flow analysis, one can either choose a threshold based analysis or a trend analysis. 

### Threshold based analys
The threshold can be set independently of the measurement series or quantile-based. Quantile-based analysis allows to compare rivers of different sizes with each other. Please choose a year of interest. You can save the plot that you created. 

<img width="484" alt="Bildschirmfoto 2025-01-31 um 12 08 40" src="https://github.com/user-attachments/assets/6f87ce92-cf89-49d4-a6c2-29e663f39519" />
### Trend analysis 
The trend analysis covers the entire timespan of the measurement time series. Two different methods are used to calculate the trend. The underlying concepts are explained in the "statistical introduction" (https://github.com/maibrittbergh/statisticalmanual). 
<img width="503" alt="Bildschirmfoto 2025-01-31 um 12 11 14" src="https://github.com/user-attachments/assets/fd8bd230-d151-4278-b8fb-968d3285ecec" />


## Resources 
GRDC, 2020. GRDC Data Download. [Online] 
Available at: https://www.bafg.de/GRDC/EN/02_srvcs/21_tmsrs/210_prtl/tou.html?nn=2862854
[Zugriff am 08 09 2023].

Chang, W. et al., 2021. shiny: Web Application Framework for R. [Online] 
Available at: https://CRAN.R-project.org/package=shiny. DOI: 10.32614/CRAN.package.shiny
[Zugriff am 12 09 2024].
