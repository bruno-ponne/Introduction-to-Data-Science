---
title: "Can institutional arrangements improve student performance?"
author: "Bruno Gasparotto Ponne - bruno.ponne@gmail.com"
output:
  html_document:
    keep_md: true
    toc: yes
    toc_float: yes
abstract: In 2007, a series of reforms were implemented in the education system in
  Ceará, Brazil. This research aims to explore data on the education quality index
  to find out if it suggests these reforms had an impact on student achievement.
  In order to do so, panel data is analysed through spatial plots, time series and
  scatter plots.
---


<br>

The complete files to this analysis can be found on: https://github.com/bruno-ponne/Introduction-to-Data-Science

### The reforms in Ceará, Brazil
<div style="color:#4E4C4C; text-align: justify">

Around 2007, the government of Ceará, a state in the northeastern part of Brazil, started a series of reforms in its education system. These reforms comprised five main pillars: fiscal incentives to municipalities based on outcomes; technical assistance for municipalities; more autonomy and accountability for local systems; regular monitoring of learning in order to collect actionable data, and elimination of political influence in the choice of principals and teachers.

In the analysis that follows, I employ data collected by the Brazilian government concerning the quality of education (IDEB - Índice de Desenvolvimento da Educação Básica) and data that might impact student outcomes, like socioeconomic level of the school/region.

The objective is to describe how the education quality evolved through time, get insights about the data and associations between the variables.

</div>





<img src="plots/plot_ceara.png" width="70%" height="70%" style="display: block; margin: auto;" />

### Data

<div style="color:#4E4C4C; text-align: justify">
The data used in this analysis is made available by the Ministry of Education of Brazil and the Brazilian Institute of Geography and Statistics. Bellow, there is a representation of how data sets connect to each other. In order to tidy data, I employed the function **gather()** that creates a data frame in which each observation has its own row. In order to join data sets, I employed **left_join()** using the keys highlighted in the scheme bellow.
</div>

<img src="plots/data_scheme.png" width="70%" style="display: block; margin: auto;" />


### Distribution of the education index

<div style="color:#4E4C4C; text-align: justify">
As a first analysis, I explore the distribution of education quality index in 2005, before intervention, for Ceará and for Brazil (excluding Ceará). In order to do that, I use density plots. Observing the graph bellow, as expected, Brazil has greater standard deviations from the mean because it is composed by 26 different states. Ceará has a much smaller variance in its distribution. The mean results of Ceará are lower compared to the Brazilian average. The distributions are centered around 3.
</div>



<img src="plots/density2005.png" width="70%" style="display: block; margin: auto;" />

<div style="color:#4E4C4C; text-align: justify">
Below, the distribution of indexes in 2019 can be seen (after intervention). Both indexes improved considerably in the period. Now the distributions are centered around 5. The average index of Ceará, however, improved more compared to the average in Brazil. The probability of finding an index higher than 7, for example, is a lot higher in Ceará data.
</div>


<img src="plots/density2019.png" width="70%" style="display: block; margin: auto;" />

### OLS: Ceará vs. Brazil

<div style="color:#4E4C4C; text-align: justify">
Here we can observe the results of a linear model that shows the association between being the state of Ceará and the quality of education index both in 2005 and in 2009. We can observe that initially, in 2005, being located in Ceará meant a decrease of -0.35 in the performance of a school. In 2019, this association changed direction and increased considerably: being located in Ceará is now associated with a 0.61 increase in the education index. 

If we control for human development index (HDI), being a school in Ceará in 2019 is associated with an increase of 0.89 in the quality of education compared to the rest of the country. This means that when we compare schools with similar HDIs, there is a stronger positive association of being located in Ceará and the education quality index. We cannot interpret, however, that the reforms in Ceará caused a 0.89 increase in the quality of education, because there are more confounders affecting this relation.
</div>


<style type="text/css">
td
{
    text-align: center;
    padding:0 50px 0 50px;
}
</style>
<div >

<table style="text-align:center"><caption><strong>Table 1 - OLS Model: Education Quality Index per group.</strong></caption>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">Education Quality Index</td></tr>
<tr><td style="text-align:left"></td><td>2005</td><td>2005</td><td>2019</td><td>2019</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Ceará</td><td>-0.353<sup>***</sup></td><td>-0.186<sup>***</sup></td><td>0.611<sup>***</sup></td><td>0.887<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.026)</td><td>(0.024)</td><td>(0.021)</td><td>(0.019)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">HDI</td><td></td><td>4.039<sup>***</sup></td><td></td><td>5.109<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.071)</td><td></td><td>(0.056)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>3.245<sup>***</sup></td><td>0.401<sup>***</sup></td><td>4.571<sup>***</sup></td><td>1.037<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.050)</td><td>(0.005)</td><td>(0.039)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>18,650</td><td>18,648</td><td>29,718</td><td>29,700</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.010</td><td>0.156</td><td>0.029</td><td>0.241</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01</td></tr>
</table>
</div>




### Spatial Data: Ceará vs. Brazil

<div style="color:#4E4C4C; text-align: justify">
The following maps show the geographical area of Brazil colored according to the average education quality index of each municipality. Blue is associated with a good index and red with a poor one. We can tell that the country had an overall improvement from 2005 to 2019 both for primary and lower secondary school.

Ceará is highlighted at the top right of the map and it is visible that this state has improved more compared to other states, specially its neighbors.
</div>



<img src="plots/spatial_plot_pri.png" width="100%" style="display: block; margin: auto;" /><img src="plots/spatial_plot_sec.png" width="100%" style="display: block; margin: auto;" />

### Time Series: Ceará vs. Brazil
<div style="color:#4E4C4C; text-align: justify">
In this section, I explore how the education quality index evolved through time before and after the intervention in Ceará. The averages of Ceará are compared to the average index of all other Brazilian states (except Ceará). Before intervention, the trends follow a parallel development although the curves are not perfectly parallel. Unfortunatelly there are only two observation before intervention: 2005 and 2007.
</div>



<img src="plots/plot_s_p.png" width="70%" style="display: block; margin: auto;" /><img src="plots/plot_ls_p.png" width="70%" style="display: block; margin: auto;" />

### Education Quality vs. socioeconomic level
<div style="color:#4E4C4C; text-align: justify">
In this section, I analyse the education quality index as a function of the socioeconomic level of the school for Ceará and for the rest of Brazil. What can be seen is that schools in Ceará have a lower socioeconomic level, but a higher education index compared to the rest of the country. The best primary and lower secondary school in 2019 are located in Ceará.
</div>


<img src="plots/plot_socio1.png" width="70%" style="display: block; margin: auto;" /><img src="plots/plot_socio2.png" width="70%" style="display: block; margin: auto;" />

### NAs analysis
<div style="color:#4E4C4C; text-align: justify">
Here I present the proportions of missing values (NA) relative to the number of observations for each state regarding the primary schools. Some states have more than 50% of missing values. It appears that poorer states have a bigger proportion of NAs. In the second table, the same information is presented, but regarding the Lower Secondary Schools. Again, it appears that poorer states have a bigger proportion of NAs. 
</div>

<!-- html table generated in R 3.6.1 by xtable 1.8-4 package -->
<!-- Tue Dec 14 10:41:29 2021 -->
<table border=1>
<caption align="top"> NAs in Primary Schools: 2005 - 2019 </caption>
<tr> <th>  </th> <th> State </th> <th> Number of Observations </th> <th> Number of NAs </th> <th> Percentage of NAs </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> DF </td> <td align="right"> 3280 </td> <td align="right"> 738 </td> <td align="right"> 22.50 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> PR </td> <td align="right"> 23120 </td> <td align="right"> 5550 </td> <td align="right"> 24.01 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> MS </td> <td align="right"> 6480 </td> <td align="right"> 1610 </td> <td align="right"> 24.85 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> SP </td> <td align="right"> 58232 </td> <td align="right"> 15993 </td> <td align="right"> 27.46 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> MG </td> <td align="right"> 46216 </td> <td align="right"> 13350 </td> <td align="right"> 28.89 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> ES </td> <td align="right"> 8504 </td> <td align="right"> 2525 </td> <td align="right"> 29.69 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> SC </td> <td align="right"> 18456 </td> <td align="right"> 5783 </td> <td align="right"> 31.33 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> RJ </td> <td align="right"> 28712 </td> <td align="right"> 9531 </td> <td align="right"> 33.20 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> GO </td> <td align="right"> 15288 </td> <td align="right"> 5129 </td> <td align="right"> 33.55 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> RS </td> <td align="right"> 31712 </td> <td align="right"> 10734 </td> <td align="right"> 33.85 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> MT </td> <td align="right"> 9304 </td> <td align="right"> 3322 </td> <td align="right"> 35.71 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> RO </td> <td align="right"> 5120 </td> <td align="right"> 1830 </td> <td align="right"> 35.74 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> AC </td> <td align="right"> 2544 </td> <td align="right"> 983 </td> <td align="right"> 38.64 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> TO </td> <td align="right"> 5944 </td> <td align="right"> 2345 </td> <td align="right"> 39.45 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> RN </td> <td align="right"> 11032 </td> <td align="right"> 4452 </td> <td align="right"> 40.36 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> AM </td> <td align="right"> 11072 </td> <td align="right"> 4594 </td> <td align="right"> 41.49 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> AP </td> <td align="right"> 2552 </td> <td align="right"> 1085 </td> <td align="right"> 42.52 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> PE </td> <td align="right"> 24696 </td> <td align="right"> 10548 </td> <td align="right"> 42.71 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> AL </td> <td align="right"> 11152 </td> <td align="right"> 4837 </td> <td align="right"> 43.37 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> CE </td> <td align="right"> 27968 </td> <td align="right"> 12236 </td> <td align="right"> 43.75 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> SE </td> <td align="right"> 7760 </td> <td align="right"> 3450 </td> <td align="right"> 44.46 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> PB </td> <td align="right"> 14280 </td> <td align="right"> 6475 </td> <td align="right"> 45.34 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> PA </td> <td align="right"> 28368 </td> <td align="right"> 12979 </td> <td align="right"> 45.75 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> BA </td> <td align="right"> 48984 </td> <td align="right"> 23334 </td> <td align="right"> 47.64 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> MA </td> <td align="right"> 30048 </td> <td align="right"> 15114 </td> <td align="right"> 50.30 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> PI </td> <td align="right"> 14464 </td> <td align="right"> 7541 </td> <td align="right"> 52.14 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> RR </td> <td align="right"> 1816 </td> <td align="right"> 994 </td> <td align="right"> 54.74 </td> </tr>
   </table>
<!-- html table generated in R 3.6.1 by xtable 1.8-4 package -->
<!-- Tue Dec 14 10:41:29 2021 -->
<table border=1>
<caption align="top"> NAs in Lower Secondary Schools: 2005 - 2019 </caption>
<tr> <th>  </th> <th> State </th> <th> Number of Observations </th> <th> Number of NAs </th> <th> Percentage of NAs </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> PR </td> <td align="right"> 15320 </td> <td align="right"> 3689 </td> <td align="right"> 24.08 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> MG </td> <td align="right"> 35744 </td> <td align="right"> 9408 </td> <td align="right"> 26.32 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> SP </td> <td align="right"> 46392 </td> <td align="right"> 12837 </td> <td align="right"> 27.67 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> SC </td> <td align="right"> 14224 </td> <td align="right"> 4072 </td> <td align="right"> 28.63 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> GO </td> <td align="right"> 11480 </td> <td align="right"> 3578 </td> <td align="right"> 31.17 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> RJ </td> <td align="right"> 18864 </td> <td align="right"> 6019 </td> <td align="right"> 31.91 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> ES </td> <td align="right"> 6864 </td> <td align="right"> 2211 </td> <td align="right"> 32.21 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> MS </td> <td align="right"> 5224 </td> <td align="right"> 1738 </td> <td align="right"> 33.27 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> PE </td> <td align="right"> 15360 </td> <td align="right"> 5447 </td> <td align="right"> 35.46 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> DF </td> <td align="right"> 1784 </td> <td align="right"> 633 </td> <td align="right"> 35.48 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> AL </td> <td align="right"> 5888 </td> <td align="right"> 2289 </td> <td align="right"> 38.88 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> RO </td> <td align="right"> 3864 </td> <td align="right"> 1507 </td> <td align="right"> 39.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> TO </td> <td align="right"> 4360 </td> <td align="right"> 1710 </td> <td align="right"> 39.22 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> RS </td> <td align="right"> 27504 </td> <td align="right"> 10988 </td> <td align="right"> 39.95 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> SE </td> <td align="right"> 4648 </td> <td align="right"> 1872 </td> <td align="right"> 40.28 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> MT </td> <td align="right"> 7936 </td> <td align="right"> 3286 </td> <td align="right"> 41.41 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> PB </td> <td align="right"> 9096 </td> <td align="right"> 3808 </td> <td align="right"> 41.86 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> RN </td> <td align="right"> 7232 </td> <td align="right"> 3206 </td> <td align="right"> 44.33 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> BA </td> <td align="right"> 29656 </td> <td align="right"> 13364 </td> <td align="right"> 45.06 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> AP </td> <td align="right"> 1464 </td> <td align="right"> 660 </td> <td align="right"> 45.08 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> CE </td> <td align="right"> 24504 </td> <td align="right"> 11154 </td> <td align="right"> 45.52 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> AC </td> <td align="right"> 1800 </td> <td align="right"> 874 </td> <td align="right"> 48.56 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> AM </td> <td align="right"> 8368 </td> <td align="right"> 4192 </td> <td align="right"> 50.10 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> PI </td> <td align="right"> 10824 </td> <td align="right"> 5477 </td> <td align="right"> 50.60 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> PA </td> <td align="right"> 18088 </td> <td align="right"> 9386 </td> <td align="right"> 51.89 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> RR </td> <td align="right"> 1256 </td> <td align="right"> 666 </td> <td align="right"> 53.03 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> MA </td> <td align="right"> 23576 </td> <td align="right"> 12661 </td> <td align="right"> 53.70 </td> </tr>
   </table>

### Conclusion
<div style="color:#4E4C4C; text-align: justify">
As just presented, the reforms in Ceará appear to have had a positive impact in Ceará. The spatial data shows a blue island of good education indexes in the treated state. The time series show a much more inclined line of increase for Ceará, compared to Brazil. Estimating the true impact is, however, more complex because we must account for any source of endogeneity that might be affecting both treatment assignment and the outcome of interest. Predictors of education quality index should also be balanced between control and treatment group. One possible approach to continue this investigation is to apply a fixed effect model. It is also important to investigate whether NAs are associated to poor performing municipalities and how NAs develop before and after intervention. To sum up, this was an informative descriptive analysis of the reforms in the education system of Ceará and a first step to analyse its impacts in student achievement.
</div>

