---
title: "Assignment 5"
author: "Mia Guarnieri, Lauren Harris, Wade Sedgwick"
date: "2023-05-08"
output:
  pdf_document: default
  html_document:
    code_folding: hide
---



## Source the function

```r
source(here("R", "Catm.r"))
```

## Create the random sample using lhs

```r
# set the seed so it is random
set.seed(1)

# designate parameters
pnames = c("height", "k_d", "k_o", "v")

# how many parameters
npar =  length(pnames)

# how many samples
nsample = 100

# generate samples using lhs function
parm_quant = randomLHS(nsample, npar)

# rename columns to match variables
colnames(parm_quant) = pnames 

# creating parameter dataframe
parm = as.data.frame(matrix(nrow=nrow(parm_quant), ncol=ncol(parm_quant)))
colnames(parm) = pnames

# generating sample distributions for our parameters
parm[,"height"] = qunif(parm_quant[,"height"], min = 9.5, max = 10.5)
parm[,"v"] = qnorm(parm_quant[,"v"], mean = 250, sd = 30)
parm[,"k_d"] = qnorm(parm_quant[,"k_d"], mean = 0.7, sd = 0.01)
parm[,"k_o"] = qnorm(parm_quant[,"k_o"], mean = 0.1, sd = 0.01)
```

## Run model for parameter sets


```r
# run the model
cond = parm %>% pmap(Catm)

# turn results in to a dataframe for easy display/analysis
condsd <- as.data.frame(unlist(cond))
colnames(condsd) = "ca"
```

## Plot


```r
# add uncertainty bounds on our estimates
ggplot(condsd) +
  geom_boxplot(aes(y = ca)) +
  labs(y="Conductance (mm/s)",
       title = "Conductance Uncertainty") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
```

![](assignment_4_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```r
# cumulative distribution
ggplot(condsd, aes(y = ca)) +
  stat_ecdf() +
  labs(y = "Conductance (mm/s)",
       x = "Empirical Cumulative Distribution Function",
       title = "Cumulative Conductance Distribution")
```

![](assignment_4_files/figure-latex/unnamed-chunk-4-2.pdf)<!-- --> 

```r
# plot parameter sensitivity
tmp = cbind.data.frame(condsd, parm)

tmp2 = tmp %>% gather(-ca, key="parm", value="parmvalue")

ggplot(tmp2, aes(parmvalue, ca)) +
  geom_point() +
  facet_wrap(~ parm, scales="free", ncol = 4) +
  scale_x_continuous(n.breaks = 4) +
  labs(x = "Parameter",
       y = "Conductance (mm/s)",
       title = "Parameter Sensitivity")
```

![](assignment_4_files/figure-latex/unnamed-chunk-4-3.pdf)<!-- --> 

## Quantify sensitivity - pcc


```r
# calculate and plot partial rank correlation coefficients
senresult_rank = pcc(parm, condsd$ca, rank=TRUE )

plot(senresult_rank)
```

![](assignment_4_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

```r
# extract prcc values from pcc object

parm_prcc <- as.data.frame(senresult_rank$PRCC)

rownames(parm_prcc) = c("Height", "Zero Plane Displacement (k_d)", "Roughness (k_o)", "Windspeed (v)")

parm_prcc_table <- parm_prcc %>%
  kable(digits = 3,
        col.names = "Partial Rank Correlation Coefficient") %>% 
  kable_styling(full_width = FALSE) %>% 
  kable_paper()

parm_prcc_table
```

\begin{table}
\centering
\begin{tabular}{l|r}
\hline
  & Partial Rank Correlation Coefficient\\
\hline
Height & 0.429\\
\hline
Zero Plane Displacement (k\_d) & -0.100\\
\hline
Roughness (k\_o) & 0.867\\
\hline
Windspeed (v) & 0.988\\
\hline
\end{tabular}
\end{table}

## Discussion

Wind velocity has the highest correlation with atmospheric conductance. To reduce uncertainty in atmospheric conductance estimates, getting more accurate and precise wind speeds should be the highest priority. The height of the vegetation and roughness of terrain also contribute to atmospheric conductance, but have lower correlation coefficients than wind velocity, and improving the accuracy of these measurements could help as well. The zero plane displacement had the lowest correlation with atmospheric conductance. A higher conductance rate results in more evaporation and higher water use needs. As climate change gets worse, it is possible that higher wind speeds will become more frequent and will lead to higher atmospheric conductance, since conductance is most sensitive to changes in wind velocity. More variable weather events will likely lead to more variable conductance, resulting in less certainty in conductance estimates and water use needs.



