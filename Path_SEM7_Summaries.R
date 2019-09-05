#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
#Install packages if not done so yet:
#Load packages if installed on our comp:
library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")

#APRIORI MODEL======
#This is our Apriori Path Analysis Model (for latter backward selection)
#Tested agaisnt CRMS data:
Apriori_Model <-'
#regressions:
NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien
Alien       ~ Depth + Flood  + Soil 
NatComp     ~ Depth + Flood  + Soil + Alien + NatRich + Native

#covariances:
NatRich ~~ Native
NatComp ~~ NatRich
Native  ~~  NatComp
'
fit_Apriori_Model <- sem(Apriori_Model,missing="direct",estimator="ML",data=data)
summary(fit_Apriori_Model)

#Freshwater SEM:=========
#Load data directly from previosly saved "Freshwater_Data4SEM.csv" (See Path_SEM4 for details)
Freshwater_Data <- read.csv("Freshwater_Data4SEM.csv")
#Best fit model follwoing backward selection on Apriori Model (see line 15):
model_Freshwater <- '
#regressions:
NatRich     ~ Depth + Flood     
Native      ~ Depth + Flood  + Soil + Alien
NatComp     ~ Soil + Alien 

#covariances:
NatComp ~~ 0*NatRich
Native  ~~ 0*NatComp
'
fit_Freshwater <- sem(model_Freshwater,missing="direct",estimator="ML",data=Freshwater_Data)
#Produce SemPaths FIGURE with Nodes:
semPaths(fit_Freshwater ,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,edge.labels=FALSE,
         edge.label.position = 0.2, nCharNodes=6,
         residuals =  F, exoCov = F, 
         edge.label.bg = "lightyellow",
         legend = F)
title("Freshwater path analysis (2007-2017, P < 0.05)", line =2)


summary(fit_Freshwater, fit.measures=TRUE, rsquare=T) 
#lavaan 0.6-3 ended normally after 24 iterations
#Optimization method                           NLMINB
Number of free parameters                         15

Number of observations                            42
Number of missing patterns                         1

Estimator                                         ML
Model Fit Test Statistic                       7.524
Degrees of freedom                                 6
P-value (Chi-square)                           0.275

Model test baseline model:
  
  Minimum Function Test Statistic               78.930
Degrees of freedom                                15
P-value                                        0.000

User model versus baseline model:
  
  Comparative Fit Index (CFI)                    0.976
Tucker-Lewis Index (TLI)                       0.940

Loglikelihood and Information Criteria:
  
  Loglikelihood user model (H0)               -141.565
Loglikelihood unrestricted model (H1)       -137.803

Number of free parameters                         15
Akaike (AIC)                                 313.131
Bayesian (BIC)                               339.196
Sample-size adjusted Bayesian (BIC)          292.223

Root Mean Square Error of Approximation:
  
  RMSEA                                          0.078
90 Percent Confidence Interval          0.000  0.225
P-value RMSEA <= 0.05                          0.339

Standardized Root Mean Square Residual:
  
  SRMR                                           0.050

Parameter Estimates:
  
  Information                                 Observed
Observed information based on                Hessian
Standard Errors                             Standard

Regressions:
  Estimate  Std.Err  z-value  P(>|z|)
NatRich ~                                           
  Depth            -1.110    0.315   -3.527    0.000
Flood             0.950    0.315    3.018    0.003
Native ~                                            
  Depth            -1.092    0.240   -4.546    0.000
Flood             0.870    0.233    3.740    0.000
Soil             -0.355    0.099   -3.570    0.000
Alien            -0.507    0.094   -5.381    0.000
NatComp ~                                           
  Soil              0.439    0.134    3.262    0.001
Alien             0.400    0.134    2.974    0.003

Covariances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich ~~                                          
  .NatComp           0.000                           
.Native ~~                                           
  .NatComp           0.000                           
.NatRich ~~                                          
  .Native            0.198    0.090    2.205    0.027

Intercepts:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich          -0.000    0.134   -0.000    1.000
.Native            0.000    0.094    0.000    1.000
.NatComp          -0.000    0.130   -0.000    1.000

Variances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich           0.751    0.164    4.583    0.000
.Native            0.373    0.082    4.536    0.000
.NatComp           0.706    0.154    4.583    0.000

R-Square:
  Estimate
NatRich           0.231
Native            0.590
NatComp           0.276


#Intermediate SEM ========
#You can load data directly from previosly saved "Intermediate_Data4SEM.csv" (See Path_SEM4 for details)
Intermediate_Data <- read.csv("Intermediate_Data4SEM.csv")
#Pick Best fit model following backward selection on Apriori Model (see line 15):
model_Intermediate2 <- '
#regressions:
NatRich     ~  Soil 
Native      ~  Soil + Alien
Alien       ~  Soil 
NatComp     ~  Soil + NatRich 

#covariances:
NatComp ~~ NatRich
Native ~~ NatComp
Native ~~ NatRich
'
fit_Intermediate2 <- sem(model_Intermediate2,missing="direct",estimator="ML",data=Intermediate_Data)

semPaths(fit_Intermediate2,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F)
title("Intermediate  path analysis (2007-2017, P<0.05)")

summary(fit_Intermediate2, fit.measures=TRUE, rsquare=T) 
#lavaan 0.6-3 ended normally after 21 iterations
#Optimization method                           NLMINB
Number of free parameters                         17

Number of observations                            56
Number of missing patterns                         1

Estimator                                         ML
Model Fit Test Statistic                       2.761
Degrees of freedom                                 1
P-value (Chi-square)                           0.097

Model test baseline model:
  
  Minimum Function Test Statistic              102.865
Degrees of freedom                                10
P-value                                        0.000

User model versus baseline model:
  
  Comparative Fit Index (CFI)                    0.981
Tucker-Lewis Index (TLI)                       0.810

Loglikelihood and Information Criteria:
  
  Loglikelihood user model (H0)               -265.773
Loglikelihood unrestricted model (H1)       -264.392

Number of free parameters                         17
Akaike (AIC)                                 565.545
Bayesian (BIC)                               599.976
Sample-size adjusted Bayesian (BIC)          546.546

Root Mean Square Error of Approximation:
  
  RMSEA                                          0.177
90 Percent Confidence Interval          0.000  0.442
P-value RMSEA <= 0.05                          0.120

Standardized Root Mean Square Residual:
  
  SRMR                                           0.042

Parameter Estimates:
  
  Information                                 Observed
Observed information based on                Hessian
Standard Errors                             Standard

Regressions:
  Estimate  Std.Err  z-value  P(>|z|)
NatRich ~                                           
  Soil             -0.456    0.119   -3.831    0.000
Native ~                                            
  Soil             -0.585    0.110   -5.326    0.000
Alien            -0.353    0.093   -3.784    0.000
Alien ~                                             
  Soil             -0.321    0.127   -2.541    0.011
NatComp ~                                           
  Soil              0.379  299.844    0.001    0.999
NatRich          -0.421  657.936   -0.001    0.999

Covariances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich ~~                                          
  .NatComp           0.039  511.978    0.000    1.000
.Native ~~                                           
  .NatComp          -0.120  239.556   -0.001    1.000
.NatRich ~~                                          
  .Native            0.364    0.105    3.453    0.001

Intercepts:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich          -0.000    0.118   -0.000    1.000
.Native            0.000    0.105    0.000    1.000
.Alien             0.000    0.125    0.000    1.000
.NatComp           0.000    0.100    0.000    1.000

Variances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich           0.778    0.147    5.292    0.000
.Native            0.614    0.116    5.269    0.000
.Alien             0.881    0.166    5.292    0.000
.NatComp           0.557   50.819    0.011    0.991

R-Square:
  Estimate
NatRich           0.208
Native            0.348
Alien             0.103
NatComp           0.433



#Brackish SEM=========
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Brackish_Data4SEM.csv"
Brackish_Data <- read.csv("Brackish_Data4SEM.csv")

#Best fit model follwoing backward selection on Apriori Model (see line 15):
Brackish_Data <- read.csv("Brackish_Data4SEM.csv")
model_Brackish3 <- '
#regressions:
NatRich     ~  Depth  + Soil
Native      ~  Depth 
NatComp     ~  Soil

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
NatRich ~~ 0*Native
'
fit_Brackish3 <- sem(model_Brackish3,missing="direct",estimator="ML",data=Brackish_Data)

#Plot Brackish SEM:
semPaths(fit_Brackish3,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         legend = F)
title("Brackish path analysis (2007-2017, P<0.05)")

summary(fit_Brackish3, fit.measures=TRUE, rsquare=T) 
#lavaan 0.6-3 ended normally after 11 iterations
#Optimization method                           NLMINB
Number of free parameters                         10

Number of observations                            40
Number of missing patterns                         1

Estimator                                         ML
Model Fit Test Statistic                       6.831
Degrees of freedom                                 5
P-value (Chi-square)                           0.233

Model test baseline model:
  
  Minimum Function Test Statistic               35.086
Degrees of freedom                                 9
P-value                                        0.000

User model versus baseline model:
  
  Comparative Fit Index (CFI)                    0.930
Tucker-Lewis Index (TLI)                       0.874

Loglikelihood and Information Criteria:
  
  Loglikelihood user model (H0)               -154.626
Loglikelihood unrestricted model (H1)       -151.211

Number of free parameters                         10
Akaike (AIC)                                 329.253
Bayesian (BIC)                               346.141
Sample-size adjusted Bayesian (BIC)          314.849

Root Mean Square Error of Approximation:
  
  RMSEA                                          0.096
90 Percent Confidence Interval          0.000  0.254
P-value RMSEA <= 0.05                          0.287

Standardized Root Mean Square Residual:
  
  SRMR                                           0.082

Parameter Estimates:
  
  Information                                 Observed
Observed information based on                Hessian
Standard Errors                             Standard

Regressions:
  Estimate  Std.Err  z-value  P(>|z|)
NatRich ~                                           
  Depth            -0.474    0.131   -3.629    0.000
Soil             -0.301    0.131   -2.304    0.021
Native ~                                            
  Depth            -0.438    0.142   -3.078    0.002
NatComp ~                                           
  Soil              0.325    0.150    2.171    0.030

Covariances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich ~~                                          
  .NatComp           0.000                           
.Native ~~                                           
  .NatComp           0.000                           
.NatRich ~~                                          
  .Native            0.000                           

Intercepts:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich           0.000    0.129    0.000    1.000
.Native            0.000    0.140    0.000    1.000
.NatComp          -0.000    0.148   -0.000    1.000

Variances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich           0.665    0.149    4.472    0.000
.Native            0.788    0.176    4.472    0.000
.NatComp           0.872    0.195    4.472    0.000

R-Square:
  Estimate
NatRich           0.318
Native            0.192
NatComp           0.105
#Saline SEM===========
#You can load data directly from previosly saved "Saline_Data4SEM.csv"
Saline_Data <- read.csv("Saline_Data4SEM.csv")
#Compare models to find best fit: (Best fit model follwoing backward selection on Apriori Model (see line 15))
model_Saline <- '
#regressions:
#regressions:
NatRich     ~ Depth + Soil 
Native      ~ Depth + Soil 
Alien       ~ Depth + Soil 
NatComp     ~  NatRich + Native

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
'
fit_Saline <- sem(model_Saline,missing="direct",estimator="ML",data=Saline_Data)

semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F)
title("Saline path analysis (2007-2017, P<0.05)")

summary(fit_Saline, fit.measures=TRUE, rsquare=T)
##avaan 0.6-3 ended normally after 19 iterations
#Optimization method                           NLMINB
#Number of free parameters                         17

Number of observations                            64
Number of missing patterns                         1

Estimator                                         ML
Model Fit Test Statistic                      13.935
Degrees of freedom                                 5
P-value (Chi-square)                           0.016

Model test baseline model:
  
  Minimum Function Test Statistic              159.656
Degrees of freedom                                14
P-value                                        0.000

User model versus baseline model:
  
  Comparative Fit Index (CFI)                    0.939
Tucker-Lewis Index (TLI)                       0.828

Loglikelihood and Information Criteria:
  
  Loglikelihood user model (H0)               -288.372
Loglikelihood unrestricted model (H1)       -281.404

Number of free parameters                         17
Akaike (AIC)                                 610.744
Bayesian (BIC)                               647.445
Sample-size adjusted Bayesian (BIC)          593.941

Root Mean Square Error of Approximation:
  
  RMSEA                                          0.167
90 Percent Confidence Interval          0.066  0.274
P-value RMSEA <= 0.05                          0.033

Standardized Root Mean Square Residual:
  
  SRMR                                           0.051

Parameter Estimates:
  
  Information                                 Observed
Observed information based on                Hessian
Standard Errors                             Standard

Regressions:
  Estimate  Std.Err  z-value  P(>|z|)
NatRich ~                                           
  Depth            -0.212    0.109   -1.958    0.050
Soil             -0.443    0.109   -4.080    0.000
Native ~                                            
  Depth            -0.386    0.069   -5.567    0.000
Soil             -0.725    0.069  -10.466    0.000
Alien ~                                             
  Depth             0.259    0.116    2.236    0.025
Soil             -0.295    0.115   -2.554    0.011
NatComp ~                                           
  NatRich           0.482    0.113    4.264    0.000
Native            0.296    0.112    2.632    0.008

Covariances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich ~~                                          
  .NatComp           0.000                           
.Native ~~                                           
  .NatComp           0.000                           
.Alien ~~                                            
  .NatComp          -0.069    0.086   -0.795    0.427

Intercepts:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich          -0.000    0.108   -0.000    1.000
.Native           -0.000    0.069   -0.000    1.000
.Alien             0.000    0.115    0.000    1.000
.NatComp           0.000    0.090    0.000    1.000

Variances:
  Estimate  Std.Err  z-value  P(>|z|)
.NatRich           0.741    0.131    5.657    0.000
.Native            0.302    0.053    5.657    0.000
.Alien             0.843    0.149    5.656    0.000
.NatComp           0.516    0.091    5.653    0.000

R-Square:
  Estimate
NatRich           0.247
Native            0.693
Alien             0.148
NatComp           0.455

