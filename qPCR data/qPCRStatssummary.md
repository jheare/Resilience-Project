#Quantitative PCR Stats Summary
______

|           | **GABABR** | **PGE/EP4** | **HSPb11** | **BMP2** | **PGRP** | **HSP70** | **H2A** | **H2A.V** | **H3.3** | **TLR** | **Actin** | **CRAF** | **CARM** | **GRB2** | **p29ING** |
|-----------|------------|-------------|------------|----------|----------|-----------|---------|-----------|----------|---------|-----------|----------|----------|----------|------------|
| **H:N C** |            |             | X          |          |          |           |         |           | X        |         |           |          |          |          |            |
| **H:S C** |            |             |            | X        |          |           |         |           |          |         |           | X        | X        |          |            |
| **N:S C** |            |             | X          | X        |          |           |         |           |          |         |           | X        |          |          | X          |
| **H:N T** |            |             |            |          |          |           |         |           | X        |         |           |          |          |          |            |
| **H:S T** |            |             |            |          |          |           |         |           |          |         |           |          |          |          |            |
| **N:S T** |            |             |            |          |          |           |         |           | X        |         |           |          |          |          |            |
| **H C:T** |            | X           |            | X        |          |           | X       |           |          |         | X         |          | X        | X        |            |
| **N C:T** |            | X           |            | X        | X        | X         |         |           | X        |         | X         |          |          | X        |            |
| **S C:T** | X          | X           |            |          |          |           |         |           |          |         | X         |          |          | X        |            |







___
###GABABR
_____

Adjusted Expression Bar Graph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015c/adjexprGABABR.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015c/adjboxplotGABABR.jpeg)

Stats Summary

**- Oyster Bay Treatment/Control Different**


T-TEST FOR OYSTER BAY

 Welch Two Sample t-test

data:  expression by Treat

**t = 5.4509, df = 5.775, p-value = 0.001794**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 9.488834e-13 2.521624e-12

sample estimates:

mean in group C = 2.756637e-12    

mean in group T = 1.021384e-12 

_____
###PGE/EP4
____

Adjusted Expression Bar Graph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015b/adjexprPGEEP4.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015b/adjboxplotPGEEP4.jpeg)

Stats Summary

**- Dabob Treatment/Control Different**

**- Fidalgo Treatment/Control Different**

**- Oyster Bay Treatment/Control Different**


T-TEST for DABOB 

 Welch Two Sample t-test

data:  expression by Treat

**t = 4.1437, df = 7.652, p-value = 0.003565**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 1.669890e-11 5.936667e-11

sample estimates:

mean in group C = 4.545505e-11    
mean in group T = 7.422266e-12 


T-TEST for FIDALGO

 Welch Two Sample t-test

data:  expression by Treat

**t = 2.901, df = 9.418, p-value = 0.01678**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 9.285075e-12 7.308209e-11

sample estimates:

mean in group C = 6.128693e-11       
mean in group T = 2.010334e-11

T-TEST for OYSTER BAY

 Welch Two Sample t-test

data:  expression by Treat

**t = 3.5341, df = 7.296, p-value = 0.008921**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 1.551289e-11 7.672140e-11

sample estimates:

mean in group C = 5.278801e-11    
mean in group T = 6.670868e-12 

___
###HSPb11
___

Adjusted Expression Bar Graph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015/adjexprHSPb11.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7172015/adjboxplotHSPb11.jpeg)

Stats Summary

**- Fidalgo Bay Control Different from Dabob and Oyster Bay Controls**


ONE WAY ANOVA for CONTROL

Call:
   aov(formula = expression ~ Pop, data = rep2res2[Treat == "C"])

Terms:
   Pop    Residuals
Sum of Squares  1.540748e-19 3.284131e-19
Deg. of Freedom            2           21

Residual standard error: 1.250549e-10
Estimated effects may be unbalanced

95% family-wise confidence level

Fit: aov(formula = expression ~ Pop, data = rep2res2[Treat == "C"])

$Pop

   p adj

**N-H   0.0258634**

S-H   0.9672109

**S-N  0.0436003**


____
###BMP2
___

Adjusted Expression Bar Graph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015c/adjexprH2A.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015c/adjboxplotH2A.jpeg)

Stats Summary

**- Oyster Bay Control different from Dabob and Fidalgo Controls**

**- Dabob Treatment/Control Different**

**- Fidalgo Treatment/Control Different**


Call:
   aov(formula = expression ~ Pop, data = rep2res2[Treat == "T"])

Terms:
                         Pop    Residuals
Sum of Squares  5.078040e-22 5.955295e-22
Deg. of Freedom            2           17

Residual standard error: 5.918712e-12

Estimated effects may be unbalanced

Tukey multiple comparisons of means
  95% family-wise confidence level

Fit: aov(formula = expression ~ Pop, data = rep2res2[Treat == "T"])

$Pop

  diff           lwr          upr     p adj

N-H 3.236131e-14 -7.825903e-12 7.890625e-12 0.9999385

S-H 1.165183e-11  2.995838e-12 2.030781e-11 **0.0080928**

S-N 1.161946e-11  2.728854e-12 2.051008e-11 **0.0100037**


Welch Two Sample t-test

data:  expression by Treat

**t = 3.2998, df = 7.106, p-value = 0.01284**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 9.001428e-12 5.403912e-11

sample estimates:

mean in group C = 3.58527e-11     
mean in group T =  4.33243e-12

Welch Two Sample t-test

data:  expression by Treat

**t = 6.5504, df = 7.87, p-value = 0.0001918**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 2.770679e-11 5.794744e-11

sample estimates:

mean in group C = 4.719190e-11     
mean in group T = 4.364791e-12

___
###PGRP
___

Adjusted Expression Bargraph 

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7162015b/adjexprPGRP.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7162015b/adjboxplotPGRP.jpeg)

Stats Summary

**- Fidalgo Treatment/Control Different**

T-Test for FIDALGO

Welch Two Sample t-test

data:  expression by Treat

**t = 2.3858, df = 6.45, p-value = 0.05145**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 -1.839971e-15  4.349814e-13

sample estimates:

mean in group C = 2.848375e-13    
mean in group T =  6.826683e-14


___
###HSP70
___

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7162015/adjexprHSP70.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7162015/adjboxplotHSP70.jpeg)

Stats Summary

**- Fidalgo Treatment/Control Different**

T-TEST for FIDALGO

Welch Two Sample t-test

data:  expression by Treat

**t = 2.4769, df = 9.628, p-value = 0.03356**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 2.343991e-12 4.663070e-11

sample estimates:

mean in group C = 3.25748e-11     
mean in group T = 8.08745e-12


____
###H2A
____

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015c/adjexprH2A.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015c/adjboxplotH2A.jpeg) 

Stats Summary

**- Dabob Treatment/Control Different**

T-TEST for DABOB

Welch Two Sample t-test

data:  expression by Treat

**t = -2.5837, df = 8.363, p-value = 0.03128**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 -2.099982e-11 -1.271558e-12

sample estimates:

mean in group C = 4.129758e-12     
mean in group T = 1.526545e-11

____
###H2A.V
_____

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015b/adjexprH2AV.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015b/adjboxplotH2AV.jpeg)

Stats Summary

No Significant Differences found using Two Way ANOVA, One Way ANOVA, and T-Tests

____
###H3.3
____

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015/adjexprH33.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7152015/adjboxplotH33.jpeg)

Stats Summary

**- Fidalgo Control Different than Dabob Control**

**- Fidalgo Treatment Different than Oyster Bay and Dabob Treatment**

**- Fidalgo Control/Treatment Different**

Call:
aov(formula = expression ~ Pop, data = rep2res2[Treat == "C"])

Terms:
                         Pop    Residuals

Sum of Squares  1.897027e-17 4.628055e-17

Deg. of Freedom            2           21

Residual standard error: 1.484532e-09

Estimated effects may be unbalanced

Tukey multiple comparisons of means

95% family-wise confidence level

Fit: aov(formula = expression ~ Pop, data = rep2res2[Treat == "C"])

Pop

diff           lwr          upr     p adj

**N-H  1.991789e-09  1.208552e-10 3.862723e-09 0.0356128**

S-H  2.333278e-10 -1.637606e-09 2.104262e-09 0.9471240

S-N -1.758461e-09 -3.629395e-09 1.124726e-10 0.0679234


aov(formula = expression ~ Pop, data = rep2res2[Treat == "T"])

Terms:
 
Pop    Residuals

Sum of Squares  3.946652e-18 9.305185e-18

Deg. of Freedom            2           21

Residual standard error: 6.656606e-10

Estimated effects may be unbalanced

Tukey multiple comparisons of means

95% family-wise confidence level

Fit: aov(formula = expression ~ Pop, data = rep2res2[Treat == "T"])

$Pop

diff           lwr           upr     p adj

**N-H -8.886447e-10 -1.727567e-09 -4.972258e-11 0.0366367**

S-H -5.996542e-11 -8.988876e-10  7.789567e-10 0.9822790

**S-N  8.286793e-10 -1.024284e-11  1.667601e-09 0.0532511**

T-TEST for FIDALGO

Welch Two Sample t-test

data:  expression by Treat

**t = 2.9287, df = 7.12, p-value = 0.02165**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 4.908920e-10 4.534766e-09

sample estimates:

mean in group C = 2.759142e-09     
mean in group T = 2.463125e-10


_____
###Elongation Factor
____

No amplification in qPCR. Nothing to analyze. 


_____
###TLR
_____

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7132015b/adjexprTLR.jpeg)

Adjusted Expression Boxplot 

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7132015b/adjboxplotTLR.jpeg)

No significant differences 

_____
###Actin
____
Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/762015b/adjexprActin.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/762015b/adjboxplotActin.jpeg)

Stat Summary

**- Dabob Treatment/Control Different**

**- Fidalgo Treatment/Control Different**

**- Oyster Bay Treatment/Control Different**


T-Test for DABOB

Welch Two Sample t-test

data:  avgexpr by treat

**t = 2.2989, df = 8.507, p-value = 0.04869**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 9.190714e-12 2.535167e-09

sample estimates:

mean in group C = 2.161641e-09     
mean in group T = 8.894622e-10

T-TEST for FIDALGO

Welch Two Sample t-test

data:  avgexpr by treat

**t = 2.3056, df = 7.506, p-value = 0.0521**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 -3.957277e-11  6.776319e-09

sample estimates:

mean in group C = 4.339249e-09     
mean in group T = 9.708765e-10

T-Test for Oyster Bay

Welch Two Sample t-test

data:  avgexpr by treat

**t = 2.3952, df = 7.81, p-value = 0.04423**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 5.737458e-11 3.402503e-09

sample estimates:

mean in group C = 2.352334e-09     
mean in group T = 6.223954e-10


____
###CRAF
____
Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/772015b/adjexprCRAF.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/772015b/adjboxplotCRAF.jpeg)

Stats Summary

**- Oyster Bay Control Different than Dabob and Fidalgo**


One Way ANOVA comparing Population Controls


 pop    Residuals

Sum of Squares  2.718845e-20 4.549584e-20

Deg. of Freedom            2           21

Residual standard error: 4.654534e-11

Estimated effects may be unbalanced

95% family-wise confidence level

$pop

diff           lwr          upr     p adj

N-H 7.918367e-12 -5.074203e-11 6.657877e-11 0.9383664

S-H 7.502823e-11  1.636783e-11 1.336886e-10 **0.0108836**

S-N 6.710986e-11  8.449462e-12 1.257703e-10 **0.0231716**

________
###CARM
________

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/782015b/adjexprCARM.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/782015b/adjboxplotCARM.jpeg)

Stats Summary

**- Oyster Bay Control different than Dabob Control**

**- Dabob Treatment/Control Different**


One Way ANOVA comparing Population Controls

pop    Residuals

Sum of Squares  6.735418e-21 8.077772e-21

Deg. of Freedom            2           21

Residual standard error: 1.961264e-11

Estimated effects may be unbalanced

95% family-wise confidence level

$pop

diff           lwr          upr     p adj

N-H 2.145712e-11 -3.260400e-12 4.617465e-11 0.0966125

**S-H 4.102022e-11  1.630270e-11 6.573775e-11 0.0011709**

S-N 1.956310e-11 -5.154424e-12 4.428062e-11 0.1381516




T-Test for Dabob Population

Welch Two Sample t-test

data:  avgexpr by treat

**t = -2.6739, df = 7.362, p-value = 0.03038**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 -1.254640e-10 -8.322195e-12

sample estimates:

mean in group C = 1.519137e-11     
mean in group T = 8.208448e-11

________
###GRB2
______

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7202015/adjexprGRB2.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7202015/adjboxplotGRB2.jpeg)

Stats Summary

**- Dabob Treatment/Control Different**

**- Fidalgo Treatment/Control Different**

**- Oyster Bay Treatment/Control Different**


T-Test for DABOB

Welch Two Sample t-test

data:  expression by Treat

**t = 2.9537, df = 7.1, p-value = 0.02095**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 4.449457e-11 3.966833e-10

sample estimates:

mean in group C = 2.429139e-10     
mean in group T = 2.232492e-11

T-Test for FIDALGO

Welch Two Sample t-test

data:  expression by Treat

**t = 2.6905, df = 9.489, p-value = 0.02369**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 2.685431e-11 2.971352e-10

sample estimates:

mean in group C = 2.284042e-10     
mean in group T = 6.640939e-11

T-Test for Oyster Bay

Welch Two Sample t-test

data:  expression by Treat

**t = 3.8083, df = 7.192, p-value = 0.006313**

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
 6.887209e-11 2.913015e-10

sample estimates:

mean in group C =  1.989093e-10    
mean in group T =  1.882251e-11

______
###p29ING
______

Adjusted Expression Bargraph

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7202015b/adjexprp29ING.jpeg)

Adjusted Expression Boxplot

![](https://github.com/jheare/Resilience-Project/blob/master/qPCR%20data/7202015b/adjboxplotp29ING.jpeg)

Stats Summary

**- Oyster Bay Control Different than Fidalgo Control**

One Way ANOVA comparing Population Controls 

Pop    Residuals

Sum of Squares  1.026249e-21 2.641735e-21

Deg. of Freedom            2           21

Residual standard error: 1.121592e-11

Estimated effects may be unbalanced

Tukey multiple comparisons of means

95% family-wise confidence level


$Pop

diff           lwr          upr     p adj

N-H  0.1997946

S-H   0.5595058

**S-N   0.0263669**
