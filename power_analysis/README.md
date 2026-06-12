
The power analysis uses a series of suppositions based on limited literature on humor effects
See [Walter et al. 2018](https://doi.org/10.1093/hcr/hqy005) and [Burgers & Brugman, 2022](https://doi.org/10.1177/00936502211032100)

We assume a cross-over effect interaction with d=0.242

The ideal number would be 270 per between factor, for a total of 540 participants.
But budget is *l i m i t e d*, the maximum I can pay is for 500 participants.
So let's see how statistical power for each effect with that number

| | power | effect_size |
| ---|--- |--- |
|anova_claves |86.5 | 0.021|
|anova_tema |99.9 | 0.057 |
|anova_claves:tema |79.7 |0.0168 |

Power and Effect sizes for ANOVA tests
                  power effect_size
anova_claves       86.5     0.02121
anova_tema         99.9     0.05741
anova_claves:tema  79.7     0.01669



# Power curves as effect size decreases

|scale |claves |tema |interaction | 
| ---| ---|--- |--- | 
|0.0 |6.1 | 4.8|6.9 | 
|0.1 |6.4 |8.5 |5.9 | 
|0.2 |8.5 |19.1 |10 | 
|0.3 |16.1 |36 |12.6 | 
|0.4 |22.9 |54.8 |18.3 | 
|0.5 |34.9 |77.2 |27.4 | 
|0.6 |48.8 |89.7 |35.7 | 
|0.7 |57.2 |96.7 |47.2 | 
|0.8 |70.2 |99.6 |57.7 | 
|0.9 |77.6 |99.7 |66.7 |




   scale claves tema interaction
1    0.0    6.1  4.8         6.9
2    0.1    6.4  8.5         5.9
3    0.2    8.5 19.1        10.0
4    0.3   16.1 36.0        12.6
5    0.4   22.9 54.8        18.3
6    0.5   34.9 77.2        27.4
7    0.6   48.8 89.7        35.7
8    0.7   57.2 96.7        47.2
9    0.8   70.2 99.6        57.7
10   0.9   77.6 99.7        66.7