
The power analysis uses a series of suppositions based on limited literature on humor effects
See [Walter et al. 2018](https://doi.org/10.1093/hcr/hqy005) and [Burgers & Brugman, 2022](https://doi.org/10.1177/00936502211032100)

We assume a cross-over effect interaction with d=0.242

The ideal number would be 270 per between factor, for a total of 540 participants.
But budget is *l i m i t e d*, and the maximum I can pay is for 500 participants.
So let's see how statistical power for each effect with that number

| | power | effect_size |
| ---|--- |--- |
|anova_claves |86.5 | 0.021|
|anova_tema |99.9 | 0.057 |
|anova_claves:tema |79.7 |0.0168 |

## With-in Subject
In a Within-Subjects factor, the same people are measured twice. 
Because you set the correlation between these measurements to r = 0.5, the math "cancels out" a lot of the background noise (individual differences) for that factor

- .anova_tema (Within-Subjects): Because it's within-subjects, the error term (the denominator when calculating variance explained) is much smaller. A smaller denominator means the proportion of variance explained ($\eta_p^2$) shoots way up to $0.057$ ($5.7\%$)
- .anova_claves (Between-Subjects): Since different people are in different groups, you can't cancel out individual differences. The error term is much larger, so the exact same raw mean difference explains less overall variance: $0.021$ ($2.1\%$


### Summary
- tema ($5.7\%$) wins because within-subject tracking removes individual noise
-.claves ($2.1\%$) is lower because between-subject variance includes all individual differences
-.interaction ($1.6\%$) is the lowest because interaction metrics measure cell-by-cell deviations, which naturally distribute variance differently than main effects.


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



 