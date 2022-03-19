# Burnout-measures-correlation

Short url: https://ebmgt.github.io/well-being_measurement/

**Methods:** Details of the studies included are in the:
* [R code](../master/files/code)

**Results:** Details of the studies included are in the:
* [Plots](../master/files/plots) ([spreadsheets with source data](files/data))

## Abstract (SGIM National Meeting. Orlando, FL 04/2022)

**Introduction: **

Healthcare workers report high levels of burnout that affect both their well-being and the well-being of their patients via the quality of care they receive. Unfortunately, measurement of burnout with the gold-standard Maslach-Burnout Inventory (MBI) is limited by costs, copyrights, and length of surveys. We aim to create an equation to convert group rates of burnout as measured by the single-item Mini-Z to estimated rates measured by the MBI.  

**Methods: **

We searched for studies that reported correlations between the Mini-Z and the MBI, including overall correlation as well as the correlations between Mini-Z and the subcategories of the MBI: emotional exhaustion and depersonalization.  

We meta-analyzed correlations between measures of the MBI and its subscales with the Mini-Z single item using the metacor function of the R package meta. We considered a correlation of 0.7 as acceptable per Cronbach. 

We meta-analyzed across studies the burnout rates and correlation of the Mini-Z single item with the full MBI by using a linear mixed-effects model with the lmer function of the R package lme4. We used the intercept and coefficient from the mixed-effects model to derive an equation for predicting the MBI from the Mini-Z single item. Using this model, we determined the correction coefficient (R) and two proportions of the variance explained (R2) by the model. T For R2, we used the R package Report to calculate both the total (conditional) R2 and the fixed (marginal) R2. 

**Results:  **

We included 9 studies with a total of 11,376 respondents. The response rate across studies ranged from 7% to 74% with a pooled rate of 33% 

The correlations of the Mini-Z single item with the MBI subscales were: MBI-EE 0.70 (0.65 - 0.74; I2 = 96%) and MBI-DP 0.48 (0.38 - 0.57; I2 = 89%). 

The correlations (R) and proportions of the variance explained (R2) of the Mini-Z single item with the overall MBI were: total (conditional) R2 = 0.70 and marginal (fixed) R2 = 0.56. The marginal R2 leads to a correlation between the Mini-Z single item and full MBI of 0.56.  

The equation to convert the rate of burnout at an organization using the Mini-Z question to the complete MBI is Mini-Z rate times plus 14.8 + 0.927. The 95% confidence intervals are + 13. The R and R2, of the mixed effects model were 0.73 and 0.85, respectively. 

**Conclusions: **

The Mini-Z has significant, adequate reliability for predicting MBI:EE. The Mini-Z has significant, but inadequate reliability to predict the MBI:DP and full MBI, likely because the Mini-Z statements do not conceptually map for depersonalization. Stanford’s Professional Fulfillment Index PFI) and the Copenhagen Burnout Inventory both have scales that target depersonalization and could be combined with the Mini-Z. In order to improve overall measure of burnout, we encourage collaboration among survey developers to create an adequate survey with the fewest questions.  
