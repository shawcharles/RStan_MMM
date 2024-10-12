
# RStan Marketing Mix Modelling (MMM)

## Overview
This repository contains R code for implementing a Bayesian Marketing Mix Model (MMM) using RStan. The model assesses the impact of various marketing channels on sales, incorporating advanced statistical techniques to account for adstock, diminishing returns, and other complex behaviours of marketing spend.

## Contents
- [Data Preparation](#data-preparation)
- [Adstock Transformation](#adstock-transformation)
- [Control Model](#control-model)
- [Marketing Mix Model](#marketing-mix-model)
- [Diminishing Return Model](#diminishing-return-model)
- [Model Evaluation and Interpretation](#model-evaluation-and-interpretation)
- [Visualization](#visualization)
- [Optimization Insights](#optimization-insights)
- [Running the Code](#running-the-code)
- [Contact](#contact)

## Data Preparation
The data preparation phase involves loading and cleaning sales and marketing data. Key steps include:
- Loading historical sales and marketing spend data.
- Defining variables for media impressions, spending, and external control factors that might affect sales.

## Adstock Transformation
Adstock transformation is used to model the carry-over effects of marketing activities over time. It includes:
- Implementing a decay function to reflect how past marketing expenditures continue to influence sales over varying time periods.

## Control Model
A control model establishes a baseline of sales by modelling the influence of non-marketing variables such as:
- Economic factors, seasonality, and other external influences.
- Using Bayesian methods to estimate the base level of sales, accounting for observed variabilities.

## Marketing Mix Model
The core of the analysis, this model integrates media spending data with the control model to:
- Evaluate the direct impact of each marketing channel on sales.
- Incorporate adstock transformations and diminishing returns within a Bayesian inference framework.

### Model Equations
1. **Adstock Transformation**:
   ```
   x_t* = Adstock(x_t, ..., x_{t-L+1}; L, P, D) = ∑_{l=0}^{L-1} w_{t-l} * x_{t-l} / ∑_{l=0}^{L-1} w_{t-l}
   ```
   where `w_{t-l} = D^{(l-P)^2}` and:
   - `L` is the maximum lag (length of media impact)
   - `P` is the peak effect delay
   - `D` is the decay rate

2. **Media Spend Saturation (Hill Function)**:
   ```
   Hill(x; K, S) = β * x / (1 + (x/K)^-S)
   ```
   - `β` is the maximum potential effect of the media channel
   - `K` is the half-saturation point
   - `S` is the shape parameter, describing the steepness of the curve

3. **Sales Model**:
   ```
   log(y) = τ + β_{TV} * log(x*_{TV}) + β_{SEM} * log(x*_{SEM}) + ... + β_{ctrl} * log(x_{ctrl})
   ```
   - `τ` is the model intercept
   - `β_{media}` are the coefficients for media channels
   - `x*_{media}` are the adstocked media variables
   - `x_{ctrl}` are control variables (e.g., price, promotions)

### Parameters and Priors
| Parameter       | Prior                     | Description                    |
|-----------------|---------------------------|--------------------------------|
| `L_max_lag`     | 8                         | Length of media impact         |
| `P_peak`        | uniform(0, max_lag/2)     | Peak/delay of media impact     |
| `D_decay`       | beta(3, 3)                | Decay/retain rate of media     |
| `β_beta`        | half normal(0, 1)         | Coefficients for media         |
| `τ_tau`         | normal(0, 5)              | Intercept                      |
| `K_ec`          | beta(2, 2)                | Half saturation point          |
| `S_slope`       | gamma(3, 1)               | Shape of the Hill function     |
| `Residual noise_var` | inverse gamma(0.05, 0.05 * 0.01) | Residual variance |

## Diminishing Return Model
This component models the diminishing returns of media investments using the Hill function, which describes:
- How the effectiveness of marketing spend decreases as investment increases.
- The saturation point and rate of effect decay for each channel.

## Model Evaluation and Interpretation
Post-modeling, the analysis focuses on understanding the effectiveness of marketing spends:
- Calculation of Return on Ad Spend (ROAS) and marginal ROAS (mROAS) for each channel.
- Decomposition of sales to quantify the contribution from each media.

## Visualization
Visual aids are generated to illustrate findings and support strategic decisions, including:
- Time-series plots of media contributions.
- Comparative analyses of ROAS across different marketing channels.

## Optimization Insights
Insights derived from the model are used to guide future marketing strategies:
- Identification of under or over-invested channels.
- Recommendations for budget reallocation based on model outputs.

## Running the Code
Instructions on how to set up the environment, install dependencies, and execute the model scripts.

## Contact
For further inquiries or contributions to the project, please reach out to [Contact Information].

