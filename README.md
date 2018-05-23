# HVACdisagg

This repository contains a general algorithm to disaggregate heating and cooling energy consumed
from whole-home energy consumption measured at the utility meter.
The general algorithm allows for different model forms as described in the accompanying publication
and explained in this overview.

## Getting Started

The file functions/HVACdisagg.R contains two functions: classModelFit and validateFit.
Simply source this file to access these functions.

TrialRun.R is an example script for using the functions. PecanDataLoad.R fetches and processes pecan street
data for testing the algorithm. This requires an academic license to the pecan street dataport, which is available from http://www.pecanstreet.org/.
Write your username and password for the dataport into a file names pecankeys.keys in the home directory, with your username
on the first line and your password on the second. Alternatively one can enter their password directly into the PecanDataLoad.R

### Prerequisites

There are no prerequisites for the model fitting functions.
TrialRun.R requires ggplot2. PecanDataLoad.R requires RPostgreSQL.

## Using the model
The figure below displays examples of different model forms fit to daily and hourly data.

- **Daily vs Hourly** data are altered by pre-prossessing the dataframe used as the `data` input.
- **Latent State** models are created by setting `coolingState` to `TRUE`, the cooling state is a binary variable, realized each timestep, that defines whether temperature responsiveness is present above the change point. The **Change Point Only** results when `coolingState` is `FALSE`.  Analogously `heatingState` is used to create a latent state for whether heating systems are active.
- **KDE vs Normal** density estimates for errors are set in the `emisShape` input which can take the value `kernel` for a kernel density estimate of the shape or `normal` for a normal distribution estimate.  
- **Continuous, Intercept, and Quadratic** models are set using the `coolingIntercept` and `coolSqTemp` inputs respectively. Each input takes a boolean value. In the figure, **continuous** corresponds to neither an intercept or a square term, **intercept** corresponds to only an intercept term, and **quadratic** corresponds to both an intercept and a square term.

Inputs to the model include
data: dataframe containing power consumption data, temperature data, and other load regressors.

![very good](figures/examples.png)

## classModelFit()

### Parameters
<ul>
    <li>**Data**: A data frame containing (at least) the columns </li>
        <ul>
            <li>dependentColName,    default to "powerAve"   for average power during the hour, and</li>
            <li>CPColName,           default to "outTemp"    for average outdoor temperature</li>
            <li>otherRegressors      default option contains "HOD," and "dayType"  </li>
        </ul>
    </li>
    <li>**dependentColName**: Column name of the dependent variable, default "powerAve"</li>
    <li>**CPColName**:        Column name of the independent vairable which is affected by the changepoint, default "outTemp"</li>
    <li>**otherRegressors**:  other regressors to include for non-HVAC, default "HOD:dayType"</li>
    <li>**coolingState**     : logical, whether to include a state for cooling</li>
    <li>**heatingState**     : logical, whether to include a state for heating</li>
    <li>**latent_states**    : logical, default FALSE, whether to classify cooling and heating states as latent random variables: TRUE for latent state models, FALSE for change point only models</li>
    <li>**coolingIntercept** : whether to provide a separate intercept when switching between ON/OFF states of Cooling, default FALSE</li>
    <li>**heatingIntercept** : whether to provide a separate intercept when switching between ON/OFF states of Heating, default FALSE</li>
    <li>**coolSqTmp**        : logical, whether to include squared temperature in cooling model</li>
    <li>**heatSqTmp**        : logical, whether to include squared temperature in the heating model</li>
    <li>**ChangePoints**     :  A dataframe of changepoints to try, must be a two column array if fitting heating and cooling. Columns must be labeled cool and heat. </li>
    <li>**emisShape**        : distribtuion shape for errors given the model 'normal' or 'kernel'</li>
    <li>**convCrit**         :  Convergence criterion, the maximum fraction of classifications that may be changed during the iteration preceeding convergence, default 0.005</li>
    <li>**numIters**         :  The maximum number of iterations allowed, default 100</li>
    <li>**storeall**         : Returns all models (even those that are not maximum likelihood), setting to FALSE saves space, default TRUE. </li>
</ul>

### Returns
classModelFit returns a list with the following named elements:
<ul>
    <li>**Models**       : linear models produced by "lm" (one for each changepoint, stored in a list) </li>
    <li>**ChangePoints** : the changepoints (though these were directly input)</li>
        <li>**coefficients**: matrix, one column for each changepoint, rows contain model coefficients. </li>
    <li>**PredictedValues** : ordered list with one element for each CP.  Values data frmes of predicted heating, cooling, and other energy uses. Each data frame has the columns 
        <ul>
            <li>**ProbOff**, Probability that both heating and cooling are off</li>
            <li>**ProbCool**, Probability that cooling is on</li>
            <li>**ProbHeat**, Probability that heating is on</li>
            <li>**Cool**, Disaggregated cooling energy</li>
            <li>**Heat**, Disaggregated heating energy</li>
            <li>**Other**, Disaggregated other energy consumption</li>
            <li>**CoolNeed**, Estimated cooling energy needed at steady state</li>
            <li>**HeatNeed**, Estimated heating energy needed at steady state</li>
            <li>**PowerPred**, Power consumption predicted by linear models alone (without disaggregated residuals)</li>
        </ul>
    </li>
    <li>**TotalLLs**     : the total log likelihood (one for each changepoint, stored in an array)  </li>
    <li>**NumIt**        : Number of iterations required for each fit. </li>
    <li>**prob_cool**   : ordered list with one element for each CP.  Values are vectors defining the probability of cooling at each time</li>
    <li>**prob_heat**   : ordered list with one element for each CP.  Values are vectors defining the probability of heating at each time</li>
    <li>**Fcool**       : fraction of time above cooling changepoint that the system is cooling. </li>
    <li>**Fheat**       : fraction of time below heating changepoint that the system is heating </li>
    <li>**sigmaCool**   : standard deviation of residuals when cooling is active</li>
    <li>**sigmaHeat**   : standard deviation of residuals when heating is active</li>
    <li>**sigmaOff**    : standard deviation of residuals when neither heating or cooling are active</li>
    <li>**kernels**     : if using KDE estimates for error distributions, these values contain density functions for these kernels</li>
    <li>**AIC**         : vector, AIC value of fit at each changepoint</li>
    <li>**BIC**         : vector, BIC value of fit at each changepoint</li>
    <li>**k**           : vector, model order at each changepoint</li>
</ul>

## Authors

* **Michaelangelo Tabone**
* **Mark Dyson**
* **Duncan Callaway**

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments
