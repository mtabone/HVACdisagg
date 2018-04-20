# HVACdisagg

This repository contains a general algorithm for disaggregating energy consumed for
heating and cooling loads from whole-home energy consumption measured at the utility meter.
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


![very good](figures/examples.png)
- **Daily vs Hourly** data are altered by pre-prossessing the dataframe used as the `data` input.
- **Latent State** models are created by setting `coolingState` to `TRUE`, the cooling state is a binary variable, realized each timestep, that defines whether temperature responsiveness is present above the change point. The **Change Point Only** results when `coolingState` is `FALSE`.  Analogously `heatingState` is used to create a latent state for whether heating systems are active.
- **KDE vs Normal** density estimates for errors are set in the `emisShape` input which can take the value `kernel` for a kernel density estimate of the shape or `normal` for a normal distribution estimate.  
- **Continuous, Intercept, and Quadratic** models are set using the `coolingIntercept` and `coolSqTemp` inputs respectively. Each input takes a boolean value. In the figure, **continuous** corresponds to neither an intercept or a square term, **intercept** corresponds to only an intercept term, and **quadratic** corresponds to both an intercept and a square term.

Inputs to the model include
data: dataframe containing power consumption data, temperature data, and other load regressors.


## Authors

* **Michaelangelo Tabone**
* **Mark Dyson**
* **Duncan Callaway**

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

*
