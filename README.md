# darkpeak <img src="man/figures/logo_concise.PNG" align="right" width="120" />


[![GitHub release](https://img.shields.io/badge/R-HEDS-green)](https://img.shields.io/badge/R-hello-green)
![GitHub last commit](https://img.shields.io/github/last-commit/RobertASmith/darkpeak?color=red&style=plastic)
![GitHub top language](https://img.shields.io/github/languages/top/RobertASmith/darkpeak?style=plastic)
![GitHub repo size](https://img.shields.io/github/repo-size/RobertASmith/darkpeak?style=plastic)
![R-CMD-Check](https://github.com/RobertASmith/darkpeak/actions/workflows/r_package_check.yml/badge.svg)
![Coverage](https://github.com/RobertASmith/darkpeak/actions/workflows/test-coverage.yaml/badge.svg)


This package was created by Robert Smith & Paul Schneider at the University of Sheffield and Dark Peak Analytics to speed up time dependent markov models.

## Installation

Install the development version of the package using the devtools package.

``` r
remotes::install_github("RobertASmith/darkpeak")
```

## Quick start

Load the package.

``` r
library(darkpeak)
```

- ArmaMarkovTDLoop uses C++ to speed up the running of time dependent cohort state transition models (where the transition probability matrix changes over time).

``` r
# C++ code

arma::mat ArmaTDMarkovLoop(arma::mat m_TR, arma::cube& a_P )
    {
        int rows = m_TR.n_rows;

        for(int i = 1; i < rows; i++){
            m_TR.row(i) = m_TR.row(i-1) * a_P.slice(i-1);
        }

        return m_TR;
    }
    
```

Run using the following:

``` r
# where:
# m_TR is a markov trace
# a_P is a time dependent transmission array dim = 3.

ArmaTDMarkovLoop(m_TR, a_P)

```
This results in a significantly faster model run than the equivalent in base R, the below is for a state transition model with 4 health states run over 1000 periods. The units are milliseconds.

| Method       | minimum  | mean       | maximum     |
|--------------|--------- |------------| ------------|
| darkpeak::ArmaTDMarkovLoop  | 48.2     | 58.2       | 175.6       |
| Base R       | 160.4    | 197.9      | 391.2       |

The Rcpp version runs approximately in approximately 30% of the time of the Base R version, which can make a material difference (e.g. when incorporated into a shiny app a reduction in run-time from 30 seconds to 9 seconds may avoid user frustration, or a model with a 2 hour run-time running in just over 30mins may make it feasible for a meeting).

- There are plot functions for CEAC plots, CE-Plane plots, plots to check the stability of PSA results, a rankogram for cases where n_strategies >2.
- There are colour schemes (for all visuals).
- There is a markdown templates for reporting results (with header and logo etc).
- Also data for teaching (making markov models shiny parameters?).

[Robert Smith](https://www.linkedin.com/in/robert-smith-53b28438) <sup> 1,2 </sup>, [Paul Schneider](https://www.sheffield.ac.uk/scharr/staff-pgrs/studentprofiles/paulschneider) <sup> 1,2

<sup> 1 </sup> *Public Health Economics and Decision Science, Wellcome Trust Doctoral Training Center, ScHARR, University of Sheffield, UK* <br>
<sup> 2 </sup> *Dark Peak Analytics, Sheffield, UK*

Contact: rasmith3@sheffield.ac.uk

Website: [Dark Peak Analytics](https://www.darkpeakanalytics.com)

## Funding
Rob & Paul are joint funded by the Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science [108903] and the University of Sheffield.
