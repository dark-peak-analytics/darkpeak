# darkpeak <img src="man/figures/logo_concise.PNG" align="right" width="120" />


[![GitHub release](https://img.shields.io/badge/R-HEDS-green)](https://img.shields.io/badge/R-hello-green)
![GitHub last commit](https://img.shields.io/github/last-commit/RobertASmith/darkpeak?color=red&style=plastic)
![GitHub top language](https://img.shields.io/github/languages/top/RobertASmith/darkpeak?style=plastic)
![GitHub repo size](https://img.shields.io/github/repo-size/RobertASmith/darkpeak?style=plastic)
![R-CMD-Check](https://github.com/RobertASmith/darkpeak/actions/workflows/r_package_check.yml/badge.svg)


This package is for use by Robert Smith & Paul Schneider at the University of Sheffield and Dark Peak Analytics.

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
