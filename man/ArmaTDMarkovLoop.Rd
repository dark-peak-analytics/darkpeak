\name{ArmaMarkovFunctions}
\alias{ArmaTDMarkovLoop}
\title{Set of functions in the darkpeak package for cohort state transition models}
\description{
  The ArmaTDMarkovLoop function can be used to speed up time dependant cohort state transition models (state transition models where the transition probability matrix changes over time).
}
\usage{
ArmaTDMarkovLoop(m_TR,a_P)
}
\arguments{
  \item{m_TR}{a markov trace matrix with ncol equal to the number of states and nrows equal to the number of time periods + 1. The first row of the matrix contains the proportion of the population in each of the initial health states}
  \item{a_P}{a three-dimensional array where the t-th slice is the transition matrix for period t.}

}
\value{
  \code{
    ArmaTDMarkovLoop()} returns a completed markov trace using initialised but empty trace \code{m_TR} with n+1 rows, using \code{a_P} time dependant transition probabilities in an array.
}
\details{
  This function computes a markov trace using a time-dependant cohort state transition matrix. takes a markov Trace matrix, with the first row initialised (e.g. everyone healthy) and .}
\references{
  See the documentation for Armadillo, and RcppArmadillo, for more details.
    }
\examples{
  #ArmaTDMarkovLoop(m_TR, a_P)
}
\author{Robert Smith, Jack Smith}

