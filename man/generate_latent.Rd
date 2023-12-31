% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_latent.R
\name{generate_latent}
\alias{generate_latent}
\title{Data generation}
\usage{
generate_latent(m, n, ife1, ife2, corIfe, gfeI, gfeS, greI, greS, corIS)
}
\arguments{
\item{m}{number of groups}

\item{n}{number of observations within each group}

\item{ife1}{value of the fixed effect the first individual-level latent
predictor has on the latent dependent variable}

\item{ife2}{value of the fixed effect the second individual-level latent
predictor has on the latent dependent variable}

\item{corIfe}{correlation between effects described by \code{ife1} and \code{ife2}}

\item{gfeI}{value the fixed effect the group-level predictor has on latent
dependent variable}

\item{gfeS}{value the fixed effect the group-level predictor has on the slope
of the first latent dependent variable while predicting the latent dependent
variable}

\item{greI}{standard deviation of the group-level random effect on the
intercept (mean of the latent dependent variable)}

\item{greS}{standard deviation of the group-level random effect on the slope
of the first latent dependent variable while predicting the latent dependent
variable}

\item{corIS}{correlation of the (group-level) above random effects}
}
\value{
a numeric matrix with \code{n*m} rows and columns:
\describe{
\item{gr}{group-membership indicator}
\item{x1}{values of the first individual-level latent predictor}
\item{x2}{values of the second individual-level latent predictor}
\item{w}{values of the group-level predictor}
\item{ri}{values of the group random intercept}
\item{rs}{values of the group random slope}
\item{y}{values of the latent dependent variable}
}
}
\description{
Generates a matrix with the sampled values of latent variables.
}
