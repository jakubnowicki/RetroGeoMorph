% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterative_distance_seeker_gpg.R
\name{iterative.minimal.distance.seeker.gpg}
\alias{iterative.minimal.distance.seeker.gpg}
\title{Find minimal distance}
\usage{
iterative.minimal.distance.seeker.gpg(data.1, data.2, a.min = 0.1,
  a.max = 1.9, a.skok = 0.1, theta.min = -0.9, theta.max = 0.9,
  theta.skok = 0.1, iteracje = 10, curves = NULL, istotne.cyfry = 10,
  wydruk = T)
}
\arguments{
\item{data.1}{First specimen}

\item{data.2}{Second specimen}

\item{a.min}{a.min}

\item{a.max}{a.max}

\item{a.skok}{a.skok}

\item{theta.min}{theta.min}

\item{theta.max}{theta.max}

\item{theta.skok}{theta.skok}

\item{iteracje}{How precize should the search be}

\item{curves}{Curve matrix for semilandmarks sliding}

\item{istotne.cyfry}{Digit number in comparing distance}

\item{wydruk}{Print distance}
}
\description{
Function to find minimal Procrustes distance between two specimens in iterative way with increasing degree of accuracy
}

