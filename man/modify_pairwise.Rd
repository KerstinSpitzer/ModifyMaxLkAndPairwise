\name{modify_pairwise}
\alias{modify_pairwise}
\alias{ModifyMaxLkAndPairwise}
\title{
Rescaling of the pairwise output in LDhat to reduce the mean squared error
}
\description{
The output of the pairwise function in LDhat is improved in terms of the mean squared error by rescaling it with a certain constant, see Gaertner and Futschik (2016).
The optimal constant depends on the unknown recombination rate. Here the output of pairwise is used insetad of the true recombination rate.
}
\usage{
modify_pairwise(rhohat,theta,n,l,all.ests=TRUE)
}
\arguments{
\item{rhohat}{
Output of the pairwise function in LDhat (population recombination rate over the considered sequence)
}
\item{theta}{
Population mutation rate in 1/bp
}
\item{n}{
Sample size
}
\item{l}{
Sequence length in bp
}
\item{all.ests}{
Binary vector to choose if every input for the estimated recombination rate \code{rhohat} is modified (\code{all.ests=TRUE}) or if \code{rhohat} is modified only if it is in the range tested in Gaertner and Futschik (2016) and left unchanged otherwise (\code{all.ests=FALSE}).
}
}
\value{
Modified recombination rate over the considered sequence
}

\references{
McVean, G. and Auton, A., 2007. LDhat 2.1: a package for the population genetic
analysis of recombination. Department of Statistics, Oxford, OX1 3TG, UK.

Gaertner, K., and Futschik, A., 2016. Improved versions of common estimators of the recombination rate. Journal of Computational Biology 23(9), 756-768.
}
\author{
Kerstin Gaertner, Andreas Futschik
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
EstRho <- 155 #Output of paiwise
modify_pairwise(EstRho,0.02,20,5000)
modify_pairwise(EstRho,0.02,20,5000,all.ests=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shrinkage}
\keyword{estimate population recombination rate}% __ONLY ONE__ keyword per line
