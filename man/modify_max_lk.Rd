\name{modify_max_lk}
\alias{modify_max_lk}
\alias{ModifyMaxLkAndPairwise}
\title{
Rescaling of the max_lk output in LDhelmet to reduce the mean squared error
}
\description{
The output of the max_lk function in LDhelmet is improved in terms of the mean squared error by rescaling it with a certain constant, see Gaertner and Futschik (2016).
}
\usage{
modify_max_lk(rhohat,theta,n,l,all.ests=TRUE)
}
\arguments{
\item{rhohat}{
Output of the max_lk function in LDhelmet (population recombination rate in 1/bp)
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
Rescaled recombination rate in 1/bp
}
\references{
Chan, A.H., Jenkins, P.A., and Song, Y.S., 2012. Genome-wide fine-scale recombination
rate variation in Drosophila melanogaster. PLoS Genet, 8(12): e1003090.

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
EstRho <- 0.031 #Output of max_lk
modify_max_lk(EstRho,0.02,10,15000)
modify_max_lk(EstRho,0.02,10,15000,all.ests=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shrinkage}
\keyword{estimate population recombination rate}% __ONLY ONE__ keyword per line
