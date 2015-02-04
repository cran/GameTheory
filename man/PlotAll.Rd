\name{PlotAll}
\alias{PlotAll}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot all conficting claims rules
}
\description{
   A concise (1-5 lines) description of what the function does. 
}
\usage{
PlotAll(x, y = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
     Output of \code{AllRules} function}
 \item{y}{
     Agent}


}
  


\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
## replication of Gallastegui et al. (2003), Table 7.

CLAIMS <- c(158,299,927,2196,4348,6256,13952)
COUNTRIES <- c("Germany","Netherlands","Belgium","Ireland","UK","Spain","France")
INARRA <- AllRules(13500,CLAIMS,COUNTRIES)
INARRA

PlotAll(INARRA,5) ## Display allocations for UK
LorenzRules(INARRA) ## Inequality graph
}


