\name{plot.ClaimsRules}
\alias{plot.ClaimsRules}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot all conficting claims rules
}
\description{
  Plot results of every rule for a given player. 
}
\usage{
\method{plot}{ClaimsRules}(x, y, ...)
}

\arguments{
  \item{x}{Object of class ClaimsRules}
 \item{y}{Agent}
 \item{...}{Other graphical parameters}


}
  


\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
## replication of Gallastegui et al. (2003), Table 7.

CLAIMS <- c(158,299,927,2196,4348,6256,13952)
COUNTRIES <- c("Germany","Netherlands","Belgium","Ireland","UK","Spain","France")
INARRA <- AllRules(13500,CLAIMS,COUNTRIES)
summary(INARRA)

plot(INARRA,5) ## Display allocations for UK
LorenzRules(INARRA) ## Inequality graph
}


