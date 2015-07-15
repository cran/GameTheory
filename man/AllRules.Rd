\name{AllRules}
\alias{AllRules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  All conflicting claims rules simultaneously
}
\description{
   This function runs simultaneously all conflicting claims rules available in the package. It also calculates 
   the Gini Index to check inequality among them. 
}
\usage{
AllRules(E, C, Names = NULL, pct = 0, r = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{E}{Endowment}
  \item{C}{ Claims}
  \item{Names}{Labels of the agents}
  \item{pct}{Format of the results. If \code{pct=1}, the output is given in percentage}
  \item{r}{Decimals of the table}
}

\references{
Gallastegui M, Inarra E, Prellezo R (2003). "Bankruptcy of Fishing Resources: The Northern European Anglerfish Fishery." \emph{Marine Resource Economics}, \bold{17}, 291--307.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
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

