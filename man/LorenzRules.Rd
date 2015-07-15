\name{LorenzRules}
\alias{LorenzRules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Inequality plot among rules
}
\description{
   Displays a graph with a Lorenz curve for each confliciting claims rule. 
}
\usage{
LorenzRules(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Output object from \code{AllRules}
     
}

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

