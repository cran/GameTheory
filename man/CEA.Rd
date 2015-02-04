\name{CEA}
\alias{CEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Constrained Equal Awards Rule
}
\description{
   This function calculates how to distribute a given endowment by the CEA rule.
}
\usage{
CEA(E, C, Names = NULL)
}
\arguments{
  \item{E}{
      Endowment
}
  \item{C}{
     Claims of the agents
}
  \item{Names}{
     Labels of the agents
}
}


\details{
	
The \bold{constrained equal awards (CEA)}  rule (Maimonides, 12th century), proposes equal awards to all agents
subject to no one receiving more than his claim.
	
 
}

\references{
Aumann, R.J. and Maschler, M., (1985) "Game Theoretic Analysis of a bankruptcy from the Talmud."  \emph{Journal of Economic Theory} \bold{36}, pp.195--213.
}

\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}
\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
}

