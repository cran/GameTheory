\name{AdjustedProportional}
\alias{AdjustedProportional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Proportional Rule
}
\description{
   This function calculates how to distribute a given endowment by the Adjusted Proportional rule.
}
\usage{
AdjustedProportional(E, C, Names = NULL)
}
%- maybe also 'usage' for other objects documented here.
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


\references{
	


	Curiel, I. J., Maschler, M., & Tijs, S. H. (1987). "Bankruptcy games." \emph{Zeitschrift fur Operations Research}, \bold{31}(5), A143-A159.
}


\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
}




