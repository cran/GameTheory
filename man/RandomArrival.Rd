\name{RandomArrival}
\alias{RandomArrival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Arrival Rule
}

\description{
   This function calculates how to distribute a given endowment by the Random Arrival rule.
}
\usage{
RandomArrival(E, C, Names = NULL)
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
The \bold{random arrival} rule
(O'Neill, 1982) works in the following fashion: suppose  that each claim is fully honored
until the  endowment runs out following the order of the claimants
arrival. In order to remove the unfairness of the first-come
first-serve scheme  associated with any particular order of
arrival, the rule proposes to take the average of the awards
vectors calculated in this way when all orders are equally
probable.
}

\references{

O'Neill B. (1982). "A problem of rights arbitration from the Talmud." \emph{Mathematical Social Sciences}, \bold{2}(4), pp.345--371.

}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}
\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
}

