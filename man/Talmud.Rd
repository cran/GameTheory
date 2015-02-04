\name{Talmud}
\alias{Talmud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Talmud Rule
}
\description{
   This function calculates how to distribute a given endowment by the Talmud rule.
}
\usage{
Talmud(E, C, Names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{E}{Endowment}
  \item{C}{Claims of the agents}
  \item{Names}{Labels of the agents}
}
\details{
The \bold{Talmud} rule (Aumann 1985)
proposes to apply the constrained equal awards rule, if the
endowment is not enough to satisfy the half-sum of the claims.
Otherwise, each agent receives the half of her claim and the
constrained equal losses rule is applied to distribute the
remaining endowment.
}


\references{
Aumann, R.J. and Maschler, M., (1985) Game Theoretic Analysis of a bankruptcy from the Talmud.  \emph{Journal of Economic Theory} \bold{36}, pp.195--213.
}

\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}
\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
}

