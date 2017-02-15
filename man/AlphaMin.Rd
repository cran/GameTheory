\name{AlphaMin}
\alias{AlphaMin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AlphaMin Rule
}
\description{
   This function calculates how to distribute a given endowment by the Alphamin rule.
}
\usage{
AlphaMin(E, C, Names = NULL)
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

\details{

For each endowment and each claim, the \eqn{\alpha-min} rule ensures an equal division of the endowment among the claimants as far as the smallest claim is totally honoured; then, the remainig endowment is distributed proportionally among the revised claims.


}


\references{
	

Gimenez-Gomez J.M., & Peris J.E. (2014). "A proportional approach to claims problems with a guaranteed minimun." \emph{European Journal of Operational Research}, \bold{232}(1), pp.109--116.

	
}

\author{
Maria Jose Solis-Baltodano <mary2014sep@gmail.com>
}

\note{
In order to calculate the rule properly, input the claims of the agents in ascending order.
}


\examples{


CLAIMS<-c(10,20,30,40)
AGENTS<-c("Paul","John","George","Ringo")
AlphaMin(67,CLAIMS,AGENTS)->ALPHA
summary(ALPHA)

# Assignment according to the Alpha-min Rule rule for an Endowment of 67 

#        Claims Amin
# Paul       10 10.0
# John       20 14.5
# George     30 19.0
# Ringo      40 23.5

}