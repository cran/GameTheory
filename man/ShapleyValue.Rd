\name{ShapleyValue}
\alias{ShapleyValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Shapley Value Solution
}
\description{
   Calculates the Shapley value for a N-agent cooperative game.
}
\usage{
ShapleyValue(x, Names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
     object of class Game
}
  \item{Names}{
     Labels of the agents
}
}
\details{
  Please check \code{ShapleyShubik} for an extension to voting power index. 

}

\references{
Shapley L (1953). \emph{A value for n-person games. In Tucker A, Kuhn H (Eds.), Contributions to the theory of games II (pp. 307-317). Princeton University Press: Princeton NJ.}
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}




\examples{



# Begin defining the game

COALITIONS <- c(46125,17437.5,5812.5,69187.5,53812.5,30750,90000)
LEMAIRE<-DefineGame(3,COALITIONS)

# End defining the game

NAMES <- c("Investor 1","Investor 2","Investor 3")
LEMAIRESHAPLEY <- ShapleyValue(LEMAIRE,NAMES)
summary(LEMAIRESHAPLEY)

}
