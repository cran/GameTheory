\name{Nucleolus}
\alias{Nucleolus}
%\alias{names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Nucleolus solution of a gains game
}
\description{
This function computes the nucleolus solution of a gains game with a maximum of 4 agents. 
}
\usage{
Nucleolus(n, V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
     Number of agents
}
  \item{V}{
     Coalition values
}
}
\details{
  The nucleolus looks for an individually rational distribution of the worth of the grand coalition in which the maximum dissatisfaction is minimized. The nucleolus selects the element in the core, if this is nonempty, that lexicographically minimizes the vector of non-increasing ordered excesses of coalitions. In order to compute this solution we consider a sequence of linear programs, which looks for an imputation that minimizes the maximum excess among all coalitions.
}

\value{
The command returns a table with the following elements:
  \item{v(S)}{Individual value of player \emph{i}}
  \item{x(S)}{Nucleolus solution of the player \emph{i}}
  \item{Ei }{Excess of the player \emph{i}}  
}

\note{
  Please check \code{NucleolusCost} for a cost game. 

}

\references{

Lemaire J (1991). "Cooperative game theory and its insurance applications." Astin Bulletin, \bold{21}(01), 17--40.	
	
Schmeidler D (1969). "The Nucleolus of a characteristic function game." \emph{SIAM Journal of Applied Mathematics}, \bold{17}, pp.1163--1170.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
	
## EXAMPLE FROM LEMAIRE (1991)
	
COALITIONS<- c(
46125,   # v(1)
17437.5, # v(2)
69187.5, # v(12)
5812.5,  # v(3)
53812.5, # v(13)
30750,   # v(23)
90000    # v(123)
)

LEMAIRE <- Nucleolus(3,COALITIONS)

LEMAIRE	# Gains Game, the excess should be negative

#     v(S)     x(S)       Ei
# 1 46125.0 52687.50 -6562.50
# 2 17437.5 24468.75 -7031.25
# 3  5812.5 12843.75 -7031.25

}




