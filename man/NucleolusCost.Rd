\name{NucleolusCost}
\alias{NucleolusCost}
%\alias{Nucleolus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Nucleolus solution of a cost game
}
\description{
This function computes the nucleolus solution of a cost game with a maximum of 4 agents. 
}
\usage{
NucleolusCost(n, V)
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
  Please check \code{Nucleolus} for a gains game. 

}

\references{	
	
Schmeidler D (1969). "The Nucleolus of a characteristic function game." \emph{SIAM Journal of Applied Mathematics}, \bold{17}, pp.1163--1170.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
	
## Airport Problem (cost game)

AIR <-NucleolusCost(4,c(
26, # v(1)
27, # v(2)
53, # v(12)
55, # v(3)
81, # v(13)
82, # v(23)
108,# v(123)
57, # v(4)
83, # v(14)
84, # v(24)
110,# v(124)
110,# v(34)
110,# v(134)
110,# v(234)
110 # v(1234)
))


AIR ## Cost Game, the excess should be positive

#  v(S)  x(S)    Ei
# 1   26 13.00 13.00
# 2   27 13.50 13.50
# 3   55 40.75 14.25
# 4   57 42.75 14.25

}

