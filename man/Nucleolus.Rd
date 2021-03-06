\name{Nucleolus}
\alias{Nucleolus}
%\alias{names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Nucleolus solution 
}
\description{
This function computes the nucleolus solution of a game with a maximum of 4 agents. 
}


%%%%%%%%

\usage{
Nucleolus(x, type = "Gains")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class Game
}
  \item{type}{
Specify if the game refers to Gains or Cost
}



%%%%%%




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



\references{

Lemaire J (1991). "Cooperative game theory and its insurance applications." Astin Bulletin, \bold{21}(01), 17--40.	
	
Schmeidler D (1969). "The Nucleolus of a characteristic function game." \emph{SIAM Journal of Applied Mathematics}, \bold{17}, pp.1163--1170.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
	
## EXAMPLE FROM LEMAIRE (1991)
	
# Begin defining the game

COALITIONS <- c(46125,17437.5,5812.5,69187.5,53812.5,30750,90000)
LEMAIRE<-DefineGame(3,COALITIONS)

# End defining the game

LEMAIRENUCLEOLUS<-Nucleolus(LEMAIRE)
summary(LEMAIRENUCLEOLUS) # Gains Game, the excess should be negative


}




