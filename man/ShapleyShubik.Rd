\name{ShapleyShubik}
\alias{ShapleyShubik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Shapley Shubik Power Index
}
\description{
   This function computes Shapley - Shubik Power Index of a coalition.
}
\usage{
ShapleyShubik(quota, y, Names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quota}{Minimum amount of votes to pass a vote}
  \item{y}{Seats of every party}
  \item{Names}{Labels of the parties}
}


\details{
The \emph{Shapley and Shubik index} works as follows. There is a group of individuals all willing to vote on a proposal. They vote in order and as soon as a majority has voted for the proposal, it is declared passed and the member who voted last is given credit for having passed it. Let us consider that the members are voting randomly. Then we compute the frequency with which an individual is the one that gets the credit for passing the proposal. That measures the number of times that the action of that individual joining the coalition of their predecessors makes it a winning coalition. Note that if this index reaches the value of 0, then it means that this player is a dummy. When the index reaches the value of 1, the player is a dictator.
}


\references{
Shapley L, Shubik M (1954). "A Method for Evaluating the Distribution of Power in a Committee System." \emph{The American Political Science Review}, \bold{48}(3), 787--792.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}





\examples{

## SHAPLEY - SHUBIK POWER INDEX APPLIED TO THE CATALAN PARLIAMENT

# 2003 Elections
SEATS<-c(46,42,23,15,9)
PARTIES<-c("CiU","PSC","ERC","PP","ICV")
ShapleyShubik(68,SEATS,PARTIES)

# 2006 Elections
SEATS<-c(48,37,21,14,12,3)
PARTIES<-c("CiU","PSC","ERC","PP","ICV","C's")
ShapleyShubik(68,SEATS,PARTIES)

# 2012 Elections
SEATS<-c(50,20,21,19,13,9,3)
PARTIES<-c("CiU","PSC","ERC","PP","ICV","C's","CUP")
ShapleyShubik(68,SEATS,PARTIES)

# Results for 2012 elections 

#                    CiU    PSC    ERC     PP     ICV    C's    CUP
# Votes           50.000 20.000 21.000 19.000 13.0000 9.0000 3.0000
# Votes (%)        0.370  0.148  0.156  0.141  0.0963 0.0667 0.0222
# Shapley-Shubick  0.533  0.133  0.133  0.133  0.0333 0.0333 0.0000


}
