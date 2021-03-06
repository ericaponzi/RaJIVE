% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rajive_helpfunctions.R
\name{get_individual_rank}
\alias{get_individual_rank}
\title{Individual Rank}
\usage{
get_individual_rank(ajive_output, k)
}
\arguments{
\item{ajive_output}{List. The decomposition from Rajive}

\item{k}{Integer. The index of the data block.}
}
\value{
The individual ranks
}
\description{
Gets the individual ranks from the Rajive decomposition
}
\examples{
\donttest{
n <- 10
pks <- c(20, 10)
Y <- ajive.data.sim(K =2, rankJ = 2, rankA = c(7, 4), n = n,
                 pks = pks, dist.type = 1)
initial_signal_ranks <-  c(7, 4)
data.ajive <- list((Y$sim_data[[1]]), (Y$sim_data[[2]]))
ajive.results.robust <- Rajive(data.ajive, initial_signal_ranks)
get_individual_rank(ajive.results.robust, 2)
}
}
