% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rajive.R
\name{get_joint_decomposition_robustH}
\alias{get_joint_decomposition_robustH}
\title{Computes the individual matrix for a data block}
\usage{
get_joint_decomposition_robustH(X, joint_scores, full = TRUE)
}
\arguments{
\item{X}{Matrix. The original data matrix.}

\item{joint_scores}{Matrix. The basis of the joint space (dimension n x joint_rank).}

\item{full}{Boolean. Do we compute the full J, I matrices or just the SVD (set to FALSE to save memory).}
}
\description{
Computes the individual matrix for a data block
}
