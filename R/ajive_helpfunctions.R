
#' Computes the SVD of a matrix
#'
#'
#'
#' @param decomposition X matrix.
#' @param rank number rank of SVD decomposition
#'
#' @return The SVD of X.


get_svd <- function(X, rank=NULL){
  # SVD <- get_svd(X, rank=2)

  if(is.null(rank)){
    svd(X)
  } else if(rank == 0){
    # TODO: what to do
    decomposition <- list()
    decomposition[['u']] <- matrix(0, ncol=1, nrow=dim(X)[1])
    decomposition[['d']] <- 0
    decomposition[['v']] <- matrix(0, ncol=1, nrow=dim(X)[2])
    decomposition

  } else{
    decomposition <- svd(X, nu=rank, nv=rank)
    decomposition[['d']] <- decomposition[['d']][1:rank]
    decomposition
  }

}



#' Truncates an SVD.
#'
#' Removes columns from the U, D, V matrix computed form an SVD.
#'
#'
#' @param decomposition List. List with entries 'u', 'd', and 'v'from the svd function.
#' @param rank List. List with entries 'u', 'd', and 'v'from the svd function.
#'
#' @return The trucated SVD of X.
truncate_svd <- function(decomposition, rank){

  if(rank==0){
    n <- dim(decomposition[['u']])[1]
    d <- dim(decomposition[['v']])[1]
    decomposition[['u']] <- matrix(0, ncol=1, nrow=n)
    decomposition[['d']] <- 0
    decomposition[['v']] <- matrix(0, ncol=1, nrow=d)
  }else{
    decomposition[['u']] <- decomposition[['u']][, 1:rank, drop=FALSE]
    decomposition[['d']] <- decomposition[['d']][1:rank]
    decomposition[['v']] <- decomposition[['v']][, 1:rank, drop=FALSE]
  }

  decomposition
}


#' Reconstruces the original matrix from its SVD.
#'
#' Computes UDV^T to get the approximate (or full) X matrix.
#'
#' @param decomposition List. List with entries 'u', 'd', and 'v'from the svd function.
#'
#' @return Matrix. The original matrix.
svd_reconstruction <- function(decomposition){

  # decomposition rank -- need to truncated singluar values
  r <- dim(decomposition[['u']])[2]

  decomposition[['u']]  %*%
    diag(decomposition[['d']][1:r], nrow=r, ncol=r) %*%
    t(decomposition[['v']])

}

get_wedin_bound_samples <- function(X, SVD, signal_rank, num_samples=1000){

  # resample for U and V
  U_perp <- SVD[['u']][ , -(1:signal_rank)]
  U_sampled_norms <- wedin_bound_resampling(X=X,
                                            perp_basis=U_perp,
                                            right_vectors=FALSE,
                                            num_samples=num_samples)

  V_perp <- SVD[['v']][ , -(1:signal_rank)]
  V_sampled_norms <- wedin_bound_resampling(X=X,
                                            perp_basis=V_perp,
                                            right_vectors=TRUE,
                                            num_samples=num_samples)

  sigma_min <- SVD[['d']][signal_rank]
  wedin_bound_samples <- mapply(function(u, v)  min(max(u, v)/sigma_min, 1)^2, U_sampled_norms, V_sampled_norms)

  wedin_bound_samples
}


#' Resampling procedure for the wedin bound
#'
#' @param X Matrix. The data matrix.
#' @param perp_basis Matrix. Either U_perp or V_perp: the remaining left/right singluar vectors of X after estimating the signal rank.
#' @param right_vectors Boolean. Right multiplication or left multiplication.
#' @param num_samples Integer. Number of vectors selected for resampling procedure.
wedin_bound_resampling <- function(X, perp_basis, right_vectors, num_samples=1000){

  rank <- dim(perp_basis)[2]
  resampled_norms <- rep(0, num_samples)

  for(s in 1:num_samples){

    sampled_col_index <- sample.int(n=dim(perp_basis)[2],
                                    size=rank,
                                    replace=TRUE)


    perp_resampled <- perp_basis[ , sampled_col_index]

    if(right_vectors){
      resampled_projection <- X %*% perp_resampled
    } else{
      resampled_projection <- t(perp_resampled) %*% X
    }

    # operator L2 norm
    resampled_norms[s] <- norm(resampled_projection,
                               type='2')
  }

  resampled_norms
}

#' Estimate the wedin bound for a data matrix.
#'
#' Samples from the random direction bound. Returns on the scale of squared singular value.
#'
#' @param n_obs. The number of observations.
#' @param n_features. The number of features in each data matrix
#' @param num_samples Integer. Number of vectors selected for resampling procedure.
#'
#' @return rand_dir_samples
#'
get_random_direction_bound <- function(n_obs, dims, num_samples=1000){

    n_blocks <- length(dims)
    rand_dir_samples <- rep(0, num_samples)
    for(s in 1:num_samples){
        rand_subspaces <- list()
        for(b in 1:n_blocks){
            X <- matrix(rnorm(n_obs * dims[b], mean=0,sd=1), n_obs, dims[b])
            U <- get_svd(X)[['u']]

            rand_subspaces[[b]] <- U

        }
        M <- do.call(cbind, rand_subspaces)
        M_svd <- get_svd(M, rank=min(dims))

        rand_dir_samples[s] <- M_svd[['d']][1]^2

    }

    rand_dir_samples
}

