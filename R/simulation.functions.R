#' Simulation of data blocks
#'
#' Simulates blocks of data with joint and individual structures
#'
#'
#' @param K Integer. Number of data blocks.
#' @param rankJ Integer. Joint rank.
#' @param rankA Vector of Integers. Individual Ranks.
#' @param n Integer. Number of data points.
#' @param pks Vector of Integers. Number of variables in each block.
#' @param dist.type Integer. 1 for normal, 2 for uniform, 3 for exponential
#' @param noise Integer. Standard deviation in dist
#'
#' @export
#'
#' @examples
#'  Y <- jive.data.sim(K =3, rankJ = JrankTrue,
#'  rankA = initial_signal_ranks,n = n,
#'  pks = c(p1, p2, p3), dist.type = 1)





jive.data.sim <- function(K = 3, rankJ = 2,
                          rankA = c(20, 15, 10),
                          n = 100,
                          pks,
                          dist.type = 1, noise = 1){

  p <- sum(pks)

  S <- sim_from_rand_dist(num = dist.type, n = n, p = rankJ)
  U <- sim_from_rand_dist(num = dist.type, n = rankJ, p = p)
  J <- S %*% U

  Xs <- As <- list()
  idx <- 1
  for (k in 1:K) {
    rankAk <- rankA[k]
    Sk <- sim_from_rand_dist(num = dist.type, n = n, p = rankAk)
    Wk <- sim_from_rand_dist(num = dist.type, n = rankAk, p = pks[k])
    Ak <- Sk %*% Wk
    As[[k]] <- Ak

    col_idx <- idx:(idx + pks[k] - 1)
    Xs[[k]] <- J[,col_idx] + Ak +
      matrix(rnorm(n = n*pks[k], mean = 0, sd = noise), nrow = n, ncol = pks[k])
    idx <- idx + pks[k]
  }
  Sig <- J %*% t(J)
  Deltks <- lapply(As, FUN = function(A) {return(t(A) %*% A)})

  truth <- list(Sig_true = Sig, Deltks_true = Deltks, J_true = J,
                As_true = As, rankJ = rankJ, rankA = rankA)
  Xsim <- list(sim_data = Xs, truth = truth)


  return(Xsim)
}

#' Simulation of single data block from distribution
#'
#'
#'
#' @param num Integer. Type of distribution. 1 for normal, 2 for uniform, 3 for exponential
#' @param n Integer. Number of data points.
#' @param p Integers. Number of variables in  block.
#'
sim_from_rand_dist <- function(num, n, p) {
  # simulate from a random distribution and output n x pk matrix
  # num 1 for norm, 2 for unif, 3 for exp


  if (num == 1) {
    dist <- rnorm(n*p)
  }else if (num == 2) {
    dist <- runif(min=0, max=1, n=n*p)
  }else if (num == 3) {
    dist <- rexp(n=n*p, rate=1)
  }
  out <- matrix(dist, nrow = n, ncol = p)
  return(out)
}

