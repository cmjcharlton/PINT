#' Performs power calculations
#'
#' @param start  : number of level-1 per level-2 with which we start
#' @param step   : the step size (number of level-1 added per level-2 before the next calculation starts)
#' @param end    : maximum number of level-1 per level-2
#' @param n2min  : number of level-2 units with which we start
#' @param n2max  : number of level-2 units with which we end
#' @param cost   : total to be expressed in a unit to be determined available funds for data collection; starting point is that the unit is expressed as: what does it cost to train 1 student to research
#' @param extra  : the additional costs associated with sampling one new level-2 (to be expressed in the same unit as 'cost')
#' @param resid  : residual model variation
#' @param tau2   : subtriangular covariance matrix of the variance components
#' @param sigmaw : covariance matrix of the level-1 variables first fixed effects, then constant, then random effects
#' @param sigmab : covariance matrix of the level-2 variables first constant, then true level-2 variable, then level-2 average of the level-1 variable with fixed effects
#' @param mu1    : means of the level-2 means of the level-1 variables with fixed effects
#' @param mu3    : means of the variables belonging to sigma(s) excl. level-2 averages of level-1 variables included in mu1
#' @return a PiNTresult object
#' @export
PINT <- function(start, step, end, n2min = NA, n2max = NA, cost = NA, extra = NA, resid, tau2, sigmaw = NULL, sigmab = NULL, mu1 = NULL, mu3 = NULL) {
  # Get dimensions from provided matrices
  if (!is.null(tau2)) {
    if (!is.matrix(tau2)) {
      stop("tau2 must be a matrix")
    }
    if (nrow(tau2) != ncol(tau2)) {
      stop("tau2 must be a square matrix")
    }
    L2 <- ncol(tau2) - 1
  } else {
    L2 <- 0
  }
  if (!is.null(sigmaw)) {
    if (!is.matrix(sigmaw)) {
      stop("sigmaw must be a matrix")
    }
    if (nrow(sigmaw) != ncol(sigmaw)) {
      stop("sigmaw must be a square matrix")
    }
    L1 <- ncol(sigmaw) - L2
  } else {
    L2 <- 0
    L1 <- 0
  }
  if (!is.null(sigmab)) {
    if (!is.matrix(sigmab)) {
      stop("sigmab must be a matrix")
    }
    if (nrow(sigmab) != ncol(sigmab)) {
      stop("sigmab must be a square matrix")
    }
    L3 <- ncol(sigmab) - L1
  } else {
    L1 <- 0
    L3 <- 0
  }

  if (start > end) {
    stop("start must be less than or equal to end")
  }
  if (!is.na(n2min) && !is.na(n2max)) {
    if (n2min > n2max) {
      stop("n2min must be less than or equal to n2max")
    }
  }

  Q <- L2 + 1
  R <- L3 + 1
  S <- L1

  if (!is.null(mu1)) {
    if (!is.matrix(mu1)) {
      stop("mu1 must be a matrix")
    }
    if (ncol(mu1) != 1) {
      stop("mu1 must have just one column")
    }
    if (nrow(mu1) != L1) {
      stop("mu1 has incorrect length")
    }
  } else {
    if (S > 0) {
      stop("mu1 has not been specified")
    }
  }

  if (!is.null(mu3)) {
    if (!is.matrix(mu3)) {
      stop("mu3 must be a matrix")
    }
    if (ncol(mu3) != 1) {
      stop("mu3 must have just one column")
    }
    if (nrow(mu3) != L3) {
      stop("mu3 has incorrect length")
    }
  }

  # Calculate n2step if BudgetConstraint not TRUE
  if (step >= 0) {
    BudgetConstraint <- TRUE
  } else {
    BudgetConstraint <- FALSE
    step <- -step
    n2step <- 1
    if (n2max - n2min <= 20) {
      n2step <- 1
    }
    if ((n2max - n2min > 20) & (n2max - n2min <= 50)) {
      n2step <- 2
    }
    if ((n2max - n2min > 50) & (n2max - n2min <= 100)) {
      n2step <- 5
    }
    if (n2max - n2min > 100) {
      n2step <- 10
    }
  }

  # Add in constant term and ensure matrices are symmetric
  E <- matrix(0, Q, 1)
  E[1, 1] <- 1

  mu3 <- rbind(1, mu3)

  sigmaworig <- sigmaw
  sigmaw <- matrix(0, S + Q, S + Q)
  if (S + Q > 1) {
    if (S > 0) {
      sigmaw[1:S, 1:S] <- sigmaworig[1:S, 1:S]
    }
    if (Q > 1) {
      sigmaw[S + (2:Q), S + (2:Q)] <- sigmaworig[S + (1:(Q-1)), S + (1:(Q-1))]
      if (S > 0) {
        sigmaw[S + (2:Q), 1:S] <- sigmaworig[S + (1:(Q-1)), 1:S]
      }
    }
  }
  sigmaw[upper.tri(sigmaw)] <- t(sigmaw)[upper.tri(sigmaw)]

  if (!is.null(sigmab)) {
    sigmab <- cbind(rep(0, S+R-1), sigmab)
    sigmab <- rbind(rep(0, S+R), sigmab)
  } else {
    sigmab <- matrix(0, 1, 1)
  }
  sigmab[upper.tri(sigmab)] <- t(sigmab)[upper.tri(sigmab)]

  tau2[upper.tri(tau2)] <- t(tau2)[upper.tri(tau2)]

  # Calculate quantities that do not vary across rows
  E22INV <- solve(tcrossprod(E) + sigmaw[S + (1:Q), S + (1:Q), drop=FALSE])
  V22_1 <- tcrossprod(mu3) + sigmab[1:R, 1:R, drop=FALSE]
  if (S > 0) {
    AA <- sigmaw[1:S, S + (1:Q), drop=FALSE]
    V11_1 <- (sigmaw[1:S, 1:S, drop=FALSE] - tcrossprod(tcrossprod(AA, E22INV), AA))/resid
    V11_2 <- sigmab[R + (1:S), R + (1:S)] + tcrossprod(mu1)
    V11_3 <- tcrossprod(AA, E22INV)
    V12_1 <- tcrossprod(mu1, mu3) + sigmab[R + (1:S), 1:R, drop=FALSE]
  }

  # Create output table
  header <- c("N*n", "N", "n")
  if (BudgetConstraint == TRUE) {
    header <- c(header, "(n+c)*N")
    n = seq(start, end, step)
    N <- trunc(cost/(n+extra))
  } else {
    n = rep(seq(start, end, step), each = ((n2max-n2min)/n2step) + 1)
    N <- rep(seq(n2min, n2max, n2step), times = ((end-start)/step) + 1)
  }

  if (S > 0) {
    header <- c(header, paste("Fixed", 1:S, sep="_"))
  }
  header <- c(header, "Const")
  if (R > 1) {
    header <- c(header, paste("Group", 1:(R-1), sep="_"))
  }
  if (Q > 1) {
    header <- c(header, paste("Random", 1:(Q-1), sep="_"))
  }
  if (Q > 1 && R > 1) {
    header <- c(header, paste("Cross-L", 1:(Q-1)*(R-1), sep="_"))
  }
  results <- matrix(NA, length(n), length(header))
  results[, 1] <- N * n
  results[, 2] <- N
  results[, 3] <- n
  if (BudgetConstraint == TRUE) {
    results[, 4] <- N*(n+extra)
  }
  colnames(results) <- header

  # list of covariance matrices (one per row)
  covars <- list()

  # Perform calculations (one per row)
  for (count in 1:nrow(results)) {
    nlev1 <- results[count, "n"]
    nlev2 <- results[count, "N"]

    F <- solve(tau2 + resid*E22INV/nlev1)
    V22 <- F %x% V22_1
    if (S > 0) {
      V11 <- nlev1*V11_1 + crossprod(E, crossprod(F, E))[1, 1] * V11_2 + tcrossprod(tcrossprod(V11_3, F), V11_3)
      V12 <- tcrossprod(V11_3, F) %x% t(mu3) + crossprod(E, F) %x% V12_1
      V <- nlev2*rbind(cbind(V11, V12), cbind(t(V12), V22))
    } else {
      V <- nlev2*V22
    }

    Vinv <- solve(V)
    covars[[paste0("N = ", nlev2, "; n = ", nlev1)]] <- Vinv
    result <- sqrt(diag(Vinv))
    results[count, (ncol(results) - length(result) + 1):ncol(results)] <- result
  }

  # Save outputs
  output <- list()
  output$results <- results
  output$covars <- covars
  output$S <- S
  output$Q <- Q
  output$R <- R
  output$BudgetConstraint <- BudgetConstraint
  if (BudgetConstraint == TRUE) {
    output$cost <- cost
    output$extra <- extra
  } else {
    output$n2min <- n2min
    output$n2max <- n2max
    output$n2step <- n2step
  }
  output$start <- start
  output$end <- end
  output$step <- step
  output$sigmaw <- sigmaw
  output$sigmab <- sigmab
  output$resid <- resid
  output$tau2 <- tau2
  output$mu1 <- mu1
  output$mu3 <- mu3
  output$E <- E

  class(output) <- "PINTresult"
  return(output)
}

#'@export
print.PINTresult <- function(x, ...) {
  cat("Design:\n")
  cat("(Between parentheses, the symbol is mentioned that is used for this parameter;\n")
  cat(" if the PINT manual uses a different symbol than the Snijders-Bosker paper,\n")
  cat(" then the symbol from the paper is given between parentheses,\n")
  cat(" the symbol from the manual between square brackets.)\n\n")

  cat("NUMBER OF FIXED EFFECTS              (K_1)            [L_1    ]       :", x$S, "\n")
  cat("NUMBER OF RANDOM EFFECTS INCL. CONST (K_2)            [L_2 + 1]       :", x$Q, "\n")
  cat("NUMBER OF LEVEL-2 VARS INCL. CONST   (length of W_3j) [L_3 + 1]       :", x$R, "\n")
  if (x$BudgetConstraint == TRUE) {
    cat("TOTAL COSTS                          (k)              [K]             :", x$cost, "\n")
    cat("RELATIVE COST PER LEVEL-2 UNIT       (c)                              :", x$extra, "\n")
  }
  cat("SMALLEST VALUE OF n                                   [n_min]         :", x$start, "\n")
  cat("STEP SIZE FOR n                                       [n_step]        :", x$step, "\n")
  cat("LARGEST VALUE FOR n                                   [n_max]         :", x$end, "\n")
  if (x$BudgetConstraint == FALSE) {
    cat("SMALLEST NUMBER OF GROUPS                             [N_2min]        :", x$n2min, "\n")
    cat("LARGEST NUMBER OF GROUPS                              [N_2max]        :", x$n2max, "\n")
  }

  cat("\nParameters:\n")
  cat("\nWITHIN-GROUPS COVARIANCE MATRIX  (SIGMA-W)\n\n")
  print(x$sigmaw)

  cat("\nBETWEEN-GROUPS COVARIANCE MATRIX (SIGMA-B)\n\n")
  print(x$sigmab)

  cat("\nRESIDUAL VARIANCE (sigma-squared)\n\n")
  print(x$resid)

  cat("\nCOVARIANCE MATRIX OF RANDOM EFFECTS (tau_2)\n\n")
  print(x$tau2)

  cat("\nEXPECTATION OF LEVEL-1 VARIABLES WITH FIXED EFFECTS (mu_1)\n\n")
  print(x$mu1)

  cat("\nEXPECTATION OF LEVEL-2 VARIABLES (mu_3)\n\n")
  print(x$mu3)

  cat("\nCONSTANT MEAN VECTOR (mu_2 = e)\n\n")
  print(x$E)

  cat("\nThe following table contains the standard errors (s.e.):\n")
  cat("Fixed:   s.e. of regr. coeff.s of level-1 variables with a fixed effect only.\n")
  cat("Const:   s.e. of the intercept.\n")
  if (x$R > 1) {
    cat("Group:   s.e. of regr. coeff.s of level-2 variables.\n")
  }
  if (x$Q > 1) {
    cat("Random:  s.e. of regr. coeff.s of level-1 variables with a random effect.\n")
    cat("Cross-L: s.e. of regr. coeff.s of cross-level interactions\n")
    cat("         (product of \"Group\" with \"Random effect\" variables).\n")
  }
  cat("\n")
  print(x$results)
}
