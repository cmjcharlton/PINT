library(PINT)

# 2.3.3a
resid <- 0.839
tau2 <- matrix(0.161, 1, 1)
sigmaw <- matrix(0.24, 1, 1)
sigmab <- matrix(0, 1, 1)
mu1 <- matrix(0.6, 1, 1)

ans1 <- PINT(start = 10, step = -10, end = 60, n2min = 10, n2max = 50, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu1 = mu1)
print(ans1)

# 2.3.3b
resid <- 0.839
tau2 <- matrix(0.161, 1, 1)
sigmaw <- matrix(0.12, 1, 1)
sigmab <- matrix(0.12, 1, 1)
mu1 <- matrix(0.6, 1, 1)

ans2 <- PINT(start = 10, step = -10, end = 60, n2min = 10, n2max = 50, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu1 = mu1)
print(ans2)

#2.3.4
resid <- 0.848
tau2 <- matrix(0.159, 1, 1)
sigmab <- matrix(0.249, 1, 1)
mu3 <- matrix(0.462, 1, 1)

ans3 <- PINT(start = 40, step = -10, end = 40, n2min = 10, n2max = 150, resid = resid, tau2 = tau2, sigmab = sigmab, mu3 = mu3)
print(ans3)

# 2.3.5
resid <- 0.562
tau2 <- matrix(0.081, 1, 1)
sigmaw <- matrix(c(0.12, 0.02, 0.02, 0.902), 2, 2)
sigmab <- matrix(c(0.249, 0.045, -0.006, 0.045, 0.125, 0.013, -0.006, 0.013, 0.116), 3, 3)
mu1 <- matrix(c(0.6, 0), 2, 1)
mu3 <- matrix(0.462, 1, 1)
ans4 <- PINT(start = 40, step = -10, end = 40, n2min = 10, n2max = 150, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu1 = mu1, mu3 = mu3)
print(ans4)

# 2.4
resid <- 8.098
tau2 <- matrix(c(1.215, 0.198, 0.198, 0.18), 2, 2)
sigmaw <- matrix(0.249964, 1, 1)
ans5 <- PINT(start = 5, step = -5, end = 25, n2min = 20, n2max = 100, resid = resid, tau2 = tau2, sigmaw = sigmaw)
print(ans5)
