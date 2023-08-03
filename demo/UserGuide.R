library(PINT)

# 4.3
resid <- 0.5
tau2 <- matrix(c(0.09, -0.01, -0.01, 0.0075), 2, 2)
sigmaw <- matrix(c(0.8, 0.3, 0.3, 1), 2, 2)
sigmab <- matrix(c(1, -0.13, -0.13, 0.2), 2, 2)
mu1 <- matrix(0, 1, 1)
mu3 <- matrix(0, 1, 1)
ans1 <- PINT(start = 10, step = 5, end = 60, cost = 1000, extra = 5, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu1 = mu1, mu3 = mu3)

# 4.4.1
print(ans1)

# 4.4.2
print(ans1$covar)

# 5.1
resid <- 0.8
tau2 <- matrix(0.2, 1, 1)
ans2 <- PINT(start = 1, step = 1, end = 30, cost = 2500, extra = 0, resid = resid, tau2 = tau2)
print(ans2)

ans3 <-  PINT(start = 25, step = 1, end = 35, cost = 17000, extra = 0, resid = resid, tau2 = tau2)
print(ans3)

# 5.2
resid <- 0.7
tau2 <- matrix(0.1, 1, 1)
sigmaw <- matrix(0.9, 1, 1)
sigmab <- matrix(0.1, 1, 1)
mu1 <- matrix(0, 1, 1)
ans4 <-  PINT(start = 8, step = 2, end = 30, cost = 1000, extra = 8, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu1 = mu1)
print(ans4)

# 5.3
resid <- 0.8
tau2 <- matrix(0.19, 1, 1)
sigmab <- matrix(1, 1, 1)
mu3 <- matrix(0, 1, 1)
ans5 <-  PINT(start = 1, step = 1, end = 23, cost = 1000, extra = 23, resid = resid, tau2 = tau2, sigmab = sigmab, mu3 = mu3)
print(ans5)

# 5.4
resid <- 0.25
tau2 <- matrix(c(0.125, 0.09, 0.09, 0.125), 2, 2)
sigmaw <- matrix(1, 1, 1)
sigmab <- matrix(1, 1, 1)
mu3 <- matrix(0, 1, 1)

ans6 <-  PINT(start = 3, step = 1, end = 30, cost = 1280, extra = 26, resid = resid, tau2 = tau2, sigmaw = sigmaw, sigmab = sigmab, mu3 = mu3)
print(ans6)
