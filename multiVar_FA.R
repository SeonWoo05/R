## (1)
setwd("C:/Users/sunpro/Git_Hub/R/R_data")
X <- read.table("trackrecord2005-women.txt", header = TRUE)
X

# Compute the correlation matrix
R <- cor(X)

# Perform eigenvalue decomposition
r_eigen <- eigen(R)
r_value <- r_eigen$values
r_vector <- r_eigen$vectors

# Display eigenvalues and the proportion of variance explained
r_value
r_gof <- r_value / sum(r_value) * 100
round(r_gof,3)

# Plot Scree Graph
plot(r_value, type = "b", main = "Scree Graph", xlab = "Factor Number", ylab = "Eigenvalue")



##(2)
library(psych)

# Standardize the data
Z <- scale(X, scale = TRUE)

# Perform PCFA without rotation
pcfa <- principal(Z, nfactors = 2, rotate = "none")

# Get and display the factor loadings
L <- pcfa$loadings[, 1:2]
round(L, 2)

# Perform PCFA with varimax rotation
pcfa_varimax <- principal(Z, nfactors = 2, rotate = "varimax")
L_varimax <- pcfa_varimax$loadings[, 1:2]
round(L_varimax, 2)

# Plot the factor loadings
par(mfrow = c(1, 2))
lim <- range(pretty(L))
plot(L[, 1], L[, 2], main = "Factor Loadings", xlab = "f1", ylab = "f2", xlim = lim, ylim = lim)
text(L[, 1], L[, 2], labels = rownames(L), cex = 0.8, col = "blue", pos = 1)
abline(v = 0, h = 0)
arrows(0, 0, L[, 1], L[, 2], col = 2, code = 2, length = 0.1)
plot(L_varimax[, 1], L_varimax[, 2], main = "Factor Loadings (Varimax)", xlab = "f1", ylab = "f2", xlim = lim, ylim = lim)
text(L_varimax[, 1], L_varimax[, 2], labels = rownames(L_varimax), cex = 0.8, col = "blue", pos = 1)
abline(v = 0, h = 0)
arrows(0, 0, L_varimax[, 1], L_varimax[, 2], col = 2, code = 2, length = 0.1)



##(3)
# Get factor scores from varimax-rotated solution
fpcfa <- pcfa_varimax$scores
round(fpcfa, 3)

# Plot factor scores
par(mfrow = c(1, 1))
par(pty = "s")
lim <- range(pretty(fpcfa))
plot(fpcfa[, 1], fpcfa[, 2], main = "Factor Scores: f1 and f2", xlab = "f1", ylab = "f2", xlim = lim, ylim = lim)
text(fpcfa[, 1], fpcfa[, 2], labels = rownames(fpcfa), cex = 0.8, col = "blue", pos = 1)
abline(v = 0, h = 0)



##(4)-(2)
# MLFA 수행하기
mlfa <- factanal(Z, factors = 2, rotation = "varimax", scores = "regression")
mlfa

L_mlfa <- mlfa$loadings[, 1:2]
round(L_mlfa,2)
xlim <- c(-0.4, 1.0)
ylim <- c(-0.4, 1.0)
plot(L_mlfa[,1], L_mlfa[,2], main="MLFA Factor Loadings", xlab="f1", ylab="f2",
     xlim=xlim, ylim=ylim)
text(L_mlfa[,1], L_mlfa[,2], labels=rownames(L_mlfa), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0, 0, L_mlfa[,1], L_mlfa[,2], col=2, code=2, length=0.1)

##(4)-(3)
# Get factor scores from MLFA varimax-rotated solution
fmlfa <- mlfa$scores
round(fmlfa, 3)

# Plot factor scores
par(mfrow = c(1, 1))
par(pty = "s")
lim <- range(pretty(fmlfa))
plot(fmlfa[, 1], fmlfa[, 2], main = "Factor Scores: f1 and f2 (MLFA)", xlab = "f1", ylab = "f2", xlim = lim, ylim = lim)
text(fmlfa[, 1], fmlfa[, 2], labels = rownames(fmlfa), cex = 0.8, col = "blue", pos = 1)
abline(v = 0, h = 0)


##(4)-비교
#인자점수그림을 통한 결과 비교
par(mfrow=c(1,2))
par(pty="s")
lim<-range(pretty(fmlfa))
plot(fmlfa[,1],fpcfa[,1], main="Factor Scores : ml f1 and pc f1", xlab="ml f1", ylab="pc f1", xlim=lim, ylim=lim)
text(fmlfa[,1],fpcfa[,1], labels=rownames(fmlfa), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
plot(fmlfa[,2],fpcfa[,2], main="Factor Scores : ml f2 and pc f2", xlab="ml f2", ylab="pc f2", xlim=lim, ylim=lim)
text(fmlfa[,2],fpcfa[,2], labels=rownames(fmlfa), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)



##(5)
# 회전 전의 PCFA 결과
pcfa <- principal(Z, nfactors = 2, rotate = "none")
factor_matrix_pcfa <- pcfa$loadings[, 1:2]

# 회전 후의 PCFA 결과
pcfa_varimax <- principal(Z, nfactors = 2, rotate = "varimax")
factor_matrix_pcfa_varimax <- pcfa_varimax$loadings[, 1:2]

# 인자행렬도 그리기
lim <- range(pretty(factor_matrix_pcfa, factor_matrix_pcfa_varimax))
par(mfrow=c(1,2))
par(pty="s")

# 회전 전의 PCFA
plot(factor_matrix_pcfa, type="n", main="Factor Matrix (PCFA - No Rotation)", 
     xlab="Factor 1", ylab="Factor 2", xlim=lim, ylim=lim)
text(factor_matrix_pcfa, labels=rownames(factor_matrix_pcfa), cex=0.8, col="blue", pos=1)
arrows(0,0, factor_matrix_pcfa[,1], factor_matrix_pcfa[,2], col=2, code=2, length=0.1)

# 회전 후의 PCFA
plot(factor_matrix_pcfa_varimax, type="n", main="Factor Matrix (PCFA - Varimax Rotation)", 
     xlab="Factor 1", ylab="Factor 2", xlim=lim, ylim=lim)
text(factor_matrix_pcfa_varimax, labels=rownames(factor_matrix_pcfa_varimax), cex=0.8, col="blue", pos=1)
arrows(0,0, factor_matrix_pcfa_varimax[,1], factor_matrix_pcfa_varimax[,2], col=2, code=2, length=0.1)

# Varimax 회전 행렬 구하기
varimax <- varimax(L_varimax)
T <- varimax$rotmat
T
