setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.3.2<-read.table("klpga.txt", header=T)
X<-Data1.3.2[,-1]

# Descriptive Statistics
summary(X)

# Covariance Matrix
cov(X)

# Correlation Matrix
cor(X)

# Multiple Scatter Plot
plot(X)

# Boxplot of 3 Subjects 
boxplot(X)
