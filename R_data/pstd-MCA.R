setwd("C:/Users/stat/Desktop/����_WORK/�ܺ���û����/�λ����������")
ptsd<-read.table("ptsd.txt", header=T)
ptsd
library("ca")

v1 <- as.numeric(ptsd$�ھ�������>=3)
v2 <- as.numeric(ptsd$�λ�������>=3)
v3<- as.numeric(ptsd$��Ʈ������>=3)
v4 <- as.numeric(ptsd$������ӷ�>=6)

nptsd <- data.frame(ptsd[,1:3],�ھ�������=v1,�λ�������=v2, ��Ʈ������=v3, ������ӷ�=v4)

nptsd<-nptsd[, -1] 

mjca(nptsd)
par(pty="s")
plot(mjca(nptsd), main="Multiple CA for PTSD")

