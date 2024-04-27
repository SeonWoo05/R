library(shapes)
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
mouseT2<-read.in("mouseT2.txt",6,2)
cm<-mouseT2[,,1:30] # �����׷� n1=30
sm<-mouseT2[,,31:53] # �����׷� n2=23
lm<-mouseT2[,,54:76] # ū �׷� n3=23

booksm<-bookstein2d(sm)
plotshapes(sm,color=1:6, symbol=1, joinline=c(1, 6, 2, 3, 4, 5,1))
plotshapes(booksm$mshape,color=1:6, symbol=1, joinline=c(1, 6, 2, 3, 4, 5,1))
plotshapes(booksm$bshpv,color=1:6, symbol=1, joinline=c(1, 6, 2, 3, 4, 5,1))
list(sm,booksm$bshpv) 

