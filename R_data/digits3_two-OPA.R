library(shapes)
setwd("c:/R과 함께하는 다변량자료분석/R_code_data")
digit3<-read.in("digit3.txt",13,2)
digit3_two<-digit3[,, 2:3]
A<-digit3_two[,,1]
B<-digit3_two[,,2]
plotshapes(A, B,symbol=1, joinline=1:13)
PAB<-procOPA(B, A) #matching A onto B
plotshapes(PAB$Bhat,symbol=1, joinline=1:13)
PBA<-procOPA(A, B) #matching B onto A
plotshapes(PBA$Bhat,symbol=1, joinline=1:13)

