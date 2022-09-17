Mex<-c(MIST2$MEX) 
Tur<-c(MIST2$TUR)
Ind<-c(MIST2$IND)
Combine_Groups<-data.frame(cbind(Mex,Tur,Ind))
Combine_Groups 
Stack_Groups<-stack(Combine_Groups)
Stack_Groups
Anova_Results<-aov(values~ind,data=Stack_Groups)
summary(Anova_Results)
TukeyHSD(Anova_Results)
pairwise.t.test(Stack_Groups$values,Stack_Groups$ind,p.adj="bonferroni")
plot(TukeyHSD(Anova_Results), main="Mexico, Indonesia and Turkey")