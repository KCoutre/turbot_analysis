
library(mgcv)
library(lme4)

#cut gam(act~s(Temp_C)+s(jday)+factor(TAG_NUM)+factor(Sex)+factor(code), data=df15)
gamdf<-gamm(act~s(Temp_C)+factor(code)+s(jday), data=df15)
gamdf2<-gam(act~s(Temp_C), data=df15)
summary(gamdf)
plot(gamdf)
gam.check(gamdf)


gamdf3<-gam(Depth_M~s(Temp_C)+s(jday)+factor(TAG_NUM)+factor(Sex), data=df15)
summary(gamdf3)
plot(gamdf3)
gam.check(gamdf3)

glm.df<-glmer(act~Temp_C+factor(code)+jday+(1|TAG_NUM), data=df15)
glm.df<-glmer(Depth_M~Temp_C+factor(code)+jday+(1|TAG_NUM), data=df15)
summary(glm.df)
