
setwd("~/Rstuff")
load("df_join.Rda")

library(lubridate)
library(ggplot2)
library(ggmap)
library(plyr)
library(maptools)
library(dplyr)
library(gridExtra)

#### subsetting the temps which are considered 'cold pool' in Bering sea
cp<-filter(df, Temp_C <= 2)
head(cp)
str(cp)
table(cp$TAG_NUM)

#plot the depths when these fish are in temp conditions of CP
depth_plot<-ggplot(cp, aes(na.omit(Time_UTC), -na.omit(Depth_M))) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales = "free_x") +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

plot(df$Temp_C, df$Depth_M)

#error in some temps
err<-filter(df, Temp_C <= -15)
head(err)
table(err$TAG_NUM)
