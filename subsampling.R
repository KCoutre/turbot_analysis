library(lubridate)
library(ggplot2)
library(ggmap)
library(maptools)
library(dplyr)
library(gridExtra)

setwd("~/Rstuff")
load("df_analyze.Rda")
load("df_year_plus_15int.Rda")
load("release_recapture.Rda")
head(df15)

#subset tags that are 1 min interval to sample 1 out of 15
# 
yr_plus<-filter(df2, TAG_NUM == "386"|TAG_NUM == "406"|TAG_NUM == "4122"|TAG_NUM == "4126"|TAG_NUM == "5282"|TAG_NUM == "5319"|TAG_NUM == "5364"|TAG_NUM == "5385")
one_min<-filter(df2, TAG_NUM == "386"|TAG_NUM == "406")
fif_min<-filter(yr_plus, !TAG_NUM == "386" & !TAG_NUM == "406")
om_samp<-one_min[seq(1, nrow(one_min), 15), ]

#check w/ plot
ggplot(om_samp, aes(Time_UTC, -Depth_M)) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales="free_x") +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# add back together om_samp + fif_min.
# this will create df with all tags > 1yr sampled @ 15 min intervals.
?join
df15<-full_join(om_samp, fif_min)
 
save(df15, file="df_year_plus_15int.Rda")

#check w/ plot
ggplot(df15, aes(Time_UTC, -Depth_M)) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales="free_x") +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))



#calculate change in depth over 1 timestep to measure activity..
#this doesn't account for breaks between tag-num

head(df15)
df15$act<-NA
df15$act[1]<-0
for (i in 2:nrow(df15)){
  print(i)
df15$act[i]<-df15[i,"Depth_M"]-df15[i-1,"Depth_M"]

}
save(df15, file="df_year_plus_15int.Rda")  ##### saved over previous after the for loop
tail(df15)
ggplot(df15, aes(Time_UTC, -Depth_M)) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales="free_x") +
  labs(x="Time", y="Activity (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

ggplot(df15, aes(Time_UTC, -act)) +
  geom_point(shape=1) +
  facet_wrap(~TAG_NUM, scales="free_x") +
  labs(x="Time", y="-Activity (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

act_diel<-ddply(df15,.(TAG_NUM, month, code), summarise, mean_act=mean(act), sd_act=sd(act) )
p <- ggplot(act_diel, aes(fill=code, y=-mean_act, x=factor(month)))
p + geom_bar(position="dodge", stat="identity") + facet_wrap(~TAG_NUM)
geom_errorbar(limits, position=dodge, width=0.25)
dodge <- position_dodge(width=0.9)
limits <- aes(ymax = act_diel$mean_act + act_diel$sd_act, ymin= act_diel$mean_act - act_diel$sd_act)




#another way of calculating the same activity metric
# mean_depth<-ddply(df, .(TAG_NUM), summarize, ind_mean=mean(Depth_M), sdev=sd(Depth_M))
# ddply(df15, .(TAG_NUM), mutate, )

df15$jday<-yday(as.Date(df15$Time_UTC))
df15$hr_bin<-hour(df15$Time_UTC)
ggplot(df15, aes(jday, -act))+geom_point()
ggplot(hr_diel, aes(hr_bin, -mean_act, color=code))+geom_bar(position="dodge", stat="identity")+facet_wrap(~TAG_NUM)
hr_diel<-ddply(df15,.(TAG_NUM, hr_bin, code), summarise, mean_act=mean(act), sd_act=sd(act) )

diel_temp<-ddply(df15,.(TAG_NUM, month, code), summarise, mean_act=mean(Temp_C), sd_act=sd(Temp_C) )
p <- ggplot(diel_temp, aes(fill=code, y=mean_act, x=factor(month)))
p + geom_bar(position="dodge", stat="identity") + facet_wrap(~TAG_NUM)
geom_errorbar(limits, position=dodge, width=0.25)
dodge <- position_dodge(width=0.9)
limits <- aes(ymax = act_diel$mean_act + act_diel$sd_act, ymin= act_diel$mean_act - act_diel$sd_act)
