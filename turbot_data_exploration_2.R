library(lubridate)
library(ggplot2)
library(ggmap)
library(plyr)
library(maptools)
library(dplyr)
library(gridExtra)

setwd("~/Rstuff")
load("df_analyze.Rda")
load("release_recapture.Rda")
load("df_year_plus_15int.Rda")


ggplot(df2[!df2$TAG_NUM==406,], aes(Temp_C, -Depth_M, color=Temp_C)) +
  geom_point(size=1.8)+facet_wrap(~TAG_NUM)

df2$hour<-hour(df2$Time_UTC)
hrly<-ddply(df2, .(TAG_NUM, hour), summarize, depth_change=max(Depth_M)-min(Depth_M), var=var(Depth_M))

ggplot(hrly, aes(hour, var, color=TAG_NUM)) +
  geom_line(size=1.8)
ggplot(df2, aes(factor(month),-Depth_M))+
  geom_boxplot(aes(colour=factor(Sex)))

sub<-df2[df2$TAG_NUM==386,]
min(sub$Temp_C)

##### new summary of depths by fish
ggplot(df2, aes(na.omit(Time_UTC), -na.omit(Depth_M))) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales="free_x") +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))


## smoothing for annual trends 
yr_plus<-filter(df2, TAG_NUM == "386"|TAG_NUM == "406"|TAG_NUM == "4122"|TAG_NUM == "4126"|TAG_NUM == "5282"|TAG_NUM == "5319"|TAG_NUM == "5364"|TAG_NUM == "5385")

# df15 is already yr. plus and at 15 min intervals
ggplot(df15, aes(jday, -na.omit(Depth_M))) +
  stat_smooth(span=.5) +
  facet_wrap(~TAG_NUM) +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))


# # subset tags
a<-filter(df15,TAG_NUM==386)
b<-filter(df15,TAG_NUM==4122)
c<-filter(df15,TAG_NUM==4126)
d<-filter(df15,TAG_NUM==5364)
e<-filter(df15,TAG_NUM==5319)
f<-filter(df15,TAG_NUM==5282)
g<-filter(df15,TAG_NUM==5385)
h <-filter(df15,TAG_NUM==406)
table(df15$TAG_NUM)
b$ID <- seq.int(nrow(b))

tags<- c("f386", "f406", "f4122", "f4126", "f5364", "f5319", "f5282","f5385")
as.data.frame(tags)
# make a plot for each one but set all axes the same, use grid extra, xlim ylim

str(df15)
bw<-theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  #depth plots

  p1<-ggplot(a, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw+
    labs(x="Time", y="Depth (Meters)") + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x = element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p2<-ggplot(b, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)") + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x =element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p3<-ggplot(c, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)")  + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x =element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p4<-ggplot(d, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)")  + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x = element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p5<-ggplot(e, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)")  + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x =element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p6<-ggplot(f, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)") + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x = element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())
  p7<-ggplot(g, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)") + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x = element_blank(),axis.title.y  =element_blank(),axis.text.x = element_blank())
  p8<-ggplot(h, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw +
    labs(x="Time", y="Depth (Meters)") + scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
    theme(axis.title.x = element_text(size=12),axis.title.y  = element_blank())
  #grid.arrange(bf_d, bf_t, ncol=1)
#  file_name<-paste("TD_plots_", tag_num, ".tiff", sep="")
#  tiff(file_name) # Open a new tiff file
  grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow=8, ncol=1) # Write the grid.arrange in the file
 
  
   dev.off() #

qplot(Temp_C, data=df15, geom="histogram", bins=15)
qplot(Depth_M, data=df15, geom="histogram", bins=15, color=Temp_C)
ggplot(df15, aes(x=Temp_C), color=Temp_C) + geom_histogram(bins=15)

#Temp depth profile
ggplot(df15, aes(Temp_C, -Depth_M, color = Pool))+geom_point()+bw
ggplot(df15, aes(Temp_C, -Depth_M, color = TAG_NUM))+geom_point()+bw
ggplot(df15, aes(Temp_C, -Depth_M, color = Temp_C))+geom_point()+bw

# depth boxplot
ggplot(df15, aes(TAG_NUM, -Depth_M, color = factor(Sex)))+geom_boxplot()+bw+
  labs(x="Fish", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# example depth time series: 2 panel plot, 1 week and full TS
ggplot(g, aes(Time_UTC, -Depth_M, color = factor(code))) + geom_point()
ggplot(f, aes(Time_UTC, -Depth_M )) + 
  geom_point() + bw + 
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# decided to use fish c, 4126, this one moved quite far, from aleutians to BS  ## NEVERMIND changed my mind
p1<-ggplot(f, aes(Time_UTC, -Depth_M )) + 
  geom_point(shape=1) + bw + 
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14))

#sub<-filter(c, jday >= 213 & jday<=219)
#ggplot(sub, aes(Time_UTC, -Depth_M )) + 
#  geom_point() + bw + 
#  labs(x="Time", y="Depth (Meters)") +
#  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# now for temp

p2<-ggplot(f, aes(Time_UTC, Temp_C )) + 
  geom_point(shape=1) + bw + 
  labs(x="Time", y="Temperature (C)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14))

#sub<-filter(c, jday >= 213 & jday<=219)
#ggplot(sub, aes(Time_UTC, Temp_C )) + 
#  geom_point() + bw + 
#  labs(x="Time", y="Temperature (c)") +
#  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

grid.arrange(p1,p2, nrow=2, ncol=1)

ggplot(b, aes(Time_UTC, Temp_C )) + 
  geom_point() + bw + 
  labs(x="Time", y="Temperature (C)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

c$Temp_C<-round(c$Temp_C, 1)
# adding warm year / cold year data to df15

Year<-c(2003:2013)
Pool<-c("W","W","W","T","C","C","C","C","C","C","C")
cp<-data.frame(Year,Pool)
head(df15)
df15$Year<-year(df15$Time_UTC)
df15<-inner_join(df15,cp)


#sizes caught
ggplot(RR, aes(factor(PRIM_TAG_TYPE), HSIZE)) + geom_boxplot()
range(RR$HSIZE)
range(RR$RSIZE, na.rm=TRUE)

# average depth for ind fish
mean_depth<-ddply(df15, .(TAG_NUM), summarize, Mean=trunc(mean(Depth_M)), SD=trunc(sd(Depth_M)))



ggplot(df15, aes(ID, -Depth_M)) + stat_smooth(span=.5) + bw+
  labs(x="Time", y="Depth (Meters)") + 
  scale_x_continuous(limits=c(0,105313),breaks=c(1,35040, 70080, 105120)) +
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_blank(),
        axis.text.x = element_blank())
tail(g)

#wtf is wrong with 406 it was AL for 2 years, has enough detections for 3 yrs.

df15$time_diff<-NA
df15$time_diff[1]<-0
for (i in 2:nrow(df15)){
  print(i)
  df15$time_diff[i]<-df15[i,"Time_UTC"]-df15[i-1,"Time_UTC"]
  
}

table(h$time_diff)
h$time_diff==-803.614583333333]
h$time_diff<-trunc(h$time_diff)
?trunc
h[h$time_diff==-803,]
-803 %in% h$time_diff
h[77140:77180,]
dupes<-duplicated(h$Time_UTC)
table(dupes)

check_dupes<-ddply(df15, .(TAG_NUM), summarize, num_dup= length(which(duplicated(Time_UTC)==TRUE)), nrow=length(Time_UTC))

test<-df15 %>% group_by(TAG_NUM) %>% filter(!duplicated(Time_UTC))
head(test)
nrow(df15)-nrow(test)
head(test)
table(test$TAG_NUM)
df15<-test
df15<-as.data.frame(df15)
save(df15, file="df_final.Rda")



# activity

monthly_act<-ddply(df15, .(TAG_NUM, month), summarize, 
               monthly_max=max(act, na.rm=TRUE),
               monthly_min=min(act, na.rm=TRUE), 
               monthly_median=mean(act, na.rm=TRUE))
ggplot()+geom_line(data=monthly_act, aes(month , -monthly_max)) +
  geom_line(data=monthly_act, aes(month,-monthly_min))+
  bw + facet_wrap(~TAG_NUM, nrow=2) + 
  geom_hline(yintercept=0, color="red") + 
  geom_line(data=monthly_act,aes(month, -monthly_median))

length(which(df15$act==0))/nrow(df15)
