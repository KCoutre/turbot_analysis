
library(lubridate)
library(ggplot2)
library(ggmap)
library(plyr)
library(maptools)
library(dplyr)
library(gridExtra)
install.packages("gridExtra")
#load tag detection dataframe and the release/recapture dataframe, which were setup in turbot_setup.R 

load("release_recapture.Rda")
load("df_raw.Rda")
head(df_raw)

# subset sections where each tag was in water based on release/recovery dates (RR dataframe)

df <- data.frame()

for (i in 1:nrow(RR))
{
  tag_num <- RR[i,"PRIM_TAG_NUM"]
  start<-RR[RR$PRIM_TAG_NUM==tag_num,"HAUL_DATE"]
  end<-RR[RR$PRIM_TAG_NUM==tag_num,"rec_utc"]
  
  
  
  df <- rbind(df,
                 df_raw[df_raw$TAG_NUM==tag_num &                #change df to dataset to match old code if desired
                           df_raw$Time_UTC >= start &
                           df_raw$Time_UTC <= end,])
  
  
}

save(df, file="df_check.Rda")
## Tag 406 stopped working in Jan 2014 though it was not recaptured until August 2014. 
## Filtering by recap date is not enough for tag 406
## I zoomed in on the first week of Jan in 2014 and found that Jan 3 is the failure date

# Remove detections past Jan 3, 2014 for tag 406 in df using dplyr package filter function:

new_df <- filter(df, !(TAG_NUM == 406 & Time_UTC > as.POSIXct("2014-01-02 23:59:00")))

#Checked above step w/ following code:

# sub.406<-subset(df,TAG_NUM==406)
# sub.406.2<-subset(sub.406, Time_UTC < as.Date("2014-01-03"))<---- same number of obs. as 406 in new_df

# so we can save new_df as df

df<-new_df
#and save it
save(df, file="df.Rda") # can go back to the df_check.Rda file if needed from b4 406 subset

### add column for sex

#setup
names(RR)[names(RR) == 'PRIM_TAG_NUM'] <- 'TAG_NUM'
rr<-select(RR, TAG_NUM, SEX)
rr$TAG_NUM<-as.character(rr$TAG_NUM)
rr<-rr[!rr$TAG_NUM== 383,]
rr<-unique(rr)
test<-df
test$TAG_NUM<-as.character(test$TAG_NUM)

#join & fix the name
test.3<-inner_join(test,rr)
head(test.3)
test.3$Sex<-test.3$SEX
test.3$SEX<-NULL
df<-test.3

save(df, file="df_join.Rda")   ###

setwd("~/Rstuff")
load("df_join.Rda")
load("release_recapture.Rda")
df2 <- data.frame()

for (i in 1:nrow(RR))
{
  tag_num <- RR[i,"PRIM_TAG_NUM"]
  start<-RR[RR$PRIM_TAG_NUM==tag_num,"HAUL_DATE"] + days(2)
  end<-RR[RR$PRIM_TAG_NUM==tag_num,"rec_utc"] - days(2)
  
  
  
  df2 <- rbind(df2,
               df[df$TAG_NUM==tag_num &                #change df to dataset to match old code if desired
                    df$Time_UTC >= start &
                    df$Time_UTC <= end,])
  
  
}

head(df2)
# remove 406 error time. cutoff determined at 8/19/13 based on interactive depth plot.
cutoff<-ymd_hm("2013-08-13 00:00", tz="UTC")
new_df<-df2[df2$Time_UTC < cutoff,]
df2<-new_df
save(df2, file="df_analyze.Rda") 
##############################################################################################
# average depth for ind fish
mean_depth<-ddply(df, .(TAG_NUM), summarize, ind_mean=mean(Depth_M), sdev=sd(Depth_M))
mean_depth
summary_table<-ddply(df2, .(TAG_NUM), summarize, ind_mean_depth=mean(Depth_M),
                     sdev_d=sd(Depth_M), mean_temp=mean(Temp_C),
                               sd_t=sd(Temp_C))

ranges<-ddply(df2, .(TAG_NUM), summarise, min_d=min(Depth_M),max_d=max(Depth_M), min_t=min(Temp_C), max_t=max(Temp_C))
#monthly boxplot fish combined
head(df)
df2$month<-month(df2$Time_UTC)
ggplot(df, aes(factor(month),-Depth_M))+
  geom_boxplot()+
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

#depth by sex # would be int. to do just monthly means w/ no err
ggplot(df, aes(factor(month), -Depth_M, color=TAG_NUM)) +
  geom_boxplot() +
  facet_wrap(~Sex) +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# get monthly means
mm<-ddply(df, .(TAG_NUM,month), summarise, mean=mean(Depth_M), sd=sd(Depth_M))
mm<-inner_join(mm,rr)

ggplot(mm, aes(month, -mean, color=TAG_NUM)) +
  geom_line() +
  facet_wrap(~SEX) +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))
#monthly, individual boxplots
ggplot(df2, aes(factor(month), -Depth_M)) +
  geom_boxplot() +
  facet_wrap(~TAG_NUM) +
  labs(x="Month", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

#relationship with month to depth
library(lme4)
month_model<-lm(Depth_M~month+Sex, df)
summary(month_model)
#.....add sex... will get to this
df[df$TAG_NUM==c(386,390,5282)]


#basic hist
ggplot(df15, aes(x=-Depth_M))+geom_histogram(binwidth = 100)+coord_flip()+
  labs(x="Depth", y="Frequency")

ggplot(df15, aes(x=Temp_C))+geom_histogram(binwidth = 0.5)+coord_flip()+
  labs(x="Temperature (Degrees C)", y="Frequency")

#daily depth range
?ddply

jan_d<-subset(df, month==1)
ggplot(jan_d, aes(Time3, -Depth_M)) +
  geom_point() +facet_wrap(~TAG_NUM, scales = "free_x")

#feb 1
feb1<-df[df$TimeUTC>=as.POSIXct('2004-02-01 00:00:00', tz="UTC") & df$Time_UTC<as.POSIXct('2004-02-02 00:00:00', tz="UTC"),]
ggplot(feb1, aes(Time_UTC, -Depth_M, color=TAG_NUM)) +
  geom_point(size=1.8)+facet_wrap(~TAG_NUM)

ggplot(feb1, aes(Time_UTC, -Depth_M, color=TAG_NUM)) +
  geom_line(size=1.8)
+theme(axis.title = element_text(size=18),
       axis.text.y  = element_text(size=16))
#aug 1
aug1<-df[df$Time3>=as.POSIXct('2003-08-01 00:00:00', tz="UTC") & df$Time3<as.POSIXct('2003-08-02 00:00:00', tz="UTC"),]
ggplot(aug1, aes(Time3, -Depth_M, color=TAG_NUM)) +
  geom_point(size=1.8)+facet_wrap(~TAG_NUM)

ggplot(aug1, aes(Time3, -Depth_M, color=TAG_NUM)) +
  geom_line(size=1.8)
#combine on same plot?
?hours()
ggplot(aug1, aes(hour(Time3), -Depth_M, color=TAG_NUM)) +
  geom_line(size=1.5)+geom_line(feb1, aes(x=hour(Time3),y=-Depth_M, color=TAG_NUM))
+facet_wrap(~TAG_NUM)

?as.POSIXct
df$day_bin<-trunc(df$Time3,"days")
daily_depth<-ddply(df, .(TAG_NUM, day_bin), summarize, day_mean=mean(Depth_M), sdev=sd(Depth_M))
daily_depth$month<-month(daily_depth$day_bin)
#, day_range=range(Depth_M)
ggplot(daily_depth, aes(day_bin, -day_mean)) +
  geom_point() +facet_wrap(~month)
ggplot(daily_depth, aes(day_bin, sdev)) +
  geom_point() 
jan_d<-subset(daily_depth, month==1)
ggplot(jan_d, aes(day_bin, -day_mean)) +
  geom_point() +facet_wrap(~TAG_NUM, scales = "free_x")
############ facet plot to view all tags by depth #######

# from gist

depth_plot<-ggplot(df15, aes(na.omit(Time_UTC), -na.omit(Depth_M))) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales = "free_x") +
  labs(x="Time", y="Depth (Meters)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

# need to remove tag 5282 before plotting temperature

t_sub<-df[!df$TAG_NUM==5282,]

temp_plot<-ggplot(df, aes(Time_UTC, na.omit(Temp_C))) +
  geom_point() +
  facet_wrap(~TAG_NUM, scales = "free_x") +
  scale_y_continuous(limits=c(0,6)) +
  labs(x="Time", y="Temperature (degrees C)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))


# from before
# p<-ggplot(dataset, aes(Time3,-Depth_M))+geom_point()
# 
# p+facet_grid(.~ TAG_NUM)
# 
# p<-ggplot(t1, aes(Time,-as.numeric(Depth_M)))+geom_point()
# 
# p+facet_grid(.~ TAG_NUM, scales = "free_x")

#turbo<-subset(tags, time <= ym(2012-07-01 01:12:25)) # didnt work
turbo<-tag_db[year(tag_db$date_time)< 2013,]
subset_midday<-subset(tag_db,hour(date_time) > 11 & hour(date_time) < 13)

p2<-ggplot(tags, aes(time,-depth))+geom_point()

p2+facet_grid(tag_num ~.)

p3<-ggplot(turbo, aes(time,-depth))+geom_point()

p3+facet_grid(tag_num ~.)


f386_2011<-subset(subset_date,tag_num==386)
p<-ggplot(f386_2011, aes(date_time,-depth))+geom_point()

#subset tag 386 through Aug. 2012
?subset
head(f386)
sub.386<-subset(f386,time< as.Date("2012-08-01"))
p4<-ggplot(sub.386, aes(time,-depth))+geom_point()
p4+geom_smooth()
p5<-ggplot(sub.386, aes(time, temp))+geom_point()+xlab("Time")+ylab("Temp")

#release information for tag 386: haul date 5/30/2011 HLAT: 58.8 HLONG: -177.72
#recovery information for tag386 : rec date 7/3/2013  RLAT: 59.45 RLONG: -177.74
#check release depth

#ggmap the release and recovery spots with bathymetry

map386<-get_map(location = c(lon = -177.72, lat = 58.8),
                zoom = 5, scale = "auto",
                maptype = "terrain",
                messaging = FALSE, urlonly = FALSE,
                filename = "ggmapTemp", crop = TRUE,
                color = "color",
                source = "google",
                api_key)

ggmap(map386)
#subset the time when tag 386 was shallow to look for diel patterns
#incorporate tide
#temperature

# # subset tags
f386<-subset(df,TAG_NUM==386)
f387<-subset(df,TAG_NUM==387)
f390<-subset(df,TAG_NUM==390)
f403<-subset(df,TAG_NUM==403)
f4122<-subset(df,TAG_NUM==4122)
f4126<-subset(df,TAG_NUM==4126)
f5364<-subset(df,TAG_NUM==5364)
f5319<-subset(df,TAG_NUM==5319)
f5282<-subset(df,TAG_NUM==5282)
f5385<-subset(df,TAG_NUM==5385)
f406 <-subset(df,TAG_NUM==406)

bf_t<-ggplot(f5282, aes(Time_UTC, Temp_C)) +
  geom_point()+
  labs(x="Time", y="Temperature (degrees C)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

bf_d<-ggplot(f5282, aes(Time_UTC, -Depth_M)) +
  geom_point()+
  labs(x="Time", y="-Depth (M)") +
  theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))

grid.arrange(bf_d, bf_t, ncol=1)


###For loop

for (i in 1:nrow(RR)){
  tag_num <- RR[i,"PRIM_TAG_NUM"]
  plot_df<-df[df$TAG_NUM==tag_num,]
  
  #temp plot
  bf_t<-ggplot(plot_df, aes(Time_UTC, Temp_C)) +
    geom_point()+
    labs(x="Time", y="Temperature (degrees C)") +
    theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))
  
  #depth plot
  bf_d<-ggplot(plot_df, aes(Time_UTC, -Depth_M)) +
    geom_point()+
    labs(x="Time", y="-Depth (M)") +
    theme(axis.title.x = element_text(size=16),axis.title.y  = element_text(size=16))
  
  #grid.arrange(bf_d, bf_t, ncol=1)
  file_name<-paste("TD_plots_", tag_num, ".tiff", sep="")
  tiff(file_name) # Open a new tiff file
  grid.arrange(bf_d, bf_t, nrow=2, ncol=1) # Write the grid.arrange in the file
  dev.off() #
}

