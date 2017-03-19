pcks <- list('dplyr', 'lubridate', 'ggmap', 'maptools', 'rgdal', 'maps', 'mapdata')
sapply(pcks, require, character = TRUE)
setwd("~/Rstuff")
rdf<-read.csv("Conv_Electr_Turb.csv")
head(rdf)
yr<-filter(rdf,TIME_OUT >=365 )
nyr<-filter(rdf,TIME_OUT < 365 )
ml<-filter(rdf,SEX == 1)
fml<-filter(rdf,SEX == 2)
arch<-filter(rdf,PRIM_TAG_TYPE == "E")
par(mfrow=c(1,1))
plot(rdf[,c("HLNG","HLAT")], type="p", main="Turbot Release Locations")
plot(rdf[,c("RLNG","RLAT")], type="p", main="Turbot Recovery Locations")

dev.off()
# 8/23/16
require(mapdata) # also loads maps
map("world", xlim = c(-179,-150), ylim = c(50,64), fill=TRUE, col="grey", bor=NA)
with(rdf, lines(HLNG, HLAT, pch=19, type="p", col=rgb(0,0,1,.3), cex=0.5))
box()

#segments command, draw the line between two points
data(wrld_simpl)
plot(wrld_simpl, ylim=c(50,64), xlim=c(-178,-165), col="lightgray", border="gray", axes=F) 
with(rdf, points(HLNG, HLAT, pch=19, col=1:length(PRIM_TAG_NUM), cex=0.5))
with(rdf, points(RLNG, RLAT, pch=19, col=1:length(PRIM_TAG_NUM), cex=0.5))
with(rdf, arrows(HLNG, HLAT, RLNG, RLAT, pch=19, cex=0.5, lty=2,col=1:length(PRIM_TAG_NUM)))
title("Release to Recapture of Tagged Turbot")
with(rdf, legend("bottomleft", legend=PRIM_TAG_NUM, col=1:length(PRIM_TAG_NUM), pch=15, cex=0.8, bty="n"))
box()
#text(x,y, labels)
#



## GGMAP
require(ggplot2)
ggplot(rdf, aes(x=HLNG, y=HLAT, col=PRIM_TAG_NUM)) + geom_point(size=2.5)
require(ggmap)
latrange <- c(48,70)
longrange <- c(-195,-150)
basemap <- get_map(location = c(longrange[1], latrange[1], longrange[2], latrange[2]), maptype = "satellite", source="google")
ggmap(basemap)
 #add data

ggmap(basemap) + geom_point(data = rdf, mapping = aes(x=HLNG, y=HLAT, col=I('red'), size=1.5)) + coord_map() + labs(x = "Longitude", y = "Latitude")

ggmap(basemap) + geom_segment(data = rdf, aes(y = HLAT, x = HLNG, yend = RLAT,
                                             xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow()) + 
  labs(title = "Release to Recovery of Tagged Turbot")

ggmap(basemap) + geom_segment(data = rdf, aes(y = HLAT, x = HLNG, yend = RLAT,
                                             xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow(), linetype=5) + 
  labs(title = "Release to Recovery of Tagged Turbot")
#would be cool to add the days out or make the line thickness based on the time @ liberty
ggsave(file="turbot_RR.pdf",width=8, height=8)
head(RR)


# over a year at liberty


ggmap(basemap) + geom_segment(data = yr, aes(y = HLAT, x = HLNG, yend = RLAT,
                                              xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow(),linetype=5) + 
  labs(title = "Release to Recovery of Tagged Turbot AL > 1 Yr")




# under a year


ggmap(basemap) + geom_segment(data = nyr, aes(y = HLAT, x = HLNG, yend = RLAT,
                                             xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow()) + 
  labs(title = "Release to Recovery of Tagged Turbot")

# Males

ggmap(basemap) + geom_segment(data = ml, aes(y = HLAT, x = HLNG, yend = RLAT,
                                              xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow()) + 
  labs(title = "Release to Recovery of Male Turbot")



# Females


ggmap(basemap) + geom_segment(data = fml, aes(y = HLAT, x = HLNG, yend = RLAT,
                                             xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow()) + 
  labs(title = "Release to Recovery of Female Turbot")



# archivals


ggmap(basemap) + geom_segment(data = arch, aes(y = HLAT, x = HLNG, yend = RLAT,
                                              xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow(), linetype=5) + 
  labs(title = "Release to Recovery of Archival Turbot")





rr<-subset(RR, PRIM_TAG_NUM==5282)

ggmap(basemap) + geom_segment(data = rr, aes(y = HLAT, x = HLNG, yend = RLAT,
                                             xend = RLNG, col=factor(PRIM_TAG_NUM)), 
                              arrow = arrow()) + 
  labs(title = "Release to Recovery of Fish 5282")+annotate("text", x = rr$HLNG+1, 
                                                            y=rr$HLAT+.5, label = "682 Days")

ggsave(file="turbot_5282.tiff",width=8, height=8)
