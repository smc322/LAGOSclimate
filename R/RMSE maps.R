library(ggplot)
library(cowplot)
library(RJSONIO)
library(magrittr)
library(ggsn)
library(scales)
library(ggmap)
library(dplyr)
library(gridExtra)
library(grid)

file_names_03<-c("chl_l3_03_rmse.rds", "n_l3_03_rmse.rds", "p_l3_03_rmse.rds", "sec_l3_03_rmse.rds")

file_names_05<-c("chl_l3_05_rmse.rds", "n_l3_05_rmse.rds", "p_l3_05_rmse.rds", "sec_l3_05_rmse.rds")

file_names_nobs<-c("nobs_chla.rds", "nobs_n.rds", "nobs_p.rds", "nobs_sec.rds")


for (i in 1:length(file_names_03)){
assign(gsub(".rds","",file_names_03[i]),readRDS(gzcon(url(paste("https://github.com/smc322/LAGOSclimate/blob/master/Data/",file_names_03[i],"?raw=true", sep="")))))
}


for (i in 1:length(file_names_05)){
  assign(gsub(".rds","",file_names_05[i]),readRDS(gzcon(url(paste("https://github.com/smc322/LAGOSclimate/blob/master/Data/",file_names_05[i],"?raw=true", sep="")))))
}

for (i in 1:length(file_names_nobs)){
  assign(gsub(".rds","",file_names_nobs[i]),readRDS(gzcon(url(paste("https://github.com/smc322/LAGOSclimate/blob/master/Data/",file_names_nobs[i],"?raw=true", sep="")))))
}



###number of observations by RMSE plots

#merging rmse and nobs data, then plotting

#chl
nobs_chla_rmse<-left_join(chl_l3_03_rmse[!is.na(chl_l3_03_rmse$RMSE),], nobs_chla, by=c("Lakeid"="lagoslakeid"))

chla_scatter<-ggplot() + geom_point(data=nobs_chla_rmse, aes(nobs_chla,RMSE), size=.5, alpha=.3) + labs(x="Number of Observations")+ theme(plot.background = element_rect(fill = 'white', color="black", linetype = "solid", size=1), text = element_text(size=6), axis.text = element_text(size=4),axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

#phosphorus
nobs_p_rmse<-left_join(p_l3_03_rmse[!is.na(p_l3_03_rmse$RMSE),], nobs_p, by=c("Lakeid"="lagoslakeid"))

p_scatter<-ggplot() + geom_point(data=nobs_p_rmse, aes(nobs_p,RMSE), size=.5, alpha=.3) + labs(x="Number of Observations")+ theme(plot.background = element_rect(fill = 'white', color="black", linetype = "solid", size=1), text = element_text(size=6), axis.text = element_text(size=4),axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))


#nitrogen
nobs_n_rmse<-left_join(n_l3_03_rmse[!is.na(n_l3_03_rmse$RMSE),], nobs_n, by=c("Lakeid"="lagoslakeid"))

n_scatter<-ggplot() + geom_point(data=nobs_n_rmse, aes(nobs_n,RMSE), size=.5, alpha=.3) + labs(x="Number of Observations")+ theme(plot.background = element_rect(fill = 'white', color="black", linetype = "solid", size=1), text = element_text(size=6), axis.text = element_text(size=4),axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

#secchi
nobs_sec_rmse<-left_join(sec_l3_03_rmse[!is.na(sec_l3_03_rmse$RMSE),], nobs_sec, by=c("Lakeid"="lagoslakeid"))

sec_scatter<-ggplot() + geom_point(data=nobs_sec_rmse, aes(nobs_sec,RMSE), size=.5, alpha=.3) + labs(x="Number of Observations")+ theme(plot.background = element_rect(fill = 'white', color="black", linetype = "solid", size=1), text = element_text(size=6), axis.text = element_text(size=4),axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))



style <- '[{"featureType":"administrative","elementType":"labels.text.fill","stylers":[{"color":"#444444"}]},{"featureType":"administrative.country","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"administrative.province","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"administrative.locality","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"administrative.neighborhood","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"landscape","elementType":"all","stylers":[{"color":"#f2f2f2"},{"visibility":"on"}]},{"featureType":"landscape.natural","elementType":"geometry.fill","stylers":[{"visibility":"on"},{"saturation":"17"}]},{"featureType":"poi","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"road","elementType":"all","stylers":[{"saturation":-100},{"lightness":45},{"visibility":"off"}]},{"featureType":"road.highway","elementType":"all","stylers":[{"visibility":"simplified"}]},{"featureType":"road.highway","elementType":"labels","stylers":[{"visibility":"off"}]},{"featureType":"road.arterial","elementType":"labels.icon","stylers":[{"visibility":"off"}]},{"featureType":"transit","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"water","elementType":"all","stylers":[{"color":"#aaaaaa"},{"visibility":"on"}]}]'
style_list <- fromJSON(style, asText=TRUE)

create_style_string<- function(style_list){
  style_string <- ""
  for(i in 1:length(style_list)){
    if("featureType" %in% names(style_list[[i]])){
      style_string <- paste0(style_string, "feature:", 
                             style_list[[i]]$featureType, "|")      
    }
    elements <- style_list[[i]]$stylers
    a <- lapply(elements, function(x)paste0(names(x), ":", x)) %>%
      unlist() %>%
      paste0(collapse="|")
    style_string <- paste0(style_string, a)
    if(i < length(style_list)){
      style_string <- paste0(style_string, "&style=")       
    }
  }  
  # google wants 0xff0000 not #ff0000
  style_string <- gsub("#", "0x", style_string)
  return(style_string)
}

style_string <- create_style_string(style_list)






map1<-get_googlemap(center="youngstown", style=style_string, extent="device", zoom=4,size = c(640, 500))


bb <- attr(map1, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))


###maps for 03 lambda val
chl_03_map<-ggmap(map1, extent="device", legend="bottomright", padding=c(0.001)) + geom_point(data=chl_l3_03_rmse[!is.na(chl_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(34.795344,48.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill="transparent"), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=9), legend.key.width =unit(1.2,"line")) +scalebar(data = bb2, dist = 250, dd2km = TRUE, model  = "WGS84",  location = "bottomright", height=.007, st.bottom=F,st.size=3,anchor = c(
  x = bb$ll.lon + .37* (bb$ur.lon - bb$ll.lon), 
  y = bb$ll.lat + .34*(bb$ur.lat - bb$ll.lat)
))

png(file="chl_rmse.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.23, height = 0.173, x = 0.767, y = 0.275) #plot area for the inset map
print(chl_03_map,vp=v1) 
print(chla_scatter,vp=v2)
dev.off()



n_03_map<-ggmap(map1, extent="device", legend="bottomright", padding=c(0.001)) + geom_point(data=n_l3_03_rmse[!is.na(n_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(34.795344,48.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill="transparent"), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=9), legend.key.width =unit(1.2,"line")) +scalebar(data = bb2, dist = 250, dd2km = TRUE, model  = "WGS84",  location = "bottomright", height=.007, st.bottom=F,st.size=3,anchor = c(
  x = bb$ll.lon + .37* (bb$ur.lon - bb$ll.lon), 
  y = bb$ll.lat + .34*(bb$ur.lat - bb$ll.lat)
))

png(file="n_rmse.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.23, height = 0.173, x = 0.767, y = 0.275) #plot area for the inset map
print(n_03_map,vp=v1) 
print(n_scatter,vp=v2)
dev.off()



p_03_map<-ggmap(map1, extent="device", legend="bottomright", padding=c(0.001)) + geom_point(data=p_l3_03_rmse[!is.na(p_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(34.795344,48.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill="transparent"), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=9), legend.key.width =unit(1.2,"line")) +scalebar(data = bb2, dist = 250, dd2km = TRUE, model  = "WGS84",  location = "bottomright", height=.007, st.bottom=F,st.size=3,anchor = c(
  x = bb$ll.lon + .37* (bb$ur.lon - bb$ll.lon), 
  y = bb$ll.lat + .34*(bb$ur.lat - bb$ll.lat)
))

png(file="p_rmse.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.23, height = 0.173, x = 0.767, y = 0.275) #plot area for the inset map
print(p_03_map,vp=v1) 
print(p_scatter,vp=v2)
dev.off()




sec_03_map<-ggmap(map1, extent="device", legend="bottomright", padding=c(0.001)) + geom_point(data=sec_l3_03_rmse[!is.na(sec_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(34.795344,48.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill="transparent"), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=9), legend.key.width =unit(1.2,"line")) +scalebar(data = bb2, dist = 250, dd2km = TRUE, model  = "WGS84",  location = "bottomright", height=.007, st.bottom=F,st.size=3,anchor = c(
  x = bb$ll.lon + .37* (bb$ur.lon - bb$ll.lon), 
  y = bb$ll.lat + .34*(bb$ur.lat - bb$ll.lat)
))

png(file="sec_rmse.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.23, height = 0.173, x = 0.767, y = 0.275) #plot area for the inset map
print(sec_03_map,vp=v1) 
print(sec_scatter,vp=v2)
dev.off()


# NOT USED FOR UPDATED MAP WITH SCATTERPLOT
# n_03_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=n_l3_03_rmse[!is.na(n_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# 
# 
# p_03_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=p_l3_03_rmse[!is.na(p_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# 
# sec_03_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=sec_l3_03_rmse[!is.na(sec_l3_03_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# panel_03<-plot_grid(chl_03_map, n_03_map, p_03_map, sec_03_map, ncol=2, nrow=2,labels = "auto")
# 
# ggsave("panel_03.png",plot = panel_03, device = "png", width=10.65, height=7)
# 
# 
# 
# ###maps for 05 lambda val
# 
# chl_05_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=chl_l3_05_rmse[!is.na(chl_l3_05_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +scalebar(data = bb2, dist = 250, dd2km = TRUE, model  = "WGS84",  location = "bottomright", height=.007, st.bottom=F,st.size=3,anchor = c(
#   x = bb$ll.lon + .67* (bb$ur.lon - bb$ll.lon), 
#   y = bb$ll.lat + .42*(bb$ur.lat - bb$ll.lat)
# ))
# 
# 
# 
# 
# 
# 
# n_05_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=n_l3_05_rmse[!is.na(n_l3_05_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# 
# 
# p_05_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=p_l3_05_rmse[!is.na(p_l3_05_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# 
# sec_05_map<-ggmap(map1, extent="device", legend="bottomright") + geom_point(data=sec_l3_05_rmse[!is.na(sec_l3_05_rmse$RMSE),], aes(x=lon, y=lat, color=RMSE), size=1.5, alpha=.4) + scale_x_continuous(limits=c(-95.630028,-66.615625))+ scale_y_continuous(limits=c(35.795344,49.678137))  + scale_color_gradientn(colors=rainbow(3) ,breaks=c(0,.5,1),labels=c(0,.5,">1"), limits=c(0,1), na.value="blue") + theme(axis.text=element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background  = element_rect(fill=alpha("white", .7)), legend.key = element_blank(), legend.title = element_blank(), legend.text=element_text(size=10), legend.key.width =unit(1.2,"line")) +guides(color=F)
# 
# panel_05<-plot_grid(chl_05_map, n_05_map, p_05_map, sec_05_map, ncol=2, nrow=2,labels = "auto")
# 
# ggsave("panel_05.png",plot = panel_05, device = "png", width=10.65, height=7)
# 
