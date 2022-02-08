# Making a monochrome map in R for Fred
# but also to show myself some new packages for geospatial data in R
library(tidyverse)
library(ggplot2)
# for when we want to do direct querying
# library(DBI)
# library(RPostgres)
library(sf)

# Grab the data from various database nodes, or from discovery?
# q: is OTNDC discovery up to date?

# files for now, due to connectivity issues. later, can access GeoServer's otn:stations_history layers directly
# these files generated against OTNDBs with the following query:
# SELECT hdr.collectioncode,hdr.seriescode,hdr.ocean,hdr.station_name,hdr.station_type,hdr.latitude AS station_lat,hdr.longitude AS station_long,
# hdr.stationstatus,  hdr.locality
# ,his.instrument,his.type,his.deploy_date,his.last_download,his.recovery_date,his.downloads, his.off_set
# FROM discovery.stations_header hdr
# LEFT JOIN  discovery.stations_history_rows his
# ON hdr.collectioncode = his.collectioncode
# AND hdr.station_name = his.station_name

#otn_deps = read_csv('data/OTN_stations_receivers_202012090955.csv') # station_long and station_lat
otn_deps = read_csv("https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:stations_receivers&outputFormat=csv") %>%
            mutate(station_long = stn_long, station_lat = stn_lat)


fact_deps = read_csv('data/FACT_stations_receivers_202202071440.csv.csv')
# original matos receivers file:
matos_deps = read_csv('data/MATOS_stations_receivers_202202071419.csv.csv')
# Remove the project where it was a traveling citsci project
matos_deps = matos_deps %>% filter(collectioncode != 'PROJ126')

# TODO: What was this?
# post_push receivers file:
#matos_deps = read_csv('data/ACT_moorings_202006011446.csv')
#matos_deps = matos_deps %>% mutate(station_name=catalognumber,
#                                   deploy_date=startdatetime,
#                                   recover_date=enddatetime,
#                                   station_lat=latitude,
#                                   station_long=longitude
#                                   )

# OTN keeps track of which projects are also FACT members?
otn_fact_members = read_csv('data/OTN_FACT_membership_202003311657.csv')

fact_deps_in_otn = otn_deps %>% filter(collectioncode %in% otn_fact_members$collectioncode)
# How many distinct stations at each?
length(unique(matos_deps$station_name))
length(unique(fact_deps$station_name))
length(unique(otn_deps$station_name))

# What's the date range for each node?
otn_deps %>% filter(!is.na(as.Date(deploy_date))) %>% summarize(old_dep=min(deploy_date), new_dep=max(deploy_date))
fact_deps %>% filter(!is.na(as.Date(deploy_date))) %>% summarize(old_dep=min(deploy_date), new_dep=max(deploy_date))
matos_deps %>% filter(!is.na(as.Date(deploy_date))) %>% summarize(old_dep=min(deploy_date), new_dep=max(deploy_date))


theme_set(theme_bw())

otn_no_glatos_deps = otn_deps %>% 
  filter(!str_detect(collectioncode, 'V2LGLFC')) %>%
  filter(!str_detect(collectioncode, 'V2LEOR')) %>%
  filter(!str_detect(collectioncode, 'SMRSL')) %>%
  filter(!str_detect(collectioncode, 'V2LENGLB'))  %>%
  filter(!str_detect(collectioncode, 'ACT.')) %>% # otn holds some ACT metadata 
  filter(!str_detect(collectioncode, 'FACT.'))    # otn holds some FACT metadata
#  filter(!str_detect(collectioncode, 'LSB')) %>%
#  filter(!str_detect(collectioncode, 'GEERG')) %>%
#  filter(!str_detect(collectioncode, 'JDE'))

# Do we need the world's coastlines? the Great Lakes?
library(rnaturalearth)
library(rnaturalearthdata)
library(shadowtext)
world <- ne_countries(scale="medium", returnclass="sf")

# US-centric online example...
# library(maps)
# states <- st_as_sf(maps::map("state", plot=FALSE, fill=TRUE))
# head(states)
# states <- cbind(states, st_coordinates(st_centroid(states)))

# get state and provincial boundaries
state_prov <- st_as_sf(rnaturalearth::ne_states(c("united states of america", "canada")))
state_prov <- cbind(state_prov, st_coordinates(st_centroid(state_prov)))

# Get great lakes shapes
g_lakes <- st_as_sf(ne_download(scale=110, type='lakes', category='physical'))


# XLSX of Chuck's annotated positions
overview_pts = readxl::read_xlsx('data/Bangley - Telemetry Overview Points.xlsx')
# make a column of letters for the points
overview_pts = overview_pts %>% mutate(letter = row_number())


rpt_sizes = 2
### Make the plot ###
mad_plot = ggplot(data = world) +
  geom_sf() +
  
  # Add state / provincial boundaries
  geom_sf(data = state_prov, fill = NA) +

  # State / province labels
  #  geom_text(data = state_prov, aes(X, Y, label=name), size=2)+
  
  # White out great lakes
  geom_sf(data=g_lakes, fill='white')+
  

  # Network receiver deployments on top of one another
  geom_point(data = otn_no_glatos_deps, aes(x= station_long, y=station_lat), size=rpt_sizes, shape=16, color='darkblue') +
  geom_point(data = fact_deps, aes(x= station_long, y=station_lat), size=rpt_sizes, shape=16, color='darkred') +
  geom_point(data = matos_deps, aes(x= station_long, y=station_lat), size=rpt_sizes, shape=16, color='darkgreen') +

  # Plot FACT members hosted in OTN as both OTN and FACT
  # First attempt to show the dual nature of man, er.. receiver station.
  # geom_point(data = fact_deps_in_otn, aes(x=station_long, y=station_lat), size=rpt_sizes, shape=22, color='darkred', fill='darkblue') +
  geom_point(data = fact_deps_in_otn, aes(x=station_long, y=station_lat), size=rpt_sizes, shape=16, color='darkorchid3') +
  
#  Attempt to make two half circles  
#  geom_text(data=fact_deps_in_otn, aes(x=station_long, y=station_lat), size=1, label='\u25D0', 
#              color='darkred', family='Arial Unicode MS') +
#  geom_text(data=fact_deps_in_otn, aes(x=station_long, y=station_lat), size=1, label='\u25D1', 
#            color='darkblue', family='Arial Unicode MS') +
  
# Attempt to label the project code of each project to figure out which to cut
#  geom_text(data=otn_no_glatos_deps %>% 
#                    group_by(collectioncode) %>% 
#                    summarize(avg_lat = mean(station_lat), avg_long=mean(station_long)), aes(x=avg_long, y=avg_lat, label=collectioncode, check_overlap=TRUE)) +

# Limit to just northeastern US (NW Atlantic)
#  coord_sf(xlim=c(-94, -45), ylim = c(16.5, 60), expand=FALSE) +

  
  # # Point out the regions mentioned in the paper  
  # geom_point(data=overview_pts, aes(x=Lon, y=Lat), size=4) +
  # geom_shadowtext(data=overview_pts, aes(x=Lon, y=Lat, label=letter), color='white',
  #                 fontface='bold', check_overlap=F, size=3
  #         #       position=position_jitter(width=0.75, height=0.75)  # can't do jitter + background circles
  #                 ) +  
  # 
  # # Pseudo-legend for the numbered regions. 
  # # Position looks bad in RStudio but works well on pdf export. 
  # # Inf is supposed to work, but doesn't position things properly.
  # geom_label(data=overview_pts,
  #            aes(-54, 21.5, hjust = 0,
  #                label=paste(paste(letter, Name), collapse='\n')), label.size=0, size=3
  #                ) +
  # annotate("text", x=-60, y=35, label="Atlantic\n Ocean ", size=5, hjust='center') +
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16, face="bold"), 
        legend.text = element_text(size = 8, colour = "red"),
        legend.position='right',
      #  legend.key=element_blank()
        ) +
 # guides(colour=guide_legend(override.aes = list(alpha = 0))) +
  labs(x='Longitude', y='Latitude')

mad_plot



