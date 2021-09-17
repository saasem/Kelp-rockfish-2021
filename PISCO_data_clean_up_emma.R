#PISCO data clean up 
#Generalized code to look at any species
#PISCO data are maintained in a flat file and so this code turns that into 
#transects and a structure that can be used to develope indices of abundance
#Data also selected for length comps
#Melissa Monk January 2021

#--------------------------------------------------------------
#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

#classcode is species column; Define species of interest here
SPP_X = c('SATR')

#species x length at maturity (cm)
mature_l_x <- (18) 

#load libraries - tidyverse loads a number of libraries
library(tidyverse)
library(plotly)

#data is a flat file, meaning each line is an entry of a fish
#******Update this file with the newest version*****
PISCO <- read.csv('PISCO_kelpforest_fish.1.3(1).csv')

#turn character columns into factors
PISCO <- PISCO %>% mutate_if(is.character,as.factor)

#Add a column to the dataframe that is PRESENT if the row is for a species of 
#interest and ABSENT if it's another species
PISCO <- PISCO %>%  mutate(spp_present = 
                             case_when(classcode %in% SPP_X ~ 'PRESENT',
                                       TRUE ~ 'ABSENT'))

#Look at which campuses sample which sites
with(PISCO, table(site, campus))

#read in pisco site table
site.location <- read.csv('PISCO_kelpforest_site_table.1.2.csv')
sites <- site.location %>%
  select(site, year, latitude, longitude, MPA_Name, site_designation, site_status) %>%
  unique()
#----------------------------------------------------------------------------
#Try and find unique "transects" 
#One row doesn't have year - remove this row

PISCO <- PISCO %>%
  filter(!is.na(year)) 
PISCO <- droplevels(PISCO)

##Location
#merge location and PISCO dataframes
PISCO <- left_join(PISCO, sites, by=c("year", "site"))

PISCO <- PISCO %>%
  mutate(Region = case_when(latitude > 34.4486 ~ "NCA",
                            TRUE ~ "SCA"))

#make sure they were all assigned                          
summary(as.factor(PISCO$Region))

#sheep, this doesn't quite work and i'm not sure about making groups and declaring they are different populations
# #create location groups. currently: or, nca,   
# #sca_mainland, sca_isl
# PISCO <- PISCO %>%
#   mutate(loc_group = case_when(
#     latitude > 39 ~ "or",
#     latitude > 34.4486 & latitude < 39 ~ "nca",
#     latitude < 34.4486 & latitude > 34.2 & longitude > 119.2 ~ "sca_mainland",
#     latitude < 34.2 & latitude > 33.6 & longitude < 119.2 ~ "sca_mainland",
#     latitude < 34 & longitude < 118 ~ "sca_mainland",
#     latitude < 34.2 & latitude > 33.6 & longitude > 118 ~ "sca_isl",
#     latitude < 33.5 & longitude > 118 ~ "sca_isl"))

#This will set spp_counts to 1 where it's an adult kelp rockfish - just removes
#some extra steps; also adds in a column for juveniles
PISCO <- PISCO %>%
             mutate(count_spp_x = 
             case_when((classcode==SPP_X & fish_tl>=mature_l_x) ~ 1, 
                       TRUE ~ 0)) %>%
             mutate(count_spp_x_juv = 
             case_when((classcode==SPP_X & fish_tl<mature_l_x) ~ 1, 
                     TRUE ~ 0))

#remove transects with <3m visibility
PISCO <- subset(PISCO, vis >= 3)

#remove CNMD
PISCO <- subset(PISCO, level != "CNMD")
  
#create spp x dataframes
PISCO_SPP_X <- subset(PISCO, count_spp_x==1)
PISCO_SPP_X_juv <- subset(PISCO, count_spp_x_juv==1 )



#--------------------------------------------------------------------------------
#Melissa script additions
##check to see how many transects there are
PISCO_transects  <- PISCO %>%
  select(campus, site, year, month, day, zone, level, transect, vis, surge, pctcnpy,) %>%
  unique()

#plot visibility by surge - don't get much
#removal of tr with <3m vis now coded earlier
bb <- ggplot(PISCO_transects, aes(surge, vis)) + geom_boxplot()
x11(); bb

#a lot of missing data
summary(as.factor(PISCO_transects$pctcnpy))
summary(as.factor(PISCO_transects$surge))

#look at levels by campus and site
with(PISCO_transects, table(campus, level))

#NAs in PISCO for count - does this work...?
length(is.na(PISCO$count_spp_x))

#get all unique transects 
with(PISCO_transects, table(campus,level))

#CNMD now removed earlier
#get sites with canopy mid and compare the length 
#distributions to canopy and mid
cnmd.sites <- subset(PISCO_transects, level=='CNMD' & campus=='UCSB')
PISCOb <- subset(PISCO_transects, site %in% cnmd.sites$site)
PISCOb <- droplevels(PISCOb)
with(PISCOb, table(site, level))

#Expand the lengths by the count rows
#can double check the result by getting the length of the sum of the counts
PISCO_SPP_X_lengths <- PISCO_SPP_X %>%
  uncount(weights = count, .remove=FALSE) %>%
  select(year, campus, method, month, day, site, Region, zone, 
  level,transect, classcode, fish_tl, depth, count_spp_x)

PISCO_SPP_X_lengths_juv <- PISCO_SPP_X_juv %>%
  uncount(weights = count, .remove=FALSE) %>%
  select(year, campus, method, month, day, site, zone, level,
         transect, classcode, fish_tl, depth, count_spp_x)

#plot lengths by level and campus
cc <- ggplot(PISCO_SPP_X_lengths, aes(level, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();cc

#plot lengths by level, zone, and campus
zz <- ggplot(PISCO_SPP_X_lengths, aes(zone, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();zz

#plot lengths over time by region
lengths.mean <- subset(PISCO_SPP_X_lengths, level == c("MID", "BOT")) %>%
  group_by(year, Region) %>%
  summarize(mean.length = mean(fish_tl))

ll <- ggplot(lengths.mean, aes(year, mean.length)) + 
  geom_line(size = 1.5) + 
  facet_wrap(~Region)
x11();ll

#plot adult lengths of canopy
can <- ggplot(subset(PISCO_SPP_X_lengths, level = "CAN"), aes(zone, fish_tl)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();can

#plot juvenile lengths
juv <- ggplot(PISCO_SPP_X_lengths_juv, aes(zone, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();juv

#plot juvenile counts (something isn't working)
juv_count <- ggplot(PISCO_SPP_X_lengths_juv, aes(year, count_spp_x)) + 
  geom_line() + 
  facet_wrap(~campus)

x11();juv_count

#plot count by level and campus
#frog spp_length expanded so the counts are all =1
count <- ggplot(subset(PISCO_SPP_X, level != "CAN"), aes(level, count, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();count

#look at just the canopy mid lengths
PISCO_SPP_X_CNMD <- subset(PISCO_SPP_X_lengths, site %in% PISCOb$site & campus=="UCSB")
PISCO_SPP_X_CNMD <- droplevels(PISCO_SPP_X_CNMD)
dd <- ggplot(PISCO_SPP_X_CNMD, aes(level, fish_tl, colour=level)) + geom_boxplot() + facet_wrap(~campus)
x11();dd

#look at juvenile lengths
jj <- ggplot(PISCO_SPP_X_lengths_juv, aes(level, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();jj

#get rid of canopy mid - the distribution of lengths is similar to the mid,
#but very few transects and only done in SCA

#fish count vs surge by level and proportion of fish
#in each level by surge category 
#(defs can be simplified, not yet sure how)

surge_level <- PISCO_SPP_X %>%
  group_by(surge, level) %>%
  summarize(mean.count = mean(count, rm.na= T))

proportion_surge <- surge_level %>%
  summarize(sum.mean.count = sum(mean.count))

x <- left_join(surge_level, proportion_surge)

final_surge <- x %>%
  group_by(surge, level) %>%
  summarize(mean.count = mean.count,
            sum.mean.count = sum.mean.count,
            prop = mean.count/sum.mean.count)

#(table doesn't work yet)
with(final_surge, table(surge, level))


#look at length distributions of fish by level and site
##FIRST need to expand the lengths because there's a count column for how many
##fish were observed in that length bin


ee <- ggplot(subset(PISCO_SPP_X_lengths, level !='CAN'), aes(fish_tl, fill = level)) + 
  geom_density(alpha=0.3) + 
  facet_wrap(~campus) 
x11();ee

ee1 <- ggplot(subset(PISCO_SPP_X_lengths, level !='CAN'), aes(fish_tl, colour = level)) + 
  geom_freqpoly() + 
  facet_wrap(~campus) 
x11();ee1


#look at length distributions of fish by level and site
ff <- ggplot(subset(PISCO_SPP_X_lengths, level !='CAN'), aes(fish_tl, fill = as.factor(month))) + 
  geom_density(alpha=0.3) + 
  facet_wrap(~as.factor(level)) 
x11();ff


#frog
#look at how many samples by level
spp_by_levels = PISCO_SPP_X %>%
                group_by(level) %>%
                summarise(totfish = sum(count),
                          n_transects_present = n())
spp_by_levels

#look at how many samples
transects_by_level = PISCO_transects %>%
                     group_by(level) %>%
                     summarise(tottransects = n())
transects_by_level
summary_info = cbind(spp_by_levels, transects_by_level)
summary_info$percent_present = summary_info$n_transects_present/summary_info$tottransects
#write.table(summary_info, file = "summary_info.csv")

#are there sites where the species was never observed?
length(unique(PISCO$site))
length(unique(PISCO_SPP_X$site))

#Remove sites that never saw specie of interest
PISCO <- droplevels(subset(PISCO, site %in% PISCO_SPP_X$site))

#adding an effort column #sheep, bot and mid are currently counted as 1 unit of effort for each. bot/mid still separate and not combined.
PISCO.aggregate.transect <- PISCO %>%
  subset(level %in% c("BOT", "MID")) %>%
  group_by(campus, site, year, month, day, zone, transect, level) %>%
  summarise(SATRtot = sum(count_spp_x, na.rm = T), 
            ntransect = 1,
            pctcnpy = mean(pctcnpy, na.rm = T),
            CPUE.tr = (SATRtot / ntransect), 
            vis = mean(vis), na.rm = T)
  # %>% #currently commented out to retain level separation
  # group_by(campus, site, year, month, day, zone, transect) %>%
  # summarise(sum.SATRtot = sum(SATRtot), sum.ntransect = sum(ntransect), 
  #           sum.vol = sum(vol.transect),
  #           CPUE.tr = (sum.SATRtot / sum.ntransect), 
  #           CPUE.vol = (sum.SATRtot / sum.vol),
  #           pctcnpy = mean(pctcnpy))

#add in spp_present col again NEEDS EDITING
PISCO.aggregate.transect <- PISCO.aggregate.transect %>%
  mutate(spp_present = case_when(=>1 %in% SATRtot ~ 'PRESENT',
                                  TRUE ~ 'ABSENT'))
#how many rows with NA vis values
length(is.na(PISCO.aggregate.transect$vis))

#how many transects with vis <3 
vis.tr <- subset(PISCO.aggregate.transect, vis < 3)

#get total number of spp_x paired with ave vis for the transect
#would this still work using the mean spp x count 
#values now in the aggregate df?
vis.plot.group <- PISCO.aggregate.transect %>%
  group_by(campus, site, year, transect) %>%
  summarise(SATRtot = sum(SATRtot),
            vis = mean(vis))

#check for NA values in vis
length(is.na(vis.plot.group$vis))

vis.plot <- ggplot(vis.plot.group, aes(vis, SATRtot, group = vis))+
  geom_boxplot()+
  scale_x_continuous(breaks=seq(0, 30, 0.5))
x11(); vis.plot

#sheep, i think this is a repeat maneuver (around line 86)
#reasonable to remove transects with <3 m vis
PISCO.aggregate.transect <- subset(PISCO.aggregate.transect,
                                   PISCO.aggregate.transect$vis >= 3)

#sheep, confirming this is treating the count values correctly (e.g. taking the mean)
#raw average count by year, with ave CPUE and transect count columns
PISCO.year.mean <- PISCO.aggregate.transect %>%
  group_by(year, site) %>%
  summarize(mean.count = mean(SATRtot, na.rm = T), 
            ntransect = sum(ntransect, na.rm = T),
            CPUE.tr = mean(CPUE.tr, na.rm = T), 
            pctcnpy = mean(pctcnpy), na.rm = T)

#sheep, note to self: this is added in at the beginning- still necessary?. i also need to add in a northern boundry if we do not wish to include oregon sites (i'll check becky's map)
#add in location data
PISCO.aggregate.transect <- left_join(PISCO.aggregate.transect, sites, by=c("year", "site"))

PISCO.aggregate.transect <- PISCO.aggregate.transect %>%
  mutate(Region = case_when(latitude > 34.4486 ~ "NCA",
                            TRUE ~ "SCA"))
#make sure they were all assigned                          
summary(as.factor(PISCO.aggregate.transect$Region))


#cpue by year only
# cpue.year <- PISCO.aggregate.transect %>%
#   group_by(year) %>%
#   summarize(mean.count = mean(SATRtot), 
#             ntransect = sum(ntransect),
#             CPUE.tr = mean(CPUE.tr), 
#             CPUE.vol = mean(CPUE.vol),
#             pctcnpy = mean(pctcnpy))

c.y <- ggplot(PISCO.aggregate.transect, aes(year, SATRtot, group = year)) +
  geom_boxplot()
x11();c.y

##Look at trends north and south of Conception 
PISCO.Region.mean <- PISCO.aggregate.transect %>%
  group_by(Region, year, zone) %>%
  summarize(mean.count = mean(SATRtot, na.rm = T), 
            ntransect = sum(ntransect, na.rm = T),
            CPUE.tr = mean(CPUE.tr, na.rm = T), 
            pctcnpy = mean(pctcnpy), na.rm = T)

ff <- ggplot(PISCO.Region.mean, aes(year, mean.count, colour = zone))+
  geom_line(lwd=1.5) +
  facet_wrap(~Region)
x11(); ff

#MPA vs reference sites
PISCO.Region.site.status <- PISCO.aggregate.transect %>%
  group_by(year, Region, site_status) %>%
  summarize(mean.count = mean(SATRtot), 
            ntransect = sum(ntransect),
            CPUE.tr = mean(CPUE.tr),
            pctcnpy = mean(pctcnpy))

region.site.status <- ggplot(PISCO.Region.site.status, aes(year, mean.count, colour = site_status))+
  geom_line(lwd=1.5) +
  facet_wrap(~Region)
x11(); region.site.status


##Look at trends by level
##will have to make a new table for this because aggregate doesn't have the 
##levels
PISCO.level <- PISCO.aggregate.transect %>%
  group_by(year, level) %>%
  summarize(mean.count = mean(SATRtot), 
            ntransect = sum(ntransect),
            CPUE.tr = mean(CPUE.tr),
            pctcnpy = mean(pctcnpy))

ii <- ggplot(PISCO.level, aes(year, mean.count, colour = level)) + geom_line(lwd=1.5)
x11(); ii

#trends by zone
PISCO.zone <- PISCO.aggregate.transect %>%
  group_by(year, zone) %>%
  summarize(mean.count = mean(SATRtot), 
            ntransect = sum(ntransect),
            CPUE.tr = mean(CPUE.tr),
            pctcnpy = mean(pctcnpy))

zone <- ggplot(PISCO.zone, aes(year, mean.count, colour = zone)) + geom_line(lwd=1.5)
x11(); zone


#how do replicate transects within a zone differ
#from one another

within_zone <- PISCO.aggregate.transect %>%
  group_by(site, year, month, day, zone, level) %>%
  summarize(sd.count = sd(SATRtot))

sd_within_zone <- ggplot(within_zone, aes(site, sd.count)) +
  facet_wrap(~year)
x11(); sd_within_zone

###How different are the bottom and mid paired transects in terms of counts
#think you can do this with gather
#First need to collapse the counts and
#this is where I need to figure out the log situation
PISCO_SPP_counts <- PISCO_SPP_X %>%
  filter(level !='CAN') %>%
  group_by(year, campus, month, day, site, zone, transect, level) %>%
  summarise(SumCount = log(sum(count))) #log here?

PISCO_SPP_pairs <- PISCO_SPP_counts %>%
  group_by(year, month, day, site, zone, transect) %>%
  spread(level,SumCount)

#plot bottom vs mid

hh <- ggplot(PISCO_SPP_pairs, aes(BOT, MID)) + 
  geom_jitter(alpha=0.3)
x11(); hh

#sheep, i think this is already done by removing sites that never saw SPP_X earlier in the code (line 273, maybe should be much earlier?)
#% of transects per site that saw at least 1 SPP_X
#bot/mid not combined ALSO MAYBE CODE IN THE 600s
#NEEDS EDITING
at_least_1 <- PISCO.aggregate.transect %>%
  group_by(year, month, day, site, level, transect)
at_least_1 <- mutate(SPP_X_present = if_else(
                       SATRtot = 0, F, T, missing = NULL
                     ))
#trying this for transect relationships w/ in zones
PISCO_SPP_zone <- PISCO_SPP_counts %>%
  group_by(year, month, day, site, level, transect) #%>%
  #spread(zone,SumCount)
winzone <- ggplot(PISCO_SPP_zone, aes(year, SumCount), colour = as.factor(level)) + 
  geom_jitter(alpha=0.3) +
  facet_wrap(~zone)
x11(); winzone




#sheep, code below, not sure what's relevant and 
#what's just me trying to figure things out early on
#in the process. On the plus side, I've learned a lot
#about what I need to do to stay organized right off
#the bat for new projects!



#------------------------------------------------------------------------------

#graph CPUE by site (using transect count as effort)
# x11()
#code doesn't work yet (plot is created but unreadable)
# ggplot(PISCO.year.mean, mapping = aes(year, CPUE))+
#   geom_point() +
#   facet_wrap_paginate(~site)
# ggsave('cpue.site', cpue.site, device = "jpeg")

#kelp graph by year and region
kelp <- ggplot(PISCO.aggregate.transect, aes(year, pctcnpy, group = year)) +
  geom_boxplot() +
  facet_wrap(~Region) +
  theme(legend.position = "none") +
  labs(title = "Estimated Percent Transect Occupied by Kelp",
       caption = "1 = 0-33%, 2 = 34-66%, 3 = 67-100%")
x11();kelp

kelp.lat <- ggplot(PISCO.aggregate.transect, aes(latitude, pctcnpy)) +
  geom_point() +
  # facet_wrap(~Region) +
  theme(legend.position = "none") +
  labs(title = "Estimated Percent Transect Occupied by Kelp",
       caption = "1 = 0-33%, 2 = 34-66%, 3 = 67-100%")
x11();kelp.lat

#cpue graph by year, subset for the s cal sites. line and boxplot and?

# ggplot(south.ca.yr.mean, aes(year, CPUE.vol, color = factor(site))) +
#   geom_line() +
#   theme(legend.position = "none") +
#   labs(title = "Southern CA CPUE")
# 
# 
# ggplot(south.ca.sites, aes(year, CPUE.vol, color = factor(site))) +
#   geom_boxplot() +
#   theme(legend.position = "none")
# labs(title = "Southern CA CPUE")

#join swath and taxon table
swath_classcode <- left_join(
  read.csv('PISCO_kelpforest_swath.1.2(1).csv'),
  read.csv('PISCO_kelpforest_taxon_table.1.2.csv')
)
swath_classcode <- swath_classcode %>%
  subset(sample_subtype == "ALGAE", rm.na = T)

#graph of algae species
algae <- swath_classcode %>%
  group_by(campus, year, site, zone, transect) %>%
  summarize(count = sum(count, rm.na = T),
            mean.size = mean(size, rm.na = T))

algae_figure <- ggplot(algae, aes(zone, count)) +
  facet_wrap(~campus)
x11(); algae_figure


###############################

#emma attempts making maps
install.packages(c("sf", "raster", "spData", "spDataLarge", "tmap"))

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)


###############################
PISCO.transect <- PISCO %>%
  subset(level %in% c("BOT", "MID")) %>%
  add_count(campus, site, year, month, day, zone, transect)




year_mean_count_BM <- PISCO %>%
  subset(PISCO$level == c("MID","BOT"),) %>%
  group_by(year) %>%
  summarize(mean = mean(count_spp_x))

year_mean_count_C <- PISCO %>%
  subset(PISCO$level == c("CAN"),) %>%
  group_by(year) %>%
  summarize(mean = mean(count_spp_x))

year_mean_count_BMC <- PISCO %>%
  subset(PISCO$level == c("MID","BOT","CAN"),) %>%
  group_by(year) %>%
  summarize(mean = mean(count_spp_x))

#plot for mean count (BOT and MID still combined)
x11()
ggplot(PISCO_mean_count, aes(year, mean, 
                             fill=factor(campus)))+
  geom_boxplot()

#mean count by site
site_mean_count <- PISCO %>%
  subset(PISCO$level == c("MID","BOT"),) %>%
  group_by(year, site,campus) %>%
  summarize(mean = mean(count_spp_x))

#plot mean count by site 
x11()
ggplot(site_mean_count, aes(year, mean, 
                            fill=factor(campus))) + geom_boxplot() +
  facet_wrap(~site)

#bot/mid mean total lengths
site_mean_length <- PISCO_SPP_X %>%
  subset(PISCO_SPP_X$level == c("MID","BOT"),) %>%
  group_by(year, site, campus) %>%
  summarize(mean_tl = mean(fish_tl))
#plot
x11()
ggplot(site_mean_length, aes(year, mean_tl, 
                             fill=factor(campus))) + geom_boxplot()


#canopy mean counts
CAN_site_mean_count <- PISCO %>%
  subset(PISCO$level == c("CAN"),) %>%
  group_by(year, site, campus) %>%
  summarize(mean = mean(count_spp_x))

#canopy mean total lengths
CAN_site_mean_count <- PISCO_SPP_X %>%
  subset(PISCO_SPP_X$level == c("CAN"),) %>%
  group_by(year, site,campus) %>%
  summarize(mean_tl = mean(fish_tl))

# width = 18, height = 12, units = "in", res=600, pointsize = 9)
# ggplot(dat, aes(x=YEAR, y=LNGTH, fill=factor(dat_source))) +
#   geom_boxplot() +
#   xlab("Fork Length [mm]") +
#   facet_wrap(~CNTY_NAME)
# dev.off()

#for density
#(width = 8, height = 4, units = "in", res=600, pointsize = 9)
#ggplot(PISCO, aes(x=fish_tl, fill=factor(campus))) +
#geom_density(alpha=0.3) +
#xlab("Total Length [mm]")
#dev.off()
# 
# #plot by county
# #png(filename="Deb_vs_Recfin_density_by_county_1987-1998.png",
#     width = 18, height = 12, units = "in", res=600, pointsize = 9)
# ggplot(dat, aes(LNGTH, fill=factor(dat_source))) +
#   geom_density(alpha=0.3) +
#   xlab("Fork Length [mm]") +
#   facet_wrap(~YEAR) +
#   geom_text()
# dev.off()

#boxplot of tail lengths by level grouped by site,
#factor of level, facet wrap of campus
PISCO_sites <- unique(PISCO_SPP_X$site)

ggplot(PISCO_SPP_X, mapping = aes(site, fish_tl, 
                                  color = factor(level))) + facet_wrap(~campus) + 
  geom_boxplot()

ggplot(PISCO_SPP_X, mapping = aes(site, fish_tl, 
                                  color = factor(level))) + facet_wrap(~campus) + 
  geom_boxplot()

#focus in on one site (not the best, but easier to
#view in R than facet wrapping by site)

ggplot(subset(PISCO_SPP_X, site=='WESTON_DC'), mapping = aes(fish_tl, 
                                                             color = factor(level))) + facet_wrap(~campus) + 
  geom_boxplot()



#find fraction or percent of transects that saw SPP_X
#by site and year

percent_present <- PISCO %>%
  group_by(site, year) %>%
  summarize(sum(spp_present == "PRESENT") / 
              sum(spp_present == "PRESENT" | 
                    spp_present == "ABSENT")*100)



#number of SPP_X by site and year
count_SPP_X <- PISCO_SPP_X %>%
  group_by(site, year) %>%
  summarize(sum(count))


#Find unique transects based on the variables below
#transect in the data is actually the transect number for that day
#emma removed selection of 'side', as it doesn't exist
#in PISCO_kelpforest_fish.1.3(1)
PISCO.transect <- PISCO %>%
  select(campus, method, year, month, day, site, zone,
         level, transect) %>%
  unique() %>%
  mutate(id = row_number())   #adds a row number as a trip identifier


#Merge in the transect identifer (row_number from PISCO.trasect) 
#to the original dataframe
PISCO <- right_join(PISCO, PISCO.transect)

#check join by look at the min, max of the transect id's
summary(PISCO.transect$id)
summary(PISCO$id)

#Get fish by transect 
#Columns will be counts of the species by transect
#Rows are the transects
#Add in 0's where the species wasn't present
PISCO.fish  <- PISCO %>%
  select(id, classcode, count) %>%  #keep just transect id, species and count
  group_by(id, classcode) %>%       #group by the id and species and sum counts
  summarise(sum_cnt = sum(count)) %>%
  spread(classcode, sum_cnt) %>%
  replace(is.na(.), 0)
#This dataframe could be used to do something like stephens-maccall filtering
#or looking at community structure

#Select only the columns you need
PISCO.fish_SPP_X <- PISCO.fish %>% select(id, all_of(SPP_X))

PISCO.fish_SPP_X <- PISCO.fish_SPP_X %>%
  ungroup() %>%
  mutate(SPP = rowSums(select(.,-id)))


#Merge PISCO.fish and PISCO.transect
PISCO.SPP_DF <- inner_join(PISCO.transect, PISCO.fish_SPP_X)

#Add a column to the dataframe that is PRESENT if the row is for a species of 
#interest and ABSENT if not
PISCO.SPP_DF  <- PISCO.SPP_DF  %>%  mutate(spp_present = 
                                             case_when(SPP>0 ~ 'PRESENT',
                                                       TRUE ~ 'ABSENT'))

dim(subset(PISCO.SPP_DF, spp_present=="PRESENT"))

# write.csv(PISCO.SPP_DF, file = "PISCO.SPP_DF.csv")



#determine how many years each site has been sampled
site_years_n <- PISCO.SPP_DF %>%
  group_by(site) %>%
  summarise(n_years = n_distinct(year))

#determine how many sites sampled each year
n_sites_each_year <- PISCO.SPP_DF %>%
  group_by(year) %>%
  summarise(n_site = n_distinct(site))

#number of transects by site
site_transects_n <- PISCO.SPP_DF %>%
  group_by(site) %>%
  summarise(n_transect = n_distinct(transect))
#remove sites with fewest years sampled
#Finished data alterations and prep
#-------------------------------------------------------------------------------

#Start looking at data summaries and filtering
summary(PISCO.SPP_DF)

#Look at sampling by year and campus; 
with(PISCO.SPP_DF, table(spp_present, year, campus))

#Look at presence/absence by level
with(PISCO.SPP_DF, table(level, spp_present))

#for gopher/byel, they're really only in the BOT level,
# remove other levels depending on where they are for the 
#species of interest

-----
  # emma messes around with things (data prep level)
  
  ## the search for the densest data
  
  #determine how many years each site has been sampled
  site_years_n <- PISCO.SPP_DF %>%
  group_by(site) %>%
  summarise(n_years = n_distinct(year))


#determine what years each site has been sampled
site_years_named <- PISCO.SPP_DF %>%
  group_by(site) %>%
  summarise(what_years = unique(year))

#find what sampling year gaps exist for each site (doesn't work yet)
# site_year_gaps <- PISCO.SPP_DF %>%
#   group_by(site) %>%
#   range(what_years)

#how many sites sampled each year
n_sites_each_year <- PISCO.SPP_DF %>%
  group_by(year) %>%
  summarise(n_site = n_distinct(site))

#how many transects each site has had each year
#group by site, look at one site over all the years

#create logical vector 
cond <- PISCO.SPP_DF$transect >= 1

#bind to df to keep values linked
site_yr_trsct_ct <- table(subset(PISCO.SPP_DF,
                                 select = 
                                   PISCO.SPP_DF$year,
                                 PISCO.SPP_DF$site,
                                 PISCO.SPP_DF$zone,
                                 PISCO.SPP_DF$level,
                                 PISCO.SPP_DF$cond))
tr_pres <- cbind(PISCO.SPP_DF, cond)

#group by site

tr_pres <- count(tr_pres, tr_pres$site)
tr_pres                 







