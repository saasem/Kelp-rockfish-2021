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
#PISCO <- PISCO %>%  mutate(spp_present = 
#                             case_when(classcode %in% SPP_X ~ 'PRESENT',
#                                       TRUE ~ 'ABSENT'))

#Look at which campuses sample which sites
with(PISCO, table(site, campus))

#read in pisco site table
site.location <- read.csv('PISCO_kelpforest_site_table.1.2.csv')
sites <- site.location %>%
  filter(method=="SBTL_FISH") %>%
  select(site, year, latitude, longitude, MPA_Name, site_designation, site_status) %>%
  unique()
#----------------------------------------------------------------------------
#cow - Moved all the filtering here
#One row doesn't have year - remove this row
PISCO <- PISCO %>%
  filter(!is.na(year)) %>%
#remove transects with <3m visibility
  filter(vis >= 3) %>%
#remove CNMD - only sampled in certain instances. All UCSB samples
#for kelp rockfish 51 observations
#remove canopy - mostly smaller fish and only sampled when they are expecting
#juveniles
  filter(level %in% c("BOT","MID")) %>%
  droplevels


##ALL PISCO data and join to sites
##Location
#merge location and PISCO dataframes
PISCO <- left_join(PISCO, sites, by=c("year", "site"))

PISCO <- PISCO %>%
  mutate(Region = case_when(latitude > 34.4486 ~ "NCA",
                            TRUE ~ "SCA"))

#make sure they were all assigned                          
summary(as.factor(PISCO$Region))
##cow - changing count_spp_x to spp_x since it's not actuall a count
#This will set spp_counts to 1 where it's an adult kelp rockfish - just removes
#some extra steps; also adds in a column for juveniles
PISCO <- PISCO %>%
  mutate(count_spp_x = 
           case_when((classcode==SPP_X & fish_tl>=mature_l_x) ~ 1, 
                     TRUE ~ 0)) %>%
  mutate(count_spp_x_juv = 
           case_when((classcode==SPP_X & fish_tl<mature_l_x) ~ 1, 
                     TRUE ~ 0))

#------------------------------------------------------------------
#Create data frame to use in indices
#One row per transect and each row will have the number of target species that are
#present - mature and juveniles
#removing the length data from this dataframe
##check to see how many unique transects there are
#removing surge and percent canopy since we aren't going to use them - too many NAs
PISCO_transects  <- PISCO %>%
  select(campus, site, year, month, day, zone, level, transect, vis, latitude, 
         longitude, MPA_Name, site_status, Region) %>% #, surge, pctcnpy
  unique()

PISCO_aggregate_samples <- PISCO_transects %>%
  group_by(campus, site, year, month, day, zone, level, latitude, 
         longitude, MPA_Name, site_status, Region) %>% 
  summarise(ntransects = n_distinct(transect), 
            mean_vis = mean(vis))


#Get just the sums by transect of fish
PISCO_fish_cnts <- PISCO %>%
  filter(classcode==SPP_X) %>%
  mutate(SPP_X_cnt = ifelse(fish_tl>=mature_l_x,count, 0)) %>%
  mutate(SPP_X_cnt_juv = ifelse(fish_tl<mature_l_x,count, 0))%>%
  group_by(campus, site, year, month, day, zone, level) %>% 
  summarise(cnt_adults = sum(SPP_X_cnt),
            cnt_juvs = sum(SPP_X_cnt_juv))


#Merge back in with transect data
PISCO_dat <- left_join(PISCO_aggregate_samples,PISCO_fish_cnts)

#remove samples with no lat/long - only sampled once
PISCO_dat <- PISCO_dat %>%
  filter(!is.na(latitude)) %>%
  mutate(cnt_adults = ifelse(is.na(cnt_adults), 0, cnt_adults),
         cnt_juvs = ifelse(is.na(cnt_juvs), 0, cnt_juvs))

#PISCO_dat is the dataset you need for the indices
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Look for sites that never saw the species as an adult


PISCO_sites_presence <- PISCO_dat %>%
  group_by(site) %>%
  summarise(adult_presence = sum(cnt_adults)) %>%
  filter(adult_presence > 0)

#Subset PISCO_dat to sites that observed kelp rockfish at least once
PISCO_dat <- PISCO_dat %>%
  filter(site %in% PISCO_sites_presence$site)

#Figure out how many years each site was sampled
with(PISCO_dat, table(MPA_Name,year))

summary(PISCO_dat)

#remove mid - only 92 samples
#change site status to factor
PISCO_dat <- PISCO_dat %>%
  filter(zone != "MID") %>%
  mutate_at(vars(site_status, site, MPA_Name),as.factor) %>%
  droplevels

#MPA_Name missing for some
#see which ones
MPA_names <- PISCO_dat %>%
  ungroup %>%
 # filter(is.na(MPA_Name)) %>%
  select(campus, site, MPA_Name, site_status) %>%
  unique


MPA_names_na <- PISCO_dat %>%
  ungroup %>%
   filter(is.na(MPA_Name)) %>%
  select(campus, site, MPA_Name, site_status, latitude, longitude) %>%
  unique
#Assign all thse to footprint SMR
PISCO_dat <- PISCO_dat %>%
  mutate(MPA_Name = case_when(is.n))

with(PISCO_dat, table(MPA_Name, year, campus))
#Table of samples by year and how many years MPAs sampled
with(PISCO_dat, table(year,campus))

MPA_years <- PISCO_dat %>%
  group_by(MPA_Name,campus) %>%
  summarise(nyears = n_distinct(year))








#-------------------------------------------------------------------------------
#Expand the lengths by the count rows
#can double check the result by getting the length of the sum of the counts
PISCO_SPP_X_lengths <- PISCO %>%
  filter(spp_x==1) %>%
  uncount(weights = count, .remove=FALSE) %>%
  select(year, campus, method, month, day, site, Region, zone, 
         level,transect, classcode, fish_tl, depth)

PISCO_SPP_X_lengths_juv <- PISCO %>%
  filter(spp_x_juv==1)
  uncount(weights = count, .remove=FALSE) %>%
  select(year, campus, method, month, day, site, zone, level,
         transect, classcode, fish_tl, depth)

#--------------------------------------------------------------------------------
#Melissa script additions



#-------
#Look at missing data for surge and percent canopy and summary of the two
#Percent canopy only recorded for canopy transects. Not sure if it can be 
#extended to the other transects at the same site
#A lot of missing data, so can't really use these in the modelling
#plot visibility by surge - don't get much
#removal of tr with <3m vis now coded earlier

#look at levels by campus and site
with(PISCO_transects, table(campus, level))

#NAs in PISCO for count - does this work...?
#length(is.na(PISCO$spp_x))

#get all unique transects 
with(PISCO_transects, table(campus,level))




#------------------------------------------------------
#PLOTTING

#boxplot of lengths by level, zone, and campus
zz <- ggplot(PISCO_SPP_X_lengths, aes(zone, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();zz

#cow
#Mean length over time and region
lengths.mean <- PISCO_SPP_X_lengths %>%
  # filter(level %in% c("MID", "BOT")) %>%
  group_by(year, Region) %>%
  summarize(mean.length = mean(fish_tl))

ll <- ggplot(lengths.mean, aes(year, mean.length)) + 
  geom_line(size = 1.5) + 
  facet_wrap(~Region)
x11();ll



#look at juvenile lengths
jj <- ggplot(PISCO_SPP_X_lengths_juv, aes(level, fish_tl, colour=level)) + 
  geom_boxplot() + 
  facet_wrap(~campus)
x11();jj


#look at length distributions of fish by level and site
##FIRST need to expand the lengths because there's a count column for how many
##fish were observed in that length bin


ee <- ggplot(PISCO_SPP_X_lengths, aes(fish_tl, fill = level)) + 
  geom_density(alpha=0.3) + 
  facet_wrap(~campus) 
x11();ee

ee1 <- ggplot(PISCO_SPP_X_lengths, aes(fish_tl, colour = level)) + 
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


#are there sites where the species was never observed?
length(unique(PISCO$site))
length(unique(PISCO_SPP_X$site))

#Remove sites that never saw specie of interest
PISCO <- droplevels(subset(PISCO, site %in% PISCO_SPP_X$site))

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

