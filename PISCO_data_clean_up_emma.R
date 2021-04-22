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

#load libraries - tidyverse loads a number of libraries
library(tidyverse)
library(plotly)

#Set working directory
setwd('~/NOAA Lab Assistant/R code')

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

#----------------------------------------------------------------------------
#Try and find unique "transects" 
#One row doesn't have year - remove this row
PISCO <- PISCO %>%
  filter(!is.na(year)) 
PISCO <- droplevels(PISCO)

#drop years before 2001
# PISCO <- PISCO %>%
#   filter(year >= 2001)



#create a depth bin column (since depths are approx.)
PISCO <- PISCO %>%
  mutate(depth_bin_m = case_when(
    (depth <= 7.4) ~ 5,
    (depth >= 7.5 & depth <= 12.4) ~ 10,
    (depth >= 12.5 & depth <= 17.4) ~ 15,
    (depth >= 17.5) ~ 20))
    
#still need to remove NAs?
#PISCO <- PISCO[-which(is.na(
 # PISCO$campus)),]

##visualize data 
PISCO_SPP_X <- subset(PISCO, PISCO$classcode == SPP_X)

#add column of SPP_X counts, set counts of other 
#species and empty transects to 0

PISCO$count_spp_x <- ifelse(PISCO$classcode != SPP_X, 
                            0,PISCO$count)

#optional: rearrange columns to put new column next to count

PISCO <- subset(PISCO, select = c(campus:count, 
                   count_spp_x, fish_tl:depth_bin_m))

#for kelp rf: find average number of kelp rockfish 
#per year by campus (in BOT and MID levels)

#campus_mean_count <- PISCO %>%
  # subset(PISCO$level == c("MID","BOT"),) %>%
  # group_by(year, campus) %>%
  # summarize(mean = mean(count_spp_x))

#use visibility and/or 2*2*30 m transect structure
#to estimate transect volume

PISCO$vol.transect <- ifelse(PISCO$vis >= 2 | is.na(PISCO$vis), 
                             120, PISCO$vis * 60)
  
  
#create df to combine BOT and MID counts, adding an
#ntransect column to account for the paired BOT/MID
#transects (effort column)

PISCO.aggregate.transect <- PISCO %>%
  subset(level %in% c("BOT", "MID")) %>%
  group_by(campus, site, year, month, day, zone, transect, level) %>%
  summarise(SATRtot = sum(count_spp_x), 
            ntransect = 1, 
            vol.transect = mean(vol.transect),
            pctcnpy = mean(pctcnpy)) %>%
  group_by(campus, site, year, month, day, zone, transect) %>%
  summarise(sum.SATRtot = sum(SATRtot), sum.ntransect = sum(ntransect), 
            sum.vol = sum(vol.transect),
            CPUE.tr = (sum.SATRtot / sum.ntransect), 
            CPUE.vol = (sum.SATRtot / sum.vol),
            pctcnpy = mean(pctcnpy))
  

#raw average count by year, with ave CPUE and transect count columns
PISCO.year.mean <- PISCO.aggregate.transect %>%
  group_by(year, site) %>%
  summarize(mean.count = mean(sum.SATRtot), 
            ntransect = sum(sum.ntransect),
            CPUE.tr = mean(CPUE.tr), 
            CPUE.vol = mean(CPUE.vol),
            pctcnpy = mean(pctcnpy))

#group sites
south.ca.yr.mean <- PISCO.year.mean %>%
  subset(site %in% c("ANACAPA_ADMIRALS_CEN", "ANACAPA_ADMIRALS_E","ANACAPA_ADMIRALS_W","ANACAPA_BLACK_SEA_BASS","ANACAPA_EAST_FISH_CAMP_CEN","ANACAPA_EAST_FISH_CAMP_E","ANACAPA_EAST_FISH_CAMP_W","ANACAPA_EAST_ISLE_CEN",
              "ANACAPA_EAST_ISLE_E","ANACAPA_EAST_ISLE_W","ANACAPA_LIGHTHOUSE_REEF_CEN","ANACAPA_LIGHTHOUSE_REEF_E",
              "ANACAPA_LIGHTHOUSE_REEF_W","ANACAPA_MIDDLE_ISLE_CEN","ANACAPA_MIDDLE_ISLE_E","ANACAPA_MIDDLE_ISLE_W",
              "ANACAPA_WEST_ISLE_CEN","ANACAPA_WEST_ISLE_E","ANACAPA_WEST_ISLE_W","CAT_BLUE_CAVERN","CAT_INTAKE_PIPES","CAT_PUMPERNICKEL",
              "COJO_E","COJO_W","COUNTY_LINE","EL_MATADOR","HORSESHOE_REEF_E","HORSESHOE_REEF_W","IV_REEF_E","IV_REEF_W","LEO_CARRILLO",
              "LITTLE_DUME","NAPLES_CEN","NAPLES_E","NAPLES_W","POINT_DUME","SBI_ARCH_POINT_CEN","SBI_ARCH_POINT_N","SBI_ARCH_POINT_S","SBI_CAT_CANYON_CEN","SBI_CAT_CANYON_W","SBI_GRAVEYARD_CANYON_CEN","SBI_GRAVEYARD_CANYON_N",
              "SBI_SOUTHEAST_REEF_CEN","SBI_SOUTHEAST_REEF_S","SBI_SOUTHEAST_SEA_LION_CEN","SBI_SOUTHEAST_SEA_LION_N","SBI_WEBSTERS_ARCH_CEN",
              "SBI_WEBSTERS_ARCH_E","SBI_WEBSTERS_ARCH_N","SCI_CAVERN_POINT_E","SCI_CAVERN_POINT_W","SCI_COCHE_POINT_E","SCI_COCHE_POINT_W","SCI_FORNEY_E","SCI_FORNEY_W","SCI_GULL_ISLE_E","SCI_GULL_ISLE_W","SCI_HAZARDS_CEN","SCI_HAZARDS_E","SCI_HAZARDS_W","SCI_LITTLE_SCORPION_E","SCI_LITTLE_SCORPION_W",
              "SCI_PAINTED_CAVE_CEN","SCI_PAINTED_CAVE_E","SCI_PAINTED_CAVE_W","SCI_PELICAN_CEN","SCI_PELICAN_E","SCI_PELICAN_FAR_WEST","SCI_PELICAN_W","SCI_POTATO_PASTURE_E","SCI_POTATO_PASTURE_W",
              "SCI_SAN_PEDRO_POINT_E","SCI_SAN_PEDRO_POINT_W","SCI_SCORPION_ANCHORAGE","SCI_SCORPION_E","SCI_SCORPION_W","SCI_VALLEY_CEN","SCI_VALLEY_E",
              "SCI_VALLEY_W","SCI_YELLOWBANKS_CEN","SCI_YELLOWBANKS_E","SCI_YELLOWBANKS_W","SMI_BAY_POINT","SMI_CROOK_POINT_E","SMI_CROOK_POINT_W","SMI_CUYLER_E","SMI_CUYLER_W",
              "SMI_HARE_ROCK","SMI_HARRIS_PT_RESERVE_E","SMI_HARRIS_PT_RESERVE_W","SMI_PRINCE_ISLAND_CEN",
              "SMI_PRINCE_ISLAND_N","SMI_TYLER_BIGHT_E","SMI_TYLER_BIGHT_W","SRI_BEACON_REEF_E","SRI_BEACON_REEF_W","SRI_BEE_ROCK_E","SRI_BEE_ROCK_W","SRI_CARRINGTON_CEN","SRI_CARRINGTON_E","SRI_CARRINGTON_W","SRI_CHICKASAW_E","SRI_CHICKASAW_W","SRI_CLUSTER_POINT_N","SRI_CLUSTER_POINT_S","SRI_FORD_POINT","SRI_JOHNSONS_LEE_NORTH_E",
              "SRI_JOHNSONS_LEE_NORTH_W","SRI_JOHNSONS_LEE_SOUTH_E","SRI_JOHNSONS_LEE_SOUTH_W","SRI_JOLLA_VIEJA_E", "SRI_JOLLA_VIEJA_W","SRI_MONACOS_E","SRI_MONACOS_W","SRI_RODES_REEF_E","SRI_RODES_REEF_W",
              "SRI_SOUTH_POINT_E","SRI_SOUTH_POINT_W","SRI_TRANCION_CANYON_E","SRI_TRANCION_CANYON_W"))

north.ca.yr.mean <- PISCO.year.mean %>%
  subset(!site %in% c("ANACAPA_ADMIRALS_CEN", "ANACAPA_ADMIRALS_E","ANACAPA_ADMIRALS_W","ANACAPA_BLACK_SEA_BASS","ANACAPA_EAST_FISH_CAMP_CEN","ANACAPA_EAST_FISH_CAMP_E","ANACAPA_EAST_FISH_CAMP_W","ANACAPA_EAST_ISLE_CEN",
                     "ANACAPA_EAST_ISLE_E","ANACAPA_EAST_ISLE_W","ANACAPA_LIGHTHOUSE_REEF_CEN","ANACAPA_LIGHTHOUSE_REEF_E",
                     "ANACAPA_LIGHTHOUSE_REEF_W","ANACAPA_MIDDLE_ISLE_CEN","ANACAPA_MIDDLE_ISLE_E","ANACAPA_MIDDLE_ISLE_W",
                     "ANACAPA_WEST_ISLE_CEN","ANACAPA_WEST_ISLE_E","ANACAPA_WEST_ISLE_W","CAT_BLUE_CAVERN","CAT_INTAKE_PIPES","CAT_PUMPERNICKEL",
                     "COJO_E","COJO_W","COUNTY_LINE","EL_MATADOR","HORSESHOE_REEF_E","HORSESHOE_REEF_W","IV_REEF_E","IV_REEF_W","LEO_CARRILLO",
                     "LITTLE_DUME","NAPLES_CEN","NAPLES_E","NAPLES_W","POINT_DUME","SBI_ARCH_POINT_CEN","SBI_ARCH_POINT_N","SBI_ARCH_POINT_S","SBI_CAT_CANYON_CEN","SBI_CAT_CANYON_W","SBI_GRAVEYARD_CANYON_CEN","SBI_GRAVEYARD_CANYON_N",
                     "SBI_SOUTHEAST_REEF_CEN","SBI_SOUTHEAST_REEF_S","SBI_SOUTHEAST_SEA_LION_CEN","SBI_SOUTHEAST_SEA_LION_N","SBI_WEBSTERS_ARCH_CEN",
                     "SBI_WEBSTERS_ARCH_E","SBI_WEBSTERS_ARCH_N","SCI_CAVERN_POINT_E","SCI_CAVERN_POINT_W","SCI_COCHE_POINT_E","SCI_COCHE_POINT_W","SCI_FORNEY_E","SCI_FORNEY_W","SCI_GULL_ISLE_E","SCI_GULL_ISLE_W","SCI_HAZARDS_CEN","SCI_HAZARDS_E","SCI_HAZARDS_W","SCI_LITTLE_SCORPION_E","SCI_LITTLE_SCORPION_W",
                     "SCI_PAINTED_CAVE_CEN","SCI_PAINTED_CAVE_E","SCI_PAINTED_CAVE_W","SCI_PELICAN_CEN","SCI_PELICAN_E","SCI_PELICAN_FAR_WEST","SCI_PELICAN_W","SCI_POTATO_PASTURE_E","SCI_POTATO_PASTURE_W",
                     "SCI_SAN_PEDRO_POINT_E","SCI_SAN_PEDRO_POINT_W","SCI_SCORPION_ANCHORAGE","SCI_SCORPION_E","SCI_SCORPION_W","SCI_VALLEY_CEN","SCI_VALLEY_E",
                     "SCI_VALLEY_W","SCI_YELLOWBANKS_CEN","SCI_YELLOWBANKS_E","SCI_YELLOWBANKS_W","SMI_BAY_POINT","SMI_CROOK_POINT_E","SMI_CROOK_POINT_W","SMI_CUYLER_E","SMI_CUYLER_W",
                     "SMI_HARE_ROCK","SMI_HARRIS_PT_RESERVE_E","SMI_HARRIS_PT_RESERVE_W","SMI_PRINCE_ISLAND_CEN",
                     "SMI_PRINCE_ISLAND_N","SMI_TYLER_BIGHT_E","SMI_TYLER_BIGHT_W","SRI_BEACON_REEF_E","SRI_BEACON_REEF_W","SRI_BEE_ROCK_E","SRI_BEE_ROCK_W","SRI_CARRINGTON_CEN","SRI_CARRINGTON_E","SRI_CARRINGTON_W","SRI_CHICKASAW_E","SRI_CHICKASAW_W","SRI_CLUSTER_POINT_N","SRI_CLUSTER_POINT_S","SRI_FORD_POINT","SRI_JOHNSONS_LEE_NORTH_E",
                     "SRI_JOHNSONS_LEE_NORTH_W","SRI_JOHNSONS_LEE_SOUTH_E","SRI_JOHNSONS_LEE_SOUTH_W","SRI_JOLLA_VIEJA_E", "SRI_JOLLA_VIEJA_W","SRI_MONACOS_E","SRI_MONACOS_W","SRI_RODES_REEF_E","SRI_RODES_REEF_W",
                     "SRI_SOUTH_POINT_E","SRI_SOUTH_POINT_W","SRI_TRANCION_CANYON_E","SRI_TRANCION_CANYON_W"))


#cpue by year only
PISCO.mean <- PISCO.year.mean <- PISCO.aggregate.transect %>%
  group_by(year) %>%
  summarize(mean.count = mean(sum.SATRtot), 
            ntransect = sum(sum.ntransect),
            CPUE.tr = mean(CPUE.tr), 
            CPUE.vol = mean(CPUE.vol),
            pctcnpy = mean(pctcnpy))

#graph CPUE by site (using transect count as effort)
# x11()
#code doesn't work yet (plot is created but unreadable)
# ggplot(PISCO.year.mean, mapping = aes(year, CPUE))+
#   geom_point() +
#   facet_wrap_paginate(~site)
# ggsave('cpue.site', cpue.site, device = "jpeg")

#kelp graph by year, subset for the s cal sites
x11()
ggplot(south.ca.yr.mean, aes(year, pctcnpy, color = factor(site))) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Estimated Percent Transect Occupied by Kelp (Southern CA)",
       caption = "1 = 0-33%, 2 = 34-66%, 3 = 67-100%")

#cpue graph by year, subset for the s cal sites. line and boxplot and?
x11()
ggplot(south.ca.yr.mean, aes(year, CPUE.vol, color = factor(site))) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Southern CA CPUE")

x11()
ggplot(south.ca.sites, aes(year, CPUE.vol, color = factor(site))) +
  geom_boxplot() +
  theme(legend.position = "none")
  labs(title = "Southern CA CPUE")

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







