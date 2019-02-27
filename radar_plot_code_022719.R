packages <- c("stargazer","devtools","tidyverse","vegan","MASS","aod","ResourceSelection","graphics",
              "lmtest","stats","boot","dave","gridExtra","ggpubr","fmsb","indicspecies","broom","RColorBrewer","ggradar","ggrepel")
# Then, check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# now, use the lapply function to load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)

setwd("C:\\Users\\jascott\\Documents\\Project\\_RMB_paper\\Veg_Data\\Analysis")
source("GuildedFloraFunction_120517.R")
source("other_functions.R")

############################
### bring in guild trait data
trait_table <- read.table("C:\\Users\\jascott\\Documents\\Project\\_RMB_paper\\Veg_Data\\Trait_and_Guilds\\RMB_Traits_102516_Guilds_DMM.csv",head=T, sep=",",stringsAsFactors = FALSE)
### bring in species guild membership data, where first column is species, followed by guild membership columns
gmem <- read.table("C:\\Users\\jascott\\Documents\\Project\\_RMB_paper\\Veg_Data\\Analysis\\October2016\\Guild25_06-22-17.csv", header=T, sep=",")
gmem$Species <- as.character(gmem$Species)

# add column of final guild membership to trait table
trait_table <- left_join(trait_table,gmem,by = "Species")
# update name of column that contains the guild code to be "Guild"
names(trait_table)[which(colnames(trait_table) == "Guild25_06-22-17")] <- "Guild"

### remove any species without a guild if nessecary 
# trait_table <- trait_table[which(!is.na(trait_table$Guild)),]

### create look up table to add new names for each guild by alpha ID
lup_1 <- data.frame(Guild = c("a","b","c","d","e","f","g","i","j","k","l","m"),
                    name = c("Tall Tree",
                             "Xeric Re-sprout",
                             "ShallowRoot SexRepro",
                             "Xeric Woody",
                             "Hydric Woody",
                             "Shade Tolerant Shrub",
                             "DeepRoot SexRepro",
                             "Hydric Small-Seeded",
                             "Hydric Large-Seeded",
                             "Hydric Herbaceous",
                             "Xeric herbaceous",
                             "FloodAvoid Herb"))

# select just the species column, guild membership column desired for plotting, and the species traits
t1_hist <- trait_table[,!(colnames(trait_table) %in% c("HT","seed","RMBGuilds_05","Guild25_11_03_16"))]
# gather up the traits values into a single column
t1_hist <- gather( t1_hist,Trait,TraitValue,c("lnHT","Anae_tol","drgt_tol","Sal_tol","Sha_tol","RootMax_ord","lnSeed","Resprout"))
# join up with look up table 1up_1 to add desired guild name convention
t1_hist <- left_join(t1_hist,lup_1,by = "Guild")

# # Create look up table to modify trait names for plotting
# lup_4 <- data.frame(Trait = unique(t1_hist$Trait),
#                     Trait_n = c("Height","Anaerobic Tol", "Drought Tol",
#                                 "Salinity Tol","Shade Tol","Rooting Dpth","Seed Mass","Resprout"))
#####################################
# Not nessecary, but informative - summaries of trait data
#####################################

# get the mean trait value for each trait for each guild
# ord_summary <- dplyr::summarise(group_by(t1_hist,name,Trait),mean=mean(TraitValue))
# ord_summary.df <- data.frame(ord_summary)

# get overall max and min for each trait, across all guilds
# maxmin_df <- t1_hist %>%
  # spread(Trait,TraitValue)

# max_tv <- summarise_at(maxmin_df,c("lnHT","Anae_tol","drgt_tol","Sal_tol","Sha_tol","RootMax_ord","lnSeed","Resprout"),funs(max))
# min_tv <- summarise_at(maxmin_df,c("lnHT","Anae_tol","drgt_tol","Sal_tol","Sha_tol","RootMax_ord","lnSeed","Resprout"),funs(min))


### ggradar 
t1_hist %>%
  spread(Trait,TraitValue) %>%
  mutate_at(vars(-c(Species,Guild,name)),funs(scales::rescale)) %>%
  group_by(name) %>%
  summarise_at(vars(-c(Species,Guild,name)),funs(mean)) %>%
  dplyr::rename(group=name) -> st_tv

colnames(st_tv) <- c("name","anaerobic","drought","height","seed","resprout","root depth","salinity","shade")

# create ggplots and save
# jpeg("radar_3.jpeg", units="in", width=14, height=10, res=600)

ggradar(st_tv,
        background.circle.transparency = 0,
        axis.label.offset = 1.0,
        axis.label.size = 5,
        grid.label.size = 0)+
  theme(legend.position = "none")+
  facet_wrap(~group)
