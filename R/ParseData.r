####################################################################
# Script to pars feeding observations and size distribution data from Feeding Observations database.
# Project-specific data are also parsed out to their respective folders.
# Projects are (as of 10/1/15):
#		-ExpPatch
#		-NaturalPatches
####################################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)
#############
# setwd('~/Desktop')
#############
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r') # To bin dates into "survey set" dates.

############################
# Import data
#~~~~~~~~~~~~~
# NOTE: This assumes the most recent version of the database tables have been placed on the server.  They should be located in ".../OR_Intertidal/FeedingObs_db/FeedingObs_db-Tables/".
# NOTE: The data are stored in MS Access which Macs require proprietary drivers to access.  Therefore it is necessary to export the tables from MS Access to csv files first (using MDB ACCDB Viewer, http://www.macexplorer.co, which costs $5).
############################
# Convenience function to remove extra columns added by MDB Viewer export
RemFunc<-function(x){x[,-which(names(x) %in% c("s_ColLineage","s_Generation","s_GUID","s_Lineage"))]}
#############################
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/FeedingObs_db/FeedingObs_db-Tables/')
Census<-RemFunc(read.csv('Census.csv'))
Obs<-RemFunc(read.csv('DataObservation.csv'))
IndArea<-RemFunc(read.csv('IndexArea.csv'))
IndCoast<-RemFunc(read.csv('IndexCoast.csv'))
IndCounter<-RemFunc(read.csv('IndexCounter.csv'))
IndDate<-RemFunc(read.csv('IndexDate.csv'))
IndDiel<-RemFunc(read.csv('IndexDiel.csv'))
IndRegion<-RemFunc(read.csv('IndexRegion.csv'))
IndRepeatObs<-RemFunc(read.csv('IndexRepeatObs.csv'))
IndSeason<-RemFunc(read.csv('IndexSeason.csv'))
IndShellCol<-RemFunc(read.csv('IndexShellColour.csv'))
IndSite<-RemFunc(read.csv('IndexSite.csv'))
IndSpp<-RemFunc(read.csv('IndexSpecies.csv'))
IndTidalZone<-RemFunc(read.csv('IndexTidalZone.csv'))
IndType<-RemFunc(read.csv('IndexType.csv'))
IndYear<-RemFunc(read.csv('IndexYear.csv'))
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/')

###########################
# Merge tables and clean-up
###########################
IndDate$Date<-as.Date(IndDate$Date)
IndDate<-merge(IndDate,IndSeason,all.x=TRUE)
IndDate<-merge(IndDate,IndYear,all.x=TRUE)

dCens<-merge(Census,IndDate[,c('DateID','Date')],all.x=TRUE)
dCens<-merge(dCens,IndCounter,all.x=TRUE)
dCens<-merge(dCens,IndSite[,c('SiteID','Site')],all.x=TRUE)
dCens<-merge(dCens,IndArea[,c('AreaID','Area','Type','Height_rel_MLLW_cm')],all.x=TRUE)
dCens<-merge(dCens,IndDiel,all.x=TRUE)
dCens<-merge(dCens,IndType,all.x=TRUE)
dCens<-dCens[,-c(1:6)]
dCens<-dCens[,c(1,7:12,14,2:5)]
dCens<-dCens[order(dCens$Date,dCens$Site,dCens$Area,dCens$TimeStart),]


dObs<-merge(Obs,dCens[,c('CensusID','Date','Site','Area','Type','ObservationType')],all.x=TRUE)
dObs<-merge(dObs,IndSpp[,c('SpeciesID','SpeciesName','Abb')],by.x='PredID',by.y='SpeciesID',all.x=TRUE)
colnames(dObs)[colnames(dObs)%in%c('SpeciesName','Abb')]<-c('Pred','PredAbb')
dObs<-merge(dObs,IndSpp[,c('SpeciesID','SpeciesName','Abb')],by.x='PreyID',by.y='SpeciesID',all.x=TRUE)
colnames(dObs)[colnames(dObs)%in%c('SpeciesName','Abb')]<-c('Prey','PreyAbb')
dObs<-merge(dObs,IndTidalZone[,c(1,2)],all.x=TRUE,by.x='TidalZone',by.y='TidalZoneID')
dObs<-merge(dObs,IndRepeatObs[,c(1,2)],all.x=TRUE)
dObs<-merge(dObs,IndShellCol[,c(1,2)],all.x=TRUE,by.x='PredShellColour',by.y='ShellColourID')
dObs<-dObs[,-c(1:5)]
dObs<-dObs[,c(1:2,12:20,3:10,21:23)]
dObs<-dObs[order(dObs$Date,dObs$Site,dObs$Type,dObs$Area,dObs$Pred),]

##############################
# Export general clean files
##############################
write.table(dCens, 'FeedingObs_db/FeedingObs_db-Data/FeedingObs-Census-All.csv',sep=',',row.names=FALSE)
write.table(dObs, 'FeedingObs_db/FeedingObs_db-Data/FeedingObs-Obs-All.csv',sep=',',row.names=FALSE)

################################
# Export Project-specific files
################################
#~~~~~~~~~~~~~~~~~~~~~~~~
# Anectodal observations:
#~~~~~~~~~~~~~~~~~~~~~~~~
dAnec<-subset(dObs,ObservationType=='Anecdotal Observation')
write.table(dAnec, 'FeedingObs_db/FeedingObs_db-Data/FeedingObs-Obs-Anecdotal.csv',sep=',',row.names=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For (Systematic) Natural Patch Surveys:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nat<-subset(IndArea[-1,],Type=='Natural')
dNatCens<-dCens[which(dCens$Area%in%Nat$Area),]
dNatObs<-dObs[which(dObs$Area%in%Nat$Area),]
write.table(dNatCens, 'NaturalPatches/NatPatch-Data/NatPatch_FeedingObs-Census.csv',sep=',',row.names=FALSE)
write.table(dNatObs, 'NaturalPatches/NatPatch-Data/NatPatch_FeedingObs-Obs.csv',sep=',',row.names=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For ExpPatch Surveys:
# Aggregate dates into "survey dates"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExpPatch<-subset(IndArea[-1,],Type=='Experimental')
dExpPatchCens<-dCens[which(dCens$Area%in%ExpPatch$Area),]
dExpPatchObs<-dObs[which(dObs$Area%in%ExpPatch$Area),]

dExpPatchCens<-ConvertDate2SurveyDate(dExpPatchCens)
dExpPatchObs<-ConvertDate2SurveyDate(dExpPatchObs)
colnames(dExpPatchObs)[grep('Area',colnames(dExpPatchObs))]<-'Patch'

write.table(dExpPatchCens, 'ExperimentalPatches/ExpPatch-Data/ExpPatch_FeedingObs-Census.csv',sep=',',row.names=FALSE)
write.table(dExpPatchObs, 'ExperimentalPatches/ExpPatch-Data/ExpPatch_FeedingObs-Obs.csv',sep=',',row.names=FALSE)

#######################################################################
#######################################################################
#######################################################################
