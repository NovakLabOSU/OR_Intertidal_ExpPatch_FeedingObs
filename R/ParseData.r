####################################################################
# Script to pars feeding observations and size distribution data from Feeding Observations database.
# Project-specific data are also parsed out to their respective folders.
# Projects are (as of 12/1/23):
#		-ExpPatch
#		-NaturalPatches
####################################################################
rm(list = ls()) # clears workspace
options(stringsAsFactors = F)
#############

table.loc <- '../DB/Tables/'
out.loc <- '../Data/'

############################
# Import data
#~~~~~~~~~~~~~
# NOTE: This assumes the most recent version of the database tables have been placed on the server.  They should be located in ".../OR_Intertidal/FeedingObs_db/FeedingObs_db-Tables/".
# NOTE: The data are stored in MS Access which Macs require proprietary drivers to access.  Therefore it is necessary to export the tables from MS Access to csv files first (using MDB ACCDB Viewer, http://www.macexplorer.co, which costs $5).
############################
# Convenience functions
#######################

# Remove extra columns added by MDB Viewer export
RemFunc <-
  function(x) {
    sel <- which(names(x) %in% c("s_ColLineage", "s_Generation", "s_GUID", "s_Lineage"))
    if(length(sel) > 0){
      x <- x[, -sel]
    }
    return(x)
  }

# minDiff is Minimum assumed difference between survey sets
ConvertDate2SurveyDate <- function(dat, minDiff = 9) {
  dat$Date <- as.Date(dat$Date)
  SDates <- sort(unique(dat$Date))
  Dates <- data.frame(SurveyDate = SDates, Date = SDates)
  Dates$difDates <- c(0, diff(Dates$Date))
  for (i in 2:nrow(Dates)) {
    if (Dates$difDates[i] < minDiff) {
      Dates$SurveyDate[i] <- Dates$SurveyDate[i - 1]
    }
  }
  dat <- merge(dat, Dates[, -3], by = 'Date', all.x = TRUE)
  # Switch names (in order to use 'Date' subsequently)
  colnames(dat)[which(colnames(dat) == 'Date')] <- 'TrueDate'
  colnames(dat)[which(colnames(dat) == 'SurveyDate')] <- 'Date'
  dat <- dat[, c(length(dat), 1:(length(dat) - 1))]
  return(dat)
}


# Match-up and rename Survey dates in two datasets
# (e.g., abundance dynamics and feeding observations)
# Spits out only the 2nd dataset (i.e. it conforms the dates in the 2nd dataset
# to match up with those of the 1st dataset.
MatchDates <- function(datA, datB, minDiff = 9) {
  datA$Date <- as.Date(datA$Date)
  datB$Date <- as.Date(datB$Date)
  SDatesA <- sort(unique(datA$Date))
  SDatesB <- sort(unique(datB$Date))
  for (i in 1:length(SDatesA)) {
    tDate <- SDatesA[i]
    tDateSeq <- seq(tDate - minDiff, tDate + minDiff, 1)
    Match <- which(datB$Date %in% tDateSeq)
    if (length(Match) != 0) {
      datB$Date[Match] <- tDate
    }
    if (length(Match) == 0) {
      warning(paste0('No date match found for ', tDate))
    }
  }
  return(datB)
}
#############################
Census <- RemFunc(read.csv(paste0(table.loc, 'Census.csv')))
Obs <- RemFunc(read.csv(paste0(table.loc, 'DataObservation.csv')))
IndArea <- RemFunc(read.csv(paste0(table.loc, 'IndexArea.csv')))
IndCoast <- RemFunc(read.csv(paste0(table.loc, 'IndexCoast.csv')))
IndCounter <- RemFunc(read.csv(paste0(table.loc, 'IndexCounter.csv')))
IndDate <- RemFunc(read.csv(paste0(table.loc, 'IndexDate.csv')))
IndDiel <- RemFunc(read.csv(paste0(table.loc, 'IndexDiel.csv')))
IndRegion <- RemFunc(read.csv(paste0(table.loc, 'IndexRegion.csv')))
IndRepeatObs <- RemFunc(read.csv(paste0(table.loc, 'IndexRepeatObs.csv')))
IndSeason <- RemFunc(read.csv(paste0(table.loc, 'IndexSeason.csv')))
IndShellCol <- RemFunc(read.csv(paste0(table.loc, 'IndexShellColour.csv')))
IndSite <- RemFunc(read.csv(paste0(table.loc, 'IndexSite.csv')))
IndSpp <- RemFunc(read.csv(paste0(table.loc, 'IndexSpecies.csv')))
IndTidalZone <- RemFunc(read.csv(paste0(table.loc, 'IndexTidalZone.csv')))
IndType <- RemFunc(read.csv(paste0(table.loc, 'IndexType.csv')))
IndYear <- RemFunc(read.csv(paste0(table.loc, 'IndexYear.csv')))


###########################
# Merge tables and clean-up
###########################
IndDate$Date <- as.Date(IndDate$Date)
IndDate <- merge(IndDate, IndSeason, all.x = TRUE)
IndDate <- merge(IndDate, IndYear, all.x = TRUE)

dCens <- merge(Census, IndDate[, c('DateID', 'Date')], all.x = TRUE)
dCens <- merge(dCens, IndCounter, all.x = TRUE)
dCens <- merge(dCens, IndSite[, c('SiteID', 'Site')], all.x = TRUE)
dCens <- merge(dCens, 
               IndArea[, c('AreaID', 'Area', 'Type', 'Height_rel_MLLW_cm')], 
               all.x = TRUE)
dCens <- merge(dCens, IndDiel, all.x = TRUE)
dCens <- merge(dCens, IndType, all.x = TRUE)
dCens <- dCens[, -c(1:6)]
dCens <- dCens[, c(1, 7:12, 14, 2:5)]
dCens <- dCens[order(dCens$Date, dCens$Site, dCens$Area, dCens$TimeStart), ]


dObs <- merge(Obs, 
              dCens[, c('CensusID', 'Date', 'Site', 'Area', 'Type', 'ObservationType')], 
              all.x = TRUE)
dObs <- merge(dObs,
        IndSpp[, c('SpeciesID', 'SpeciesName', 'Abb')],
        by.x = 'PredID',
        by.y = 'SpeciesID',
        all.x = TRUE)
colnames(dObs)[colnames(dObs) %in% c('SpeciesName', 'Abb')] <- c('Pred', 'PredAbb')
dObs <- merge(dObs,
              IndSpp[, c('SpeciesID', 'SpeciesName', 'Abb')],
              by.x = 'PreyID',
              by.y = 'SpeciesID',
              all.x = TRUE)
colnames(dObs)[colnames(dObs) %in% c('SpeciesName', 'Abb')] <- c('Prey', 'PreyAbb')
dObs <- merge(dObs,
              IndTidalZone[, c(1, 2)],
              all.x = TRUE,
              by.x = 'TidalZone',
              by.y = 'TidalZoneID')
dObs <- merge(dObs, IndRepeatObs[, c(1, 2)], all.x = TRUE)
dObs <- merge(dObs,
              IndShellCol[, c(1, 2)],
              all.x = TRUE,
              by.x = 'PredShellColour',
              by.y = 'ShellColourID')
dObs <- dObs[, -c(1:5)]
dObs <- dObs[, c(1:2, 12:20, 3:10, 21:23)]
dObs <-
  dObs[order(dObs$Date, dObs$Site, dObs$Type, dObs$Area, dObs$Pred), ]

##############################
# Export general clean files
##############################
write.table(
  dCens,
  paste0(out.loc,'All_FeedingObs-Census-All.csv'),
  sep = ',',
  row.names = FALSE
)
write.table(
  dObs,
  paste0(out.loc,'All_FeedingObs-Obs-All.csv'),
  sep = ',',
  row.names = FALSE
)

################################
# Export Project-specific files
################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For (Systematic) Natural Patch Surveys:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nat <- subset(IndArea[-1, ], Type == 'Natural')
dNatCens <- dCens[which(dCens$Area %in% Nat$Area), ]
dNatObs <- dObs[which(dObs$Area %in% Nat$Area), ]
write.table(
  dNatCens,
  paste0(out.loc,'NatPatch_FeedingObs-Census.csv'),
  sep = ',',
  row.names = FALSE
)
write.table(
  dNatObs,
  paste0(out.loc,'NatPatch_FeedingObs-Obs.csv'),
  sep = ',',
  row.names = FALSE
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For ExpPatch Surveys:
# Aggregate dates into "survey dates"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExpPatch <- subset(IndArea[-1, ], Type == 'Experimental')
dExpPatchCens <- dCens[which(dCens$Area %in% ExpPatch$Area), ]
dExpPatchObs <- dObs[which(dObs$Area %in% ExpPatch$Area), ]

dExpPatchCens <- ConvertDate2SurveyDate(dExpPatchCens)
dExpPatchObs <- ConvertDate2SurveyDate(dExpPatchObs)
colnames(dExpPatchObs)[grep('Area', colnames(dExpPatchObs))] <-
  'Patch'

write.table(
  dExpPatchCens,
  paste0(out.loc,'ExpPatch_FeedingObs-Census.csv'),
  sep = ',',
  row.names = FALSE
)
write.table(
  dExpPatchObs,
  paste0(out.loc,'ExpPatch_FeedingObs-Obs.csv'),
  sep = ',',
  row.names = FALSE
)

#######################################################################
#######################################################################
#######################################################################
