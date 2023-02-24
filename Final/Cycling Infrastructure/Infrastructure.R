# Install CycleInfraLnd package to wotk with TFL provided list of infrastructure.
# install.packages("devtools")
# devtools::install_github("PublicHealthDataGeek/CycleInfraLnd")

# Load packages.
library(CycleInfraLnd)
library(tidyverse)
library(sf)
library(dplyr)
library(mapview)

# Import boroughs bounary map (ONS London data - file = lon_boundaries_Dec_2020_BFC.Rds)
lon_2020 <- readRDS(file.choose())

##################################################

# 1. PARKING SPACES FOR CYCLES IN LONDON.

# Create df with all parking spaces available.
cycle_parking = get_cid_points(type = "cycle_parking")

# View the df and its size. 
head(cycle_parking)
dim(cycle_parking)   # 23758 records and 22 columns
as_tibble(cycle_parking)

# Check for missing values in the data frame.
sum(is.na(cycle_parking))   # 4 missing values found.

# Look for missing values column.
colnames(cycle_parking)[colSums(is.na(cycle_parking)) > 0]   # "PRK_PROVIS" and "PRK_CPT"
# Create a df with missing values.
nan_cp = cycle_parking[rowSums(is.na(cycle_parking)) > 0, ] 
view(nan_cp)

# Replace PRK_PROVIS = 1 for each of NaN for the 2 locations (as there is only one TRUE
# value for each of the locations in the df).
cycle_parking$PRK_PROVIS[is.na(cycle_parking$PRK_PROVIS)] <- 1

# Replace PRK_CPT with a mean capacity.
cycle_parking$PRK_CPT[is.na(cycle_parking$PRK_CPT)] <- round(mean(cycle_parking$PRK_CPT, na.rm=TRUE),0)

# Check for missing values in the data frame.
sum(is.na(cycle_parking))   # none

# Checking the values.
unique(cycle_parking$FEATURE_ID)   # 23758 records
unique(cycle_parking$BOROUGH)   # 33 boroughs
unique(sort(cycle_parking$SVDATE))   # 331 dates
unique(cycle_parking$PRK_CARR)
unique(cycle_parking$PRK_COVER)
unique(cycle_parking$PRK_CPT)
unique(cycle_parking$PRK_SECURE)
unique(cycle_parking$PRK_LOCKER)
unique(cycle_parking$PRK_SHEFF)
unique(cycle_parking$PRK_MSTAND)
unique(cycle_parking$PRK_PSTAND)
unique(cycle_parking$PRK_HOOP)
unique(cycle_parking$PRK_POST)
unique(cycle_parking$PRK_BUTERF)
unique(cycle_parking$PRK_WHEEL)
unique(cycle_parking$PRK_HANGAR)
unique(cycle_parking$PRK_TIER)
unique(cycle_parking$PRK_OTHER)

# Replace TRUE and FALSE with numerical values.
cycle_parking$PRK_CARR <- as.integer(as.logical(cycle_parking$PRK_CARR))
cycle_parking$PRK_COVER <- as.integer(as.logical(cycle_parking$PRK_COVER))
cycle_parking$PRK_SECURE <- as.integer(as.logical(cycle_parking$PRK_SECURE))
cycle_parking$PRK_LOCKER <- as.integer(as.logical(cycle_parking$PRK_LOCKER))
cycle_parking$PRK_SHEFF <- as.integer(as.logical(cycle_parking$PRK_SHEFF))
cycle_parking$PRK_MSTAND <- as.integer(as.logical(cycle_parking$PRK_MSTAND))
cycle_parking$PRK_PSTAND <- as.integer(as.logical(cycle_parking$PRK_PSTAND))
cycle_parking$PRK_HOOP <- as.integer(as.logical(cycle_parking$PRK_HOOP))
cycle_parking$PRK_POST <- as.integer(as.logical(cycle_parking$PRK_POST))
cycle_parking$PRK_BUTERF <- as.integer(as.logical(cycle_parking$PRK_BUTERF))
cycle_parking$PRK_WHEEL <- as.integer(as.logical(cycle_parking$PRK_WHEEL))
cycle_parking$PRK_HANGAR <- as.integer(as.logical(cycle_parking$PRK_HANGAR))
cycle_parking$PRK_TIER <- as.integer(as.logical(cycle_parking$PRK_TIER))
cycle_parking$PRK_OTHER <- as.integer(as.logical(cycle_parking$PRK_OTHER))
view(cycle_parking)

# Check the descriptive statistics.
summary(cycle_parking)   # survey dates: 2017-01-06 - 2019-06-10 

# Save the file with the data frame used in your wd.
write_csv(cycle_parking, file='cycle_parking.csv')

# Parking availability by borough.
parking_borough <- cycle_parking %>% group_by(BOROUGH)%>%
  summarise(capacity = sum(PRK_CPT),
            stands = sum(PRK_PROVIS),
            PRK_CARR = sum(PRK_CARR),
            PRK_COVER = sum(PRK_COVER),
            PRK_SECURE = sum(PRK_SECURE),
            PRK_LOCKER = sum(PRK_LOCKER),
            PRK_SHEFF = sum(PRK_SHEFF),
            PRK_MSTAND = sum(PRK_MSTAND),
            PRK_PSTAND = sum(PRK_PSTAND),
            PRK_HOOP = sum(PRK_HOOP),
            PRK_POST = sum(PRK_POST),
            PRK_BUTERF = sum(PRK_BUTERF),
            PRK_WHEEL = sum(PRK_WHEEL),
            PRK_HANGAR = sum(PRK_HANGAR),
            PRK_TIER = sum(PRK_TIER),
            PRK_OTHER = sum(PRK_OTHER),
            .groups = 'drop') %>%
            arrange(desc(capacity))
parking_borough
view(parking_borough)

# Visualize the concentration of cycle parkings on the map.
mapview(parking_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

### Code, if needed to see parking by filtering for borough for example.
#WF = parking_borough %>% 
#   filter(BOROUGH == "Waltham Forest")
#mapview(WF)

#####################################################

# 2. CYCLE LANES IN LONDON.

# Create df with all cycle lanes available.
cycle_lane = get_cid_lines(type = "cycle_lane_track")

# View the df and its size
head(cycle_lane)
dim(cycle_lane)   # 24976 record and  23 columns.
as_tibble(cycle_lane)

# Check for missing values in the data frame.
sum(is.na(cycle_lane))   # 22414 values are missing.

# Look for missing values column.
colnames(cycle_lane)[colSums(is.na(cycle_lane)) > 0]   # "CLT_ACCESS" and "BOROUGH"

# Create a df with missing values.
nan_cl = cycle_lane[rowSums(is.na(cycle_lane)) > 0, ] 
view(nan_cl)

## Replace CLT_ACCESS with 'not available' (as there are no any notes added).
cycle_lane$CLT_ACCESS[is.na(cycle_lane$CLT_ACCESS)] <- 'not available'

# Check for missing values in the data frame again.
sum(is.na(cycle_lane))   # 354 boroughs names missing

# Add the length of the cycle lanes in meters using geometry information.
cycle_lane = cycle_lane %>%
  mutate(length_m = st_length(geometry))
view(cycle_lane)

# Investigate NaN boroughs.
nan_borough <- cycle_lane[is.na(cycle_lane$BOROUGH),]
view(nan_borough)

# Put the  cycle lanes with missing boroughs on a map with boroughs boundaries.
mapview(nan_borough, zcol = "BOROUGH", lwd = 2) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

# Update NaN boroughs with the values according to the map.
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290719'] <- 'Croydon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG235336'] <- 'Hounslow'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290078'] <-  'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG235661'] <- 'Hounslow'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG221075'] <- 'Newham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG276485'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG110033'] <- 'Wandsworth'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290813'] <- 'Croydon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG199014'] <- 'Greenwich'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG154871'] <- 'Hackney'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG278754'] <- 'Bromley'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG291579'] <- 'Hounslow'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG154182'] <- 'Wandsworth'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG150143'] <- 'Waltham Forest'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG109844'] <- 'Wandsworth'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG155734'] <- 'Newham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG274955'] <- 'Brent'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG123764'] <- 'Richmond upon Thames'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG275227'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG292414'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG289969'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG236676'] <- 'Hillingdon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG236723'] <- 'Hillingdon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG235399'] <- 'Richmond upon Thames'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG279987'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG124500'] <- 'Sutton'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG277432'] <- 'Lewisham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG279975'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG198753'] <- 'Lewisham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG155710'] <- 'Tower Hamlets'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG221279'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG030533'] <- 'Hackney'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG235707'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG151549'] <- 'Hackney'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG102636'] <- 'Westminster'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG221043'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG280202'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG154671'] <- 'Havering'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG280124'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG155721'] <- 'Tower Hamlets'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290822'] <- 'Croydon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG279906'] <- 'Barnet'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG235043'] <- 'Hounslow'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG280207'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG066544'] <- 'Westminster'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG237461'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG204661'] <- 'Bromley'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG108205'] <- 'Hammersmith & Fulham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG275434'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG280113'] <- 'Barnet'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG277446'] <- 'Lewisham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG198262'] <- 'Bromley'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG185748'] <- 'Greenwich'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG154731'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG275434'] <- 'Ealing'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG291369'] <- 'Hillingdon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG274994'] <- 'Brent'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG198770'] <- 'Bexley'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290014'] <- 'Redbridge'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG135275'] <- 'Lambeth'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG999310'] <- 'Richmond upon Thames'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG151466'] <- 'Hackney'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG124133'] <- 'Merton'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG280290'] <- 'Haringey'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG198560'] <- 'Bexley'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG205234'] <- 'Lewisham' 
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG277446'] <- 'Lewisham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG277441'] <- 'Lewisham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG290313'] <- 'Newham'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG234862'] <- 'Hillingdon'
cycle_lane$BOROUGH[cycle_lane$FEATURE_ID == 'RWG056021'] <- 'Lambeth'


###########
# Looking for individual lanes on the map.
#long_nan = nan_borough %>% 
# filter(FEATURE_ID == "RWG056021")
#mapview(long_nan, zcol = "BOROUGH", lwd = 2) + 
#  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)
###########

# We can see that some of them are situated in 2 boroughs or on the border of 2.
# Decision was made to manually match the lanes longer than 500 meters and not divided 
# between boroughs and subset the rest from the df for now.

# Drop NA Boroughs records from the dataset.
cycle_lane <- cycle_lane[rowSums(is.na(cycle_lane)) == 0, ] 
sum(is.na(cycle_lane))   # none.
dim(cycle_lane)   # 24691 records and 24 columns.

# Checking the values.

unique(cycle_lane$FEATURE_ID) # 24691 unique variables
unique(cycle_lane$BOROUGH)  # 35 Boroughs
unique(sort(cycle_lane$SVDATE))   # 345 dates and "6482-04-01" needs to be investigated.
unique(cycle_lane$CLT_CARR)
unique(cycle_lane$CLT_SEGREG)
unique(cycle_lane$CLT_STEPP)
unique(cycle_lane$CLT_PARSEG)
unique(cycle_lane$CLT_SHARED)  # "FALSE" "TRUE" and "TCB" to investigate.
unique(cycle_lane$CLT_MANDAT)  # "FALSE" "TRUE" and "TCB" to investigate.
unique(cycle_lane$CLT_ADVIS)
unique(cycle_lane$CLT_PRIORI)  # FALSE" "TRUE" and "TRE" to investigate.
unique(cycle_lane$CLT_CONTRA)
unique(cycle_lane$CLT_BIDIRE)
unique(cycle_lane$CLT_CBYPAS)
unique(cycle_lane$CLT_BBYPAS)
unique(cycle_lane$CLT_PARKR)
unique(cycle_lane$CLT_WATERR)
unique(cycle_lane$CLT_PTIME)
unique(cycle_lane$CLT_ACCESS)  # lots of NA plus 724 comments mostly with timing regulations. 
unique(cycle_lane$CLT_COLOUR)  # 6 different options and "NONE"

# Replace CLT_PRIOTI = TRE to TRUE as a misspelling.
cycle_lane$CLT_PRIORI[cycle_lane$CLT_PRIORI == 'TRE'] <- 'TRUE'
unique(cycle_lane$CLT_PRIORI) 

# Investigate TCB in CLT_MANDATE.
tcb_clt_mandat = filter(cycle_lane, CLT_MANDAT == 'TCB')
view(tcb_clt_mandat)

# Replace CLT_MANDAT = TCB to TRUE after checking photos for this sight.
cycle_lane$CLT_MANDAT[cycle_lane$CLT_MANDAT == 'TCB'] <- 'TRUE'
unique(cycle_lane$CLT_MANDAT)

# Investigate TCB in CLT_SHARED.
tcb_clt_shared = filter(cycle_lane, CLT_SHARED == 'TCB')
view(tcb_clt_shared)

# Replace CLT_SHARED = TCB to TRUE after checking photos in 4 records.
cycle_lane$CLT_SHARED[cycle_lane$CLT_SHARED == 'TCB'] <- 'TRUE'
unique(cycle_lane$CLT_SHARED)

# Investigate "6482-04-01" in SVDATE
weird_date = filter(cycle_lane, SVDATE == '6482-04-01')
weird_date
# Drop the record.
cycle_lane <- cycle_lane[!(cycle_lane$SVDATE == '6482-04-01'),]
dim(cycle_lane)   # 24690 records and 24 columns.

# Replace TRUE and FALSE values to 1 and 0.
cycle_lane$CLT_CARR <- as.integer(as.logical(cycle_lane$CLT_CARR))
cycle_lane$CLT_SEGREG <- as.integer(as.logical(cycle_lane$CLT_SEGREG))
cycle_lane$CLT_STEPP <- as.integer(as.logical(cycle_lane$CLT_STEPP))
cycle_lane$CLT_PARSEG <- as.integer(as.logical(cycle_lane$CLT_PARSEG))
cycle_lane$CLT_SHARED <- as.integer(as.logical(cycle_lane$CLT_SHARED))
cycle_lane$CLT_MANDAT <- as.integer(as.logical(cycle_lane$CLT_MANDAT))
cycle_lane$CLT_ADVIS <- as.integer(as.logical(cycle_lane$CLT_ADVIS))
cycle_lane$CLT_PRIORI <- as.integer(as.logical(cycle_lane$CLT_PRIORI))
cycle_lane$CLT_CONTRA <- as.integer(as.logical(cycle_lane$CLT_CONTRA))
cycle_lane$CLT_BIDIRE <- as.integer(as.logical(cycle_lane$CLT_BIDIRE))
cycle_lane$CLT_CBYPAS <- as.integer(as.logical(cycle_lane$CLT_CBYPAS))
cycle_lane$CLT_BBYPAS <- as.integer(as.logical(cycle_lane$CLT_BBYPAS))
cycle_lane$CLT_PARKR <- as.integer(as.logical(cycle_lane$CLT_PARKR))
cycle_lane$CLT_WATERR <- as.integer(as.logical(cycle_lane$CLT_WATERR))
cycle_lane$CLT_PTIME <- as.integer(as.logical(cycle_lane$CLT_PTIME))
view(cycle_lane)

# Check general statistics.
summary(cycle_lane)   # SVDATE between 2017-01-06 and 2019-09-02 

# Save the file with the data frame used in your wd.
write_csv(cycle_lane, file='cycle_lane.csv')

# Visualise all cycle lanes on the map with boroughs.
mapview(cycle_lane) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

# Cycle lanes by boroughs.
cycle_lane_borough <- cycle_lane %>% group_by(BOROUGH)%>%
  summarise(LENGTH = sum(length_m),
            CLT_ADVIS = sum(CLT_ADVIS),
            CLT_BBYPAS = sum(CLT_BBYPAS),
            CLT_BIDIRE = sum(CLT_BIDIRE),
            CLT_CARR = sum(CLT_CARR),
            CLT_CBYPAS = sum(CLT_CBYPAS),
            CLT_CONTRA = sum(CLT_CONTRA),
            CLT_MANDAT = sum(CLT_MANDAT),
            CLT_PARKR = sum(CLT_PARKR),
            CLT_PARSEG = sum(CLT_PARSEG),
            CLT_PRIORI = sum(CLT_PRIORI),
            CLT_PTIME = sum(CLT_PTIME),
            CLT_SEGREG = sum(CLT_SEGREG),
            CLT_SHARED = sum(CLT_SHARED),
            CLT_STEPP = sum(CLT_STEPP),
            CLT_WATERR = sum(CLT_WATERR),
            .groups = 'drop') %>%
  arrange(desc(LENGTH))
cycle_lane_borough
view(cycle_lane_borough)

### Code, if needed to see parking by filtering for borough for example.
#Richmond = cycle_lane %>% 
#  filter(BOROUGH == "Richmond upon Thames")
#mapview(Richmond)

## Visualize only segregated and partially segregated cycle lanes.

# Filter the data set for segregated/ partially segregated cycle lanes.
segregated <- cycle_lane %>%
  filter((cycle_lane$CLT_SEGREG != 0) | (cycle_lane$CLT_PARSEG != 0))
view(segregated)
# View on the map.
mapview(segregated) +
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

## Visualize only waterside and park cycle lanes (not on carriageways)

# Filter the data set for parks/waterside cycle lanes.
parks <- cycle_lane %>%
  filter((cycle_lane$CLT_PARKR != 0) & (cycle_lane$CLT_CARR == 0) | (cycle_lane$CLT_WATERR != 0))
view(parks)
# View on the map.
mapview(parks) +
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

######################################################

# 3. TRAFFIC CALMING MEASURES IN LONDON.

# Create a df with traffic calming sites in London.
cycle_calm = get_cid_points(type = "traffic_calming")

# View the df and its size.
head(cycle_calm)
dim(cycle_calm)   # 58565 records and 14 columns.
as_tibble(cycle_calm)

# Check for missing values in the data frame.
sum(is.na(cycle_calm))   # none.

# Checking 
unique(cycle_calm$FEATURE_ID)   # 58565 records
unique(cycle_calm$BOROUGH)   # 33 boroughs
unique(sort(cycle_calm$SVDATE))   # 334 dates
unique(cycle_calm$TRF_RAISED)
unique(cycle_calm$TRF_ENTRY)
unique(cycle_calm$TRF_CUSHI)
unique(cycle_calm$TRF_HUMP)
unique(cycle_calm$TRF_NAROW)
unique(cycle_calm$TRF_CALM)
unique(cycle_calm$TRF_SINUSO)
unique(cycle_calm$TRF_BARIER)
       
# Replace TRUE and FALSE values to 1 and 0.
cycle_calm$TRF_RAISED <- as.integer(as.logical(cycle_calm$TRF_RAISED))
cycle_calm$TRF_BARIER <- as.integer(as.logical(cycle_calm$TRF_BARIER))
cycle_calm$TRF_ENTRY <- as.integer(as.logical(cycle_calm$TRF_ENTRY))
cycle_calm$TRF_CUSHI <- as.integer(as.logical(cycle_calm$TRF_CUSHI))
cycle_calm$TRF_HUMP <- as.integer(as.logical(cycle_calm$TRF_HUMP))
cycle_calm$TRF_SINUSO <- as.integer(as.logical(cycle_calm$TRF_SINUSO))
cycle_calm$TRF_NAROW <- as.integer(as.logical(cycle_calm$TRF_NAROW))
cycle_calm$TRF_CALM <- as.integer(as.logical(cycle_calm$TRF_CALM))
view(cycle_calm)

# Descriptive statistics.
summary(cycle_calm)   # Survey dates between 2017-01-06 and 2018-08-21

# Save the file with the data frame used in your wd.
write_csv(cycle_calm, file='cycle_calm.csv')

# See the traffic calming measures by borough.
cycle_calm_borough <- cycle_calm %>% group_by(BOROUGH)%>%
  summarise(TRF_RAISED = sum(TRF_RAISED),
            TRF_ENTRY = sum(TRF_ENTRY),
            TRF_CUSHI = sum(TRF_CUSHI),
            TRF_HUMP = sum(TRF_CUSHI),
            TRF_SINUSO = sum(TRF_SINUSO),
            TRF_BARIER = sum(TRF_BARIER),
            TRF_NAROW = sum(TRF_NAROW),
            TRF_CALM = sum(TRF_CALM),
            .groups = 'drop') %>%
  arrange(desc(BOROUGH))
cycle_calm_borough
view(cycle_calm_borough)

# Visualize the concentration of traffic calming on the map.
mapview(cycle_calm_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

#####################################################

# 4. ADVANCED STOP LINES (ASL) FOR CYCLES IN LONDON.

# Create a df with ASL in London.
cycle_asl = get_cid_lines(type = "advanced_stop_line")

# View the df and check its size.
head(cycle_asl)
dim(cycle_asl)   # 3775 records and 12 columns.
as_tibble(cycle_asl)

# Check for missing values in the data frame.
sum(is.na(cycle_asl))   # 1 record

# Look for missing values column.
colnames(cycle_asl)[colSums(is.na(cycle_asl)) > 0]  # 1 BOROUGH is missing.

# Investigate NaN boroughs
nan_borough <- cycle_asl[is.na(cycle_asl$BOROUGH),]
view(nan_borough)

# Check the location on the map.
mapview(nan_borough) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

# ASL is between Greenwich and Lewisham. Let's assign it to Greenwich.
cycle_asl <- replace(cycle_asl, is.na(cycle_asl), 'Greenwich')

# Check for missing values.
sum(is.na(cycle_asl))   # no missing values.

# Checking 
unique(cycle_asl$FEATURE_ID)   #3775 records
unique(cycle_asl$BOROUGH) # 33 boroughs
unique(sort(cycle_asl$SVDATE)) 
unique(cycle_asl$ASL_FDR)
unique(cycle_asl$ASL_FDRLFT)
unique(cycle_asl$ASL_FDCENT)
unique(cycle_asl$ASL_FDRIGH)
unique(cycle_asl$ASL_SHARED)
unique(cycle_asl$ASL_COLOUR)

# Replace TRUE and FALSE values to 1 and 0.
cycle_asl$ASL_FDR <- as.integer(as.logical(cycle_asl$ASL_FDR))
cycle_asl$ASL_FDRLFT <- as.integer(as.logical(cycle_asl$ASL_FDRLFT))
cycle_asl$ASL_FDCENT <- as.integer(as.logical(cycle_asl$ASL_FDCENT))
cycle_asl$ASL_FDRIGH <- as.integer(as.logical(cycle_asl$ASL_FDRIGH))
cycle_asl$ASL_SHARED <- as.integer(as.logical(cycle_asl$ASL_SHARED))
view(cycle_asl)

# Adding the length of the ASL.
cycle_asl = cycle_asl %>%
  mutate(asl_length_m = st_length(geometry))
view(cycle_asl)

# Descriptive summary.
summary(cycle_asl)   # Survey dates between 2017-01-06 and 2018-08-01

# Save the file with the data frame used in your wd.
write_csv(cycle_asl, file='cycle_asl.csv')

# Group information by borough.
cycle_asl_borough <- cycle_asl %>% group_by(BOROUGH)%>%
  summarise(ASL_FDR = sum(ASL_FDR),
            ASL_FDRLFT = sum(ASL_FDRLFT),
            ASL_FDCENT = sum(ASL_FDCENT),
            ASL_FDRIGH = sum(ASL_FDRIGH),
            ASL_SHARED = sum(ASL_SHARED),
            asl_length_m= sum(asl_length_m),
            .groups = 'drop') %>%
  arrange(desc(asl_length_m))
cycle_asl_borough
view(cycle_asl_borough)

# Visualize the concentration of ASL on the map.
mapview(cycle_asl_borough, cex = 1) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

#################################################

# 5. SIGNALS FOR CYCLES IN LONDON.

# Create a df with signals in London.
cycle_signal = get_cid_points(type = "signal")

# View the df and its size.
head(cycle_signal)
dim(cycle_signal)   # 443 records and 11 columns.
as_tibble(cycle_signal)

# Check for missing values in the data frame.
sum(is.na(cycle_signal))   # no missing values found.

# Checking 
unique(cycle_signal$FEATURE_ID)   # 443 records
unique(cycle_signal$BOROUGH)   # 23 boroughs
unique(sort(cycle_signal$SVDATE))   # 111 dates
unique(cycle_signal$SIG_HEAD)
unique(cycle_signal$SIG_SEPARA)
unique(cycle_signal$SIG_EARLY)
unique(cycle_signal$SIG_TWOSTG)
unique(cycle_signal$SIG_GATE)

# Replace TRUE and FALSE values to 1 and 0.
cycle_signal$SIG_HEAD <- as.integer(as.logical(cycle_signal$SIG_HEAD))
cycle_signal$SIG_SEPARA <- as.integer(as.logical(cycle_signal$SIG_SEPARA))
cycle_signal$SIG_EARLY <- as.integer(as.logical(cycle_signal$SIG_EARLY))
cycle_signal$SIG_TWOSTG <- as.integer(as.logical(cycle_signal$SIG_TWOSTG))
cycle_signal$SIG_GATE <- as.integer(as.logical(cycle_signal$SIG_GATE))
view(cycle_signal)

# Descriptive statistics.
summary(cycle_signal)   # Survey dates between 2017-02-06 and 2018-05-15

# Save the file with the data frame used in your wd.
write_csv(cycle_signal, file='cycle_signal.csv')

# Group information by borough.
cycle_signal_borough <- cycle_signal %>% group_by(BOROUGH)%>%
  summarise(SIG_EARLY = sum(SIG_EARLY),
            SIG_HEAD = sum(SIG_HEAD),
            SIG_GATE = sum(SIG_GATE),
            SIG_SEPARA = sum(SIG_SEPARA),
            SIG_TWOSTG = sum(SIG_TWOSTG),
            .groups = 'drop') %>%
  arrange(BOROUGH)
cycle_signal_borough
view(cycle_signal_borough)

# Visualize the concentration of signals on the map.
mapview(cycle_signal_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

########################################################

# 6. RESTRICTED ROUTES FOR CYCLES IN LONDON.

# Create a df with restricted routes in London.
cycle_restricted = get_cid_lines(type = "restricted_route")

# View the df and check its size.
head(cycle_restricted)
dim(cycle_restricted)   # 1378 records and 11 columns.
as_tibble(cycle_restricted)

# Check for missing values in the data frame.
sum(is.na(cycle_restricted))   # 18 missing values found.

# Look for missing values column.
colnames(cycle_restricted)[colSums(is.na(cycle_restricted)) > 0]  # 18 boroughs are missing.

# Investigate NaN boroughs
nan_borough <- cycle_restricted[is.na(cycle_restricted$BOROUGH),]
view(nan_borough)

# Put the restricted routes with missing boroughs on a map with boroughs boundaries.
mapview(nan_borough) + 
  mapview(lon_lad_2022, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

# Update NaN boroughs with the values according to the map.
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG109327"] <- 'Southwark'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG205452"] <- 'Croydon'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG109078"] <- 'Newham'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG109094"] <- 'Tower Hamlets'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG038491"] <- 'Camden'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG280843"] <- 'Camden'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG280846"] <- 'Camden'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG118072"] <- 'Westminster'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG999982"] <- 'Barnet'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG280839"] <- 'Camden'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG275874"] <- 'Harrow'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG119662"] <- 'Lambeth'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG280905"] <- 'Haringey'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG102665"] <- 'Westminster'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG278215"] <- 'Merton'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG280847"] <- 'Barnet'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG237863"] <- 'Hillingdon'
cycle_restricted$BOROUGH[cycle_restricted$FEATURE_ID == "RWG205451"] <- 'Bromley'

# Check for missing values.
sum(is.na(cycle_restricted))  # none.

# Checking 
unique(cycle_restricted$FEATURE_ID)   # 1378 records
unique(cycle_restricted$BOROUGH)   # 33 borough
unique(sort(cycle_restricted$SVDATE))   # 196 dates
unique(cycle_restricted$RES_PEDEST)
unique(cycle_restricted$RES_BRIDGE)
unique(cycle_restricted$RES_TUNNEL)
unique(cycle_restricted$RES_STEPS)
unique(cycle_restricted$RES_LIFT)

# Replace TRUE and FALSE values to 1 and 0.
cycle_restricted$RES_PEDEST <- as.integer(as.logical(cycle_restricted$RES_PEDEST))
cycle_restricted$RES_BRIDGE <- as.integer(as.logical(cycle_restricted$RES_BRIDGE))
cycle_restricted$RES_TUNNEL <- as.integer(as.logical(cycle_restricted$RES_TUNNEL))
cycle_restricted$RES_STEPS <- as.integer(as.logical(cycle_restricted$RES_STEPS))
cycle_restricted$RES_LIFT <- as.integer(as.logical(cycle_restricted$RES_LIFT))
view(cycle_signal)

# Adding the length of the restricted roads.
cycle_restricted = cycle_restricted %>%
  mutate(res_length_m = st_length(geometry))
view(cycle_restricted)

# Descriptive statistics.
summary(cycle_restricted)   # Survey dates between 2017-06-04 and 2018-08-06

# Save the file with the data frame used in your wd.
write_csv(cycle_restricted, file='cycle_restricted.csv')

# Group information by borough.
cycle_restricted_borough <- cycle_restricted %>% group_by(BOROUGH)%>%
  summarise(RES_PEDEST = sum(RES_PEDEST),
            RES_BRIDGE = sum(RES_BRIDGE),
            RES_TUNNEL = sum(RES_TUNNEL),
            RES_STEPS = sum(RES_STEPS),
            RES_LIFT = sum(RES_LIFT),
            res_length_m = sum(res_length_m),
            .groups = 'drop') %>%
  arrange(desc(res_length_m))
cycle_restricted_borough
view(cycle_restricted_borough)

# Visualize the concentration of restricted routes on the map.
mapview(cycle_restricted_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

#####################################################

# 7. CROSSINGS FOR CYCLES IN LONDON.

# Create a df with crossings information in London.
cycle_cross = get_cid_lines(type = "crossing")

# View the df and check its size.
head(cycle_cross)
dim(cycle_cross)   # 1687 records and 11 columns.
as_tibble(cycle_cross)

# Check for missing values in the data frame.
sum(is.na(cycle_cross))   # 28 values missing.

# Look for missing values column.
colnames(cycle_cross)[colSums(is.na(cycle_cross)) > 0]  # 28 borough names are missing

# Adding the length of crossings.
cycle_cross = cycle_cross %>%
  mutate(cross_length_m = st_length(geometry))
view(cycle_cross)

# Investigate NaN boroughs
nan_borough <- cycle_cross[is.na(cycle_cross$BOROUGH),]
view(nan_borough)

# Put the restricted routes with missing boroughs on a map with boroughs boundaries.
mapview(nan_borough) + 
  mapview(lon_2020, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

# Update NaN boroughs with the values according to the map
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG108005'] <- 'Southwark'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG055869'] <- 'Southwark'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG108004'] <- 'Lambeth'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG107385'] <- 'Lambeth'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG153061'] <- 'City of London'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG065992'] <- 'Camden'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG003325'] <- 'Islington'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG073152'] <- 'Hackney'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG184365'] <- 'Bexley'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG154508'] <- 'Barking & Dagenham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG154512'] <- 'Newham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG154511'] <- 'Newham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG187249'] <- 'Enfield'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG293100'] <-  'Harrow'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG049417'] <-  'Hammersmith & Fulham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG273940' ] <- 'Brent'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG003326'] <- 'Islington'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG150947'] <- 'Camden'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG025515'] <- 'Haringey'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG025510'] <- 'Haringey'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG244675'] <- 'Haringey'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG042145'] <- 'Hackney'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG236647'] <- 'Hillingdon'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG199197'] <- 'Lewisham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG135349'] <- 'Lambeth'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG205317'] <- 'Lewisham'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG273946'] <- 'Brent'
cycle_cross$BOROUGH[cycle_cross$FEATURE_ID == 'RWG199184'] <- 'Lewisham'

# Check for missing values.                    
nan_borough <- cycle_cross[is.na(cycle_cross$BOROUGH),]
view(nan_borough)   # None found.

# Checking the values.
unique(cycle_cross$FEATURE_ID)   # 1687 records.
unique(cycle_cross$BOROUGH)   # 33 boroughs.
unique(sort(cycle_cross$SVDATE))   # 257 dates.
unique(cycle_cross$CRS_SIGNAL)
unique(cycle_cross$CRS_SEGREG)
unique(cycle_cross$CRS_CYGAP)
unique(cycle_cross$CRS_LEVEL)
unique(cycle_cross$CRS_PEDEST)

# Replace TRUE and FALSE values to 1 and 0.
cycle_cross$CRS_SIGNAL <- as.integer(as.logical(cycle_cross$CRS_SIGNAL))
cycle_cross$CRS_SEGREG <- as.integer(as.logical(cycle_cross$CRS_SEGREG))
cycle_cross$CRS_CYGAP <- as.integer(as.logical(cycle_cross$CRS_CYGAP))
cycle_cross$CRS_LEVEL <- as.integer(as.logical(cycle_cross$CRS_LEVEL))
cycle_cross$CRS_PEDEST <- as.integer(as.logical(cycle_cross$CRS_PEDEST))
view(cycle_signal)

# Descriptive summary.
summary(cycle_cross)   # Survey dates between 2017-05-31 and 2019-09-02

# Save the file with the data frame used in your wd.
write_csv(cycle_cross, file='cycle_cross.csv')

# Group information by borough.
cycle_cross_borough <- cycle_cross %>% group_by(BOROUGH)%>%
  summarise(CRS_SIGNAL = sum(CRS_SIGNAL),
            CRS_SEGREG = sum(CRS_SEGREG),
            CRS_CYGAP = sum(CRS_CYGAP),
            CRS_LEVEL = sum(CRS_LEVEL),
            CRS_PEDEST = sum(CRS_PEDEST),
            cross_length_m = sum(cross_length_m),
            .groups = 'drop') %>%
  arrange(desc(cross_length_m))
cycle_cross_borough
view(cycle_cross_borough)

# Visualize the concentration of restricted routes on the map.
mapview(cycle_cross_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

#####################################################

# 8. RESTRICTED POINTS FOR CYCLES IN LONDON.

# Create a df with restricted points information in London.
cycle_point = get_cid_points(type = "restricted_point")

# View the df and check its size.
head(cycle_point)
dim(cycle_point)   # 180 records and 8 columns.
as_tibble(cycle_point)

# Check for missing values in the data frame.
sum(is.na(cycle_point))   # no missing values.

# Checking 
unique(cycle_point$FEATURE_ID)   # 180 records.
unique(cycle_point$BOROUGH)   # 27 boroughs
unique(sort(cycle_point$SVDATE))   # 73 dates
unique(cycle_point$RST_STEPS)
unique(cycle_point$RST_LIFT)

# Replace TRUE and FALSE values to 1 and 0.
cycle_point$RST_STEPS <- as.integer(as.logical(cycle_point$RST_STEPS))
cycle_point$RST_LIFT <- as.integer(as.logical(cycle_point$RST_LIFT))
view(cycle_point)

# Descriptive statistics.
summary(cycle_point)   # Survey dates between 2017-06-13 and 2018-08-21.

# Save the file with the data frame used in your wd.
write_csv(cycle_point, file='cycle_point.csv')

# Group information by borough.
cycle_point_borough <- cycle_point %>% group_by(BOROUGH)%>%
  summarise(RST_STEPS = sum(RST_STEPS),
            RST_LIFT = sum(RST_LIFT),
            total_point = sum(RST_STEPS, RST_LIFT),
            .groups = 'drop') %>%
  arrange(desc(total_point))
cycle_point_borough
view(cycle_point_borough)

# Visualize the concentration of restricted points on the map.
mapview(cycle_point_borough, cex = 1, legend = FALSE) + 
  mapview(lon_2020, alpha.regions = 0.3, zcol = "BOROUGH", legend = FALSE, lwd = 0.5)

##################################################

# 9. SIGNS FOR CYCLISTS IN LONDON.

cycle_sign = get_cid_points(type = "signage")

# View the df and check its size.
head(cycle_sign)
dim(cycle_sign)   # 118,834 records and 37 columns.
as_tibble(cycle_sign)

# Check for missing values in the data frame.
sum(is.na(cycle_sign))   # 224647 missing values.

# Look for missing values column.
colnames(cycle_sign)[colSums(is.na(cycle_sign)) > 0]  # "SS_ROAD", "SS_ROUTEN",
#"SS_ACCESS", "SS_NAME", "SS_COLOUR", "BOROUGH" missing.

# Create a df with missing values.
nan_sign = cycle_sign[rowSums(is.na(cycle_sign)) > 0, ] 
view(nan_sign)

# Replace SS_ROUTEN and SS_ACCESS missing values as 'NONE'.
cycle_sign$SS_ROUTEN[is.na(cycle_sign$SS_ROUTEN)] <- 'NONE'
cycle_sign$SS_ACCESS[is.na(cycle_sign$SS_ACCESS)] <- 'NONE'
cycle_sign$SS_NAME[is.na(cycle_sign$SS_NAME)] <- 'NONE'
cycle_sign$SS_COLOUR[is.na(cycle_sign$SS_COLOUR)] <- 'NONE'

# Replace missing SS_ROAD with 'TRUE' after checking photos.
cycle_sign$SS_ROAD[is.na(cycle_sign$SS_ROAD)] <- 'TRUE'

# Replace missing borough with Havering, after checking the map.
cycle_sign$BOROUGH[is.na(cycle_sign$BOROUGH)] <- 'Havering'

# Checking 
unique(cycle_sign$FEATURE_ID)   # 118833 records
unique(cycle_sign$BOROUGH)   # 33 boroughs
unique(sort(cycle_sign$SVDATE))   #349 dates
unique(cycle_sign$SS_ROAD)
unique(cycle_sign$SS_PATCH)
unique(cycle_sign$SS_FACING)
unique(cycle_sign$SS_NOCYC)
unique(cycle_sign$SS_NOVEH)
unique(cycle_sign$SS_CIRC)
unique(cycle_sign$SS_EXEMPT)
unique(cycle_sign$SS_END)
unique(cycle_sign$SS_NOLEFT)
unique(cycle_sign$SS_NORIGH)
unique(cycle_sign$SS_LEFT)
unique(cycle_sign$SS_RIGHT)
unique(cycle_sign$SS_NOEXCE)
unique(cycle_sign$SS_DISMOU)
unique(cycle_sign$SS_CYCSMB)   # misspelling FASLE record investigate
unique(cycle_sign$SS_PEDSMB)
unique(cycle_sign$SS_BUSSMB)
unique(cycle_sign$SS_SMB)
unique(cycle_sign$SS_LNSIGN)
unique(cycle_sign$SS_ARROW)
unique(cycle_sign$SS_NRCOL)
unique(cycle_sign$SS_NCN)
unique(cycle_sign$SS_LCN)
unique(cycle_sign$SS_SUPERH)
unique(cycle_sign$SS_QUIETW)
unique(cycle_sign$SS_GREENW)
unique(cycle_sign$SS_ROUTEN)   # 428 notes
unique(cycle_sign$SS_DESTN)
unique(cycle_sign$SS_ACCESS)   # 802 time periods
unique(cycle_sign$SS_NAME)   # 65 records
unique(cycle_sign$SS_COLOUR)

# Replace SS_CYCSMB = FASLE to FALSE as a misspelling.
cycle_sign$SS_CYCSMB[cycle_sign$SS_CYCSMB == 'FASLE'] <- 'FALSE'
unique(cycle_sign$SS_CYCSMB)   # only TRUE and FALSE left.

# Replace TRUE and FALSE values to 1 and 0.
cycle_sign$SS_ROAD <- as.integer(as.logical(cycle_sign$SS_ROAD))
cycle_sign$SS_PATCH <- as.integer(as.logical(cycle_sign$SS_PATCH))
cycle_sign$SS_FACING <- as.integer(as.logical(cycle_sign$SS_FACING))
cycle_sign$SS_NOCYC <- as.integer(as.logical(cycle_sign$SS_NOCYC))
cycle_sign$SS_NOVEH <- as.integer(as.logical(cycle_sign$SS_NOVEH))
cycle_sign$SS_CIRC <- as.integer(as.logical(cycle_sign$SS_CIRC))
cycle_sign$SS_EXEMPT <- as.integer(as.logical(cycle_sign$SS_EXEMPT))
cycle_sign$SS_NOLEFT <- as.integer(as.logical(cycle_sign$SS_NOLEFT))
cycle_sign$SS_NORIGH <- as.integer(as.logical(cycle_sign$SS_NORIGH))
cycle_sign$SS_LEFT <- as.integer(as.logical(cycle_sign$SS_LEFT))
cycle_sign$SS_RIGHT <- as.integer(as.logical(cycle_sign$SS_RIGHT))
cycle_sign$SS_NOEXCE <- as.integer(as.logical(cycle_sign$SS_NOEXCE))
cycle_sign$SS_DISMOU <- as.integer(as.logical(cycle_sign$SS_DISMOU))
cycle_sign$SS_END <- as.integer(as.logical(cycle_sign$SS_END))
cycle_sign$SS_CYCSMB <- as.integer(as.logical(cycle_sign$SS_CYCSMB))
cycle_sign$SS_PEDSMB <- as.integer(as.logical(cycle_sign$SS_PEDSMB))
cycle_sign$SS_BUSSMB <- as.integer(as.logical(cycle_sign$SS_BUSSMB))
cycle_sign$SS_SMB <- as.integer(as.logical(cycle_sign$SS_SMB))
cycle_sign$SS_LNSIGN <- as.integer(as.logical(cycle_sign$SS_LNSIGN))
cycle_sign$SS_ARROW <- as.integer(as.logical(cycle_sign$SS_ARROW))
cycle_sign$SS_NRCOL <- as.integer(as.logical(cycle_sign$SS_NRCOL))
cycle_sign$SS_NCN <- as.integer(as.logical(cycle_sign$SS_NCN))
cycle_sign$SS_LCN <- as.integer(as.logical(cycle_sign$SS_LCN))
cycle_sign$SS_SUPERH <- as.integer(as.logical(cycle_sign$SS_SUPERH))
cycle_sign$SS_QUIETW <- as.integer(as.logical(cycle_sign$SS_QUIETW))
cycle_sign$SS_GREENW <- as.integer(as.logical(cycle_sign$SS_GREENW))
cycle_sign$SS_DESTN <- as.integer(as.logical(cycle_sign$SS_DESTN))

# Descriptive summary.
summary(cycle_sign)   # Survey dates between 2017-01-06 and 2018-08-21

# Cycle lanes by boroughs.
cycle_sign_borough <- cycle_sign %>% group_by(BOROUGH)%>%
  summarise(SS_ROAD = sum(SS_ROAD),
            SS_DESTN = sum(SS_PATCH),
            SS_FACING = sum(SS_FACING),
            SS_NOCYC = sum(SS_NOCYC),
            SS_NOVEH = sum(SS_NOVEH),
            SS_CIRC = sum(SS_CIRC),
            SS_EXEMPT = sum(SS_EXEMPT),
            SS_NOLEFT = sum(SS_NOLEFT),
            SS_NORIGH = sum(SS_NORIGH),
            SS_LEFT = sum(SS_LEFT),
            SS_RIGHT = sum(SS_RIGHT),
            SS_NOEXCE = sum(SS_NOEXCE),
            SS_DISMOU = sum(SS_DISMOU),
            SS_END = sum(SS_END),
            SS_CYCSMB  = sum(SS_CYCSMB),
            SS_PEDSMB = sum(SS_PEDSMB),
            SS_BUSSMB = sum(SS_BUSSMB),
            SS_SMB = sum(SS_SMB),
            SS_LNSIGN = sum(SS_LNSIGN),
            SS_ARROW = sum(SS_ARROW),
            SS_NRCOL = sum(SS_NRCOL),
            SS_NCN = sum(SS_NCN),
            SS_LCN = sum(SS_LCN),
            SS_SUPERH = sum(SS_SUPERH),
            SS_QUIETW = sum(SS_QUIETW),
            SS_GREENW = sum(SS_GREENW),
            SS_DESTN = sum(SS_DESTN),
            .groups = 'drop') %>%
  arrange(desc(BOROUGH))
cycle_sign_borough
view(cycle_sign_borough)


# no map image, as there are too many signs. You can filter the dataset
# by some criteria an see the distribution on the map.





