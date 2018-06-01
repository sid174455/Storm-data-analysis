library("neuralnet")
library("stringr")
library("caret")
library("pROC")

## Global variables and flags

ann.DEBUG <- TRUE

# Run with actual parameters
ann.BEEFY <- TRUE

## SETUP
# vars <- ls()
# namespace <- grep("kknn.", ls())
# rm(list=vars[namespace])

# Copy over storm data so we don't mess with anyone else's code
# If you get errors at this part, run, Project_Data_Preprocess.R first!
ann.sda <- storm_data_all
ann.sdr <- storm_data_relevant

if (ann.DEBUG) {
  set.seed(1337)
}

if (ann.BEEFY) {
  ann.THRESHOLD <- 0.01
  ann.REPEATS <- 5
  ann.STEPMAX <- 1e+07
} else {
  ann.THRESHOLD <- 0.01
  ann.REPEATS <- 1
  ann.STEPMAX <- 1e+05
}

## Helper functions
softmax <- function (x) { log(1 + exp(x)) }
coefficient <- function(a) {
  if (a == "B") { 1000000000 }
  else if (a == "M") { 1000000 }
  else if (a == "K") { 1000 }
  else { 0 }
}

standardScale <- function (x) { (x - mean(x)) / sd(x) }
normalize <- function (x) { (x - min(x)) / (max(x) - min(x)) }
special_chars <- "[^[:alnum:]]"

## Clean Data

# Convert MONTH_NAME to integer
month_map <- seq(1, 12)
names(month_map) = c("January", "February", "March", "April", "May", "June",
                     "July", "August", "September", "October", "November", "December")
change_months <- function (b) { month_map[b] }
ann.sdr$MONTH_NAME <- normalize(
  as.numeric(
    lapply(ann.sdr$MONTH_NAME, change_months)))

# Convert STATE to set of boolean attributes
for (j in unique(ann.sdr$STATE)) {
  name <- paste("STATE", toupper(gsub(special_chars, "_", j)), sep="_")
  ann.sdr[, name] <- as.numeric(ann.sdr$STATE == j)
}

# Convert SOURCE to set of boolean attributes
for (j in unique(ann.sdr$SOURCE)) {
  name <- paste("SOURCE", toupper(gsub(special_chars, "_", j)), sep="_")
  ann.sdr[, name] <- as.numeric(ann.sdr$SOURCE == j)
}

# Convert EVENT_TYPE to set of boolean attributes
for (j in unique(ann.sdr$EVENT_TYPE)) {
  name <- paste("EVENT_TYPE", toupper(gsub(special_chars, "_", j)), sep="_")
  ann.sdr[, name] <- as.numeric(ann.sdr$EVENT_TYPE == j)
}

# Convert some strings to numeric
ann.sdr$DAMAGE_PROPERTY_LOW <- as.numeric(ann.sdr$DAMAGE_PROPERTY == "Low")
ann.sdr$DAMAGE_PROPERTY_MED <- as.numeric(ann.sdr$DAMAGE_PROPERTY == "Medium")
ann.sdr$DAMAGE_PROPERTY_HIGH <- as.numeric(ann.sdr$DAMAGE_PROPERTY == "High")

ann.sdr$DAMAGE_PROPERTY[ann.sdr$DAMAGE_PROPERTY == "Low"] <- 0
ann.sdr$DAMAGE_PROPERTY[ann.sdr$DAMAGE_PROPERTY == "Medium"] <- 1
ann.sdr$DAMAGE_PROPERTY[ann.sdr$DAMAGE_PROPERTY == "High"] <- 2
ann.sdr$DAMAGE_PROPERTY <- as.numeric(ann.sdr$DAMAGE_PROPERTY)
ann.sdr$DAMAGE_PROPERTY <- normalize(as.numeric(ann.sdr$DAMAGE_PROPERTY))

ann.sdr$DAMAGE_CROPS_LOW <- as.numeric(ann.sdr$DAMAGE_CROPS == "Low")
ann.sdr$DAMAGE_CROPS_MED <- as.numeric(ann.sdr$DAMAGE_CROPS == "Medium")
ann.sdr$DAMAGE_CROPS_HIGH <- as.numeric(ann.sdr$DAMAGE_CROPS == "High")

ann.sdr$DAMAGE_CROPS[ann.sdr$DAMAGE_CROPS == "Low"] <- 0
ann.sdr$DAMAGE_CROPS[ann.sdr$DAMAGE_CROPS == "Medium"] <- 1
ann.sdr$DAMAGE_CROPS[ann.sdr$DAMAGE_CROPS == "High"] <- 2
ann.sdr$DAMAGE_CROPS <- as.numeric(ann.sdr$DAMAGE_CROPS)
ann.sdr$DAMAGE_CROPS <- normalize(as.numeric(ann.sdr$DAMAGE_CROPS))

# Convert DEATH to binary
ann.sdr$DEATHS <- as.numeric(ann.sdr$DEATHS == "Yes")

# Convert INJURIES to binary
ann.sdr$INJURIES <- as.numeric(ann.sdr$INJURIES == "Yes")

# Remove old columns
ann.sdr$SOURCE <- NULL
ann.sdr$EVENT_TYPE <- NULL
ann.sdr$STATE <- NULL

ann.sdr$MAGNITUDE <- NULL
ann.sdr$MAGNITUDE_TYPE <- NULL

# Normalize
ann.sdr$BEGIN_DAY <- normalize(ann.sdr$BEGIN_DAY)
ann.sdr$END_DAY <- normalize(ann.sdr$END_DAY)

## Sample Data
ann.sdr <- sample(ann.sdr) # Shuffle order
index <- seq(1, nrow(ann.sdr), by=5)
ann.test <- ann.sdr[index, ]
ann.training <- ann.sdr[-index, ]

if (ann.BEEFY) {
  ann.SAMPLE_SIZE <- nrow(ann.training) / 2
} else {
  ann.SAMPLE_SIZE <- nrow(ann.training) / 5
  ann.SAMPLE_SIZE
}

ann.training <- ann.training[sample(nrow(ann.training), ann.SAMPLE_SIZE), ]

# ann.training <- as.data.frame(ann.training)
# ann.test <- as.data.frame(ann.test)

## Build Equations

build_eq <- function(dep, indep) {
  pattern <- paste(dep, collapse = "|")
  indep_ <- indep[ -str_detect(dep, pattern) ]
  as.formula(paste0(
    paste(dep, collapse = " + "),
    " ~ ",
    paste(indep_, collapse = " + ")
  ))
}

cn <- names(ann.sdr)
dmg_prop_eq <- DAMAGE_PROPERTY_LOW + DAMAGE_PROPERTY_MED + DAMAGE_PROPERTY_HIGH ~
  BEGIN_DAY +
  END_DAY +
  MONTH_NAME +
  DAMAGE_CROPS +
  INJURIES +
  DEATHS +
  STATE_NEW_JERSEY +
  STATE_OHIO +
  STATE_GEORGIA +
  STATE_VIRGINIA +
  # STATE_GULF_OF_MEXICO +
  STATE_NEBRASKA +
  # STATE_ATLANTIC_NORTH +
  STATE_FLORIDA +
  STATE_MONTANA +
  STATE_MISSOURI +
  STATE_KANSAS +
  # STATE_ATLANTIC_SOUTH +
  STATE_ALABAMA +
  STATE_ARIZONA +
  STATE_MASSACHUSETTS +
  STATE_SOUTH_CAROLINA +
  STATE_ILLINOIS +
  STATE_KENTUCKY +
  STATE_TEXAS +
  STATE_MARYLAND +
  # STATE_LAKE_SUPERIOR +
  STATE_LOUISIANA +
  STATE_WISCONSIN +
  # STATE_DISTRICT_OF_COLUMBIA +
  STATE_NORTH_CAROLINA +
  STATE_DELAWARE +
  STATE_WYOMING +
  STATE_PENNSYLVANIA +
  STATE_CALIFORNIA +
  STATE_ARKANSAS +
  STATE_MISSISSIPPI +
  STATE_TENNESSEE +
  STATE_INDIANA +
  STATE_ALASKA +
  STATE_CONNECTICUT +
  STATE_WEST_VIRGINIA +
  STATE_OREGON +
  STATE_OKLAHOMA +
  STATE_NEW_YORK +
  STATE_VERMONT +
  STATE_NORTH_DAKOTA +
  STATE_MINNESOTA +
  STATE_NEVADA +
  # STATE_LAKE_MICHIGAN +
  STATE_MICHIGAN +
  STATE_NEW_MEXICO +
  # STATE_LAKE_ERIE +
  STATE_IOWA +
  STATE_SOUTH_DAKOTA +
  # STATE_PUERTO_RICO +
  # STATE_E_PACIFIC +
  STATE_HAWAII +
  STATE_UTAH +
  STATE_WASHINGTON +
  STATE_COLORADO +
  STATE_RHODE_ISLAND +
  STATE_MAINE +
  # STATE_LAKE_ST_CLAIR +
  STATE_IDAHO +
  # STATE_LAKE_HURON +
  STATE_NEW_HAMPSHIRE +
  # STATE_AMERICAN_SAMOA +
  # STATE_LAKE_ONTARIO +
  # STATE_ST_LAWRENCE_R +
  SOURCE_TRAINED_SPOTTER +
  SOURCE_AMATEUR_RADIO +
  SOURCE_911_CALL_CENTER +
  SOURCE_PUBLIC +
  SOURCE_AWOS +
  SOURCE_LAW_ENFORCEMENT +
  SOURCE_BROADCAST_MEDIA +
  SOURCE_BUOY +
  SOURCE_MESONET +
  SOURCE_SOCIAL_MEDIA +
  SOURCE_COUNTY_OFFICIAL +
  SOURCE_OFFICIAL_NWS_OBSERVATIONS +
  SOURCE_FIRE_DEPARTMENT_RESCUE +
  SOURCE_EMERGENCY_MANAGER +
  SOURCE_ASOS +
  SOURCE_UTILITY_COMPANY +
  SOURCE_COOP_OBSERVER +
  SOURCE_C_MAN_STATION +
  SOURCE_DEPARTMENT_OF_HIGHWAYS +
  SOURCE_NWS_EMPLOYEE +
  SOURCE_NEWSPAPER +
  SOURCE_RAWS +
  SOURCE_OTHER_FEDERAL_AGENCY +
  SOURCE_STATE_OFFICIAL +
  SOURCE_NWS_STORM_SURVEY +
  SOURCE_UNKNOWN +
  SOURCE_LOCAL_OFFICIAL +
  SOURCE_STORM_CHASER +
  SOURCE_COCORAHS +
  SOURCE_TRIBAL_OFFICIAL +
  SOURCE_COAST_GUARD +
  SOURCE_PARK_FOREST_SERVICE +
  SOURCE_AWSS +
  SOURCE_POST_OFFICE +
  SOURCE_WLON +
  SOURCE_INSURANCE_COMPANY +
  SOURCE_RIVER_STREAM_GAGE +
  SOURCE_SHAVE_PROJECT +
  SOURCE_MARINER +
  SOURCE_LIFEGUARD +
  SOURCE_AIRPLANE_PILOT +
  EVENT_TYPE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_STRONG_WIND +
  EVENT_TYPE_MARINE_HIGH_WIND

dmg_crop_eq <- DAMAGE_CROPS_LOW + DAMAGE_CROPS_MED + DAMAGE_CROPS_HIGH ~
  BEGIN_DAY +
  END_DAY +
  MONTH_NAME +
  DAMAGE_PROPERTY +
  DEATHS +
  DEATHS +
  STATE_NEW_JERSEY +
  STATE_OHIO +
  STATE_GEORGIA +
  STATE_VIRGINIA +
  # STATE_GULF_OF_MEXICO +
  STATE_NEBRASKA +
  # STATE_ATLANTIC_NORTH +
  STATE_FLORIDA +
  STATE_MONTANA +
  STATE_MISSOURI +
  STATE_KANSAS +
  # STATE_ATLANTIC_SOUTH +
  STATE_ALABAMA +
  STATE_ARIZONA +
  STATE_MASSACHUSETTS +
  STATE_SOUTH_CAROLINA +
  STATE_ILLINOIS +
  STATE_KENTUCKY +
  STATE_TEXAS +
  STATE_MARYLAND +
  # STATE_LAKE_SUPERIOR +
  STATE_LOUISIANA +
  STATE_WISCONSIN +
  # STATE_DISTRICT_OF_COLUMBIA +
  STATE_NORTH_CAROLINA +
  STATE_DELAWARE +
  STATE_WYOMING +
  STATE_PENNSYLVANIA +
  STATE_CALIFORNIA +
  STATE_ARKANSAS +
  STATE_MISSISSIPPI +
  STATE_TENNESSEE +
  STATE_INDIANA +
  STATE_ALASKA +
  STATE_CONNECTICUT +
  STATE_WEST_VIRGINIA +
  STATE_OREGON +
  STATE_OKLAHOMA +
  STATE_NEW_YORK +
  STATE_VERMONT +
  STATE_NORTH_DAKOTA +
  STATE_MINNESOTA +
  STATE_NEVADA +
  # STATE_LAKE_MICHIGAN +
  STATE_MICHIGAN +
  STATE_NEW_MEXICO +
  # STATE_LAKE_ERIE +
  STATE_IOWA +
  STATE_SOUTH_DAKOTA +
  # STATE_PUERTO_RICO +
  # STATE_E_PACIFIC +
  STATE_HAWAII +
  STATE_UTAH +
  STATE_WASHINGTON +
  STATE_COLORADO +
  STATE_RHODE_ISLAND +
  STATE_MAINE +
  # STATE_LAKE_ST_CLAIR +
  STATE_IDAHO +
  # STATE_LAKE_HURON +
  STATE_NEW_HAMPSHIRE +
  # STATE_AMERICAN_SAMOA +
  # STATE_LAKE_ONTARIO +
  # STATE_ST_LAWRENCE_R +
  SOURCE_TRAINED_SPOTTER +
  SOURCE_AMATEUR_RADIO +
  SOURCE_911_CALL_CENTER +
  SOURCE_PUBLIC +
  SOURCE_AWOS +
  SOURCE_LAW_ENFORCEMENT +
  SOURCE_BROADCAST_MEDIA +
  SOURCE_BUOY +
  SOURCE_MESONET +
  SOURCE_SOCIAL_MEDIA +
  SOURCE_COUNTY_OFFICIAL +
  SOURCE_OFFICIAL_NWS_OBSERVATIONS +
  SOURCE_FIRE_DEPARTMENT_RESCUE +
  SOURCE_EMERGENCY_MANAGER +
  SOURCE_ASOS +
  SOURCE_UTILITY_COMPANY +
  SOURCE_COOP_OBSERVER +
  SOURCE_C_MAN_STATION +
  SOURCE_DEPARTMENT_OF_HIGHWAYS +
  SOURCE_NWS_EMPLOYEE +
  SOURCE_NEWSPAPER +
  SOURCE_RAWS +
  SOURCE_OTHER_FEDERAL_AGENCY +
  SOURCE_STATE_OFFICIAL +
  SOURCE_NWS_STORM_SURVEY +
  SOURCE_UNKNOWN +
  SOURCE_LOCAL_OFFICIAL +
  SOURCE_STORM_CHASER +
  SOURCE_COCORAHS +
  SOURCE_TRIBAL_OFFICIAL +
  SOURCE_COAST_GUARD +
  SOURCE_PARK_FOREST_SERVICE +
  SOURCE_AWSS +
  SOURCE_POST_OFFICE +
  SOURCE_WLON +
  SOURCE_INSURANCE_COMPANY +
  SOURCE_RIVER_STREAM_GAGE +
  SOURCE_SHAVE_PROJECT +
  SOURCE_MARINER +
  SOURCE_LIFEGUARD +
  SOURCE_AIRPLANE_PILOT +
  EVENT_TYPE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_STRONG_WIND +
  EVENT_TYPE_MARINE_HIGH_WIND

inj_eq <- INJURIES ~
  BEGIN_DAY +
  END_DAY +
  MONTH_NAME +
  DAMAGE_PROPERTY +
  DAMAGE_CROPS +
  DEATHS +
  STATE_NEW_JERSEY +
  STATE_OHIO +
  STATE_GEORGIA +
  STATE_VIRGINIA +
  # STATE_GULF_OF_MEXICO +
  STATE_NEBRASKA +
  # STATE_ATLANTIC_NORTH +
  STATE_FLORIDA +
  STATE_MONTANA +
  STATE_MISSOURI +
  STATE_KANSAS +
  # STATE_ATLANTIC_SOUTH +
  STATE_ALABAMA +
  STATE_ARIZONA +
  STATE_MASSACHUSETTS +
  STATE_SOUTH_CAROLINA +
  STATE_ILLINOIS +
  STATE_KENTUCKY +
  STATE_TEXAS +
  STATE_MARYLAND +
  # STATE_LAKE_SUPERIOR +
  STATE_LOUISIANA +
  STATE_WISCONSIN +
  STATE_DISTRICT_OF_COLUMBIA +
  STATE_NORTH_CAROLINA +
  STATE_DELAWARE +
  STATE_WYOMING +
  STATE_PENNSYLVANIA +
  STATE_CALIFORNIA +
  STATE_ARKANSAS +
  STATE_MISSISSIPPI +
  STATE_TENNESSEE +
  STATE_INDIANA +
  STATE_ALASKA +
  STATE_CONNECTICUT +
  STATE_WEST_VIRGINIA +
  STATE_OREGON +
  STATE_OKLAHOMA +
  STATE_NEW_YORK +
  STATE_VERMONT +
  STATE_NORTH_DAKOTA +
  STATE_MINNESOTA +
  STATE_NEVADA +
  # STATE_LAKE_MICHIGAN +
  STATE_MICHIGAN +
  STATE_NEW_MEXICO +
  # STATE_LAKE_ERIE +
  STATE_IOWA +
  STATE_SOUTH_DAKOTA +
  # STATE_PUERTO_RICO +
  # STATE_E_PACIFIC +
  STATE_HAWAII +
  STATE_UTAH +
  STATE_WASHINGTON +
  STATE_COLORADO +
  STATE_RHODE_ISLAND +
  STATE_MAINE +
  # STATE_LAKE_ST_CLAIR +
  STATE_IDAHO +
  # STATE_LAKE_HURON +
  STATE_NEW_HAMPSHIRE +
  # STATE_AMERICAN_SAMOA +
  # STATE_LAKE_ONTARIO +
  # STATE_ST_LAWRENCE_R +
  SOURCE_TRAINED_SPOTTER +
  SOURCE_AMATEUR_RADIO +
  SOURCE_911_CALL_CENTER +
  SOURCE_PUBLIC +
  SOURCE_AWOS +
  SOURCE_LAW_ENFORCEMENT +
  SOURCE_BROADCAST_MEDIA +
  SOURCE_BUOY +
  SOURCE_MESONET +
  SOURCE_SOCIAL_MEDIA +
  SOURCE_COUNTY_OFFICIAL +
  SOURCE_OFFICIAL_NWS_OBSERVATIONS +
  SOURCE_FIRE_DEPARTMENT_RESCUE +
  SOURCE_EMERGENCY_MANAGER +
  SOURCE_ASOS +
  SOURCE_UTILITY_COMPANY +
  SOURCE_COOP_OBSERVER +
  SOURCE_C_MAN_STATION +
  SOURCE_DEPARTMENT_OF_HIGHWAYS +
  SOURCE_NWS_EMPLOYEE +
  SOURCE_NEWSPAPER +
  SOURCE_RAWS +
  SOURCE_OTHER_FEDERAL_AGENCY +
  SOURCE_STATE_OFFICIAL +
  SOURCE_NWS_STORM_SURVEY +
  SOURCE_UNKNOWN +
  SOURCE_LOCAL_OFFICIAL +
  SOURCE_STORM_CHASER +
  SOURCE_COCORAHS +
  SOURCE_TRIBAL_OFFICIAL +
  SOURCE_COAST_GUARD +
  SOURCE_PARK_FOREST_SERVICE +
  SOURCE_AWSS +
  SOURCE_POST_OFFICE +
  SOURCE_WLON +
  SOURCE_INSURANCE_COMPANY +
  SOURCE_RIVER_STREAM_GAGE +
  SOURCE_SHAVE_PROJECT +
  SOURCE_MARINER +
  SOURCE_LIFEGUARD +
  SOURCE_AIRPLANE_PILOT +
  EVENT_TYPE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_STRONG_WIND +
  EVENT_TYPE_MARINE_HIGH_WIND

death_eq <- DEATHS ~
  BEGIN_DAY +
  END_DAY +
  MONTH_NAME +
  DAMAGE_PROPERTY +
  DAMAGE_CROPS +
  INJURIES +
  STATE_NEW_JERSEY +
  STATE_OHIO +
  STATE_GEORGIA +
  STATE_VIRGINIA +
  # STATE_GULF_OF_MEXICO +
  STATE_NEBRASKA +
  # STATE_ATLANTIC_NORTH +
  STATE_FLORIDA +
  STATE_MONTANA +
  STATE_MISSOURI +
  STATE_KANSAS +
  # STATE_ATLANTIC_SOUTH +
  STATE_ALABAMA +
  STATE_ARIZONA +
  STATE_MASSACHUSETTS +
  STATE_SOUTH_CAROLINA +
  STATE_ILLINOIS +
  STATE_KENTUCKY +
  STATE_TEXAS +
  STATE_MARYLAND +
  # STATE_LAKE_SUPERIOR +
  STATE_LOUISIANA +
  STATE_WISCONSIN +
  STATE_DISTRICT_OF_COLUMBIA +
  STATE_NORTH_CAROLINA +
  STATE_DELAWARE +
  STATE_WYOMING +
  STATE_PENNSYLVANIA +
  STATE_CALIFORNIA +
  STATE_ARKANSAS +
  STATE_MISSISSIPPI +
  STATE_TENNESSEE +
  STATE_INDIANA +
  STATE_ALASKA +
  STATE_CONNECTICUT +
  STATE_WEST_VIRGINIA +
  STATE_OREGON +
  STATE_OKLAHOMA +
  STATE_NEW_YORK +
  STATE_VERMONT +
  STATE_NORTH_DAKOTA +
  STATE_MINNESOTA +
  STATE_NEVADA +
  # STATE_LAKE_MICHIGAN +
  STATE_MICHIGAN +
  STATE_NEW_MEXICO +
  # STATE_LAKE_ERIE +
  STATE_IOWA +
  STATE_SOUTH_DAKOTA +
  # STATE_PUERTO_RICO +
  # STATE_E_PACIFIC +
  STATE_HAWAII +
  STATE_UTAH +
  STATE_WASHINGTON +
  STATE_COLORADO +
  STATE_RHODE_ISLAND +
  STATE_MAINE +
  # STATE_LAKE_ST_CLAIR +
  STATE_IDAHO +
  # STATE_LAKE_HURON +
  STATE_NEW_HAMPSHIRE +
  # STATE_AMERICAN_SAMOA +
  # STATE_LAKE_ONTARIO +
  # STATE_ST_LAWRENCE_R +
  SOURCE_TRAINED_SPOTTER +
  SOURCE_AMATEUR_RADIO +
  SOURCE_911_CALL_CENTER +
  SOURCE_PUBLIC +
  SOURCE_AWOS +
  SOURCE_LAW_ENFORCEMENT +
  SOURCE_BROADCAST_MEDIA +
  SOURCE_BUOY +
  SOURCE_MESONET +
  SOURCE_SOCIAL_MEDIA +
  SOURCE_COUNTY_OFFICIAL +
  SOURCE_OFFICIAL_NWS_OBSERVATIONS +
  SOURCE_FIRE_DEPARTMENT_RESCUE +
  SOURCE_EMERGENCY_MANAGER +
  SOURCE_ASOS +
  SOURCE_UTILITY_COMPANY +
  SOURCE_COOP_OBSERVER +
  SOURCE_C_MAN_STATION +
  SOURCE_DEPARTMENT_OF_HIGHWAYS +
  SOURCE_NWS_EMPLOYEE +
  SOURCE_NEWSPAPER +
  SOURCE_RAWS +
  SOURCE_OTHER_FEDERAL_AGENCY +
  SOURCE_STATE_OFFICIAL +
  SOURCE_NWS_STORM_SURVEY +
  SOURCE_UNKNOWN +
  SOURCE_LOCAL_OFFICIAL +
  SOURCE_STORM_CHASER +
  SOURCE_COCORAHS +
  SOURCE_TRIBAL_OFFICIAL +
  SOURCE_COAST_GUARD +
  SOURCE_PARK_FOREST_SERVICE +
  SOURCE_AWSS +
  SOURCE_POST_OFFICE +
  SOURCE_WLON +
  SOURCE_INSURANCE_COMPANY +
  SOURCE_RIVER_STREAM_GAGE +
  SOURCE_SHAVE_PROJECT +
  SOURCE_MARINER +
  SOURCE_LIFEGUARD +
  SOURCE_AIRPLANE_PILOT +
  EVENT_TYPE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_THUNDERSTORM_WIND +
  EVENT_TYPE_MARINE_STRONG_WIND +
  EVENT_TYPE_MARINE_HIGH_WIND

## METHOD 0: Classifier on DAMAGE_PROPERTY
damage_cols = c("DAMAGE_PROPERTY_LOW", "DAMAGE_PROPERTY_MED", "DAMAGE_PROPERTY_HIGH")
damage_classes = c("Low", "Medium", "High")

ann.method0 <- function () {
  m0.fit <<- neuralnet(
    dmg_prop_eq,
    data = ann.training,
    threshold = ann.THRESHOLD,
    rep = ann.REPEATS,
    stepmax = ann.STEPMAX,
    hidden = 5
  )
  
  m0.fit.comp <- compute(m0.fit, ann.test[, m0.fit$model.list$variables])
  m0.pred <<- apply(m0.fit.comp$net.result, which.max, MARGIN=1)
  m0.orig <<- apply(ann.test[, damage_cols], which.max, MARGIN=1)
  
  m0.mse <<- mean((m0.pred != m0.orig) ^ 2)
  m0.mse
  
  plot(roc(m0.orig, m0.pred), main=paste("DAMAGE_PROPERTY ROC ", ann.SAMPLE_SIZE))
  plot(m0.fit)
}

## METHOD 1: Classifier on DAMAGE_CROPS
ann.method1 <- function () {
  m1.fit <<- neuralnet(
    dmg_crop_eq,
    data = ann.training,
    threshold = ann.THRESHOLD
    )

  m1.fit.comp <- compute(m1.fit, ann.test[, m1.fit$model.list$variables])
  m1.pred <<- apply(m1.fit.comp$net.result, which.max, MARGIN=1)
  m1.orig <<- apply(ann.test[, damage_cols], which.max, MARGIN=1)
  
  m1.mse <<- mean((m1.pred != m1.orig) ^ 2)
  m1.mse
  
  plot(roc(m1.pred, m1.orig), main=paste("DAMAGE_CROPS ", ann.SAMPLE_SIZE))
  plot(m1.fit)
}

## METHOD 2: Classification on INJURIES
ann.method2 <- function () {
  m2.fit <<- neuralnet(
    inj_eq,
    data = ann.training,
    threshold = ann.THRESHOLD,
    rep = ann.REPEATS
  )
  
  m2.fit.comp <- compute(m2.fit, ann.test[, m2.fit$model.list$variables])
  m2.pred <<- as.numeric(m2.fit.comp$net.result)
  m2.mse <<- mean((m2.pred == ann.test$INJURIES) ^ 2)
  m2.mse
  
  plot(roc(ann.test$INJURIES, m2.pred), main=paste("INJURIES ROC ", ann.SAMPLE_SIZE))
  plot(m2.fit)
}

## METHOD 3: Classification on DEATHS
ann.method3 <- function () {
  m3.fit <<- neuralnet(
    death_eq,
    data = ann.training,
    threshold = ann.THRESHOLD,
    rep = ann.REPEATS
  )
  
  m3.fit.comp <- compute(m3.fit, ann.test[, m3.fit$model.list$variables])
  m3.pred <<- as.numeric(m3.fit.comp$net.result)
  m3.mse <<- mean((m3.pred == ann.test$INJURIES) ^ 2)
  m3.mse
  
  plot(roc(ann.test$DEATHS, m3.pred), main=paste("DEATH ROC ", ann.SAMPLE_SIZE))
  plot(m3.fit)
}

ann.method0()
ann.method1()
ann.method2()
ann.method3()
