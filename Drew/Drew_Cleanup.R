library(readr)
rm(list=ls())

storm_data_raw <- read_csv("StormEvents_details-ftp_v1.0_d2017_c20180418.csv")

normalize <- function (x) { (x - min(x)) / (max(x) - min(x)) }

# Note, added in 29 (MAGNITUDE_TYPE) b/c it's mostly NA
# Also added in 41 and 44
bad_indexes = c(1, 4, 7, 8, 10, 11, 14, 15, 16, 17, 18, 20, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51)
storm_data <- storm_data_raw[, -bad_indexes]

# Convert to integer
month_map <- seq(1, 12)
names(month_map) = c(
  "January", "February", "March", "April", "May", 
  "June", "July", "August", "September", "October", 
  "November", "December"
)
change_months <- function (b) {
  month_map[b]
}
storm_data$MONTH_NAME <- normalize(as.numeric(lapply(storm_data$MONTH_NAME, change_months)))

f <- function(a) {
  if (a == "B") {
    1000000000
  } else if (a == "M") {
    1000000
  } else if (a == "K") {
    1000
  } else {
    0
  }
}
storm_data$DAMAGE_PROPERTY[is.na(storm_data$DAMAGE_PROPERTY)] <- "0.00K"
d <- substr(storm_data$DAMAGE_PROPERTY, 1, nchar(storm_data$DAMAGE_PROPERTY) - 1)
c <- substr(storm_data$DAMAGE_PROPERTY, nchar(storm_data$DAMAGE_PROPERTY), nchar(storm_data$DAMAGE_PROPERTY))
c <- lapply(c, f)
storm_data$DAMAGE_PROPERTY <- as.numeric(d) * as.numeric(c)

storm_data$DAMAGE_CROPS[is.na(storm_data$DAMAGE_CROPS)] <- "0.00K"
d <- substr(storm_data$DAMAGE_CROPS, 1, nchar(storm_data$DAMAGE_CROPS) - 1)
c <- substr(storm_data$DAMAGE_CROPS, nchar(storm_data$DAMAGE_CROPS), nchar(storm_data$DAMAGE_CROPS))
c <- lapply(c, f)
storm_data$DAMAGE_CROPS <- as.numeric(d) * as.numeric(c)

# storm_data[, "DAMAGE"] <- storm_data$DAMAGE_CROPS + storm_data$DAMAGE_PROPERTY

# Handle NA values
storm_data$DEATHS_DIRECT[is.na(storm_data$DEATHS_DIRECT)] <- 0
storm_data$DEATHS_INDIRECT[is.na(storm_data$DEATHS_INDIRECT)] <- 0
storm_data$INJURIES_DIRECT[is.na(storm_data$INJURIES_DIRECT)] <- 0
storm_data$INJURIES_INDIRECT[is.na(storm_data$INJURIES_INDIRECT)] <- 0
storm_data$MAGNITUDE[is.na(storm_data$MAGNITUDE)] <- 0

# Convert to factor
special_chars <- "[^[:alnum:]]"

storm_data$STATE <- as.factor(storm_data$STATE)
storm_data$SOURCE <- as.factor(storm_data$SOURCE)
storm_data$EVENT_TYPE <- as.factor(storm_data$EVENT_TYPE)

for (j in unique(storm_data$STATE)) {
  name <- paste("STATE", toupper(gsub(special_chars, "_", j)), sep="_")
  storm_data[, name] <- as.numeric(storm_data$STATE == j)
}

for (j in unique(storm_data$SOURCE)) {
  name <- paste("SOURCE", toupper(gsub(special_chars, "_", j)), sep="_")
  storm_data[, name] <- as.numeric(storm_data$SOURCE == j)
}

for (j in unique(storm_data$EVENT_TYPE)) {
  name <- paste("EVENT_TYPE", toupper(gsub(special_chars, "_", j)), sep="_")
  storm_data[, name] <- as.numeric(storm_data$EVENT_TYPE == j)
}

# Normalize
storm_data$MAGNITUDE <- normalize(storm_data$MAGNITUDE)
storm_data$DAMAGE_PROPERTY <- normalize(storm_data$DAMAGE_PROPERTY)
storm_data$DAMAGE_CROPS <- normalize(storm_data$DAMAGE_CROPS)
# stomr_data$DAMAGE <- normalize(storm_data$DAMAGE)
storm_data$DEATHS_DIRECT <- normalize(storm_data$DEATHS_DIRECT)
storm_data$DEATHS_INDIRECT <- normalize(storm_data$DEATHS_INDIRECT)
storm_data$INJURIES_DIRECT <- normalize(storm_data$INJURIES_DIRECT)
storm_data$INJURIES_INDIRECT <- normalize(storm_data$INJURIES_INDIRECT)

# Remove remaining NAs
storm_data <- na.omit(storm_data)
nrow(storm_data)
col(storm_data)