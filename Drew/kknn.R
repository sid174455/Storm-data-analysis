# detach("package:caret", unload=TRUE)
# vars <- ls()
# namespace <- grep("kknn.", ls())
# rm(list=vars[namespace])

library(kknn)

# Copy over storm data so we don't mess with anyone else's code
# If you get errors at this part, run, Project_Data_Preprocess.R first!
kknn.sdr <- storm_data_relevant
kknn.sda <- storm_data_all

## Helper functions
standardScale <- function (x) { (x - mean(x)) / sd(x) }
normalize <- function (x) { (x - min(x)) / (max(x) - min(x)) }
special_chars <- "[^[:alnum:]]"

## Clean Data

# Replace some special chars
kknn.sdr$SOURCE <- gsub(special_chars, "_", kknn.sdr$SOURCE)
kknn.sdr$EVENT_TYPE <- gsub(special_chars, "_", kknn.sdr$EVENT_TYPE)

# Factorize
kknn.sdr$DAMAGE_CROPS <- factor(kknn.sdr$DAMAGE_CROPS, c("Low", "Medium", "High"))
kknn.sdr$DAMAGE_PROPERTY <- factor(kknn.sdr$DAMAGE_PROPERTY, c("Low", "Medium", "High"))
kknn.sdr$SOURCE <- factor(kknn.sdr$SOURCE)
kknn.sdr$INJURIES <- factor(kknn.sdr$INJURIES, c("Yes", "No"))
kknn.sdr$DEATHS <- factor(kknn.sdr$DEATHS, c("Yes", "No"))

# Remove old columns
kknn.sdr$MAGNITUDE <- NULL
kknn.sdr$MAGNITUDE_TYPE <- NULL

# Normalize
kknn.sdr$BEGIN_DAY <- normalize(kknn.sdr$BEGIN_DAY)
kknn.sdr$END_DAY <- normalize(kknn.sdr$END_DAY)

## Sample Data

index <- seq(1, nrow(kknn.sdr), by=5)
kknn.test <- kknn.sdr[index, ]
kknn.training <- kknn.sdr[-index, ]
# kknn.training <- kknn.training[sample(nrow(kknn.training), 10000), ]

# kknn.training <- data.frame(kknn.training)
# kknn.test <- data.frame(kknn.test)

## TRAIN MODELS

kknn.model0 <<- kknn(
  DAMAGE_PROPERTY ~ BEGIN_DAY + END_DAY + STATE + MONTH_NAME + EVENT_TYPE + DAMAGE_CROPS + SOURCE + INJURIES + DEATHS,
  kknn.training,
  kknn.test,
  distance = 1,
  kernel = "rectangular"
)

CM <- table(kknn.test[, "DAMAGE_PROPERTY"], kknn.model0$fitted.values)
CM

mse <- mean((kknn.model0$fitted.values != kknn.test[, "DAMAGE_PROPERTY"]) ^ 2)
mse
CM <- table(kknn.test[, "DAMAGE_PROPERTY"], kknn.model0$fitted.values)
CM

kknn.model1 <<- kknn(
  formula = DAMAGE_CROPS ~ BEGIN_DAY + END_DAY + STATE + MONTH_NAME + EVENT_TYPE + DAMAGE_PROPERTY + SOURCE + INJURIES + DEATHS,
  kknn.training,
  kknn.test,
  distance = 1,
  kernel = "rectangular"
)

mse <- mean((kknn.model1$fitted.values != kknn.test[, "DAMAGE_CROPS"]) ^ 2)
mse
CM <- table(kknn.test[, "DAMAGE_CROPS"], kknn.model1$fitted.values)
CM

kknn.model2 <<- kknn(
  INJURIES ~ BEGIN_DAY + END_DAY + STATE + MONTH_NAME + EVENT_TYPE + DAMAGE_PROPERTY + DAMAGE_CROPS  + SOURCE + DEATHS,
  kknn.training,
  kknn.test,
  distance = 1,
  kernel = "rectangular"
)

mse <- mean((kknn.model2$fitted.values != kknn.test[, "INJURIES"]) ^ 2)
mse
CM <- table(kknn.test[, "INJURIES"], kknn.model2$fitted.values)
CM

kknn.model3 <<- kknn(
  DEATHS ~ BEGIN_DAY + END_DAY + STATE + MONTH_NAME + EVENT_TYPE + DAMAGE_PROPERTY + DAMAGE_CROPS  + SOURCE + INJURIES,
  kknn.training,
  kknn.test,
  distance = 1,
  kernel = "rectangular"
)

mse <- mean((kknn.model3$fitted.values != kknn.test[, "DEATHS"]) ^ 2)
mse
CM <- table(kknn.test[, "DEATHS"], kknn.model3$fitted.values)
CM
