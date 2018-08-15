
# note: in RStudio (collapse/expand all), Alt-O, Alt-Shift-O
# choose Edit -> Folding -> Collapse All / Expand All to view functions

# ---read_glow_dataset <- function() {


# read_glow_dataset ------------------------------------------------------------

read_glow_dataset <- function() {
  library(here)

  glow_data_file <- here("data", "glow500.csv")
  glow <- read.csv(glow_data_file)
  
  glow$PRIORFRAC <- factor(glow$PRIORFRAC, levels=c(0,1), labels=c("No","Yes"))
  glow$PREMENO <- factor(glow$PREMENO, levels=c(0,1), labels=c("No","Yes"))
  glow$MOMFRAC <- factor(glow$MOMFRAC, levels=c(0,1), labels=c("No","Yes"))
  glow$ARMASSIST <- factor(glow$ARMASSIST, levels=c(0,1), labels=c("No","Yes"))
  glow$SMOKE <- factor(glow$SMOKE, levels=c(0,1), labels=c("No","Yes"))
  glow$RATERISK <- factor(glow$RATERISK, levels=c(1,2,3), labels=c("Less","Same","Greater"))
  glow$FRACTURE <- factor(glow$FRACTURE, levels=c(0,1), labels=c("No","Yes"))

  return(glow)
}


# create_balanced_dataset_from_glow ---------------------------------------

create_balanced_dataset_from_glow <- function(glow_ds) {
  ## use SMOTE to create a balanced dataset 
  
  library(DMwR)
  
  set.seed(2018)
  # now use SMOTE to create a balanced dataset, perc.over=100, perc.under=200 
  newGlow <- SMOTE(FRACTURE ~ ., glow, perc.over = 100)

  return(newGlow)  
}

# save_SMOTEd_dataset -----------------------------------------------------

save_SMOTEd_dataset <- function(newGlow, filename) {
  # save SMOTEd dataset
  glow_smoted_data_file <- here("data", filename)
  write.csv(newGlow, file = glow_smoted_data_file)
}


# create_fracture_proportion ----------------------------------------------

create_fracture_proportion <- function(df) {
  fracture<-c("No","Yes")
  tab1<-prop.table(table(df$FRACTURE))
  df<-data.frame(fracture=names(tab1), proportion=as.numeric(tab1))
  
  return(df)
}

