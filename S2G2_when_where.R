# Title: S/G2 Transition Information Gathering
# Author: Sam Altshuler
# Date: 08/10/2022


## Purpose ----
# Purpose of script:
#   The purpose of this script is to read through all of the Cell Cognition data and 
# store the location and time information of when/where cells transition from S phase
# to G2. 

# TRIAL VERSION TO TEST OUT FEASIBILITY. NOT TAKING THE DATA FROM THE PROPER SPOT AND
# USING SMALLER SAMPLE FILES
## Libraries Used ----

library(tidyverse)

## Functions ----
when_where <- function(df_in) {
  # Take the initial dataframe and only grab what is needed (secondary channel information)
  s2g2 <- data.frame("objID" = df_in$objID, "frame" = df_in$frame, "X" = df_in$centerX, 'Y' = df_in$centerY,
                     "phase_name" = df_in$className.1, "phase_num" = df_in$classLabel.1,
                     "mitotic_name" = df_in$className, "mitotic_num" = df_in$classLabel)
  
  # Initialize the vectors where the information will get stored
  xx <- vector(length = 0)
  yy <- vector(length = 0)
  fr <- vector(length = 0)
  
  # the tracking information has to have at least 4 time points (any less and not a real
  # S --> G2 transition)
  if (length(s2g2$objID) <=4) {
    # return an empty data frame if the cell is tracked for less than 4 time points
    return(df_out <- data.frame("X" = xx, "Y" = yy, "Time" = fr))
  }
  
  # loop through the phase numbers. If it is in G2, check if the previous 2 were in S phase
  # and if the next is also in G2
  for (i in 3:(length(s2g2$objID)-2)){
    # print(i)
    j <- i + 1
    k <- i - 1
    l <- i - 2
    # Conditions for what can be in the two frames pre G2
    pre_i <- c(3,4)
    pre_k <- c(2,3,4)
    # The three frames that we are 
    post <- c(i, i +1, i+2)
    # if there are three G2 classifications in a row
    if (sum(s2g2$phase_num[post] == 5) == 2){
      
      if (s2g2$phase_num[k] %in% pre_i && s2g2$phase_num[l] %in% pre_k){
        # Store the data if it matches the criteria listed above
        xx <- append(xx, s2g2$X[i])
        yy <- append(yy, s2g2$Y[i])
        fr <- append(fr, s2g2$frame[i])
      }
    } else {
      
      next
      
    }
  }
  # put the data into a data frame
  df_out <- data.frame("X" = xx, "Y" = yy, "Time" = fr)
  
  # return the data frame with all of the data for that tracking file
  return(df_out)
}

## Import the Data ----
# fpath <- "Y:/bromodomain/Sam (server)/CellCognition/220525/Analysis_4_RKO_full analysis_W50-W52/analyzed/00050_01/statistics/full/"
fpath <- choose.dir()
files <- list.files(fpath)

# Read in the .txt file. You want to skip the first two rows as they are describing if it is 
# primary or secondary channel. You want to make sure that it knows that the third row is the 
# column headers. The .txt file is really a tab separate values (tsv) file, so specify the 
# values are separated by a tab when being read in.
cell1 <- read.table("TEST_P00050_01__T00001__O0212__B01.txt",  sep = "\t", skip = 2, header = TRUE)

xx <- vector(length = 0)
yy <- vector(length = 0)
fr <- vector(length = 0)
# Initialize an empty data frame
tot_s2g2 <- data.frame("X" = xx, "Y" = yy, "Time" = fr)

for (n in 1:length(files)){
  # Read in the .txt file using the file path since the file is not currently in your working directory.
  # You want to skip the first two rows as they are describing if it is primary or secondary channel. 
  # You want to make sure that it knows that the third row is the column headers. The .txt file is 
  # really a tab separate values (tsv) file, so specify the values are separated by a tab when being read in.
  
  cell1 <- read.table(paste0(fpath, files[n]),  sep = "\t", skip = 2, header = TRUE)
  # call the function that looks through to file to find the instance of S -> G2 transition
  tot <- when_where(cell1)
  # add those values to the existing data frame
  tot_s2g2 <- rbind(tot_s2g2, tot)
  # quick test so I can visually see the loop is working and where it might stop working
  # TAKE OUT BEFORE USING FOR REAL, PRINTING SLOWS EVERYTHING DOWN
  if (n%%5 ==0){
    print(n)
  }
}
tot_s2g2_unique <- unique(tot_s2g2)
total_S2G2 <- tot_s2g2_unique[order(tot_s2g2_unique$Time),]

# will need to change the file name each time since it's a different well/position
write.csv(total_S2G2,
          file = "Y:/bromodomain/Sam (server)/CellCognition/220525/Analysis_4_RKO_full analysis_W50-W52/W00050_01_s2g2_2.csv",
          row.names = FALSE)
