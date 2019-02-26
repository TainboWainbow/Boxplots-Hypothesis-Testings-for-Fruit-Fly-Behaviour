setwd("C:/Users/User/Documents/MY_DIRECTORY")
library(readxl)
dat <- read_xlsx("DATA_FILE_NAME.xlsx", col_names=TRUE)
dat$"ON/OFF" <- as.integer(dat$"ON/OFF")

# initializing time data (in seconds)
dat$Seconds <- dat$Seconds-dat$Seconds[1]
###############################################################################################################

# Temporary list to record the indices at which the fly's state changes
# from 0 to 1, or from 1 to 0
temp <- c(1) # adding the first index
flag <- 0
for (i in 2:nrow(dat)){
  if (dat$`ON/OFF`[i] != flag | i == nrow(dat)){ # we add the last index to calculate the duration later
    temp <- c(temp, i)
    flag <- dat$`ON/OFF`[i]
  }
}


# Obtain the duration in seconds between those changes of states
set_of_dur <- c()
for (k in 1:length(temp)){
  if (k == length(temp)){
    break
  }
  time1 <- dat$Seconds[temp[k]]
  time2 <- dat$Seconds[temp[k+1]]
  duration <- time2 - time1
  
  set_of_dur <- c(set_of_dur, duration)
}


# Check if interval between two states is a true interval (if >=10 seconds for state0 and >=5 seconds for state1)
# See the section 'Definitions' in 'key.pdf'
truefalse <- c(TRUE) # adding TRUE first to print first index later
for (t in 1:length(set_of_dur)){
  
  if (t%%2 == 1){ # if ODD-numbered-index, it is state0
    truefalse <- c(truefalse, set_of_dur[t] >= 10)
    
  } else { # if (t%%2 == 0){ # if EVEN-numbered-index, it is state1
    truefalse <- c(truefalse, set_of_dur[t] >= 5)
  }
}
# NOTE1: this time we don't need to add the last index
# (since if last interval is FALSE, there's no need to calculate the duration of this interval)
# NOTE2: 'truefalse' is the same length as 'temp'
# NOTE3: each entry 'i' in 'truefalse' represents if time interval between 
# 'i' and 'i-1' is a true state (if >=10 seconds for state0 and >=5 seconds for state1)


# From 'truefalse', get the indices where changes to true states occur and record in 'truefalse2'
# Then, obtain the indices of the original data, 'dat', where the changes of true states occur
# See 'Assumption 2' in 'key.pdf'
truefalse2 <- which(truefalse == TRUE)
final_indx <- c(1)
oddeven <- "ODD" #first, we look for an ODD numbered index 
for (j in 2:length(truefalse2)){ # start from the second index
  
  if (oddeven == "ODD" & truefalse2[j]%%2 == 1){
    final_indx <- c(final_indx, truefalse2[j] - 1)
    # for the next true state, we're looking for the next even-number-indexed state
    oddeven <- "EVEN"
    
  } else if (oddeven == "EVEN" & truefalse2[j]%%2 == 0){
    final_indx <- c(final_indx, truefalse2[j] - 1)
    # for the next true state, we're looking for the next odd-number-indexed state
    oddeven <- "ODD"
  }
}
timept <- temp[c(final_indx)]


# Finally, obtain the duration (in seconds) of the oscillating intervals of the true states
# throughout the 30-minute observation of the fly
final_dur <- c()
for (e in 1:length(timept)){
  if (e == length(timept)){
    break
  }
  final_dur <- c(final_dur, dat$Seconds[timept[e+1]] - dat$Seconds[timept[e]])
}
# NOTE: the first entry of 'final_dur' is the duration of the first resting period;
# the second entry is the duration of the first eating period;
# the third entry is the duration the second resting period; ...
