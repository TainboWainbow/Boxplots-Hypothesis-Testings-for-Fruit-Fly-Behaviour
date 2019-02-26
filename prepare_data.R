setwd("C:/Users/User/Documents/MY_DIRECTORY")
library(readxl)
dat <- read_xlsx("DATA_FILE_NAME.xlsx", col_names=TRUE)
dat$"ON/OFF" <- as.integer(dat$"ON/OFF")

# initializing time data (in seconds)
dat$Seconds <- dat$Seconds-dat$Seconds[1]
###############################################################################################################

# temporary list to record the indices at which the fly's state changes
# from 0 to 1, or from 1 to 0
temp<-c(1) # adding the first index
flag <- 0
for (i in 2:nrow(dat)){
  if (dat$`ON/OFF`[i] != flag | i==nrow(dat)){ # we add the last index to calculate the duration later
    temp <- c(temp, i)
    flag <- dat$`ON/OFF`[i]
  }
}


# obtain the duration in seconds between the change of states
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


# check if interval between the change of state is >=10 for resting and for eating >=5 #
truefalse <- c(TRUE) # adding TRUE first to print first index later
for (t in 1:length(set_of_dur)){
  if (t%%2==1){ # if ODD number index, resting(0)
    truefalse <- c(truefalse, set_of_dur[t] >= 10)
  } else { # if (t%%2==0){ # if EVEN number index, eating(1)
    truefalse <- c(truefalse, set_of_dur[t] >= 5)
  }
}
# NOTE1: this time we don't need to add the last index (if last interval FALSE, we don't need 
# to calculate the duration of this interval)
# NOTE2: 'truefalse' is the same length as 'temp'
# NOTE3: each entry 'i' of the data 'truefalse' represents if the interval between 
# time of 'i' and 'i-1' is >=10 seconds (if state 0) or >=5 seconds (if state 1)
# See the section 'Definitions' in 'key.pdf' for more details


# From 'truefalse', get the indices where the duration of time satisfy the requirement (>=10 or >=5 seconds)
# Then, obtain the indices where the state truly changes - see the section 'Assumptions' in 'key.pdf'
truefalse2 <- which(truefalse==TRUE)
last_indx <- c(1)
oddeven <- "ODD" #first, we look for an ODD numbered index 
for (j in 2:length(truefalse2)){ # start from the second index (always include the first index)
  
  if (oddeven=="ODD" & truefalse2[j]%%2==1){
    last_indx <- c(last_indx, truefalse2[j]-1)
    # for the next index, we're looking for an even 
    oddeven <- "EVEN"
    
  } else if (oddeven=="EVEN" & truefalse2[j]%%2==0){
    last_indx <- c(last_indx, truefalse2[j]-1)
    # for the next index, we're looking for an odd
    oddeven <- "ODD"
  }
}
timept <- temp[c(last_indx)]


# finally, obtain the duration (in seconds) of the oscillating intervals of the true
# resting and eating states throughout the 30-minute observation of the fly
final_dur <- c()
for (e in 1:length(timept)){
  if (e==length(timept)){
    break
  }
  final_dur <- c(final_dur, dat$Seconds[timept[e+1]]-dat$Seconds[timept[e]])
}
# NOTE: the first entry of 'final_dur' is the duration of the first resting period;
# the second entry is the duration of the first eating period;
# the third entry is the duration the second resting period; ...
