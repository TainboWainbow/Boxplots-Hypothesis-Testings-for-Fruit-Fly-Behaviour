if ("readxl" %in% installed.packages()){
  install.packages("readxl")
}
library(readxl)

setwd("C:/Users/User/Documents/FOLDER_NAME")
dat <- read_xlsx("DATA_FILE_NAME.xlsx", col_names=TRUE)
dat$"ON/OFF" <- as.integer(dat$"ON/OFF")

# initializing time data (in seconds)
dat$Seconds <- dat$Seconds-dat$Seconds[1]
###############################################################################################################

# Obtain a list recording the indices of 'dat' at which the fly's state changes
# from 0 to 1, or from 1 to 0
state_change <- c()
flag <- 0 # the fly's initial state is always resting (state0)
for (i in 1:length(dat$"ON/OFF")){
  if (dat$"ON/OFF"[i] != flag){
    state_change <- c(state_change, i)
    flag <- dat$"ON/OFF"[i]
  }
}
# NOTE: not adding the last index by Assumption 2 in 'key.pdf'


if (sum(state_change)==0){
  print ("There is no change in state for this data")
}


# Obtain interval duration (unit in seconds) between those changes of states
interval_dur <- c()
for (k in 1:length(state_change)){
  if (k == length(state_change)){
    break
  }
  time1 <- dat$Seconds[state_change[k]]
  time2 <- dat$Seconds[state_change[k+1]]
  duration <- time2 - time1
  
  interval_dur <- c(interval_dur, duration)
}


# Check if interval between two states is >=10 seconds for state0 and >=5 seconds for state1
# See definitions (3) and (5) under the section 'Definitions' in 'key.pdf'
time_enough <- c()
for (t in 1:length(interval_dur)){
  
  if (t%%2 == 1){ # if ODD-numbered-index, it is state1
    time_enough <- c(time_enough, interval_dur[t] >= 5)
    
  } else { # if (t%%2 == 0){ # if EVEN-numbered-index, it is state0
    time_enough <- c(time_enough, interval_dur[t] >= 10)
  }
}

# NOTE1: this time we don't need to add the last index
# (since if last interval is FALSE, there's no need to calculate the duration of this interval)
# NOTE2: 'time_enough' is the same length as 'state_change'
# NOTE3: each entry 'i' in 'time_enough' represents if time interval between 
# 'i' and 'i-1' is a true state (if >=10 seconds for state0 and >=5 seconds for state1)


# From 'time_enough', get the indices of 'state_change' where true states changes and 
# record in 'truestates'
truestates <- which(time_enough == TRUE)

# Obtain the indices of 'state_change'  where the changes of true state intervals occur
# See 'Assumption 2' in 'key.pdf'

# first, we look for an EVEN numbered index (state1) since the initial state of the fly is always 
# resting (state0)
remainder <- truestates[1]%%2
if (remainder == 0){
  oddeven <- "EVEN"
} else if (remainder == 1){
  oddeven <- "ODD"
}

trueintervals <- c()
for (j in 1:length(truestates)){ # start from the second index
  if (oddeven == "ODD" & truestates[j]%%2 == 1){
    trueintervals <- c(trueintervals, truestates[j])
    # for the next true state, we're looking for the next even-number-indexed state
    oddeven <- "EVEN"
    
  } else if (oddeven == "EVEN" & truestates[j]%%2 == 0){
    trueintervals <- c(trueintervals, truestates[j])
    # for the next true state, we're looking for the next odd-number-indexed state
    oddeven <- "ODD"
  }
}


# Obtain the indices of data, 'dat'
get_indices <- state_change[c(trueintervals)]


# Finally, obtain the duration (in seconds) of the oscillating true intervals of 
# the true states throughout the 30-minute observation of the fly
trueintervals_dur <- c()
for (e in 1:length(get_indices)){
  if (e == length(get_indices)){
    break
  }
  trueintervals_dur <- c(trueintervals_dur, dat$Seconds[get_indices[e+1]] - dat$Seconds[get_indices[e]])
}
# NOTE: the first entry of 'trueintervals_dur' is the duration of the first resting period;
# the second entry is the duration of the first eating period;
# the third entry is the duration the second resting period; ...


# Producing a set of results
remainder <- truestates[1]%%2
if (remainder == 0){
  final_resting <- trueintervals_dur[seq(from=1, to=length(trueintervals_dur), by=2)]
  final_feeding <- trueintervals_dur[seq(from=2, to=length(trueintervals_dur), by=2)]
} else if (remainder == 1){
  final_feeding <- trueintervals_dur[seq(from=1, to=length(trueintervals_dur), by=2)]
  final_resting <- trueintervals_dur[seq(from=2, to=length(trueintervals_dur), by=2)]
}

