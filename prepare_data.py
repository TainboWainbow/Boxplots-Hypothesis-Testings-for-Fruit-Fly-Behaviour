import xlrd
import os
os.chdir("C:\Python\Scripts")
data = xlrd.open_workbook("1. fe_stv mated_stv virgin.xlsx")
sheet1 = data.sheet_by_name("Mated_Fe_strv(Jul20)")
yee = data.sheet_by_index(0)
lastrow = 166698
time0 = 

# creating sets to contain the timestamps
tempset = []
set_of_time = []
set_of_dur = []

# Adding the first entry of time
tempset.append(yee.cell_value(rowx=2, colx=2))

# the initial state of the fly is 0 (resting)
flag = 0
# this is the starting row (first row contains the row names)
row = 2

# We want to record the time when the state of the fly changes (either 0 to 1 or 1 to 0)
while row < (lastrow + 1):
  # time in seconds from the initial time (0 seconds)
  sec = yee.cell_value(rowx = row, colx = 2)
  # is the fly on the pad at time 'sec'?
  onoff = yee.cell_value(rowx = row, colx = 0)
  
  if (onoff != flag) or (row == maxrow):
    tempset.append(sec)
    
    if onoff == 1:
      flag = 1
      reqtime = 10
      q = 0
    if onoff == 0:
      flag = 0
      reqtime = 5
      q = 1
   
    if len(tempset) == 2:
      duration = tempset[1] - tempset[0]
    
      if duration >= reqtime:
        set_of_time.append((q, tempset[0]-54980, tempset[1]-54980))
        set_of_dur.append((q, duration))
      
      # if the state changes, then initialize the set
      tempset = []
      tempset.append(sec)
  
  row = row + 1
 

print set_of_dur
print set_of_time
  
