# Spark
import numpy as np
import time
from operator import add
lines = sc.textFile("/data/airline").repartition(96)

def subset(line):
  vals = line.split(',')
  return(vals[0] != 'Year' and vals[15] != 'NA' and
  float(vals[15]) >= -30 and float(vals[15]) <= 720)

# Subset flights data
lines_filtered = lines.filter(subset).collect()
lines_filtered.take(5) # Check

# Extract flights departing from SFO or OAK
print "%f " %  time.time()
lines_bayarea = lines_filtered.filter(lambda line: any(airport in line.split(',')[16] 
  for airport in ('SFO', 'OAK')))
lines_bayarea.take(5)
print "%f " %  time.time()

# Find mean/median departure delay by airport
print "%f " %  time.time()
def depdelay(line):
  vals = line.split(',')
  if vals[0] == 'Year':
    return('0', 0)
  else:
    return(vals[16], float(vals[15]))
  
def getmean(input):
  if len(input) == 2:
    if len(input[1]) > 0:
      m = np.mean(input[1])
      return((input[0], m))
    else:
      return((input[0], -999))
  else:
    return((input[0], -9999))
  
def getmedian(input):
  if len(input) == 2:
    if len(input[1]) > 0:
      m = np.median(input[1])
      return((input[0], m))
    else:
      return((input[0], -999))
  else:
    return((input[0], -9999))
  
lines_depdelay = lines.map(lines_depdelay)
depdelay_mean = lines_depdelay.groupByKey().map(getmean).collect()
depdelay_median = lines_depdelay.groupByKey().map(getmedian).collect()
print "%f " %  time.time()
#======================================================================
# Question 3(c)
print "%f " %  time.time()
# Find number of departing flights by airport and merge data
def count(line):
  vals = line.split(',')
  return(vals[16], 1)

num_departing_flights = lines_filtered.map(count).reduceByKey(add).collect()
lines_merged = lines_filtered.join(num_departing_flights).collect()

# Extract the 20 observations with the longest departure delays from
# airports with at least 1 million flights
def getlongestdelays(line):
  vals = line.split(',')
  return(int(vals[29]) > 100000 and float(vals[15]) > 700)

lines_delays_temp = lines_filtered.filter(getlongestdelays)
lines_longest_delays = lines_delays_temp.sortByKey(ascending = False, 
  keyfunc = lambda line: float(line.split(',')[15])).take(20)
print "%f " %  time.time()
