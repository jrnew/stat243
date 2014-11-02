#!/bin/bash
#--------------------------------------------------
# Problem set 6
#--------------------------------------------------
# Question 4
# Note that we can also use: for year in $(seq 1987 2006)
files_bz=$(ls data | grep bz2)
for file_bz in $files_bz
do
bzip2 -dk data/$file_bz
done

echo "Start: " > procdata/unix-systime.txt
date +"%s" >> procdata/unix-systime.txt
files=$(ls data | grep csv)
for file in $files
do
cat data/$file | egrep ",(SFO|OAK),[[:alpha:]]" | head
done
echo "End: " >> procdata/unix-systime.txt
date +"%s" >> procdata/unix-systime.txt

# Start: 
# 1414709622
# End: 
# 1414710151
