# getdata-011-project
This programming assignment include a function that make the following steps:

1.  load de required librarys
2.  Specify the data in the hard drive
3.  read the features names and save them in variable features
4.  read the train and test datasets into train and test data.table objects
5.  read the train and test activity datasets into trainy and testy 
6.  add to the activity datasets as a new column in train and test called "activity"
7.  merge the train and test data into a large dataset with 10299 rows in a table called tabla
8.  extract the features positions related with means() and std() values
9.  create a new table with only the columns realted to the means, stdevs and activity
10. rename the values of activity with the correspondings activity names i.e.(1 WALKING,2 WALKING_UPSTAIRS,3 WALKING_DOWNSTAIRS, 4 SITTING,5 STANDING and 6 LAYING)
11. rename tableAct names in order to remove "."
12. rename tableAct names in order to replace prefix t by time
13. rename tableAct names in order to replace prefix f by FFT
14. group by Activity
15. average the values of each column grouping by activity
16. write the results in a text file
