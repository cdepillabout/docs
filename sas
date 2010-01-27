
# running sas
# -nonews doesn't show the news output at the top of the file
# -pagesize specifies the amount of info that will be printed on one page
sas -nonews -pagesize 32767 apples.sas
# This will produce two files, apples.log and apples.lst.
# apples.log will contain the log of the sas session.
# apples.lst will contain the output of the SAS commands in apples.sas.

# INPUT FORMATS
#
#
# Explain this input:
input shipment_number 5. +1 shipment_date mmddyy6. +1 
	type_item $15. @30 price 6.2 @40 quantity 5.;

- shipment_number 5. -- a five character numeric with no decimals
- +1 -- tells the input pointer to skip ahead one character (one position)
- shipment_date mmddyy6. -- a Six character date field 2 digit for month, day and year
- +1 -- skip a character
- type_item $15. -- a character string of length 15, the "$" indicates characters
- @30 -- move the input pointer to position 30
- price 6.2 -- a six character numeric field with 2 decimals
- @40 -- move to column 40
- quantity 5. -- a five character numeric with no decimals

# in an input statement, a "$" indicates it is a character value



# run sas interactively, but without the GUI
sas -nodms

1?   data; 
2?		p=probnorm(3.25); 
3?	 run;
4?   proc print; 
5?	 run;
6?   endsas;


# for temporary datasets, SAS puts them in the "Work" lib, so
# a dataset "Demographic" would become the Work.Demographic dataset.


# SAS uses blanks to represent missing character values and
# periods to represent missing numeric values.


# The SAS term /observations/ is analogous to rows in a table.
# The SAS term /variables/ is analogous to columns in a table.


# When you write a DATA statement such as "data test;" SAS creates
# a temporary data set called "Test".  When you close your SAS application,
# this data set disappears.  SAS data sets actually have two-part names in the 
# form "libref.data-set-name".  The libref tells SAS where to store or retrieve
# the data set.  When you use just a data set name, and not a libref, SAS uses
# the default "Work" data set, which is created every time you run sas.  So, 
# a simple statement like "data test;" is equivalent to "data work.test;".
#
# All that is required to make your datasets permanent is to create your own
# libref using a LIBNAME statement and use that libref in the two-level SAS 
# data set name.


# SAS data sets contain two parts, a /descriptor/ portion and a /data/ portion.
# You can examine the descriptor portion using PROC CONTENTS.
