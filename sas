
# running sas
# -nonews doesn't show the news output at the top of the file
# -pagesize specifies the amount of info that will be printed on one page
sas -nonews -pagesize 32767 apples.sas
# This will produce two files, apples.log and apples.lst.
# apples.log will contain the log of the sas session.
# apples.lst will contain the output of the SAS commands in apples.sas.

