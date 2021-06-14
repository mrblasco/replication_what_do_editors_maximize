****** DO FILE for running all tables *******

set more off

*** PLEASE SET PATH ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* quietly run
run "`folderpath'/OA_Tab2.do"
run "`folderpath'/OA_Tab3.do"
run "`folderpath'/OA_Tab5.do"
run "`folderpath'/OA_Tab5_fig.do"
run "`folderpath'/OA_Tab8.do"
run "`folderpath'/OA_Tab9.do"
run "`folderpath'/OA_Tab10.do"
run "`folderpath'/OA_Tab12.do"
run "`folderpath'/OA_Tab13.do"
** longer runtime
run "`folderpath'/OA_Tab11.do"
