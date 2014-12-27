cd ../ebin
erl -name  Secret_account@192.168.5.47   -pa "ebin" -mnesia directory "database"   -s  manager_misc start
pause

