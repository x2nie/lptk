#!/bin/bash
./buildr.bat app_sql.dpr
./buildr.bat app_dic.dpr
./buildr.bat app_todo.dpr
#fpc -O2p3 -Sd2h -XX prjlptk
cp app_sql /usr/bin/app_sql
cp app_dic /usr/bin/app_dic
cp app_todo ~/bin/app_todo
