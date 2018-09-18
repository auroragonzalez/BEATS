library("RSQLServer")

DB = 'PREBYA_2017_Balsa'


con <- DBI::dbConnect(RSQLServer::SQLServer(), server = '127.0.0.1', port = '1433'
                      , database = DB
                      , properties = list(user = 'aurora', password = 'aurora123'))
tablas = dbListTables(con)


library("DBI")


df = dbGetQuery(con, 'select * from tbl_tree_third_level_3_0')
d1 = dbGetQuery(con, 'select * from tbl_unit_list')

"tbl_charac_value_data_timesignals" # Esta tiene y es muuy grande

peq=c(8,9,10,14,15,20,28,29,36,40,41,42,43)
gran = c(19)
noabre = c(11)

for(i in c(peq,gran,noabre)){
  print(tablas[i])
}

for(i in c(peq,gran)){
  print(tablas[i])
  df = dbGetQuery(con, paste0("select * from ",tablas[i]))
  write.table(head(df),paste0(tablas[i],"-nFil:", dim(df)[1],"-nCol:",dim(df)[2],".csv"), row.names=F, sep=";")
  }




i=10
df40 = dbGetQuery(con, paste0("select * from ",tablas[i]))

for(i in c(peq,gran)){
  print(head(dbGetQuery(con, paste0("select * from ",tablas[i]))))
}




           