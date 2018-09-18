library("RSQLServer")

DB = 'PREBYA_2017_Balsa'


con <- DBI::dbConnect(RSQLServer::SQLServer(), server = '127.0.0.1', port = '1433'
                      , database = DB
                      , properties = list(user = 'aurora', password = 'aurora123'))
tablas = dbListTables(con)


library("DBI")

t3rdLevel = dbGetQuery(con, 'select * from tbl_tree_third_level_3_0')
tmpoint = dbGetQuery(con, 'select * from tbl_measurepoint')
tCharacVal =  dbGetQuery(con, 'select * from tbl_charac_value_data')

tmpoint$third_level
t3rdLevel$idx
