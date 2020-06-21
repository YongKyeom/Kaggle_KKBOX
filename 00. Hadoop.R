
################################# Hadoop ##################################

## 1. 라이브러리 불러오기

library(DBI)
library(rJava)
library(RJDBC)

## 2. 데이터 가져오기

# Hive Initiation 실행 
jdbc_hive_init <- function() {
  .jinit()
  for(l in list.files('/usr/sbp/4.1.0.0-1/hadoop/')){ .jaddClassPath(paste("/usr/sbp/4.1.0.0-1/hadoop/",l,sep=""))}
  for(l in list.files('/usr/sbp/4.1.0.0-1/hive/lib/')){ .jaddClassPath(paste("/usr/sbp/4.1.0.0-1/hive/lib/",l,sep=""))}
  drv <- JDBC("org.apache.hive.jdbc.HiveDriver", "/usr/sbp/4.1.0.0-1/hive/lib/hive-jdbc-1.2.1-sbp4.1.0.jar")
  conn <- dbConnect(drv, "jdbc:hive2://192.168.0.65:10000/","admin","admin")
  
  return(conn)
}

# Query 기반 Data를 가져옴 
get_hive_query <- function(query) {
  JDBCConnection = jdbc_hive_init()
  hive_query_result <- dbGetQuery(JDBCConnection,query)
  hive_query_result = data.table(hive_query_result)
  dbDisconnect(JDBCConnection)
  
  return(hive_query_result)
}

###########################################################################

colnames(TEST_V2_LOG)
TRAIN_LOG    <- get_hive_query("SELECT * FROM train_1702_log")
TRAIN_V2_LOG <- get_hive_query("SELECT * FROM train_1703_log")

TEST_LOG     <- get_hive_query("SELECT * FROM test_1703_log")
TEST_V2_LOG  <- get_hive_query("SELECT * FROM test_1704_log")

glimpse(TRAIN_LOG)
TRAIN_LOG <- TRAIN_LOG %>% 
  transmute(msno = train_1702_log.msno,
            avg_ratio_25 = train_1702_log.avg_ratio_25,
            ratio_25_diff = train_1702_log.ratio_25_diff,
            avg_ratio_50 = train_1702_log.avg_ratio_50,
            avg_ratio_75_up = train_1702_log.avg_ratio_75_up,
            avg_unq = train_1702_log.avg_unq,
            avg_secs = train_1702_log.avg_secs)

TRAIN_V2_LOG <- TRAIN_V2_LOG %>% 
  transmute(msno = train_1703_log.msno,
            avg_ratio_25 = train_1703_log.avg_ratio_25,
            ratio_25_diff = train_1703_log.ratio_25_diff,
            avg_ratio_50 = train_1703_log.avg_ratio_50,
            avg_ratio_75_up = train_1703_log.avg_ratio_75_up,
            avg_unq = train_1703_log.avg_unq,
            avg_secs = train_1703_log.avg_secs)

TEST_LOG <- TEST_LOG %>% 
  transmute(msno = test_1703_log.msno,
            avg_ratio_25 = test_1703_log.avg_ratio_25,
            ratio_25_diff = test_1703_log.ratio_25_diff,
            avg_ratio_50 = test_1703_log.avg_ratio_50,
            avg_ratio_75_up = test_1703_log.avg_ratio_75_up,
            avg_unq = test_1703_log.avg_unq,
            avg_secs = test_1703_log.avg_secs)

TEST_V2_LOG <- TEST_V2_LOG %>% 
  transmute(msno = test_1704_log.msno,
            avg_ratio_25 = test_1704_log.avg_ratio_25,
            ratio_25_diff = test_1704_log.ratio_25_diff,
            avg_ratio_50 = test_1704_log.avg_ratio_50,
            avg_ratio_75_up = test_1704_log.avg_ratio_75_up,
            avg_unq = test_1704_log.avg_unq,
            avg_secs = test_1704_log.avg_secs)
apply(TRAIN_LOG, 2, anyNA)

Train_MST    <- Train_MST %>% left_join(TRAIN_LOG, by = 'msno')
Train_v2_MST <- Train_v2_MST %>% left_join(TRAIN_V2_LOG, by = 'msno')
Test_MST     <- Test_MST %>% left_join(TEST_LOG, by = 'msno')
Test_v2_MST  <- Test_v2_MST %>% left_join(TEST_V2_LOG, by = 'msno')

rm(TRAIN_LOG); rm(TRAIN_V2_LOG); rm(TEST_LOG); rm(TEST_V2_LOG);

Train_MST$table <- 'Train_MST'
Train_v2_MST$table <- 'Train_v2_MST'
Test_MST$table <- 'Test_MST'
Test_v2_MST$table <- 'Test_v2_MST'

Train_Total_MST <- rbind(Train_MST, Train_v2_MST)


Train_MST <- Train_MST %>% left_join(TRAIN_LOG[, c('msno', 'avg_secs')], by = 'msno')
Train_v2_MST  <- Train_v2_MST  %>% left_join(TRAIN_V2_LOG[, c('msno', 'avg_secs')], by = 'msno')

Test_MST <- Test_MST %>% left_join(TEST_LOG[, c('msno', 'avg_secs')], by = 'msno')
Test_v2_MST <- Test_v2_MST %>% left_join(TEST_V2_LOG[, c('msno', 'avg_secs')], by = 'msno')

fwrite(Train_MST, "Train_MST.csv")
fwrite(Train_v2_MST, "Train_MST.csv")
fwrite(Test_MST, "Train_MST.csv")
fwrite(Test_v2_MST, "Train_MST.csv")


fwrite(TRAIN_LOG, "TRAIN_LOG.csv")
fwrite(TRAIN_V2_LOG, "TRAIN_V2_LOG.csv")
fwrite(TEST_LOG, "TEST_LOG.csv")
fwrite(TEST_V2_LOG, "TEST_V2_LOG.csv")
rm(TRAIN_LOG); rm(TRAIN_V2_LOG); rm(TEST_LOG); rm(TEST_V2_LOG)


detach('package:RJDBC', unload = TRUE)
detach('package:DBI', unload = TRUE)


TEST_V2_LOG %>% glimpse()
TRAIN_V2_LOG %>% glimpse()

