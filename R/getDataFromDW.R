library(dplyr)
require("RPostgreSQL")

drv <- dbDriver('PostgreSQL')  
db <- 'DW_validation'  
host_db <- 'localhost'  
db_port <- '5432'  
db_user <- 'postgres'  
db_password <- 'postgres'


conn <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)

fact_table = dbReadTable(conn, 'fact_iot') 
fact_table$fk_activities= as.factor(fact_table$fk_activities)
fact_table$fk_date= as.factor(fact_table$fk_date)
fact_table$fk_sensor= as.factor(fact_table$fk_sensor)
fact_table$fk_time= as.factor(fact_table$fk_time)
fact_table$fk_elderly= as.factor(fact_table$fk_elderly)

dimension_activities = dbReadTable(conn, 'Dim_activities') 
dimension_date = dbReadTable(conn, 'Dim_date') 
dimension_elderly = dbReadTable(conn, 'Dim_elderly')
dimension_tag = dbReadTable(conn, 'Dim_tag') 
dimension_time = dbReadTable(conn, 'Dim_time')

#summary(fact_table)
#summary(dimension_activities)
#fact_table <- fact_table[,1:10]

#tail(data)
#tail(dimension_activities)

dimension_activities$activities_id=as.factor(dimension_activities$activities_id)
dimension_date$date_pk=as.factor(dimension_date$date_pk)
dimension_elderly$id_elderly=as.factor(dimension_elderly$id_elderly)
dimension_tag$id_sensor=as.factor(dimension_tag$id_sensor)
dimension_time$id_times=as.factor(dimension_time$id_times)
data<-merge(x = fact_table, y = dimension_activities, by.x = "fk_activities", by.y = "activities_id")

data<-merge(x = data, y = dimension_date, by.x = "fk_date", by.y = "date_pk")
data<-merge(x = data, y = dimension_elderly, by.x = "fk_elderly", by.y = "id_elderly")
data<-merge(x = data, y = dimension_tag, by.x = "fk_sensor", by.y = "id_sensor")
data<-merge(x = data, y = dimension_time, by.x = "fk_time", by.y = "id_times")

#tail(data)
#summary(data)
data <- data[,6:40]
data$id_minute<-NULL
data$id_millisecond<-NULL
data$id_second<-NULL
data$id_trimester<-NULL
data$id_semster<-NULL
data$id_month<-NULL
data$duration<-NULL

data$timestemp<- as.numeric(data$timestemp)
data$activities<- as.factor(data$activities)
data$year<-as.factor(data$year)
data$month<-as.factor(data$month)
data$day<-as.factor(data$day)
data$day_month_year<-as.factor(data$day_month_year)
data$semster<-as.factor(data$semster)
data$week<-as.factor(data$week)
data$date<-as.factor(data$date)
data$dayofyear<-as.factor(data$dayofyear)
data$lib_month<-as.factor(data$lib_month)
data$lib_day<-as.factor(data$lib_day)
data$day_month_char<-as.factor(data$day_month_char)
data$quarter<-as.factor(data$quarter)
data$elderly_ref<-as.factor(data$elderly_ref)
data$elderly_seq<-as.factor(data$elderly_seq)
data$sequence<-as.factor(data$sequence)
data$sensor_localisation<-as.factor(data$sensor_localisation)
data$sesor_ref<-as.factor(data$sesor_ref)
data$temps_jours<-as.factor(data$temps_jours)
data$hour<-as.factor(data$hour)
data$minute<-as.factor(data$minute)
data$second<-as.factor(data$second)
data$millisecond<-as.factor(data$millisecond)
data$hour.second.millisecond<-as.factor(data$hour.second.millisecond)

summary(data)
levels(data$activities)