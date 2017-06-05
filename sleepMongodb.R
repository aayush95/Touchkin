library(rmongodb)
library(data.table)
library(stringr)
setwd("E:/Touchkin/NLP/Sleep")

mongo <- mongo.create(host="127.0.0.1",db="touchkin")
userIds = mongo.distinct(mongo, 'touchkin.message', 'user')
user_id = vector("character", length(userIds))
i = 0
for(user in userIds) {
  i = i+1
  user_id[i] = mongo.oid.to.string(user)
}
#user_id <- c("58fdc64eafee05e30d06c8f3","58f2ab42b77149727a8b2ae4","58f6a04f78710d4415c7b430")
user_count = length(user_id)
# chatSession <- fread("chatsession.csv")
app_user <- fread("E:/Touchkin/NLP/Sleep/user.csv")
msgdata <- list()

if (mongo.is.connected(mongo)){
  for(user_instance in user_id){
    todays.readings.cursor = mongo.find(mongo, "touchkin.message", query = mongo.bson.from.list(list("who" = "user","questionId"="sleep1","user" = mongo.oid.from.string(user_instance))))
    todays.readings.count = mongo.count(mongo, "touchkin.message", query =  mongo.bson.from.list(list("who" = "user","questionId"="sleep1","user" = mongo.oid.from.string(user_instance))))
    
    i=1
    print(user_count)
    user_count <- user_count - 1
    # id = vector("character",todays.readings.count)
    user = vector("character",todays.readings.count)
    date = vector("character",todays.readings.count)
    message = vector("character",todays.readings.count)
    # who = vector("character",todays.readings.count)
    # questionId = vector("character",todays.readings.count)
    # type = vector("character",todays.readings.count)
    
    while (mongo.cursor.next(todays.readings.cursor)){
      
      #current record entry
      cval = mongo.cursor.value(todays.readings.cursor)
      if( !is.null(mongo.oid.to.string(mongo.bson.value(cval,'user')))){
        user[i] = mongo.oid.to.string(mongo.bson.value(cval,'user'))
      }else{
        user[i] <- NA
      }
      
      if( !is.null(as.character(mongo.bson.value(cval,'time')))){
        date[i] <- as.character(mongo.bson.value(cval,'time'))
        
      }else{
        date[i] <- NA
      }
      
      # if( !is.null(as.character(mongo.bson.value(cval,'_id')))){
      #   id[i] <- as.character(mongo.bson.value(cval,'_id'))
        
      # }else{
      #   id[i] <- NA
      # }
      
      if( !is.null(as.character(mongo.bson.value(cval,'message')))){
        message[i] <- as.character(mongo.bson.value(cval,'message'))
        
      }else{
        message[i] <- NA
      }
      
      # if( !is.null(as.character(mongo.bson.value(cval,'who')))){
      #   who[i] <- as.character(mongo.bson.value(cval,'who'))
        
      # }else{
      #   who[i] <- NA
      # }
      
      # if( !is.null(as.character(mongo.bson.value(cval,'questionId')))){
      #   questionId[i] <- as.character(mongo.bson.value(cval,'questionId'))
        
      # }else{
      #   questionId[i] <- NA
      # }
      
      # if( !is.null((mongo.bson.value(cval,'type')))){
      #   type[i] <- (mongo.bson.value(cval,'type'))
        
      # }else{
      #   type[i] <- NA
      # }
      i = i+1
    }
    msg <- as.data.frame(list(user=user,date=date,message=message))
    msgdata <- rbind(msgdata,msg)
  }
  
}

msgdata$date <- as.Date.POSIXct(msgdata$date,tz ='GMT')
#msgdata$date <- cut(as.Date(msgdata$date),breaks = 6)
app_user$`_id` <- gsub("ObjectId","",app_user$`_id`)
app_user$`_id` <- str_replace_all(app_user$`_id`, "[[:punct:]]", "")
msgdata <- merge(msgdata,app_user,by.x = "user",by.y = "_id",all.x = TRUE)

#### Yes, Not_really, FreeText
yes <- data.table(msgdata[(msgdata$message=="Yes"),])
not_really <- data.table(msgdata[(msgdata$message=="Not really"),])
others <- msgdata[!(msgdata$message=="Yes" | msgdata$message=="Not really"),]
freeText <- as.data.frame(others$message)
write.csv(freeText,file = "freeText.txt",row.names = FALSE, col.names=FALSE)

######passing by vader Sentiment
system('python function.py')
freeSenti <- read.table("result_freetext.txt")
#freeSenti <- data.frame(str_split_fixed(freeSenti$V1, "<", 2))
others <- cbind(others,freeSenti)

### Date wise segmentation
yes <- data.table(table(yes$date,yes$app,yes$mobile_os))
colnames(yes)<- c("date","app","mobile_os","Yes")
not_really <- data.table(table(not_really$date,not_really$app,not_really$mobile_os))
colnames(not_really)<- c("date","app","mobile_os","NotReally")

negative <- data.table(table(others$date,others$app,others$mobile_os,others$V1=="NEGATIVE"))
negative$V4 <- NULL
colnames(negative)<- c("date","app","mobile_os","Negative")
positive <- data.table(table(others$date,others$app,others$mobile_os,(others$V1=="POSITiVE" | others$V1=="NEUTRAL")))
positive$V4 <- NULL
colnames(positive)<- c("date","app","mobile_os","Positive")

yes <- yes[!(yes$Yes=="0"),]
not_really <- not_really[!(not_really$NotReally=="0"),]
negative <- negative[!(negative$Negative=="0"),]
positive <- positive[!(positive$Positive=="0"),]


l <- list(yes,not_really,negative,positive)
result <- Reduce(function(...) merge(..., by=c("date","app","mobile_os"), all=T), l)
result <- result[!duplicated(result[,c("date","app","mobile_os")]),]


# ## Yes, not really, positive, negative, neutral
# yes <- data.table(table(yes$date))
# not_really <- data.table(table(not_really$date))
# names(yes)[names(yes) == 'V1'] <- 'date'
# names(yes)[names(yes) == 'N'] <- 'YES'
# names(not_really)[names(not_really) == 'V1'] <- 'date'
# names(not_really)[names(not_really) == 'N'] <- 'NOT Really'


# others$resp <- ifelse(others$V1=="NEGATIVE", 0,1)
# posi_neu <- data.table(table(others$date,others$resp==0))
# posi_neu <- posi_neu[(posi_neu$V2=="FALSE"),]
# neg <- data.table(table(others$date,others$resp==1))
# neg <- neg[(neg$V2=="FALSE"),]

# #z <- data.table(table(others$time))
# yes_no <- merge(yes,not_really,by.x = "date",by.y = "date",all = TRUE)
# posi_neu$V2 <- NULL
# neg$V2 <- NULL
# names(posi_neu)[names(posi_neu) == 'V1'] <- 'date'
# names(posi_neu)[names(posi_neu) == 'N'] <- 'Positive'

# posi_neg <- merge(posi_neu,neg,by.x = "date",by.y = "V1",all=TRUE)
# names(posi_neg)[names(posi_neg) == 'N'] <- 'Negative'
# result <- merge.data.frame(yes_no,posi_neg,by.x = "date",by.y = "date",all.x = TRUE)
# result$date <- as.Date(result$date)

#####Mongodb Write
data = as.matrix(result)
row = vector("character", nrow(data)) 
col = vector("character", ncol(data))
i = 1
j = 1
for(t in row) {
  j = 1
  data_insert = mongo.bson.buffer.create()
  update_data = mongo.bson.buffer.create()
  for(q in col) {
    if (colnames(data)[j] == "date") {
      date = as.POSIXct(data[i])
      mongo.bson.buffer.append(data_insert, "date",  date)
      mongo.bson.buffer.append(update_data, "date",  date)
    } else if(colnames(data)[j] == "app") {
      temp = as.character(data[i, j])
      mongo.bson.buffer.append(data_insert, "app", (temp))
      mongo.bson.buffer.append(update_data, "app", (temp))
    } else if(colnames(data)[j] == "mobile_os") {
      temp = as.character(data[i, j])
      mongo.bson.buffer.append(data_insert, "mobile_os", (temp))
      mongo.bson.buffer.append(update_data, "mobile_os", (temp))
    } else {
      if (is.infinite(as.numeric(data[i, j]))) {
        data[i, j] = 2;
      }
      mongo.bson.buffer.append(data_insert, colnames(data)[j], as.numeric(data[i, j]))
      mongo.bson.buffer.append(update_data, colnames(data)[j], as.numeric(data[i, j]))
    }
    j = j+1
  }
  mongo.bson.buffer.append(data_insert, "type",  'sleep_accuracy')
  mongo.bson.buffer.append(update_data, "type",  'sleep_accuracy')
  created_at = as.POSIXct(Sys.time())
  mongo.bson.buffer.append(data_insert, "createdAt",  created_at)
  mongo.bson.buffer.append(data_insert, "updatedAt",  created_at)
  mongo.bson.buffer.append(update_data, "updatedAt",  created_at)
  data_insert = mongo.bson.from.buffer(data_insert)
  update_data = mongo.bson.from.buffer(update_data)
  
  
  query = mongo.bson.buffer.create()
  mongo.bson.buffer.append(query, "date", mongo.bson.value(data_insert, "date"))
  query_date = mongo.bson.value(data_insert, "date")
  mongo.bson.buffer.append(query, "date", query_date)
  mongo.bson.buffer.append(query, "type",  'sleep_accuracy')
  query = mongo.bson.from.buffer(query)
  mood_result = mongo.find.one(mongo, 'touchkin.dataaccuracy', query = query)
  
  if(!is.null(mood_result)) {
    mongo.update(mongo, "touchkin.dataaccuracy", query, list('$set' = update_data))
  } else {
    mongo.insert(mongo, "touchkin.dataaccuracy", data_insert)
  }
  i = i+1
}
