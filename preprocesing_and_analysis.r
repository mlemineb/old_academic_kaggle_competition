############## I :  ENVIRONEMENT's PREPARATION   ##############

# I put the packages in a vector 
packages<-c("devtools","tidyr","urltools","stringr","dplyr","readr","data.table","tm","networkD3",
            "SnowballC","Matrix","lda","LDAvis","servr","RCurl")


# I check if theses packages are installed yet 
diffp<-setdiff(packages, rownames(installed.packages()))
# if yes i do nothing if no i install them
if (length(diffp) > 0) {install.packages(diffp)}
# i do the same thing to load them 
diffloaded<-setdiff(packages, loadedNamespaces())
if (length(diffloaded) > 0) {lapply(diffloaded, require, character.only = TRUE)} 

devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)



############## STEP 1 :  DATA IMPORTATION   ##############


setwd("C:/Users/21204541/Downloads")
if (!file.exists("data")) {dir.create("data")}
setwd("C:/Users/21204541/Downloads/data")


#Set your browsing links 
loginurl = "https://www.kaggle.com/account/login"
fileUrl_train_info<-"https://www.kaggle.com/c/recipient-prediction/download/training_info_sid.csv"
fileUrl_train_set<-"https://www.kaggle.com/c/recipient-prediction/download/training_set_sid.csv"
fileUrl_test_info<-"https://www.kaggle.com/c/recipient-prediction/download/test_info_sid.csv"
fileUrl_test_set<-"https://www.kaggle.com/c/recipient-prediction/download/test_set_sid.csv"
all_files<-list(fileUrl_train_info,fileUrl_train_set,fileUrl_test_info,fileUrl_test_set)

library(RCurl)
#Set user account data and agent
pars=list(
  UserName="m.beydia@gmail.com",
  Password="mohamed99"
)
agent="Mozilla/5.0" #or whatever 

#Set RCurl pars
curl = getCurlHandle()
curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
#Also if you do not need to read the cookies. 
#curlSetOpt(  cookiejar="", useragent = agent, followlocation = TRUE, curl=curl)

#Post login form
welcome=postForm(loginurl, .params = pars, curl=curl)

bdown=function(url, file, curl){
  f = CFILE(file, mode="wb")
  curlPerform(url = url, writedata = f@ref, noprogress=FALSE, curl = curl)
  close(f)
}

ret = lapply(1:4,function(i){bdown(all_files[[i]], substring(all_files[[i]],56),curl)})

rm(curl)
gc()

train_info<-read.csv("training_info_sid.csv",header = F)
train_set<-read.csv("training_set_sid.csv",header = F)

test_info<-read.csv("test_info_sid.csv",header = F)
test_set<-read.csv("test_set_sid.csv",header = F)



############## STEP 3 :  DATA Preparation   ##############

### TRAIN ####


names(train_info)<-c("Mail_ID","Date","Content","Recipients")
names(train_set)<-c("Sender","Mail_ID")


my_funct<-function(i,df){
  subdf=subset(df,rownames(df)==i)
  Sender_list=strsplit(as.character(subdf$Mail_ID)," ")
  Mail_ID=as.numeric(unlist(Sender_list))
  Sender=subdf$Sender
  mydf=cbind.data.frame(Sender,Mail_ID)
  return(mydf)
}


require(plyr)
mydf = ldply(1:dim(train_set)[1], my_funct, df=train_set, .progress = 'text')

Training<-join(mydf, train_info,type = "left")
# rearenge order of dataset

Training<-Training[,c(2,1,3:5)]

### TRAIN ####

names(test_info)<-c("Mail_ID","Date","Content")
names(test_set)<-c("Sender","Mail_ID")

mydftest=ldply(1:dim(test_set)[1], my_funct, df=test_set, .progress = 'text')

Test<-join(mydftest, test_info,type = "left")
# rearenge order of dataset
Test<-Test[,c(2,1,3:4)]



install.packages("printr")
library(printr)


############## II :  BASIC ANALYSIS   ##############


# Here we want to count how many how many emails has been sent or received per person.

# Reshape the datset in  order to have for each row a mail send with one recipient
my_funct<-function(i,df){
  subdf=subset(df,Mail_ID==i)
  recipients_list=strsplit(as.character(subdf$Recipients)," ")
  recipient=(unlist(recipients_list))
  mydftest=cbind.data.frame(subdf[1:4],recipient)
  return(mydftest)
}


Full_mail = ldply(min(Training$Mail_ID):max(Training$Mail_ID), my_funct,df=Training, .progress = 'text')



##  Recieved Mails
library(dplyr)

email_recevied_count= Full_mail %>%
  group_by(recipient) %>%
  summarize(count = n())

email_recevied_count<-as.data.frame(email_recevied_count)[order(-email_recevied_count$count),]
# Barplot
library(ggplot2)

p<-ggplot(data=email_recevied_count[1:15,], aes(x=recipient, y=count)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits = email_recevied_count[1:15,]$Sender)
p + coord_flip() 


##  Sent Mails

email_sent_count= Full_mail %>%
  group_by(Sender) %>%
  summarize(count = n())

email_sent_count<-as.data.frame(email_sent_count)[order(-email_sent_count$count),]
# Barplot
p<-ggplot(data=email_sent_count[1:15,], aes(x=Sender, y=count)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits = email_sent_count[1:15,]$Sender)
p + coord_flip() 


## Let's make a prediction based only on frenquence
Top10_receivers<-list(as.character(email_recevied_count[1:10,]$recipient))
rv=character()
for (i in 1:10){rv=paste(rv,unlist(Top10_receivers)[i])} 


v=as.data.frame(cbind(Test$Mail_ID[1],rv))

pred_freq<-as.data.frame(cbind(Test$Mail_ID,rv))
names(pred_freq)<-c("Id","Recipients")

write.csv2(pred_freq,file="C:/Users/mlemi/Desktop/kaggle/Webmining/result/soumission_freq.csv",row.names = F)


image

##############  III :  NETWORK ANALYSIS   ##############

library(dplyr)
emailInteractionsLinks = Full_mail %>%
  group_by(Sender, recipient) %>%
  summarize(count = n())

# let's consider only the case we have more than one interaction
emailInteractionsLinks = emailInteractionsLinks[emailInteractionsLinks$count > 1, ]
# initialize target and source
emailInteractionsLinks$source = -1
emailInteractionsLinks$target = -1

emailInteractionsLinks$Sender<-as.character(emailInteractionsLinks$Sender)
emailInteractionsLinks$recipient<-as.character(emailInteractionsLinks$recipient)

uniqueInteractionPersons = unique(c(emailInteractionsLinks$Sender, emailInteractionsLinks$recipient))
emailInteractionsNodes <- data.frame(name = uniqueInteractionPersons, stringsAsFactors=FALSE) 
emailInteractionsNodes$group = 0
emailInteractionsNodes$size = 0

head(emailInteractionsNodes)


for (i in 1:nrow(emailInteractionsLinks)) {
  senderIndex = match(emailInteractionsLinks[i, "Sender"], emailInteractionsNodes$name)
  recipientIndex = match(emailInteractionsLinks[i, "recipient"], emailInteractionsNodes$name)
  
  emailInteractionsLinks[i, ]$source = senderIndex - 1
  emailInteractionsLinks[i, ]$target = recipientIndex - 1
  
  emailInteractionsNodes[senderIndex, ]$size = emailInteractionsNodes[senderIndex, ]$size + emailInteractionsLinks[i, ]$count
  emailInteractionsNodes[recipientIndex, ]$size = emailInteractionsNodes[recipientIndex, ]$size + emailInteractionsLinks[i, ]$count
}

emailInteractionsNodes$group = round(log2(emailInteractionsNodes$size + 1))
emailInteractionsNodes$size = round(sqrt(emailInteractionsNodes$size))

library(networkD3)

forceNetwork(Links = as.data.frame(emailInteractionsLinks), Nodes = emailInteractionsNodes,
             Source = "source", Target = "target",
             Value = "count", NodeID = "name",
             Group = "group", width = 800, height = 600,
             fontSize = 10, Nodesize = "size",
             opacity = 0.9, zoom = TRUE)



library(igraph)

net <- graph_from_data_frame(d=emailInteractionsLinks, vertices=emailInteractionsNodes, directed=T) 
net <- simplify(net, remove.multiple = F, remove.loops = T) 

net.sym <- as.undirected(net, mode= "collapse",
                         
                         edge.attr.comb=list(weight="sum", "ignore"))



# community clustering : fast greedy algorithm
cfg <- cluster_fast_greedy(as.undirected(net))
dendPlot(cfg, mode="hclust")
str(cfg)

com_members<-membership(cfg)
plot(cfg,net)

community<-as.data.frame(cbind(V(net)$name,cfg$membership))
names(community)<-c("Sender","Community_grp")
# Check
setdiff(unique(Full_mail$Sender),unique(community$Sender))


Train_with_comu<-join(Full_mail, community,type = "left")
Test_with_comu<-join(Test, community,type = "left")



#Create new column with weekdays, and column weekend

Train_with_comu$weekday = weekdays(as.Date(Train_with_comu$Date))
Train_with_comu$weekend = ifelse(Train_with_comu$weekday %in% c('samedi','dimanche'),1,0)

Test_with_comu$weekday = weekdays(as.Date(Test_with_comu$Date))
Test_with_comu$weekend = ifelse(Test_with_comu$weekday %in% c('samedi','dimanche'),1,0)



# SAVE Dataframes
con<-file('Test_with_comu',encoding="UTF-8")

write.csv(Train_with_comu,"Train_with_comu.csv",row.names = F,fileEncoding = "UTF-8")
write.csv(Test_with_comu,"Test_with_comu.csv")

a<-Train_with_comu[c(1,2,5,8,6)]
names(a)[2]<-"send_mail"
names(a)[1]<-"id"
names(a)[3]<-"rece_mail"

a<-a[order(a$id),]
dupl_email<-dupl_email[order(dupl_email$id),]

dupl_mail2<-join(dupl_email,a,type = "left")

dupl_mail2<-dupl_mail2 %>% distinct

dupl_mail2<-dupl_mail2[c(1:4,6:8,5)]

dupl_mail2$Community_grp<-as.integer(dupl_mail2$Community_grp)
dupl_mail2$Community_grp<-paste0("community",numbers2words(dupl_mail2$Community_grp))

write.csv(dupl_mail2,"dupl_mail2.csv",row.names = F,fileEncoding = "UTF-8")

class(dupl_email$send_mail)
class(a$send_mail)


setdiff(unique(dupl_email$send_mail),unique(a$send_mail))


names(community)[1]<-"send_mail"
test_email2<-join(test_email,community,type = "left")

test_email2$Community_grp<-as.integer(test_email2$Community_grp)
test_email2$Community_grp<-paste0("community",numbers2words(test_email2$Community_grp))
write.csv(test_email2,"test_email2.csv",row.names = F,fileEncoding = "UTF-8")



assortativity_nominal(net, V(net)$media.type, directed=F)

############## IV :  TEXT MINING ANALYSIS   ##############



## removing /n symbols 
library(stringr)
Train_with_comu$Content<-as.character(Train_with_comu$Content)
Train_with_comu$Cleaned_Content<-str_replace_all(Train_with_comu$Content, "[\r\n]" , " ")
Test_with_comu$Cleaned_Content<-str_replace_all(Test_with_comu$Content, "[\r\n]" , " ")

## Text Mining 
library(tm)

mycorpus<-Corpus(VectorSource(Train_with_comu$Cleaned_Content))

sclean<- tm_map(mycorpus, removePunctuation)
sclean <- tm_map(sclean, content_transformer(tolower))
sclean <- tm_map(sclean, removeWords, stopwords("english"))
sclean <- tm_map(sclean, removeNumbers)
sclean <- tm_map(sclean, stripWhitespace) 

mycorpustest<-Corpus(VectorSource(Test_with_comu$Cleaned_Content))

testsclean<- tm_map(mycorpustest, removePunctuation)
testsclean <- tm_map(testsclean, content_transformer(tolower))
testsclean <- tm_map(testsclean, removeWords, stopwords("english"))
testsclean <- tm_map(testsclean, removeNumbers)
testsclean <- tm_map(testsclean, stripWhitespace) 


mydtm <- DocumentTermMatrix(sclean, control =list(weighing=weightTfIdf, minWordLength=3, minDocFreq=5))
inspect(mydtm)
mydtm_test <- DocumentTermMatrix(testsclean, control =list(weighing=weightTfIdf, minWordLength=3, minDocFreq=5))
inspect(mydtm_test)


## transform into dataframe
library(data.table)
mydtm.dataframe <- as.data.table(as.matrix(mydtm), stringsAsFactors=False)
mydtm_test.dataframe <- as.data.table(as.matrix(mydtm_test), stringsAsFactors=False)


## add recipient and other features (date, community_grp, weekend)
finalTrain<-cbind(Train_with_comu,mydtm.dataframe)
finalTest<-cbind(Test_with_comu,mydtm_test.dataframe)


rm(list=(setdiff(ls(),c("Train_with_comu","Test_with_comu","mydtm","mydtm_test"))))


############## V :  Prediction   ##############

library("bigmemory")
library("caTools")
library("pander")
library("devtools")
install_github("davpinto/fastknn")

library("h2o")

# Target and features
senderid<-cbind.data.frame(Sender=unique(finalTrain$Sender),id=1:124)
senderid<-cbind.data.frame(Sender=unique(Train_with_comu$Sender),id=1:124)

finalTrain<-join(senderid,finalTrain,type = "left")
Train_with_comu<-join(senderid,Train_with_comu,type = "left")


class(finalTrain$id)
x <- as.big.matrix(finalTrain[,-c(1,3,4,5,6,7,9,11)])
x<-as.matrix(Train_with_comu[c(2,9,7)])
y <- as.factor(finalTrain$recipient)
# Split data for training and test
rm(list=(setdiff(ls(),c("x","y"))))
set.seed(111)
tr.idx <- which(caTools::sample.split(Y = y, SplitRatio = 0.5))
xtr <- x[tr.idx,]
xte <- x[-tr.idx,]
ytr <- y[tr.idx]
yte <- y[-tr.idx]


# Find the best k using 5-fold CV
set.seed(222)
cv.out <- fastknn::fastknnCV(xtr, ytr, k = c(5, 10,15), eval.metric = "logloss")

# Fit KNN with the best k
knn.time <- system.time({
  knn.out <- fastknn::fastknn(xtr, ytr, xte, k = cv.out$best_k)        
})[3]
knn.error <- fastknn::classLoss(yte, knn.out$class, eval.metric = "overall_error")
knn.logloss <- fastknn::classLoss(yte, prob = knn.out$prob, eval.metric = "logloss")

# Show results
pander::pander(cv.out$cv_table)
print(paste("Time:", knn.time))
print(paste("Accuracy:", 1 - knn.error))
print(paste("LogLoss:", knn.logloss))

check<-as.data.frame(knn.out$prob)

unique(test_set_sid$V1)
unique(finalTrain$Sender)
setdiff(unique(test_set_sid$V1),unique(finalTrain$Sender))


unique(test_info_sid$V3)


set.seed(333)
new.data <- fastknn::knnExtract(xtr, ytr, xte)

library("h2o")

# Instantiate H2O cluster
h2o.init(max_mem_size = '8G', nthreads = 8)
h2o.removeAll()       
# h2o.shutdown(prompt = FALSE)

names(finalTrain)[8]<-"date_weekend"
names(finalTrain)[5]<-"recipient_mail"
names(finalTrain)[2]<-"Sender_mail"


train.h2o<-as.h2o(finalTrain[-c(4,9)])


final.split = h2o.splitFrame(data = train.h2o,
                                ratios = 0.85)
train = final.split[[1]]
test = final.split[[2]]

# Import original features
train.hex <- as.h2o(cbind.data.frame(xtr, target = ytr), destination_frame = "train_hex")
test.hex <- as.h2o(cbind.data.frame(xte, target = yte), destination_frame = "test_hex")

# Import KNN features
train.hex.new <- as.h2o(cbind.data.frame(new.data$new.tr, target = ytr), destination_frame = "train_hex_new")
test.hex.new <- as.h2o(cbind.data.frame(new.data$new.te, target = yte), destination_frame = "test_hex_new")



colnames(train[-c(1,4,6)])[1:10]


# Train glm
glm.model1 <- h2o.glm(x = colnames(train)[-c(1,4,6)], y = "recipient_mail", family = "multinomial", 
                      training_frame = train, validation_frame = test)



# Evaluate test data
pred1 <- h2o.predict(glm.model1, h2o.cbind(test.hex[, colnames(xtr)], test.hex.new))
pred1 <- as.data.frame(pred1)$predict
glm.error <- fastknn:::classLoss(yte, pred1, eval.metric = "overall_error")
glm.logloss <- h2o.logloss(glm.model1@model$validation_metrics)




saveRDS(topic_dat, "topic_dat.rds")
