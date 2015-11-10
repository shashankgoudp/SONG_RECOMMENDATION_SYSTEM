train_users <- vector(mode="list", length=18)

#----------------------CREATING TRAINIGN DATA------------------------------------------
#--------------------------------------------------------------------------------------

# Reading the traind dataset in a temp variable
temp <-read.table("song_data_train.txt")
data_train <- data.frame(lapply(temp[1], as.character), stringsAsFactors=FALSE)

train_users <- unique(data_train[,1])


train <- list()
mylist<-NULL
user_length <- length(foo)

# For each user build a mapping to his listening history 
# It is like a list of data frames where 
# Each element in a list is a user along with all the songs it listens and the count of number of times that particular song is played.

for(i in 1:user_length)
{
  t <- subset(temp[,2:3],temp[,1]==train_users[i])
  t2 <- data.frame(lapply(t[1], as.character), stringsAsFactors=FALSE)
  mylist <- list(t2, t[2])
  train[i] <- list(data.frame(mylist[1],mylist[2]))
 }

Sys.time()

#Function to calculate the Cosine similarity for a test user with all the train users
cosineDistance <- function(trainFrame, testFrame){
  CosineD <- 0 
  product <- 0
  
  index <- sapply(trainFrame, is.factor)
  trainFrame[index] <- lapply(trainFrame[index], as.character)
  
  index <- sapply(testFrame, is.factor)
  testFrame[index] <- lapply(testFrame[index], as.character)
  
  testSongVect <- testFrame[[1]]
  trainSongVect <- trainFrame[[1]]
  
  inter <- intersect(testSongVect,trainSongVect)
  if (length(inter) == 0) {
    CosineD <- 0 
  }
  else{
    for (ind in seq_along(inter)){
      product <- product + trainFrame[trainFrame$V2==inter[ind],2]*testFrame[testFrame$V2==inter[ind],2]   }
    trainEuclid <- sqrt(sum(trainFrame[,2]^2))
    testEuclid <- sqrt(sum(testFrame[,2]^2))
    CosineD <- product / (trainEuclid*testEuclid)
  }
  return(CosineD)
}


#----------------------Reading test visible DataSet ------------------------------------------------
#--------------------------------------------------------------------------------------

test_users <- vector(mode="list", length=18)
temp_test <-read.table("song_test_visible.txt")
data_test <- data.frame(lapply(temp_test[1], as.character), stringsAsFactors=FALSE)

test_users <- unique(data_test[,1])


train1 <- list()
mylist1<-NULL
user_length_test <- length(test_users)


for(i in 1:user_length_test)
{
  t <- subset(temp_test[,2:3],temp_test[,1]==test_users[i])
  t2 <- data.frame(lapply(t[1], as.character), stringsAsFactors=FALSE)
  mylist1 <- list(t2, t[2])
  train1[i] <- list(data.frame(mylist1[1],mylist1[2]))
}

# Testing the model against any test user from the test visible data
test_user <- train1[[8]]
v <- NULL
for(i in 1:user_length)
{
  k <- cosineDistance(train[[i]],test_user)
  v <- c(v,k)
}



# Finding top N nearest neighbors for KNN

n<- 50
top <- function(x, n){
  result <- numeric()
  for(i in 1:n){
    j <- which.max(x)
    result[i] <- j
    x[j] <- -Inf
  }
  result
}

z<- NULL
if(max(v)>0)
{
  z <- top(v)
}

z


# Make the data frame of songs and count of top 3 users
df <- NULL
for(i in 1:50)
{
  df<- rbind(df,train[[z[i]]])
}
#Add the counts of common songs
songs_reco <- aggregate(V3~V2,data=df,FUN=sum)

#sort based on count and remove the existing songs in test user
sorted_songs <- songs_reco[order(songs_reco$V3, decreasing = TRUE), ]

sorted_list <- sorted_songs$V2

test_list <- train1[[8]]$V2

# Removing the songs in visible data of the test user
reco_list <- setdiff(sorted_list,test_list)