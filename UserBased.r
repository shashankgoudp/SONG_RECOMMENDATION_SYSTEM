#The mapping of all the unique User Ids
userName.Map <- vector(mode="list")

#The input Training data file
input <- read.table("song_data_train.txt")

#Extract all the User ids from the training dataset and store them as characters
user.Id <- data.frame(lapply(input[1], as.character), stringsAsFactors=FALSE)

#Store unique User ids  
userName.Map <- unique(user.Id[,1])

#Store the count of the number of unique users
user_length <- length(userName.Map)

#Builds a data frame of each user with its listening history
train  <- list()

mylist <- NULL
#For each user build a mapping to his listening history 
for(i in 1:user_length)
{
  t <- subset(input[,2:3],input[,1]==UserName.Map[i])
  t2 <- data.frame(lapply(t[1], as.character), stringsAsFactors=FALSE)
  mylist <- list(t2, t[2])
  train[i] <- list(data.frame(mylist[1],mylist[2]))
}

### Song User mapping ###
#read the training data from file to extract the unique song ids
test.Visible <- read.table("song_test_visible.txt")

# Convert the song ids into Characters
song_list = data.frame(lapply(test.Visible[2], as.character), stringsAsFactors=FALSE)

#getting unique songs ids from song_list
song_id = unique(song_list[,1])


# references from unique song to list of users listening to it and their counts
song_user_list=list()


#for all unique songs that is length(song_id)
date()

#Build a mapping of all songs and the users who listen to it 
for(i in 1:length(song_id)) 
{
  temp_list = subset(test.Visible[,1], test.Visible[,2] == song_id[i])
  temp_df = sapply(temp_list, as.character, stringsAsFactors=FALSE)
  song_user_list[i] = list(temp_df)
}

# Store the values of alpha and gamma as Global variables
assign("alpha", 0.3, envir = .GlobalEnv)
assign("gamma", 2, envir = .GlobalEnv)

#Function to calculate the Cosine similarity 
User.Based.Similarity <- function(trainFrame, testFrame){
  SimilarityDistance <- 0 
  train.count <- 0
  test.count <- 0
  
  testSongVect <- testFrame$V2
  trainSongVect <- trainFrame$V2
  
  inter <- intersect(testSongVect,trainSongVect)

  #Calculate the number of common songs
  common <- length(inter)
  print(common)
  
  if (common == 0) {
    SimilarityDistance <- 0 
  }
  else{
    train.count <- length(testSongVect)
    test.count <- length(trainSongVect)
    
    temp1 <- train.count^alpha
    temp2 <- test.count^(1-alpha)
    
    SimilarityDistance <- common /(temp1*temp2)
  }
  return(SimilarityDistance)
}
--------------------------------
#Score Caluclation
# Enter test Value
  # Use the test user which we created to test KNN. So train1 is created in KNN script
test.Value <- train1[[9]]
##################

#Calculate the weighted score for each song
score_list <- list()
for(i in 1:length(song_id)) 
{
  score <- 0
  for(j in 1:length(song_user_list[[i]])) 
  {
      k <- match(song_user_list[[i]][j],UserName.Map)
        score <- score + (User.Based.Similarity(train[[k]],test.Value)^gamma)    
  }
  score_list[i] <- score
}

#Sort the songs according to the score and return the list as the set of recommended songs
x <- c(1:length(song_id))
y <- unlist(score_list)
song.DataFrame <- data.frame(x,y)
song.DataFrame <- song.DataFrame[order(song.DataFrame$y, decreasing = TRUE), ]
y <- test.Value
x <- y[,1]
Final.SongList <- song.DataFrame[!song.DataFrame$x %in% match(x,song_id),]
