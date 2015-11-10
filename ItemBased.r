cat("\014")


library(data.table)
date()

#----------------------CREATING TRAINIGN DATA------------------------------------------
#--------------------------------------------------------------------------------------

#read training data from file
training_data2 = read.table("song_data_train.txt")
#training_data = training_data2[1:10000,1:3]

training_data = training_data2

# data has 3 columns 1- user id 2- soing id 3- count
song_list = data.frame(lapply(training_data[2], as.character), stringsAsFactors=FALSE)

#getting unique songs ids from song_list
song_id = unique(song_list[,1])


# references from unique song to list of users listening to it and their counts
song_user_list=list()


#for all unique songs that is length(song_id)

date()


for(i in 1:length(song_id)) 
{
  temp_list = subset(training_data[,1:3], training_data[,2] == song_id[i])
  temp_list = temp_list[,-c(2)]
  
  temp_df = data.frame(lapply(temp_list[1], as.character), stringsAsFactors=FALSE)
  temp_list = list(temp_df, temp_list[2])
  song_user_list[i] = list(data.frame(temp_list[1], temp_list[2]))
  
  #song_user_list[i] = list(temp_list)
}

date()
#associating song names with song_id
names(song_user_list) = c(song_id)

#----------------------END OF CREATION ------------------------------------------------
#--------------------------------------------------------------------------------------




#----------------------CREATING TESTING DATA-------------------------------------------
#--------------------------------------------------------------------------------------


#testing_data = training_data2[1:1000,1:3]
testing_data <- read.table("song_test_visible.txt")

#testing_data <- read.table("test_final_submit.txt")

#reading user ids into list 
user_list = data.frame(lapply(testing_data[1], as.character), stringsAsFactors=FALSE)

#getting unique user ids from user_list
user_id = unique(user_list[,1])


# references from unique user to list of songs he listens to and their counts
user_song_list=list()


#for all unique users that is length(user_id)
date()
for(i in 1:length(user_id)) 
{
  temp_list = subset(testing_data[,2:3], testing_data[,1] == user_id[i])
  temp_df = data.frame(lapply(temp_list[1], as.character), stringsAsFactors=FALSE)
  temp_list = list(temp_df, temp_list[2])
  user_song_list[i] = list(data.frame(temp_list[1], temp_list[2]))
}

#associating song names with song_id
names(user_song_list) = c(user_id)
date()
#date()



#----------------------END OF CREATION ------------------------------------------------
#--------------------------------------------------------------------------------------




#----------------------COSINE SIMILARITY CALCULATOR -----------------------------------
#--------------------------------------------------------------------------------------



cosine_similarity_calculator <- function(song)
{
  
  #print("4")
  
  
  
  #list of users listening to this particular song and their counts
  
  
  
  row1 <- song_user_list[[song]]
  
  #t1 list of songs
  #t2 list of their cosine similarities
  
  t1 = NULL
  t2 = NULL
  
  #data frame of similarities with respect to "song"
  similarity_df = NULL
  
  for(i1 in 1:length(song_user_list))
  {
    
    #print("5")
    
    
    cosine_similarity = 0.0
    
    #if self song then assign similarity = 
    if(names(song_user_list[i1]) == song)
    {
      t1 = append(t1, names(song_user_list[i1]))
      t2 = append(t2, 1.0)
      
      #move to next iteration in training data
      next
    }
    
    temp_row = song_user_list[[i1]]

    df <- data.frame(row1[,2],UID=as.character(row1$V1))
    df2 <- data.frame(temp_row[,2],UID=as.character(temp_row$V1))
    
    DT1 <- data.table(df,  key="UID")
    DT2 <- data.table(df2, key="UID")
    List_UID_Count=DT1[DT2,nomatch=0]    
    product= 0.0
    List_UID_Count_2_Denom = 0
    List_UID_Count_3_Denom = 0
    for(i in 1:nrow(List_UID_Count))
    {
    #  print("7")
      
      
      product=product + List_UID_Count$row1...2.[i]*List_UID_Count$temp_row...2.[i]
      List_UID_Count_2_Denom = List_UID_Count_2_Denom + List_UID_Count$row1...2.[i]*List_UID_Count$row1...2.[i]
      List_UID_Count_3_Denom = List_UID_Count_3_Denom + List_UID_Count$temp_row...2.[i]*List_UID_Count$temp_row...2.[i]
    }
    
   # print("6")
    
   pow1 <-  List_UID_Count_2_Denom^0.7
   pow2 <-  List_UID_Count_3_Denom^0.3
   
    cosine_similarity = product/(pow1*pow2)
    cosine_similarity = as.numeric(cosine_similarity)
    
    # to catch numeric(0) case and append zero in list
    if(length(cosine_similarity)==0)
    {
      cosine_similarity=0.0
    }
    
    
    t1 = append(t1, names(song_user_list[i1]))
    t2 = append(t2, cosine_similarity)
  #  print("8")
    
    
  }
  
  #merging list of unique songs their similarity 
  #with respect particular song passes to function 

  similarity_df = data.frame(t1,t2)
  
  return(similarity_df)
}




#----------------------END OF COSINE FUNCTION------------------------------------------
#--------------------------------------------------------------------------------------





#----------------------SIMILARITY CALCULATOR FUNCTION----------------------------------
#--------------------------------------------------------------------------------------

b_global= NULL

similarity_calculator <- function(number_test_users)
{
  
  #BOUND = NUMBER OF SONGS OF THE TEST USER TO CONTINUE
  
  
  
  #considering only 1 test user
  
  #number_test_users=1
  for(i in 1:number_test_users)   
  {
    
    i=number_test_users
    #print("1")
    
    bound = (length(user_song_list[[i]]$V2))/2
    
  #  if( (length(user_song_list[[i]]$V2)) == 1 )
  #   bound=1
    
    bound <-floor(bound)

    b_global <- bound
    
    song_song_map = NULL
    song_song_map = list()
    
    cat("BOUND value = ",bound)
      
    for(j in 1:bound)
    {
     # print("2")
      
      sim_temp = NULL
      sim_temp = cosine_similarity_calculator(user_song_list[[i]]$V2[j])
      song_song_map[j]=list(sim_temp)
      
    }
    
    #associating names of songs with son_song_map
    names(song_song_map)=c(user_song_list[[1]]$V2[1:bound])
    
    #print("3")
    
    #cat("suggested songs for test user = ",names(test_t[i]))
    return(song_song_map)
  
   # song_recommender(song_song_map,i,bound)
    
  }
}


#----------------------END OF SIMILARITY CALCULATOR------------------------------------
#--------------------------------------------------------------------------------------





#----------------------SONG RECOMMENDER FUNCTION---------------------------------------
#--------------------------------------------------------------------------------------


r_list = list()
r_count = 1

song_recommender=function(sng_sng_map,test_user_number,bound)
{
  
  #assigning map to variable
  x=sng_sng_map
  
  # temp_x column 2 of row 1 of x consisting of similarities 
  # temp_x2 column 1 of row 1 of x consisting of song names
  
  temp_x=x[[1]]$t2
  temp_x2=x[[1]]$t1
  
  
  temp_x2=as.character(temp_x2)
  
  count_sum= list()
  
  for(i in 2:length(x))
  {    
    temp_x = temp_x + (x[[i]]$t2)
  }
  
  names(temp_x)=c(temp_x2)
  #t1 = sorted similarity list
  # t2 number of songs test user is already listening to to not suggest him the same ones

  t1 = sort(temp_x,decreasing = TRUE) 
  t2 = user_song_list[[test_user_number]]$V2[1:bound]
  
  for(i in 1:length(t1))
  {
    t4=names(t1[i])
    t5=length(intersect(t4,t2))
  
    if(t5!=0)
    {
      
      t1[i]= -1
    }
    
  }
  
  t1=sort(t1,decreasing = TRUE)

  #r_list[r_count]= t1
  print("RECOMMENDED SONGS FOR USER")
  print(names(user_song_list[test_user_number]))
  print(t1[1:10])
  
}




#----------------------END OF SONG RECOMMENDER-----------------------------------------
#--------------------------------------------------------------------------------------


cat("\014")
date()
run1 = similarity_calculator(3)
date()

# the bound value will be printed on console .
# pass that as the 3rd parameter to song _recommender

date()
song_recommender(run1,3,6)
date()
user_song_list[[3]]


#sort(run1[1]$t2,decreasing=TRUE)
#summary(run1[1]$t1)
#run1[1]$t2[1]
#write(run1[1],"output.txt") 
