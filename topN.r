#Top N

# Read the data in temp variable
temp_top <- read.table("song_data_train.txt")
t_top <- temp_top[,2]
t2_top <- sapply(t_top, as.character, stringsAsFactors=FALSE)

# Find the frequency of each song
song.frequency <- table(t2_top)
# Make a data frame of songs id and their coreesponding frequency
TopNFrame <- as.data.frame(as.table(song.frequency),stringsAsFactors=FALSE)
# Order the songs based on decreasing order of their frequencies
sorted_songs <- TopNFrame[order(TopNFrame$Freq, decreasing = TRUE), ]