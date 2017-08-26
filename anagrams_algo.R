############# THIS CODE IS A FUNCTION THAT TAKES TWO CHARACTER STRINGS (NO PUNCTUATION OR WHITESPACES) ##################
############# AND CALCULATES THE MINIMUM NUMBER OF DELETIONS REQUIRED TO TURN THE TWO STRINGS INTO ANAGRAMS ###############
####### (e.g. "abcde" and "dcebf" require 2 deletions -- removal of "a" and "f" -- to turn the strings into anagrams)

#Function
#General idea: Sort and count all the unique characters in each string, then compare the counts to determine the number of deletions required.

min_deletions <- function(a,b){
  
  #Check for invalid characters
  if(grepl('[^A-z]', a) | grepl('[^A-z]', b)){
    return(print("Invalid non-alphabetic characters. Try again."))
  }
  
  #Sort and count unique chars
  chars_a <- sort(strsplit(a, split = "")[[1]]) #all ordered characters of string a
  chars_b <- sort(strsplit(b, split = "")[[1]]) #all ordered characters of string b
  
  df_a <- as.data.frame(table(chars_a)) #data frame with unique characters and tallies for string a
  df_b <- as.data.frame(table(chars_b)) #data frame with unique characters and tallies for string b
  
  #Customize column names in dataframes
  names(df_a)[1] <- paste("chars")
  names(df_b)[1] <- paste("chars")
  
  names(df_a)[2] <- paste("count_a")
  names(df_b)[2] <- paste("count_b")
  
  #Full join the two dataframes to get all unique characters and their respective counts
  df_all <- merge(df_a, df_b, by = "chars", all = TRUE)
  
  #Replace any NA's with 0
  df_all[is.na(df_all)] <- 0
  
  #Turn count values from character to numeric
  df_all$count_a <- as.numeric(df_all$count_a)
  df_all$count_b <- as.numeric(df_all$count_b)
  
  #Sum up differences in counts to return optimal # of removals
  removals <- sum(abs(df_all$count_a - df_all$count_b))
  if(removals >= sum(df_all$count_a)){
    return(print("The two strings have no overlapping characters and cannot be made into anagrams."))
  } else {
    return(removals)
  }
}



########## TEST CASES ###########

min_deletions("abcde","dcebf") #returns 2
min_deletions("potato","tomato") #returns 2 (p and m)
min_deletions("abcde","ghijk") #returns "The two strings have no overlapping characters and cannot be made into anagrams."
min_deletions("There are spaces","This should fail") #returns "Invalid non-alphabetic characters. Try again."

