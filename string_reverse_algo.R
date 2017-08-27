############# STRING REVERSE ALGORITHMS ##############

# Taking a string as an input, output the string reversed (backwards)

# Method 1 - Recursion

reverse_recursive <- function(string){
  #Base case for 0-1 characters
  if(nchar(string) < 2){
    return(string)
  }
  
  return(paste(reverse_recursive(substr(string, 2, nchar(string))), substr(string, 1, 1), sep = ""))
}


# Method 2 - Iterative

reverse_iterative <- function(string){
  num <- nchar(string)
  rev <- ""
  while(num > 0){
    rev = paste(rev, substr(string, num, num), sep = "")
    num = num - 1
  }
  print(rev)
}

# Test cases
reverse_recursive("abcde")
reverse_recursive("The quick brown fox jumped over the lazy dog.")

reverse_iterative("Good day, world!")
reverse_iterative("ISCREAMFORICECREAM!!!")