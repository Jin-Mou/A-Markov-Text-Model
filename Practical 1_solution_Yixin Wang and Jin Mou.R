time_start <- Sys.time() # begin timing

setwd("put/your/local/repo/location/here")
a <- scan("1581-0.txt",what="character",skip=156,encoding="UTF8")
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

split_punct <- function(x){
  ii <- grep("[^[:alnum:]///' ]",a)  
  aword <- rep("",length(ii)+length(a))
  amark <- ii+1:length(ii) 
  aword[-amark] <- gsub("[^[:alnum:]///']","",a)
  punmarkbefore <- gsub("[a-zA-Z]","",a)
  punmark <- rep("",length(ii))
  w <- 1
  for(p in 1:length(a)){
    if(punmarkbefore[p]!=""){
      punmark[w] <- punmarkbefore[p]
      w <- w+1
    }
  }
  s <- 1
  for(q in ii){
    aword[q+s] <- punmark[s]
    s <- s+1
  }
  return(aword)
}

aword <- split_punct(a)

##6(1)
bbefore <- tolower(aword)
bbefore1 <- unique(bbefore)
bbefore2 <- gsub("\\d","",bbefore1) ##deleting numbers 

##6(2)
loc <- match(bbefore,bbefore2) 


##6(3)
freq1 <- tabulate(loc)
freq <- cbind(freq1,bbefore1)

##6(4)

orderfull <- freq[order(freq[,1],decreasing=TRUE),]
b <- orderfull[1:1000,2]

##7
b <- tolower(b)
full_text <- tolower(aword)

# Converting to lower case and matching
match_index <- match(full_text,b,NA)
common_index <- match_index[-length(match_index)]
follow_index <- match_index[-1]

# Finding the two common words pairs
match_mat <- cbind(common_index,follow_index)
p_index <- which(is.na(rowSums(match_mat)) == FALSE) # return row index of pairs
p_mat <- match_mat[p_index,]

A_mat <- matrix(0, length(b),length(b)) # generate the initial matrix A
p_count <- dim(p_mat)[1] # return the row number of p_mat

# Constructing original A matrix
for (n in 1:p_count) {
  A_mat[p_mat[n,1],p_mat[n,2]] <- A_mat[p_mat[n,1],p_mat[n,2]] + 1
}


# adjusting A matrix by probability
B_mat <- matrix(1/rowSums(A_mat),length(b),length(b))
A_mat_2 <- A_mat * B_mat

##8 simulation
set.seed(1)
num <- 50 # the number of generated words
sentence_index <- rep(0,num)
sentence_index[1] <- sample(x = 1:length(b),size = 1,replace = T)

# finding the indexes of rest 49 words
for (i in 2:num) {
  prob <- A_mat_2[sentence_index[i-1],] # conditional probability on i-1
  sentence_index[i] <- sample(x = 1:length(b),size = 1,replace = T,prob = prob)
}

# converting indexes to words
sentence_arr <- b[sentence_index]
cat(sentence_arr)

##9 Considering  words that most often start with a capital letter
# calculate the frequency
b2 <- orderfull[1:1000, 1] # the number of every entry in vector b
sentence_arr_low <- cbind(unique(sentence_arr), b2[match(unique(sentence_arr),b)])

sentence_arr_cap <- unique(sentence_arr)
substr(sentence_arr_cap, 1,1) <- toupper(substr(sentence_arr_cap, 1,1)) # creating array with capital first letter
loc_2 <- match(aword,sentence_arr_cap)

# calculate the frequency
freq_2 <- tabulate(loc_2)
sentence_arr_cap_2 <- cbind(sentence_arr_cap, freq_2)

uni <- sentence_arr_low[,1]
# return the index where there are more capital letters for a word
ind <- which(as.integer(sentence_arr_low[,2]) - 2*as.integer(sentence_arr_cap_2[,2]) < 0)
uni[ind] <- sentence_arr_cap[ind]

# replacing
uni_2 <- tolower(uni)
sentence_2 <- uni[match(sentence_arr,uni_2)]
cat(sentence_2)

time_end <- Sys.time() # end timing
running_time <- time_end - time_start
cat("The whole running time is ", running_time, "secends.")