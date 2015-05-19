library(readr)


#' Read a folder
readFolder <- function(folder){
  # Set the names of files to read in the 'folder'
  snm <- sprintf("%s/subject_%s.txt", folder, folder)
  Xnm <- sprintf("%s/X_%s.txt", folder, folder)
  Ynm <- sprintf("%s/y_%s.txt", folder, folder)
  features <- read.table('features.txt', sep = ' ', stringsAsFactors = FALSE)
  allnames <- features$V2
  
  df <- data.frame()
  df <- rbind(allnames)
  df <- df[-1, ]
  
  # Read files
  sub <- read.table(snm, header = FALSE )
  yd <- read.table(Ynm, header = FALSE)
  xd <- read_fwf(Xnm, fwf_widths(561*16))

  # Convector the readed values to a vector of strings
  xd$X1 <- as.vector(xd$X1)
  
  # Create a matrix to be filled by the transformed
  mat <- matrix(, ncol = 561, nrow = length(xd$X1))
  
  for(i in 1:length(xd$X1)){
    mat[i, ] = obs2vector(xd$X1[[i]])
  }
   
  # Join the 
  df <- as.data.frame.matrix(mat)
  res <- cbind(cbind(df, yd), sub)
  names(res) <- c(allnames, "labels", "subject")
  res
}

# Convert a string of a 561 of 15-width float to a vector of
# floats
obs2vector <- function(obs){
  temp <- strsplit(obs, " ")
  temp <- sapply(temp, function(x) as.numeric(x))
  temp[!is.na(temp)]
}
test <- readFolder('test')
train <- readFolder('train')

## Question 1
dataset <- rbind(train, test)

## Question 2

dnames <- names(dataset)

meansAndSds <- sapply(dnames[1 :(561 - 2)], function(x) {
  c(mean(dataset[,x]), sd(dataset[,x]))
})

# Question 3
act_names <- read.table('activity_labels.txt', sep = ' ', header = FALSE)

toActName <- function(actId){
  act_names[actId,][[2]]
}

# Change labels of dataset
dataset$labels = toActName(dataset$labels)

## Questtion 4 
# Complete the naming 
names(dataset)[[562]] <- "Activities"


#Question 5
agg <- aggregate(. ~ Activities, data= dataset[c(1:562)], mean )
write_csv(agg, 'ave_per_activity.csv')


