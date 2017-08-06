############### Functions ################


library(quanteda)
library(plyr)
library(dplyr)
library(doParallel)
library(data.table)

#1. takes a text file and percentage as input
#2. loads a text vector, encodes uniformly, and removes null lines
#3. returns a random sample of the text vector, in the size specified by percentage
txtLoad <- function(file, percentage){
        textvec <- readLines(file(file, "rb"), encoding = "UTF-8", skipNul = T)
        textvec <- textvec[grep(1, rbinom(length(textvec), 1, percentage))]
        return(textvec)
}

##Utility Functions
# a quick and dirty function that can return object sizes in something other than bytes
hsize <- function(obj, size = 'm'){
        if(size == 'k'){
                return(object.size(obj)[[1]] / 1024) 
        } else if(size == 'm'){
                return(object.size(obj)[[1]] / 1024 ^ 2)
        } else if(size == 'g'){
                return(object.size(obj)[[1]] / 1024 ^ 3)
        }
}

#building off hsize() it loops through the global environment showing either the memory as a whole, or individually 
envCheck <- function(env = .GlobalEnv, sum = F){
        if(sum == T){
                sum(unlist(eapply(env, hsize)))
        } else {
                lapply(env, hsize)
        }
}

############### Execution ################


#takes an 8% sample of each data file and combines them into a vector called bigCorpus
bigCorpus <- c(txtLoad("Data/CapstoneData/en_US.blogs.txt", .08),
               txtLoad("Data/CapstoneData/en_US.news.txt", .08),
               txtLoad("Data/CapstoneData/en_US.twitter.txt", .08))
