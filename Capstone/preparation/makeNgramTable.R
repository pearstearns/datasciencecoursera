############### Functions ################


#ngram generator
#1. breaks the corpus(text data) into n-long (anywhere from one to four word) chunks called tokens,
#2. puts the tokens into a data.table
#3. counts the frequency of all unique words
#4. depending on whether or not it's a 1-word, or multiple word token either,
#        4a:separates the multiple worl token into two columns:
#               4c.i: "firstTerm", the word(s) before the final space
#               4c.ii: "lastTerm", the final word, split from the previous term by the final space
#       4b: continues onto step 5
#5. set a binary search key by either one or both columns
#6. returns the resulting data.table

ngramGen <- function(corp, n){
        if(n > 1){
                vec <- corp %>% 
                        tokenize("word", ngrams = n, concatenator = " ") %>% 
                        unlist %>% 
                        data.table(term = .) %>%
                        dplyr::count(term) %>%
                        tidyr::separate(term, into = c("firstTerm", "lastTerm"), sep =' (?=[^ ]+$)') %>%
                        as.data.table()
                setkeyv(vec, c('firstTerm', 'lastTerm'))
                return(vec)
        } else if(n == 1) {
                vec <- corp %>% 
                        tokenize("word", ngrams = n, concatenator = " ") %>% 
                        unlist %>% 
                        data.table(lastTerm = .) %>%
                        dplyr::count(lastTerm) %>%
                        as.data.table()
                setkey(vec, lastTerm)
                return(vec)
        }
}

#discount generator
#sets up a good turing discount table for the incoming data.table
#1. creates the column
#2. define our terms
#       2a. the frequency r
#       2b. r+1
#       2c. the number of terms that occur r times, Nr
#       2d. the number of terms that occur r + 1 times, Ni
#3. use the formula for the given data.table
#4. for each term that has a frequency of r, replace the dummy value with the current one
#5. loop through

getDisc <- function(ngram){
        ngram[, disc := 1]
        for(i in 1:5){
                r = i
                ri = r + 1
                Nr = nrow(ngram[n == r])
                Ni = nrow(ngram[n == ri])
                horse = (ri/r) * (Ni/Nr)
                ngram[n == r, disc := horse]
        }
}

############### Execution ################


#for each n we will:
#       1. get the ngram table with frequencies with ngramGen()
#       2. compute good turing discount using getDisc() 
#       3. save to disk using data.table::fwrite() their wonderful and blazing fast .csv writer

#unigram
uni <- ngramGen(cleanCorpus, 1)
getDisc(uni)
fwrite(Uni, file = "Data/Uni.csv")

#bigram
bi <- ngramGen(cleanCorpus, 2)
getDisc(bi)
fwrite(bi, file = "Data/Bi.csv")

#trigram
tri <- ngramGen(cleanCorpus, 3)
getDisc(Tri)
fwrite(tri, file = "Data/Tri.csv"); rm(Tri)

#quadrigram
quad <- ngramGen(cleanCorpus, 4)
getDisc(quad)
fwrite(quad, file = "Data/Quad.csv"); rm(Quad)
