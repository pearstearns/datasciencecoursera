predictWord <- function(prefix, tri, bi, uni, y2=.5, y3=.5) {
        #Step 1: breaking our bigram prefix into word 1 (wi -1) and word 2 (wi)
        w1 = strsplit(prefix, " ")[[1]][1]
        w2 = strsplit(prefix, " ")[[1]][2]
        #Step 2: find our observed trigrams through data.table keys
        triObs <- tri[.(prefix)]
        #Step 3
        triObs[, prob := (n - y3)/bi[.(w1, w2)]$n]
        #Step 4:
                #4a: trigram tails
                tails <- uni[uni$firstTerm %in% triObs$lastTerm, ]$firstTerm
                #4b: discounted probability mass at bigram level
                biAlpha <- 1 - sum((bi[w2]$n - y2) / uni[w2]$n)
                #4c: calculate backed off probabilities for for bigrams
                        #4c.i: get the bigrams pasted together
                        boBice <- function(bigrams, term1, term2, boStatus) {
                                if (boStatus == "on") {
                                        vec <- bigrams[term1]
                                        vec[.(unique(firstTerm), term2[1]), lastTerm := NA]
                                        for (i in term2[2:length(term2)]) {
                                                setkey(vec, lastTerm)
                                                vec[i, lastTerm := NA]
                                        }
                                        return(na.omit(vec))
                                } else if (boStatus == 'off') {
                                        secondPass <- c(term2, 
                                                        uni[!(uni$firstTerm %in% obs$lastTerm),]$firstTerm) %>% 
                                                        setdiff(triObs$lastTerm)
                                        vec <- data.table(firstTerm = term1,
                                                        lastTerm = uni[uni$firstTerm %in% secondPass,]$firstTerm,
                                                        n = NA)
                                        return(vec)
                                }
                        }
                        #4c.ii: separate bigrams into observed and unobserved
                        obs <- boBice(bi, w2, tails, "on")
                        unobs <- boBice(bi, w2, tails, "off")
                        #4c.iii: calculate observed backoffs
                        obs[, prob := (n - y2)/uni[.(w2)]$n]
                        #4c.iv: Calculate unobserved backed off bigrams
                                #4c.iv.1:
                                qbu <- do.call("rbind", 
                                        lapply(unobs$lastTerm, 
                                                function(x) uni[.(x)]))
                                #miles <- setdiff(uni$firstTerm, unobs$lastTerm)
                                #4c.iv.2:
                                unobs[, prob := biAlpha * qbu$n / sum(qbu$n)]
                #4d: calculate probability mass at the trigram level
                triAlpha <- 1 - sum((tri[.(prefix)]$n - y2) / bi[.(w1, w2)]$n)
                #4e: calculate unobserved trigram probabilities
                qbt <- rbind(unobs, obs)
                qbt[, prob := triAlpha * qbt$prob/ sum(qbt$prob)]
                qbt[, firstTerm := paste(w1, firstTerm, sep = " ")]
        #Step 5: Select highest wi wiith the highest probability
        final <- rbind(triObs, qbt) %>% arrange(desc(prob))
        #return(final[1:3, c(1,2,4)])
        return(final)
}
