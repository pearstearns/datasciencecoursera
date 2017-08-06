predictWord <- function(prefix, tri = bigtri, bi = bigbi, uni = biguni, y3=.5, y2=.5) {
        #Step 1: breaking our bigram prefix into word 1 (wi -1) and word 2 (wi)
        w1 = strsplit(prefix, " ")[[1]][1]
        w2 = strsplit(prefix, " ")[[1]][2]
        #Step 2: find our observed trigrams through the magic of data.table keys
        triObs <- tri[.(prefix)]
        #Step 3
        triObs[, prob := (n - y3)/bi[.(w1, w2)]$n]
        #Step 4:
                #4a: trigram tails
                tails <- uni[uni$lastTerm %in% triObs$lastTerm, ]$lastTerm
                #4b: discounted probability mass at bigram level
                biAlpha <- 1 - sum((bi[w2]$n - y2) / uni[w2]$n)
                #4c: calculate backed off probabilities for for bigrams
                        #4c.i: get the bigrams pasted together
                        boBiceV4 <- function(ngram, term1 = NA, term2, status) {
                                if (status == "on") {
                                        vec <- ngram[term1]
                                        if(identical(ngram, bi) == T) {
                                                vec[.(unique(firstTerm), term2[1]), lastTerm := NA]
                                        } else if(identical(ngram, uni) == T) {
                                                vec[.(term2[1]), lastTerm := NA]
                                        }
                                        if(length(term2 < 500)) {
                                                for (i in term2[2:length(term2)]) {
                                                        setkey(vec, lastTerm)
                                                        vec[i, lastTerm := NA]
                                                }
                                        } else if(length(term2) > 500) {
                                                registerDoParallel(detectCores() - 1)
                                                        foreach(i = term2[2:length(term2)]) %dopar% {
                                                        setkey(vec, lastTerm)
                                                        vec[i, lastTerm := NA]
                                                        }
                                                stopImplicitCluster()
                                        }
                                        return(na.omit(vec))
                                } else if (status == 'off') {
                                        secondPass <- c(term2, 
                                                uni[!(uni$lastTerm %in% obs$lastTerm),]$lastTerm) %>% 
                                                setdiff(triObs$lastTerm)
                                        vec <- data.table(firstTerm = term1,
                                                lastTerm = uni[uni$lastTerm %in% secondPass,]$lastTerm,
                                                n = NA)
                                        return(vec)
                                }
                        }
                        #4c.ii: separate bigrams into observed and unobserved
                        obs <- boBiceV4(bi, w2, tails, "on")
                        unobs <- boBiceV4(bi, w2, tails, "off")
                        #4c.iii: calculate observed backoffs
                        obs[, prob := (n - y2)/uni[.(w2)]$n]
                                #4c.iv: Calculate unobserved backed off bigrams
                                #4c.iv.1:
                                #qbu <- do.call("rbind", 
                                #        lapply(unobs$lastTerm, 
                                #                function(x) uni[.(x)]))
                                miles <- setdiff(uni$lastTerm, unobs$lastTerm)
                                qbu <- boBiceV4(uni, term2 = miles, status = "on")
                                #4c.iv.2:
                                unobs[, prob := biAlpha * qbu$n / sum(qbu$n)]
                #4d: calculate probability mass at the trigram level
                triAlpha <- 1 - sum((tri[.(prefix)]$n - y2) / bi[.(w1, w2)]$n)
                #4e: calculate unobserved trigram probabilities
                qbt <- rbind(unobs, obs)
                qbt[, prob := triAlpha * qbt$prob/ sum(qbt$prob)]
                qbt[, firstTerm := paste(w1, firstTerm, sep = " ")]
        #Step 5: Select highest wi wiith the highest probability
        final <- rbind(triObs, qbt)
        final[order(-final$prob), ]
        #return(final[1:10, c(1,2,4)])
        return(final)
}
