predictWord <- function(prefix, tri = bigtri, bi = bigbi, uni = biguni, y2=.5, y3=.5) {
        #Step 1: breaking our bigram prefix into word 1 (wi -1) and word 2 (wi)
        w1 = strsplit(prefix, " ")[[1]][1]
        w2 = strsplit(prefix, " ")[[1]][2]
        #Step 2: find our observed trigrams through the magic of data.table keys
        triObs <- tri[.(prefix)]
        #Step 3
        triObs[, prob := (n - y3)/bi[.(w1, w2)]$n]
        #Step 4:
                #4a: get unseen bigrams by generating trigram tails
                tails <- triObs$lastTerm
                #4b: discounted probability mass at bigram level
                biAlpha <- 1 - sum((bi[w2]$n - y2) / uni[w2]$n)
                #4c: calculate backed off probabilities for for bigrams
                        #4c.i: get the bigrams pasted together
                        boBiceV6 <- function(bigrams, term1, term2, boStatus) {
                                if (boStatus == "alpha") {
                                        vec <- bigrams[term1]
                                        return(vec[vec$lastTerm %in% term2])
                                } else if (boStatus == 'beta') {
                                        secondPass <- uni[!(uni$lastTerm %in% obs$lastTerm),]$lastTerm %>% 
                                                setdiff(tails)
                                        vec <- data.table(firstTerm = term1,
                                                lastTerm = secondPass) %>%
                                                inner_join(uni,'lastTerm') %>%
                                                data.table()
                                        return(vec)
                                }
                        }
                        #4c.ii: separate bigrams into observed and unobserved
                        obs <- boBiceV6(bi, w2, tails, 'alpha')
                        unobs <- boBiceV6(bi, w2, tails, 'beta')
                        #4c.iii: calculate observed backoffs
                        obs[, prob := (n - y2)/uni[.(w2)]$n]
                        #4c.iv: Calculate unobserved backed off bigrams
                        unobs[, prob := biAlpha * unobs$n / sum(unobs$n)]
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
