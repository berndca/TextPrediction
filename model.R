s_digit_s <- " _digit_ "
digit <- "_digit_"

preProcess <- function(charVec) {
    tibble(line=1, text=charVec) %>% 
        mutate(text = gsub(pattern = "[0-9]+[[:space:]/\\.,;:0-9\\-]*", 
                           replacement = s_digit_s, x = text)) %>%
        mutate(text = gsub(pattern = "https?://[A-Za-z0-9+&@#/%?=~_|!:,.;]*[A-Za-z0-9+&@#/%=~_|]",  "", x = text)) %>%
        mutate(text = gsub(pattern = "_{2,}", replacement = "", x = text)) %>%
        unnest_tokens(word, text) %>%
        inner_join(words, by = "word") %>%
        do(tail(., n=3)) %>%
        pull(index)
}

lookUpBigram <- function(x) {
    res2 <- bigrams[data.table(word1=x[1]), nomatch=0]$word2
    if (length(res2) == 0) { 1:5 }
    else { res2 }
}

lookUpTrigram <- function(x) {
    res3 <- trigrams[data.table(word1=x[1], word2=x[2]), nomatch=0]$word3
    if (length(res3) == 0) { lookUpBigram(x[2]) }
    else { res3 }
}

lookUpFourgram <- function(x) {
    res4 <- fourgrams[data.table(word1=x[1], word2=x[2], word3=x[3]), nomatch=0]$word4
    if (length(res4) == 0) { lookUpTrigram(x[2:3]) }
    else { res4 }
}

lookUp <- function(x) {
    if (length(x) == 3) { lookUpFourgram(x) }
    else if (length(x) == 2) { lookUpTrigram(x) }
    else if (length(x) == 1) { lookUpBigram(x) }
    else { 1:5 }
}

model <- function(c) {
    if (length(c) == 1 && c[1] == "") { "" }
    else { 
        x <- preProcess(c)
        estimate <- lookUp(x)
        words$word[estimate]
    }
}


