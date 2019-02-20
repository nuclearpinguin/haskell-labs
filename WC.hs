
main = interact wordCount where wordCount input = "Total number of letters: " ++ show(sum([length(w) | w <- words input])) ++ "\n"
