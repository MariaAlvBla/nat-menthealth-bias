#using litsearchr tutorial https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html#Loading_results
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(devtools)
library(litsearchr)

#setwd working directory

#import ris files from WOS and Scopus
naive_results1 <- import_results(file="0-1000.ris")
naive_results2 <- import_results(file="1001-2000.ris")
naive_results3 <- import_results(file="2001-3000.ris")
naive_results4 <- import_results(file="3001-4000.ris")
naive_results5 <- import_results(file="4001-5000.ris")
naive_results6 <- import_results(file="5001-6000.ris")
naive_results7 <- import_results(file="6001-7000.ris")
naive_results8 <- import_results(file="7001-8000.ris")
naive_results9 <- import_results(file="8001-9000.ris")
naive_results10 <- import_results(file="9001-10000.ris")
naive_results11 <- import_results(file="10001-11000.ris")
naive_results12 <- import_results(file="11001-12000.ris")
naive_results13 <- import_results(file="12001-13000.ris")
naive_results14 <- import_results(file="13001-14000.ris")
naive_results15 <- import_results(file="14001-15000.ris")
naive_results16 <- import_results(file="15001-15757.ris")
naive_results17 <- import_results(file="scopus.ris")

#ensure the same number of columns for WOS and Scopus files.
naive_results1 <- naive_results1[,-c(5,6,7,8)]
naive_results2 <- naive_results2[,-c(5,6,7,8)]
naive_results3 <- naive_results3[,-c(5,6,7,8)]
naive_results4 <- naive_results4[,-c(5,6,7,8)]
naive_results5 <- naive_results5[,-c(5,6,7,8)]
naive_results6 <- naive_results6[,-c(5,6,7,8)]
naive_results7 <- naive_results7[,-c(4,5,7,8)]
naive_results8 <- naive_results8[,-c(5,6,7,8)]
naive_results9 <- naive_results9[,-c(5,6,7,8)]
naive_results10 <- naive_results10[,-c(5,6,7,8)]
naive_results11 <- naive_results11[,-c(5,6,7,8)]
naive_results12 <- naive_results12[,-c(5,6,7,8)]
naive_results13 <- naive_results13[,-c(5,6,7,8)]
naive_results14 <- naive_results14[,-c(5,6,7,8)]
naive_results15 <- naive_results15[,-c(4,5,7,8)]
naive_results16 <- naive_results16[,-c(5,6,7,8)]
naive_results17 <- naive_results17[,-c(5,6,7,8,9)]

#Combine reference lists
data_all<-rbind(naive_results1, naive_results2,naive_results3,naive_results4,
                naive_results5, naive_results6,naive_results7,naive_results8,
                naive_results9, naive_results10,naive_results11,naive_results12,
                naive_results13, naive_results14,naive_results15,naive_results16,
                naive_results17)

#remove duplicates
naiveresults <-remove_duplicates(data_all, field = "title", method = "string_osa")

#Extract terms from the title and abstract
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
#Extract terms from the keyword column

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

# Build the keyword co-occurrence network and erase duplicate terms
all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

#The ‘strength’ of each term in the network is the number of other terms that it appears together with
strengths <- strength(naivegraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

#visualize the strengths of the terms
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

#points along the ranking of terms where the strength of the next strongest term is much greater than that of the previous one
cutoff_change <- find_cutoff(naivegraph, method="changepoint", knot_num=3) #this method suggests several points to cut off

#visualize the change points
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

cutoff_change

#cut off the terms under the chosen change point (in this case 1)
naivegraph_redux <- reduce_graph(naivegraph, cutoff_change[1])
selected_terms <- get_keywords(naivegraph_redux)

selected_terms

#Create a csv file to manually group the terms into concepts
write.csv(selected_terms, "./search_terms.csv")

#I will use the write_search command once I have grouped the terms