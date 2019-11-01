# minimal example for creating a semantic librarian
# corpus: Alice and Wonderland
# author: Matt Crump
# RSemanticLibrarian is available at: https://crumplab.github.io/RsemanticLibrarian/

library(tidyverse)
library(RsemanticLibrarian)
library(data.table)

# Load text
alice <- scan(file="datafiles/Alice.txt", what=character())
alice <- paste(alice, sep=" ", collapse=" ")
alice <- unlist(strsplit(alice,split="[.]"))
alice <- LSAfun::breakdown(alice)
alice <- qdapRegex::rm_endmark(alice)

# Make tidystyle dataframe (text,  factor columns, index added for convenience)
text_df <- data.table(sentences = alice,
                      index=1:length(alice),
                      factor1 = sample(letters,replace=TRUE,length(alice)),
                      factor2 = sample(letters,replace=TRUE,length(alice)),
                      factor3 = sample(letters,replace=TRUE,length(alice))
)

text_df <- data.frame(text_df)

# Define a few functions

# deals with sentences that have no periods
sl_clean_vector2 <- function(words){
  return(lapply(words,sl_clean))
}

# create vectors for each factor
factor_vectors <- function(at_list,
                           articles = article_df,
                           fct = the_factor,
                           a_vectors = article_vectors) {
  author_vectors <- matrix(0, ncol = dim(a_vectors)[2], nrow = length(at_list))
  for(i in 1:length(at_list)){
    #auth_ids <- which(articles$authorlist %in% at_list[i])
    auth_ids <- which(stringr::str_detect(articles[,fct],at_list[i]))
    
    if(length(auth_ids) > 1) {
      author_vectors[i,] <- colSums(a_vectors[auth_ids,])
    } else {
      author_vectors[i,] <- a_vectors[auth_ids,]
    }
  }
  return(author_vectors)
}

# generate all vectors, return sl_data list
sl_semantic_general <- function(abstract_df=text_df,
         text_column = "sentences",
         factor_columns,
         riv_param = c(100,6)){
  
  vector_corpus <- list()
  
  # create corpus dictionary
  vector_corpus[['word_terms']] <- sl_corpus_dictionary(
    unlist(abstract_df[,text_column], use.names=FALSE)
  )
  message("Semantic Librarian: corpus dictionary created...")
  
  # convert to sentences, clean
  clean_sentences <- sl_clean_vector2(
    unlist(abstract_df[,text_column], use.names=FALSE)
  )
  message("Semantic Librarian: Cleaned sentences...")
  
  # word ids for sentences
  sentence_ids <- sl_word_ids_sentences(clean_sentences,vector_corpus[['word_terms']])
  message("Semantic Librarian: Created word IDs for sentences...")
  
  # Create word vectors
  message("Semantic Librarian: Creating BEAGLE word vectors...")
  vector_corpus[['word_vectors']] <- sl_beagle_vectors(s_ids = sentence_ids,
                                    dictionary = vector_corpus[['word_terms']],
                                    riv = riv_param,
                                     verbose = 500)
  
  # get text vectors
  the_abstracts <- list()
  the_abstracts[['new']] <- abstract_df[,text_column]
  clean_abstracts <- lapply(the_abstracts$new,
                            sl_clean)
  message("Semantic Librarian: cleaning text...")

  abstract_word_ids <- sl_word_ids_sentences(clean_abstracts,vector_corpus[['word_terms']])
  message("Semantic Librarian: Making text-word IDs...")

  message("Semantic Librarian: Making text vectors...")
  vector_corpus[[paste(text_column,"vectors",sep="_",collapse='')]] <- sl_article_vectors(abstract_word_ids, w_matrix = vector_corpus[['word_vectors']])
  
  vector_corpus[[paste(text_column,"terms",sep="_",collapse='')]] <- abstract_df[,text_column]
  
  #get factor vectors
  message("Semantic Librarian: Making factor vectors...")
  for(i in factor_columns){
    vector_corpus[[paste(i,"terms",sep="_",collapse='')]] <- unique(abstract_df[,i])
    
    vector_corpus[[paste(i,"vectors",sep="_",collapse='')]] <- factor_vectors(at_list = vector_corpus[[paste(i,"terms",sep="_",collapse='')]],
                                        articles = abstract_df,
                                        fct = i,
                                        a_vectors = vector_corpus[[paste(text_column,"vectors",sep="_",collapse='')]])
  }
  
  message("Semantic Librarian: Corpus Complete")
 
  return(vector_corpus)

}

# run the function, save the data

sl_data <- sl_semantic_general(abstract_df = text_df,
                               text_column = c("sentences"),
                               factor_columns = c("factor1","factor2"),
                               riv_param = c(100,6))

sl_data[['text_df']] <- text_df

save(sl_data, file="sl_data.RData")
