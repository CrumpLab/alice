## helper functions
cppFunction('NumericVector rowSumsSq(NumericMatrix x) {
            int nrow = x.nrow(), ncol = x.ncol();
            NumericVector out(nrow);
            
            for (int j = 0; j < ncol; ++j) {
            for (int i = 0; i < nrow; ++i) {
            out[i] += std::pow(x(i, j), 2);
            }
            }
            
            return out;
            }')

cosine_x_to_m  <- function(x, m) {
  x <- x / as.numeric(sqrt(crossprod(x)))
  sims<-(m %*% x / sqrt(rowSumsSq(m)))
  return(sims)
}

wrap_string <- function(x) {paste(strwrap(x,50), collapse=" <br> ")}
normalize_vector <- function (x) {return(x/abs(max(x)))}

## vectorize search terms

get_search_terms <- function(words,dictionary){
  breakdown_words <- breakdown(words)
  search_items <- unlist(strsplit(breakdown_words,split=" "))
  search_items <- search_items[search_items %in% dictionary==TRUE]
  return(search_items)
}

## get similarities between search terms and articles

get_search_article_similarities <- function(search_terms,
                                            d_words = sl_data$dictionary_words,
                                            w_vectors = sl_data$word_vectors,
                                            a_vectors = sl_data$article_vectors,
                                            a_df = article_df,
                                            query_type){
  
  search_index <- which(d_words %in% search_terms)
  if(length(search_index > 0)){
    # compound vectors
    if (query_type == 1){
      
      if(length(search_index) > 1) {
        query_vector <- colSums(w_vectors[search_index,])
      }else{
        query_vector <- w_vectors[search_index,]
      }
      
      get_cos <-  cosine_x_to_m(query_vector,
                                a_vectors)
      
      article_sims_SS <- a_df %>%
                          mutate(
                                 Similarity = as.numeric(get_cos))%>%
                          select(formatted_column,
                                 title,
                                 wrap_title,
                                 year,
                                 index,
                                 type,
                                 Similarity) %>%
                          arrange(desc(Similarity))
    } else {
      if(length(search_index) > 1) {
          query_matrix     <- w_vectors[search_index,]
          get_cos_matrix   <- apply(query_matrix,1,
                                    function(x) cosine_x_to_m(x,a_vectors))
          if (query_type == 2) {
            multiply_columns <- apply(get_cos_matrix,1,prod)
          } else if (query_type == 3){
            get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
            multiply_columns <- apply(get_cos_matrix,1,max)
          }
          
          article_sims_SS <- a_df %>%
            mutate(
                   Similarity = round(multiply_columns,digits=4))%>%
            select(formatted_column,
                   title,
                   wrap_title,
                   year,
                   index,
                   type,
                   Similarity) %>%
            arrange(desc(Similarity))
      }
    }
  }
}

# get multidimensional scaling coordinates

get_mds_article_fits <- function(num_articles,
                                 num_clusters,
                                 year_range,
                                 input_df,
                                 a_vectors=sl_data$article_vectors){
  if(!is.null(input_df)){
    article_ids <- input_df %>%
      filter(year >= year_range[1],
             year <= year_range[2]) %>%
      slice(1:num_articles) %>%
      select(index)
    # input_df[1:num_articles,]$index
    temp_article_matrix <- a_vectors[article_ids$index,]
    mdistance <- cosine(t(temp_article_matrix))
    fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
    colnames(fit$points) <- c("X","Y")
    cluster <- kmeans(fit$points,num_clusters)
    input_df <- input_df %>%
      filter(year >= year_range[1],
             year <= year_range[2]) %>%
      slice(1:num_articles)
    input_df <- cbind(input_df,fit$points,
                      cluster=cluster$cluster)
    return(input_df)
  }
}
                
# article to all article similarities

get_article_article_similarities <- function(a_title,
                                             a_vectors = sl_data$article_vectors,
                                             a_df = article_df){
  
  a_id <- a_df[a_df$title == a_title,]$index

  get_cos <-  cosine_x_to_m(a_vectors[a_id,],a_vectors)
  article_sims <- a_df %>%
    mutate(
           Similarity = round(as.numeric(get_cos),digits=4)) %>%
    select(formatted_column,
           title,
           wrap_title,
           year,
           index,
           type,
           Similarity) %>%
    arrange(desc(Similarity))
  return(article_sims)
}

get_author_similarities <- function(a_name,
                                    auth_vectors = sl_data$author_vectors,
                                    authors=sl_data$author_list){
  auth_id <- which(authors %in% a_name)
  get_cos <-  cosine_x_to_m(auth_vectors[auth_id,],auth_vectors)
  selected_author <- rep("other",length(authors))
  selected_author[auth_id] <- "selected"
  selected_author <- as.factor(selected_author)
  author_sims <- data.frame(author=authors,
                            Similarity = round(as.numeric(get_cos),digits=4),
                            selected_author = selected_author,
                            index = 1:length(authors)) %>%
                            arrange(desc(Similarity))
  return(author_sims)
}

get_mds_author_fits <- function (num_authors,
                                 num_clusters,
                                 input_df,
                                 auth_vectors=sl_data$author_vectors){
  auth_ids <- input_df[1:num_authors,]$index
  temp_author_matrix <- auth_vectors[auth_ids,]
  mdistance <- cosine(t(temp_author_matrix))
  fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
  colnames(fit$points) <- c("X","Y")
  cluster <- kmeans(fit$points,num_clusters)
  input_df <- slice(input_df,1:num_authors)
  input_df <- cbind(input_df,fit$points,
                    cluster=cluster$cluster)
  return(input_df)
}

get_article_author_similarities <- function(a_title,
                                            a_vectors = sl_data$article_vectors,
                                            a_df = article_df,
                                            auth_vectors = sl_data$author_vectors,
                                            authors=sl_data$author_list){
  a_id <- a_df[a_df$title == a_title,]$index
  get_cos <-  cosine_x_to_m(a_vectors[a_id,],auth_vectors)
  author_sims <- data.frame(author=authors,
                            Similarity = round(as.numeric(get_cos),digits=4),
                            index = 1:length(authors)) %>%
                            arrange(desc(Similarity))
  return(author_sims)
}

## get article similarities from selection of articles

get_articles_mds_from_selection <- function(a_df,
                                            num_clusters,
                                            a_vectors = sl_data$article_vectors){
  a_ids <- a_df$index
  if(length(a_ids) > 2){
    mdistance <- cosine(t(sl_data$article_vectors[a_ids,]))
    fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
    colnames(fit$points) <- c("X","Y")
    cluster <- kmeans(fit$points,num_clusters)
    a_df <- cbind(a_df, fit$points, 
                  cluster=cluster$cluster)
  }
  if(length(a_ids) == 2){
    mdistance <- cosine(t(sl_data$article_vectors[a_ids,]))
    fit <- cmdscale(1-mdistance,eig=TRUE, k=1)
    colnames(fit$points) <- c("X")
    #cluster <- kmeans(fit$points,num_clusters)
    a_df <- cbind(a_df, fit$points, Y=0,
                  cluster=1)
  }
  if(length(a_ids) == 1){
    a_df <- cbind(a_df, X=0, Y=0, cluster =1)
  }
  return(a_df)
  
}



get_cluster_keywords <- function(a_df,
                                 num_kw = 10,
                                 resonance = 3,
                                 a_vectors = sl_data$article_vectors,
                                 w_vectors = sl_data$WordVectors,
                                 word_list = sl_data$dictionary_words){
  set.seed(1)
  ids <- a_df %>%
    group_by(cluster) %>%
    summarise(ids = list(index),
              meanX= mean(X),
              meanY = mean(Y))
  
  ## Compute average article vector ##
  if(dim(ids)[1] > 1){
    avg_article <- colSums(a_vectors[a_df$index,])
    avg_sim <- cosine_x_to_m(avg_article,w_vectors)
  }
  
  if(dim(ids)[1] == 1){
    avg_article <- colSums(a_vectors)
    avg_sim <- cosine_x_to_m(avg_article,w_vectors)
  }
  
  cluster_key <- data.frame()
  for( i in 1:dim(ids)[1] ){
    
    a_num <- ids$ids[[i]]
    
    if( length(a_num) > 1 ){
      cluster_sim <- cosine_x_to_m(colSums(a_vectors[a_num, ]),
                                   w_vectors)
    }
    if( length(a_num) == 1 ){
      cluster_sim <- cosine_x_to_m(a_vectors[a_num, ],
                                   w_vectors)
    }
    
    
    ## subtract cluster similarities from average similarities
    subtraction_ids <- (cluster_sim[,1]^resonance)-(avg_sim[,1]^resonance)
    
    kw <- data.frame(key_words = word_list,
                     Similarity = subtraction_ids,
                     cluster_sim,
                     avg_sim,
                     cluster = i)  %>%
      arrange(desc(Similarity)) %>%
      slice(1:num_kw)
    
    c_df <- data.frame(key_words = paste(as.character(kw$key_words),"\n", collapse = ""),
                       cluster = i,
                       X = ids[ids$cluster==i,]$meanX,
                       Y = ids[ids$cluster == i,]$meanY,
                       wrap_title = paste(as.character(kw$key_words),"<br>", collapse = ""))
    cluster_key <- rbind(cluster_key,c_df)
    
  }
  
  return(cluster_key)
  
}

get_similarities <- function(search_terms,
                             search_list = sl_data$word_terms,
                             search_vectors = sl_data$word_vectors,
                             target_vectors = sl_data$word_vectors,
                             target_terms = sl_data$word_terms){
  
  search_id <- which(search_list == search_terms)
  print(head(search_list))
  print(head(search_terms))
  print(search_id)
  #print(target_terms)
  #print(dim(search_vectors))
  #print(dim(target_vectors))
  
  if(length(search_id) > 0) {
  get_cos <-  cosine_x_to_m(search_vectors[search_id,],target_vectors)
  sims <- data.frame(terms = target_terms,
                     Similarity = round(as.numeric(get_cos),digits=4)) %>%
    arrange(desc(Similarity))
  
  return(sims)
  }
}

# get multidimensional scaling coordinates

get_mds_fits <- function(num_items,
                         num_clusters,
                         input_df,
                         target_vectors=sl_data$word_vectors,
                         target_terms = sl_data$word_terms){
  if(!is.null(input_df)){
    article_ids <- input_df %>%
      slice(1:num_items) %>%
      select(terms)
    get_terms <- which(article_ids$terms %in% target_terms)
    # input_df[1:num_articles,]$index
    temp_term_matrix <- target_vectors[get_terms,]
    mdistance <- cosine(t(temp_term_matrix))
    fit <- cmdscale(1-mdistance,eig=TRUE, k=2)
    colnames(fit$points) <- c("X","Y")
    cluster <- kmeans(fit$points,num_clusters)
    input_df <- input_df %>%
      slice(1:num_items)
    input_df <- cbind(input_df,fit$points,
                      cluster=cluster$cluster)
    return(input_df)
  }
}

# 
# get_cluster_keywords <- function(a_df,
#                                  num_kw = 10,
#                                  resonance = 3,
#                                  a_vectors = sl_data$article_vectors,
#                                  w_vectors = sl_data$word_vectors,
#                                  word_list = sl_data$dictionary_words){
#   set.seed(1)
#   ids <- a_df %>%
#     group_by(cluster) %>%
#     summarise(ids = list(index),
#               meanX= mean(X),
#               meanY = mean(Y))
#   
#   ## Compute average article vector ##
#   avg_article <- colSums(a_vectors[a_df$index,])
#   avg_sim <- cosine_x_to_m(avg_article,w_vectors)
#   
#   cluster_key <- data.frame()
#   for( i in 1:dim(ids)[1] ){
#     
#     a_num <- ids$ids[[i]]
#     
#     if( length(a_num) > 1 ){
#       cluster_sim <- cosine_x_to_m(colSums(a_vectors[a_num, ]),
#                                    w_vectors)
#     }
#     if( length(a_num) == 1 ){
#       cluster_sim <- cosine_x_to_m(a_vectors[a_num, ],
#                                    w_vectors)
#     }
#     
#     
#     ## subtract cluster similarities from average similarities
#     subtraction_ids <- (cluster_sim[,1]^resonance)-(avg_sim[,1]^resonance)
#     
#     kw <- data.frame(key_words = word_list,
#                      Similarity = subtraction_ids,
#                      cluster_sim,
#                      avg_sim,
#                      cluster = i)  %>%
#       filter(key_words %in% stop_words ==FALSE) %>%
#       arrange(desc(Similarity)) %>%
#       slice(1:num_kw)
#     
#     c_df <- data.frame(key_words = paste(as.character(kw$key_words),"\n", collapse = ""),
#                        cluster = i,
#                        X = ids[ids$cluster==i,]$meanX,
#                        Y = ids[ids$cluster == i,]$meanY,
#                        wrap_title = paste(as.character(kw$key_words),"<br>", collapse = ""))
#     cluster_key <- rbind(cluster_key,c_df)
#     
#   }
#   
#   return(cluster_key)
#   
# }

    
#     #article_sims_SS<-article_sims_SS()
#     article_sims_SS$Similarity <- round(get_cos,digits=4)
#     article_sims_SS <- cbind(article_sims_SS,
#                              year=article_df$year,
#                              title=article_df$title,
#                              index=seq(1,dim(sl_data$article_vectors)[1]))
#     
#     article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
#                                          article_sims_SS$year <= input$slider_num_year_SS[2],  ]
#     
#     article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
#     
#     article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
#     
#     top_terms <- as.character(article_sims_SS$title)
#     
#     article_index <- article_sims_SS$index
#     
#     temp_article_matrix <- sl_data$article_vectors[article_index,]
#     
#     row.names(temp_article_matrix) <- top_terms
#     
#   } else {
#     
#     query_matrix     <- sl_data$word_vectors[search_index,]
#     get_cos_matrix   <- apply(query_matrix[,new_article_beagle_inds$a],1,function(x) cosine_x_to_m(x,
#                                                                                                    sl_data$article_vectors[,new_article_beagle_inds$a]))
#     if (input$select_query_type == 2) {
#       multiply_columns <- apply(get_cos_matrix,1,prod)
#     } else if (input$select_query_type == 3){
#       get_cos_matrix <- apply(get_cos_matrix,2,normalize_vector)
#       multiply_columns <- apply(get_cos_matrix,1,max)
#     }
#     article_sims_SS<-article_sims_SS()
#     article_sims_SS$Similarity <- round(multiply_columns,digits=4)
#     article_sims_SS <- cbind(article_sims_SS,
#                              year=article_df$year,
#                              title=article_df$title,
#                              index=seq(1,dim(sl_data$article_vectors)[1]))
#     article_sims_SS <- article_sims_SS[article_sims_SS$year >= input$slider_num_year_SS[1] &
#                                          article_sims_SS$year <= input$slider_num_year_SS[2],  ]
#     article_sims_SS <- article_sims_SS[order(article_sims_SS$Similarity,decreasing=T),]
#     article_sims_SS <- article_sims_SS[1:input$slider_num_articles,]
#     top_terms <- as.character(article_sims_SS$title)
#     article_index <- article_sims_SS$index
#     temp_article_matrix <- sl_data$article_vectors[article_index,]
#     row.names(temp_article_matrix) <- top_terms
#   }
#   
# }

#   search_terms <- get_search_terms_SS()                                        
#   #if(!is.null(input$server_dictionary))
#   if(!is.null(search_terms))
#     
#     #search_index <-   which(sl_data$dictionary_words %in% input$server_dictionary)
