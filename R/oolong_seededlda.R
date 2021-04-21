


.extract_ingredients.input_model_s3_seededlda <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, n_topiclabel_words = 8,...) {
  input_model <- input_model_s3$model
  keywords<-input_model$keywords
  K <- ncol(input_model$theta)
  V <- ncol(input_model$phi)
  terms <- t(as.matrix(seededlda::terms(input_model, n =ncol(input_model$phi) )))
  all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
  
  if(!is.null(keywords)){
    goodterms<-matrix(NA,K,100)
    for(i in 1:nrow(terms)){
      if(length(keywords)>=i){
        goodterms[i,] <- terms[i,1:1000][!terms[i,]%in%keywords[[i]] ][1:100]
      }else{goodterms[i,] <- terms[i,1:100]}
    }
  terms<-goodterms
    
  }
  
  
  
  
  # exclude keywords
  model_terms <- NULL
  theta <- NULL
  return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
