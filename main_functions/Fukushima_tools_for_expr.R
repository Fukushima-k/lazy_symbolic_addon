
#' easy_parse
#' 
#' @examples
#' easy_parse("X")
#' easy_parse("tr(X%*%B)")
#' 

easy_parse <- function(text){
  parse(text=text)[[1]]
} # end of easy_parse


#' Decompose Matrix Product
#'
#' @note Currently, nested expressions may not be handled correctly in some cases, so caution is advised.
#'
#' @examples
#' # example code
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "%*%")
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "*")
#' decompose_MatProd("a+b-c+d+e", c("-", "+"), return_op = TRUE)
#' decompose_MatProd("a+b-(c+d)+e", c("-", "+"), return_op =  TRUE)
#' decompose_MatProd("a+b--e", c("-", "+"),  return_op =  TRUE)
#' decompose_MatProd("a+b-+e", c("-", "+"),  return_op =  TRUE)
#' decompose_MatProd("+a++b--c+-d-+e", c("-", "+"),  return_op =  TRUE)
#' 
#' decompose_MatProd("A%*%B%*%((X%*%C)%*%D)", "%*%", flat = TRUE)
#' decompose_MatProd("A*B*((X%*%C)*D)", "%*%", flat = TRUE)
#' decompose_MatProd("A*B*((X*C)*D)", "*", flat = TRUE)
#' decompose_MatProd("A+B-((X+C)+D)-E", c("+", "-"), flat = TRUE, return_op = TRUE)
#' 
#' 
#' decompose_MatProd("A+(B+C)+(X+D)", "+", target_X = "X")
#' decompose_MatProd("A+(B+C)+((X+D)+E)", "+", target_X = "X")
#'
#' @export
#'

decompose_MatProd <- function(expr, op, return_op = FALSE, flat = FALSE, target_X){
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  ops <- NULL
  temp_past <- list(expr)
  continue <- TRUE
  
  while(continue){
    continue <- FALSE
    temp_current <- NULL
    for(i in  seq_along(temp_past)){
      expr_temp <- temp_past[[i]]
      if (is.symbol(expr_temp)) {
        temp_current <- c(temp_current, expr_temp)
        # } else if (is.numeric(expr)) {
        #   return(expr)
      } else if (is.call(expr)) {
        if((as.character(expr_temp[[1]]) %in% op) & length(expr_temp)>2) {
          temp_current <- c(temp_current, expr_temp[[2]],  expr_temp[[3]])
          ops <- c(ops, as.character(expr_temp[[1]]))
          continue <-TRUE
        }else{
          temp_current <- c(temp_current, expr_temp)
        }
      }
    }
    temp_past <- temp_current
    # temp_current %>% print()
  }
  ops <- rev(ops)
  
  
  continue <- TRUE
  while(continue){
    continue <- FALSE
    i_range <- 1
    if(flat & missing(target_X)){
      # i <- 1
      # while(i <= length(temp_current)){
      i_range <- seq_along(temp_current)  
    }
    if(!missing(target_X)){
      i <-which(grepl(paste("\\b",target_X,"\\b", sep=""), as.character(temp_current)))
      i_range <- i[1] # i_range must be 1 length
    }
    for(i in i_range){
      if(is.call(temp_current[[i]]))
        if(temp_current[[i]][[1]] == "(")
          if(as.character(temp_current[[i]][[2]][[1]]) %in% op){
            continue <- TRUE
            additional_op = as.character(temp_current[[i]][[2]][[1]])
            temp_current[[i]] <- c(temp_current[[i]][[2]][[2]], temp_current[[i]][[2]][[3]])
            temp_current <- unlist(temp_current)
            
            ops <- as.list(ops)
            if(i > length(ops)) {
              ops <- c(ops, additional_op)
            }else{
              ops[[i]] <- list(additional_op, ops[[i]])
            }
            ops <- ops %>% unlist()
          }
    }
  }
  
  if(return_op){
    list(terms = temp_current, ops = ops)
  }else{
    temp_current
  }
} # end of decompose_MatProd


#' compose MatProd
#' 
#' @examples
#' # example code
#' terms <- decompose_MatProd("A%*%B%*%C%*%D%*%E", "%*%")
#' compose_MatProd(terms, "%*%")
#' 
#' terms <- decompose_MatProd("A%*%B%*%C%*%D%*%E", "*")
#' compose_MatProd(terms, "*")
#' 
#' terms <- decompose_MatProd("a+b-c+d+e", c("-", "+"), TRUE)
#' compose_MatProd(terms$terms, terms$ops)
#' compose_MatProd(terms)
#' 
#' terms <- decompose_MatProd("a+b-(c+d)+e", c("-", "+"), TRUE)
#' compose_MatProd(terms$terms, terms$ops)
#' compose_MatProd(terms)
#' 
#' 

compose_MatProd <- function(terms, op){
  if(!is.null(terms$ops) & missing(op)){
    op <- terms$ops
  }
  if(!is.null(terms$terms)){
    terms <- terms$terms
  } 
  
  if(length(op) == 1){
    op <- rep(op, length(terms)-1)
  }
  if(length(op) != (length(terms)-1))
    stop("length(op) must be 1 or length(terms)-1")
  
  
  past_terms <- terms
  past_op <- op
  while(length(past_terms) != 1){
    past_terms <-
      c(
        call(past_op[1], past_terms[[1]],  past_terms[[2]]),
        past_terms[-(1:2)])
    past_op <- past_op[-1]
    # print(past_terms);print(past_op)
  }
  
  return(past_terms[[1]])
}


#' Transpose expr
#' 
#' @examples
#' library(tidyr)
#' easy_parse("X") %>%  transpose_expr
#' easy_parse("t(X)") %>%  transpose_expr
#' easy_parse("t(t(X)%*%B)") %>%  transpose_expr
#' easy_parse("(t(t(X)%*%B))") %>%  transpose_expr
#' 
#' @export
#' 

transpose_expr <- function(expr){
  if(is.call(expr)){
    if(expr[[1]] == "t"){
      return(expr[[2]])
    }
  }
  return(call("t", expr))
} # end of transpose_expr


#' drop parens
#' 
#' @examples
#' # example code
#' drop_parens("X")
#' drop_parens("((X))")
#' drop_parens("(t((X)))")
#' drop_parens("(t((tr((X)))))")
#' 
#' 
#' @export 
#' 

drop_parens <- function(expr){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  if(is.call(expr)){
    if(expr[[1]] == "("){
      expr <- drop_parens(expr[[2]])
    }else if(length(expr) == 2){
      # ignore unary operator 
      expr[[2]] <- drop_parens(expr[[2]])
    }
  }
  return(expr)
}




#' reduce_sign in expression
#' 
#' @examples
#' reduce_expr_sign("A+B+-C")
#' reduce_expr_sign("A+B+C")
#' reduce_expr_sign("-A+B+C")
#' reduce_expr_sign("A")
#' reduce_expr_sign("-A-+B")
#' reduce_expr_sign("-A--B")
#' reduce_expr_sign("-A+-B")
#' reduce_expr_sign("-A++B")
#' 
#' 

reduce_expr_sign <- function(expr){
  temp <- decompose_MatProd(expr, op = c("+", "-"), return_op = TRUE)
  # compose_MatProd(temp)
  
  if(length(temp$terms) == 1) return(temp$terms[[1]])
  for(i in 2:length(temp$terms)){
    if(is.call(temp$terms[[i]])){
      if(as.character(temp$terms[[i]][[1]]) %in% c("+", "-")){
        sign_temp <- temp$terms[[i]][[1]]
        temp$terms[[i]] <- temp$terms[[i]][[2]]
        temp$ops[[i - 1]] <- ifelse(temp$ops[[i - 1]]==sign_temp, "+", "-")
      }
    }
  }
  
  compose_MatProd(temp)
}



#' gsub for expr
#'
#'
gsub_expr <- function(expr, object, replacement){
  
  for(expr_name in c("expr", "object", "replacement")){
    expr_temp <- eval(parse(text=expr_name))
    if (is.character(expr_temp)){
      assign(expr_name, 
             tryCatch(parse(text = expr_temp)[[1]], error = function(e) {
               warning(glue::glue("{expr_name}への入力が有効な R 式ではありません"))
               return(NULL)
             })
      )
    }
  }
  
  if(is.call(expr)){
    N <- length(expr)
    for(i in 2:N){
      expr[[i]] <- gsub_expr(expr[[i]], object, replacement)
    }
  }
  
  if(expr == object){
    expr <- replacement
  }else{
    expr <- expr
  }
  return(expr)
}# end of gsub_expr




#' Simplify Power
#'
#' @note Currently, nested expressions may not be handled correctly in some cases, so caution is advised.
#'
#' @export
#'

simplify_power <- function(expr){
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  powerize <- function(expr, op){
    temp_current <- decompose_MatProd(expr, op)
    symbols_past <- as.character(temp_current)
    
    if(op =="%*%"){
      length_encoding <- rle(symbols_past)
    }else if(op == "*"){
      tbl_symbol <- table(symbols_past)
      length_encoding <- list(lengths = paste0("(", tbl_symbol, ")"), values = names(tbl_symbol))
    }
    temp_factors <- paste0(length_encoding$values,  "^", length_encoding$lengths)
    temp_factors <- gsub("\\^\\(*1\\)*", "", temp_factors)
    expr_str <- paste(temp_factors, collapse = op)
    
    parse(text=expr_str)[[1]]
  }
  
  powerize(powerize(expr, "*"), "%*%")
} # end of simplify_power


#' Reorder Tracet
#'
#'
#' @export
#'
#'
#'

trace_reorder <- function(expr, X_, op=c("both", "%*%", "*"), attr = FALSE){
  # X_ become most right side
  op = match.arg(op)
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  transposed = FALSE
  
  if(is.call(expr)){
    exchange_ops <- c("t")
    ignore_ops <- c("(", "tr")
    ignore_op <- NULL
    
    if(as.character(expr[[1]]) %in% ignore_ops){
      ignore_op <- as.character(expr[[1]])
      expr <- expr[[2]]
    }
    expr <- drop_parens(expr)
    
    
    grepl("C", as.character(expr))
    as.character(expr) %in% "C"
    # if(deparse(expr) %in% paste0("t(", X_, ")"))
    
    
    if(op == "both" & is.call(expr)){
      if(as.character(expr[[1]]) %in% c("%*%", "*")){
        op = as.character(expr[[1]])
      }
    }
    if(op == "both") op = "%*%"
    
    temp_current <- decompose_MatProd(expr, op, target_X = X_) 
    N <- length(temp_current)
    symbols_current <- as.character(temp_current)
    
    if(any(grepl(X_, symbols_current))){
      # return(glue::glue("{expr_str} + {X_}"))
      # #########################################################################
      X_index <- which(grepl(paste("\\b",X_,"\\b", sep=""), symbols_current)) ###
      #    if( debug ) printm(symbols_current,X_index) #### 20250706cot
      X_index=X_index[1] #### 20250706cot
      ###########################################################################
      #
      
      # if(op == "%*%"){
      target <- temp_current[[X_index]]
      
      # is target transpose 
      # if(deparse(target)  %in% paste0("t(", X_, ")")){
      if(deparse(target) == paste0("t(", X_, ")")){
        target <- transpose_expr(target)
        transposed = TRUE
      }else if(N>1){
        # reorder target factor
        target_temp <- trace_reorder(target, X_, op = "*", attr = TRUE)
        target <- target_temp$expr
        transposed <- target_temp$transposed
        temp_current[[X_index]] <- target
      }
      
      
      # process transpose
      if(transposed){
        # symbols_current_temp <- gsub("t\\(t\\((.+)\\)\\)", "\\1",paste0("t(", symbols_current, ")"))
        # symbols_current_temp[[X_index]] <- deparse(target)
        # symbols_current <- symbols_current_temp[N:1]
        temp_current <- lapply(temp_current, transpose_expr)
        temp_current[[X_index]] <- target
        temp_current <- temp_current[N:1]
        X_index <- N-X_index+1
      }
      # }
      
      if(X_index == N){
        expr <- compose_MatProd(temp_current, op)
        # expr_str <- paste0(symbols_current, collapse = op)
        # expr <- parse(text = expr_str)[[1]]
      }else{
        
        expr <- compose_MatProd(temp_current[c((X_index+1):length(symbols_current), 1:X_index)], op)
        # expr_str <- paste0(symbols_current[c((X_index+1):length(symbols_current), 1:X_index)], collapse = op)
        # expr <- parse(text = expr_str)[[1]]
      }
    }
    
    if(!is.null(ignore_op)){
      expr <- call(ignore_op, expr)
    }
  }
  
  if(attr){
    return(list(expr=expr, transposed = transposed))
  }else{
    return(expr)
  }
} # end of general_reorder

