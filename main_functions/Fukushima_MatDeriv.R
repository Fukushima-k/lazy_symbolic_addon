#' Decompose Matrix Product
#'
#' @note Currently, nested expressions may not be handled correctly in some cases, so caution is advised.
#'
#' @export
#'

decompose_MatProd <- function(expr, op){
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
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
        if(expr_temp[[1]] == op) {
          temp_current <- c(temp_current, expr_temp[[2]],  expr_temp[[3]])
          continue <-TRUE
        }else{
          temp_current <- c(temp_current, expr_temp)
        }
      }
    }
    temp_past <- temp_current
    # temp_current %>% print()
  }
  
  temp_current
} # end of decompose_MatProd


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

trace_reorder <- function(expr, X_, op = "%*%"){
  # X_ become most right side
  
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  temp_current <- decompose_MatProd(expr, op)
  symbols_current <- as.character(temp_current)
  N <- length(symbols_current)
  
  if(any(grepl(X_, symbols_current))){
    # return(glue::glue("{expr_str} + {X_}"))
    X_index <- which(grepl(X_, symbols_current))
   
    # process transpose
    target <- temp_current[[X_index]]
    if(is.call(target)){
      if(target[[1]]=="t"){
        # symbols_current_temp <- paste0("t(", symbols_current, ")")
        symbols_current_temp <- gsub("t\\(t\\((.+)\\)\\)", "\\1",paste0("t(", symbols_current, ")"))
        symbols_current <- symbols_current_temp[N:1]
        X_index <- N-X_index+1
      }
    }
    
    if(X_index == N){
      expr_str <- paste0(symbols_current, collapse = op)
      expr <- parse(text = expr_str)[[1]]
    }else{
      expr_str <- paste0(symbols_current[c((X_index+1):length(symbols_current), 1:X_index)], collapse = op)
      expr <- parse(text = expr_str)[[1]]
    }
    return(expr)
  }else{
    return(expr)
  }
} # end of trace_reorder


#' Core Function of the Symbolic Derivative of Trace w.r.t a Matrix
#'
#' @param expr_str scalar function of a matrix argument
#' @param X A matrix variable with respect to which the derivative is taken
#' @param deparse_result = TRUE
#'
#' @export
#'

Dm_core <- function(expr, X_, deparse_result = FALSE){
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  result <- NULL
  
  expr_var <- parse(text= X_)[[1]]
  
  invs <- paste0(c("inv", "ginv"), "(", X_, ")")
  sums <- c("+", "-")
  prods <- c("*")
  
  if(is.call(expr)){
    
    #S1
    if(!grepl(X_, deparse(expr[[2]]))){
        result <- as.symbol("O")
    # 一般公式
    # 23
    }else if(as.character(expr[[1]]) %in% sums){
      expr[[2]] <-Dm_core(expr[[2]], X_)
      expr[[3]] <-Dm_core(expr[[3]], X_)
      result <- expr
    # 25
    }else if(as.character(expr[[1]]) %in% prods){
      op <- as.character(expr[[1]])
      lhand_deriv <-Dm_core(expr[[2]], X_) %>% deparse
      rhand_deriv <-Dm_core(expr[[3]], X_) %>% deparse
      lhand <- expr[[2]] %>% deparse
      rhand <- expr[[3]] %>% deparse
      res_str <-  glue::glue("{lhand_deriv} * {rhand} + {rhand_deriv} * {lhand}")
      result <- parse(text = res_str)[[1]]
    }else if(as.character(expr[[1]]) %in% "exp"){
      result <- call("*", 
                     expr,
                     Dm_core(expr[[2]], X_))
      
    }else if(as.character(expr[[1]]) %in% "log"){
      res_str <- glue::glue("1/({deparse(expr[[2]])}) * {Dm_core(expr[[2]], X_, deparse_result = TRUE)}")
      result <- parse(text = res_str)[[1]]
    # 特定公式
    }else if(as.character(expr[[1]]) == "tr"){
      expr[[2]] <- trace_reorder(expr[[2]], X_)
      if(as.character(expr[[2]][[1]]) == "%*%"){
        # トレース内最も右のファクター
        most_right <- expr[[2]][[3]]
        # それ以外の左のファクター
        other_left <- expr[[2]][[2]]
        if(most_right ==  expr_var){
          # S2
          result <- call("t", other_left)
        } else if(deparse(most_right) %in% invs){
          #S3.1 3.2
          target <- invs[invs %in% deparse(most_right)]
          res_str <- glue::glue("-t({target}%*% {deparse(other_left)} %*%{target})")
          result <- parse(text = res_str)[[1]]
        } else if(most_right[[1]] == "^"){
          # S7
          power <- most_right[[3]]
          if(is.call(power)){
            if(power[[1]]=="("){
              p <- power[[2]]
              if(is.symbol(p)){
                power[[2]] <- call("-", p, 1)
              }else{
                power[[2]] <- p-1
              }
              most_right[[3]] <- power
              res_str <- glue::glue("{p}*({deparse(most_right)} * t({other_left}))")
              result <- parse(text = res_str)[[1]]
            }
          } 
        }else if(grepl(X_, deparse(most_right)) | grepl("*", deparse(most_right))){
          #S6
          if(most_right[[1]] == "(")
            most_right <- most_right[[2]]
          # アダマール積は交換可能
          new_right <- trace_reorder(most_right, "X", "*")
          if(new_right[[1]] == "*"){
            right_in_right <- new_right[[3]]
            left_in_right <-  new_right[[2]]
            
            if(right_in_right == expr_var){
              res_str <- glue::glue("{deparse(left_in_right)} * t({other_left})")
              result <- parse(text = res_str)[[1]]
            }
          }
        }
      }
    }else if(as.character(expr[[1]]) == "det") {
      # S4
      if(expr[[2]] == expr_var){
        res_str <- glue::glue("det({X_})*inv(t({X_}))")
        result <- parse(text = res_str)[[1]]
      }
      
    } 
  }
  
  
  if(is.null(result)) result <- "undefined"
  
  if(deparse_result){
    result <- deparse(result)
  }
  return(result)
  
} # end of Dm_core

