
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
}

simplify_power <- function(expr){
  
  op <- "%*%"
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  temp_current <- decompose_MatProd(expr, op)
  symbols_past <- as.character(temp_current)
  
  length_encoding <- rle(symbols_past)
  temp_factors <- paste0(length_encoding$values,  "^", length_encoding$lengths)
  temp_factors <- gsub("\\^1", "", temp_factors)
  expr_str <- paste(temp_factors, collapse = op)
  
  parse(text=expr_str)[[1]]
}

trace_reorder <- function(expr, X_, op = "%*%"){
  # X_ become most right side
  
  
  if(is.character(expr))
  expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })
  
  temp_current <- decompose_MatProd(expr, op)
  symbols_current <- as.character(temp_current)
  
  if(any(grepl(X_, symbols_current))){
    # return(glue::glue("{expr_str} + {X_}"))
    X_index <- which(grepl(X_, symbols_current))
    
    if(X_index == length(symbols_current)){
      return(expr)
    }
    expr_str <- paste0(symbols_current[c((X_index+1):length(symbols_current), 1:X_index)], collapse = op)
    expr <- parse(text = expr_str)[[1]]
    return(expr)
  }else{
    return(expr)
  }
}

Dm_core <- function(expr_str, X_, deparse_result = FALSE){
  
  expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
  expr <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })
  result <- NULL
  
  expr_var <- parse(text= X_)[[1]]
  
  invs <- paste0(c("inv", "ginv"), "(", X_, ")")
  
  # S1 & S2
  
  if(is.call(expr)){
    if(as.character(expr[[1]]) == "tr"){
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
          p <- most_right[[3]]
          if(is.symbol(p)){
            most_right[[3]] <- call("-", p, 1)
          }else{
            most_right[[3]] <- p-1
          }
          res_str <- glue::glue("{p}*({deparse(most_right)} * t({other_left}))")
          result <- parse(text = res_str)[[1]]
        }else if(grepl(X_, deparse(most_right)) | grepl("*", deparse(most_right))){
          #S6
          if(most_right[[1]] == "(")
            most_right <- most_right[[2]]
          # 仇マール積は交換可能
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
        res_str <- glue::glue("det({X_})%*%inv(t({X_}))")
        result <- parse(text = res_str)[[1]]
      }
      
    } else {
      #S1
      if(!any(deparse(expr[[2]]) %in% X_))
        result <- as.symbol("O")
    }
  }
  
  
  if(is.null(result)) result <- "undefined"
  
  if(deparse_result){
    result <- deparse(result)
  }
  return(result)
  
}

