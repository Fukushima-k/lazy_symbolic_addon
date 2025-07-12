#' Decompose Matrix Product
#'
#' @note Currently, nested expressions may not be handled correctly in some cases, so caution is advised.
#'
#' @examples
#' # example code
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "%*%")
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "*")
#' decompose_MatProd("a+b-c+d+e", c("-", "+"), TRUE)
#' decompose_MatProd("a+b-(c+d)+e", c("-", "+"), TRUE)
#' decompose_MatProd("a+b-+e", c("-", "+"), TRUE)
#' decompose_MatProd("a+b--e", c("-", "+"), TRUE)
#' decompose_MatProd("+a++b--c+-d-+e", c("-", "+"), TRUE)
#'
#' @export
#'

decompose_MatProd <- function(expr, op, return_op = FALSE){
  
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
    # #########################################################################
    X_index <- which(grepl(paste("\\b",X_,"\\b", sep=""), symbols_current)) ###
    #    if( debug ) printm(symbols_current,X_index) #### 20250706cot
    X_index=X_index[1] #### 20250706cot
    ###########################################################################
    #
    # process transpose
    target <- temp_current[[X_index]]
    if(is.call(target)){
      if(target[[1]]=="t"){
        # symbols_current_temp <- paste0("t(", symbols_current, ")")
        symbols_current_temp <- 
          gsub("t\\(t\\((.+)\\)\\)", "\\1",paste0("t(", symbols_current, ")"))
        symbols_current <- symbols_current_temp[N:1]
        X_index <- N-X_index+1
      }
    }
    
    if(X_index == N){
      expr_str <- paste0(symbols_current, collapse = op)
      expr <- parse(text = expr_str)[[1]]
    }else{
      expr_str <- 
        paste0(symbols_current[c((X_index+1):length(symbols_current), 1:X_index)], 
               collapse = op)
      expr <- parse(text = expr_str)[[1]]
    }
    return(expr)
  }else{
    return(expr)
  }
} # end of trace_reorder



FreeQ <- function(expr, varname) {
  # check if expr_str contains varname
  # Shin-ichi Mayekawa with ChatGPT
  # 20250706cot
  
  # Args:
  #  expr: 式の文字列
  #  varname: 文字列で指定された変数名（例: "A1"）
  #
  
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  found <- FALSE
  
  find_var <- function(e) {
    
    # exit
    if (found) return(NULL)
    
    if (is.symbol(e)) {
      if (as.character(e) == varname) {
        found <<- TRUE
      }
    } else if (is.call(e) || is.language(e)) {
      for (i in seq_along(e)) {
        find_var(e[[i]])
      }
    }
    return(NULL)
    
  } # end of find_var
  
  find_var(expr)
  
  return(!found)
  
} # end of FreeQ


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


mD0 <- function( expr, X_="X", print=1, debug=0 ){
  # product rule for trace
  # Shin-ichi Mayekawa
  # 20250705cot,06cot,07,08
  #
  
  # Given a matrix A which does not contain X,
  # mD0 knows mD0( tr(A%*%X), X )  or  mD0( tr(A%*%inv(X)), X ).
  #
  # When A contains X, we must use the matrix product rule:
  #  mD0( tr(A(X)%*%X), X ) = mD0( tr(A(Xc)%*%X), X ) + mD0( tr(A(X)%*%Xc), X )
  # where Xc is treated as a constant free of X.
  # The first term is easy because A(Xc) does not contain X.
  # For the 2nd term,
  # suppose A(X) is of the form B%*%X or B%*%inv(X) where B does not contain X.
  # Then, mD0 knows mD0( tr(A(X)%*%Xc),X ) and accordingly, mD0(tr(A(X)%*%X),X).
  #
  
  
  
  
  if (is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  expr_var <- parse(text = X_)[[1]]
  
  result <- NULL
  
  if (is.call(expr)) {
    # distribute mD0 as mD0(A+B,X) = mD0(A,X) + mD0(B,X)
    # rule (23)
    sums <- c("+", "-")
    if (as.character(expr[[1]]) %in% sums) {
      cat("mD of sum is converted to sum of mDs.\n")
      expr[[2]] <- parse(text = mD0(expr[[2]], X_))[[1]]
      expr[[3]] <- parse(text = mD0(expr[[3]], X_))[[1]]
      if( debug ) show_ast(expr)
      res = deparse(expr)
      return(res)
    }
    
    
    prods <- c("*")
    if(as.character(expr[[1]]) %in% prods){
      op <- as.character(expr[[1]])
      lhand_deriv <- deparse(Dm_core(expr[[2]], X_))
      rhand_deriv <- deparse(Dm_core(expr[[3]], X_))
      lhand <- deparse(expr[[2]])
      rhand <- deparse(expr[[3]])
      res_str <-  glue::glue("{lhand_deriv} * {rhand} + {rhand_deriv} * {lhand}")
      expr <- parse(text = res_str)[[1]]
      if( debug ) show_ast(expr)
      
      res = deparse(expr)
      return(res)
    }

    
    if (as.character(expr[[1]]) == "tr") {
      #
      # Here, we must distribute tr as tr(A+B) = tr(A) + tr(B).
      # not yet ready.
      #
      
      # move X_ to the right most position  (t(X_) will be taken care of.)
      expr[[2]] <- trace_reorder(expr[[2]], X_)
      
      
      ################################################################
      ##### for Hadamar Product ######################################
      ################################################################
      # もう少し上手に書けるはず。
      # 要は、* を見つけてフラグを立てることと、
      # tr(B%*%(A*X)) の形にすること。
      #
      # expr cannot be:
      #  tr(t(X)*A)
      #
      
      # move X_ to the right most position  (t(X_) will be taken care of.)
      # ただし、 "(X*A)" や "(X%*%A)" は変わらないので要注意。
      expr[[2]] <- trace_reorder(expr[[2]], X_, op = "*")
      
      # change tr(A*B) or tr((A*B)) to tr(I%*%(A*B))
      exprstr = deparse(expr)
      exprstr = gsub(" ", "", exprstr)
      hp = 0
      if (regexpr("\\w+\\*\\w+", exprstr)[[1]] > 0) {
        if( debug ) printm("input:", exprstr)
        exprstr = gsub("^tr\\(\\(*(\\w+\\*\\w+)\\)*\\)$"
                       , "tr\\(I%*%\\(\\1\\)\\)", exprstr)
        #exprstr=gsub("))",")",exprstr, fixed=TRUE)
        if( debug ) printm("after:", exprstr)
        hp = 1
        expr = parse(text = exprstr)[[1]]
      }
      ################################################################
      ################################################################
      
      if (as.character(expr[[2]][[1]]) == "%*%") {
        # トレース内最も右のファクター
        most_right <- expr[[2]][[3]]
        ###########################################
        # remove ()          ######################
        ###########################################
        most_right <- drop_parens(most_right)
        
        mR = deparse(most_right)
        if (debug) printm(mR, X_, mR == X_, hp)
        
        # それ以外の左のファクター
        other_left <- expr[[2]][[2]]
        oL = deparse(other_left)
        oL = gsub(" ", "", oL)
        invs = c(paste0("inv(", X_, ")"), paste0("Inv(", X_, ")"))
        if (debug) printm(invs, mR %in% invs)
        
        
        
        # 3 cases exist. 
        # 1. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) -> TRUE
        # 2. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) ->FALSE
        # 3. FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) ->FALSE
        
        # 1. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) -> TRUE
        # right most factor does not contain X
        if (FreeQ(mR, X_)) {
          # S1: no X
          if (print) cat("S1: tr(A)\n")
          return(0)
        }
        
        
        # 2. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) ->FALSE
        if (FreeQ(other_left, X_)) {
          # most_right
          
          if (mR == X_ ||  mR %in% invs  ||  hp == 1) {
          # if (mR == X_  ||  hp == 1) {
          # Here mR contains X or inv(X) or Hadamar product.
          # We try to apply the specific rules here.
          # It will work if the other_left does not contain X_.
          # Otherwize, use the product rule.
          #
          
            # Here, other_left does not contain X or inv(X)
            if (mR == X_) {
              # S2
              if (print) cat("S2: tr(A%*%X)\n")
              res = paste0("t(", oL, ")")
            }
            else if (mR %in% invs) {
              # S3.1
              if (print) cat("S3.1: tr(A%*%inv(X))\n")
              res = paste0("-t(inv(", X_, ")%*%", oL, "%*%inv(", X_, "))")
            }
            else if (hp == 1) {
              # S6
              if (print) cat("S6: tr((A*X)%*%B)\n")
              # if( debug ) printm( deparse(most_right[[2]][[1]]) )
              AA = deparse(most_right[[2]]) #modified##############################################
              # if( debug ) printm(mR,oL, AA)
              # if( oL == "I" ) res=paste0("diag(",AA,")")
              # else res=paste0(AA,"*t(",oL,")")
              res = paste0(AA, "*t(", oL, ")")
            }
            return(res)
          }
          
          
          
          # Here, mR is not X_ nor inv(X_) but contains X_.
          # must use chain rule

          if (print) cat("C1: using chain rule...\n")

          expr1 = deparse(expr)
          if( debug ) printm(expr1)
          if (debug) printm(mR, oL)
          if (debug) show_ast(most_right)

          FX = deparse(most_right[[2]])

          if (debug) printm(FX)

          expr1 = gsub(FX, "FX", expr1, fixed = TRUE)
          expr1 = gsub(" ", "", expr1)

          if (debug) printm(expr1)

          res1 = mD0(expr1, "FX")

          if (debug) printm(res1)

          res1 = gsub(" ", "", res1)
          res1FX = paste0("tr(t(", res1, ")%*%", FX, ")")
          res1FX = gsub(" ", "", res1FX)

          if (debug) printm(res1FX)

          res = mD0(res1FX, X_)

          if (debug) printm(res)

          res1 = gsub("FX", FX, res, fixed = TRUE)
          res1 = gsub(" ", "", res1)

          if (debug) printm(res1)

          return(res1)

          # 
          # #     cat("\n*** chain rule not yet available.***\n")
          # #     res=paste0("mD0(",deparse(expr),", ",X_,")")
          # #     return( res )
          # 
          
          
          
          
          
          
          cat("\n*** the followling mD0 is not yet available.***\n")
          res=paste0("mD0(",deparse(expr),", ",X_,")")
          cat(res);cat("\n\n")
          return( res )

        } # end of specific rules and chani rules
        # 3. FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) ->FALSE
        else{
          
          
          # Here, mR is either X_, inv(X_) or Hadamar Prod
          # and other_left contains X_, therefor, both factors contain X_
          if (print) cat("P1: using product rule.....\n")
          if (debug) printm(mR, oL)
          if (debug) cat("the 2nd term of P1 is:", paste0("mD0(tr(oLc%*%", mR, "),", X_, ")"),"\n")
          if (debug) cat("*** processing the 1st term* ***\n")
          
          res1 = mD0(paste0("tr(oLc%*%", mR, ")"), X_)
          res11 = gsub("oLc", oL, res1, fixed = TRUE)
          res11 = gsub(" ", "", res11)
          
          if (debug) printm(res1, res11)
          if (debug) cat("*** processing the 2nd term* ***\n")
          
          res2 = mD0(paste0("tr(", oL, "%*%Xc)"), X_)
          res22 = gsub(" ", "", res2)
          res22 = gsub("Xc", mR, res2)
          
          if (debug) printm(res2, res22)
          
          res = paste0(res11, "+", res22)
          res = gsub("+-", "-", res, fixed = TRUE)
          
          if (debug) printm("final result", "/", res)
          return(res)
          
        } # end of product rule
        
      } # end of matrix product
      else{
        cat("\n*** Cannot differentiate the input expression.***\n")
        res = paste0("mD0(", deparse(expr), ", ", X_, ")")
        return(res)
        
      } # end of sorry!
      
    } # end of trace function
    else{
      
      # other scalar functions
      cat("\n**** Currently, trace is the only function available.***\n")
      res = paste0("mD0(", deparse(expr), ", ", X_, ")")
      return(res)
      
    }
    
  } # end of is.call(expr)
  else{
    # expr is not a call
    cat("\n**** expr does not have a scalar function of X.***\n")
    res = paste0("mD0(", deparse(expr), ", ", X_, ")")
    return(res)
    
    
  } # end of non-call
  
} # end of mD0


