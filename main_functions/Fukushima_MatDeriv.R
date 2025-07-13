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






#' 
#' 
#' @export

mD0 <- function( expr, X_="X", print=1, debug=0){
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
    # S1
    if(FreeQ(expr, X_)){
      # S1: no X
      if (print) cat("S1: tr(A)\n")
      return("O")
    }
    
    
    # distribute mD0 as mD0(A+B,X) = mD0(A,X) + mD0(B,X)
    # rule (23)
    sums <- c("+", "-")
    if (as.character(expr[[1]]) %in% sums) {
      if (print) cat("rule(23): mD0(A+B,X) = mD0(A,X) + mD0(B,X)\n")
      cat("mD of sum is converted to sum of mDs.\n")
      expr[[2]] <- parse(text = mD0(expr[[2]], X_))[[1]]
      expr[[3]] <- parse(text = mD0(expr[[3]], X_))[[1]]
      if( debug ) show_ast(expr)
      res = deparse(expr)
      return(res)
    }
    
    
    # rule (25)
    prods <- c("*")
    if(as.character(expr[[1]]) %in% prods){
      if (print) cat("rule(25): mD0(A*B,X) = mD0(A*Bc,X) + mD0(Ac*B,X)\n")
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
    
    # eq. (26) f(g(X)) (24)
    N1 <- length(expr)
    if(is.call(expr[[2]])){
      N2 <- length(expr[[2]])
    }else N2 <- 1
    if(N1 == 2 & N2 == 2){
      f1_op <- as.character(expr[[1]])
      if(f1_op %in% c("exp", "log")){
        if (print) cat("rule(24): the chain rule......")
        if(f1_op == "exp")
          df1_factor <- expr
        if(f1_op == "log")
          df1_factor <- call("/", 1, expr[[2]])
        
        df2_factor <- mD0(expr[[2]], X_)
        res = call("*", df1_factor, parse(text=df2_factor)[[1]])
        res = deparse(res)
        return(res)
      }
      # else{
      #   if (print) cat("rule(26): the chain rule......")
      #   if (print) cat("In general, rule(26) is not yet available.")
      #   df1_factor <- parse(text="df1(f2)/df2")[[1]]
      #   df2_factor <- mD0(expr[[2]], X_)
      #   res = call("*", df1_factor, parse(text=df2_factor)[[1]])
      #   res = deparse(res)
      #   return(res)
      # }
    }
    
    
    
    if(0){
      # 開発中領域#########################################################################################
      
      if (print) cat("C1: using chain rule for all ...\n")
      
      expr1 = deparse(expr)
      if( debug ) printm(expr1)
      if (debug) printm(oL, mR)
      if (debug) show_ast(most_right)
      
      N <- length(most_right)
      FX = deparse(most_right[[N]])
      
      if(N>2 & !FreeQ(most_right[[2]], X_)){
        # C1yはF(X)以外にXが影響しているので使えない。
        cat("\n*** tr(A%*%(F(X)%*%G(X))) is not yet available.***\n")
        res=paste0("mD0(",deparse(expr),", ",X_,")")
        cat(res);cat("\n\n")
        return( res )
      }
      
      if (debug) printm(FX)
      
      expr1 = gsub(FX, "FX", expr1, fixed = TRUE)
      expr1 = gsub(" ", "", expr1)
      
      if (debug) printm(expr1)
      if (debug){
        cat("mD0(f(F(X)), X) = tr(t(mD0(f(FX), FX)) %*% F(X))\n")
        cat("f(FX) =", expr1, "\n")
        cat("F(X) =", FX, "\n\n")
      } 
      
      # mD0(f(FX), X)
      res1 = mD0(expr1, "FX")
      if (debug) printm(res1)
      
      
      res1FX = paste0("tr(t(RES1)%*%", FX, ")")
      res1FX = gsub(" ", "", res1FX)
      
      if (debug) printm(res1FX)
      
      res = mD0(res1FX, X_)
      
      if (debug) printm(res)
      
      res1 = gsub("RES1", res1, res, fixed = TRUE)
      res1 = gsub("FX", FX, res1, fixed = TRUE)
      res1 = gsub(" ", "", res1)
      
      if (debug) printm(res1)
      
      return(res1)
    }
    
    
    
    
    
    
    
    
      
    if (as.character(expr[[1]]) == "tr") {
      #
      # Here, we must distribute tr as tr(A+B) = tr(A) + tr(B).
      # not yet ready.
      #
      
      # move X_ to the right most position  (t(X_) will be taken care of.)
      expr[[2]] <- trace_reorder(expr[[2]], X_)
      if(debug) cat("reordered trace: ", deparse(expr), "\n\n")
      
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
      # expr[[2]] <- trace_reorder(expr[[2]], X_, op = "*")
      
      # change tr(A*B) or tr((A*B)) to tr(I%*%(A*B))
      hp = 0
      if(0){
        exprstr = deparse(expr)
        exprstr = gsub(" ", "", exprstr)
        if (regexpr("\\w+\\*\\w+", exprstr)[[1]] > 0) {
          if( debug ) printm("input:", exprstr)
          exprstr = gsub("^tr\\(\\(*(\\w+\\*\\w+)\\)*\\)$"
                         , "tr\\(I%*%\\(\\1\\)\\)", exprstr)
          #exprstr=gsub("))",")",exprstr, fixed=TRUE)
          if( debug ) printm("after:", exprstr)
          hp = 1
          expr = parse(text = exprstr)[[1]]
        } 
      } # 代わりに下の部分で評価
      
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
        invs <- paste0(c("inv", "Inv", "ginv", "Ginv"), 
                       "(", X_, ")")
        if (debug) printm(invs, mR %in% invs)

        # Hadamar の特定公式フラグ        
        most_right_temp <- drop_parens(most_right)
        if(is.call(most_right_temp))
        if(length(most_right_temp)>2){
          if(most_right_temp[[1]] == "*" &  most_right_temp[[3]]==X_){
            hp = 1
          }
        }
        
        # 2 cases exist. 
        # 1. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) ->FALSE
        # 2. FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) ->FALSE
        
        # FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) -> TRUE
        # 全体に含まれていないことはif(FreeQ(expr, X_))で確認済みなので不要
        # FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) -> TRUE
        # trace_reorderが正しく機能している限り、X_が含まれているなら必ずmR存在。
        
        # 1. FreeQ(oL, X_) -> TRUE; FreeQ(mR, X_) ->FALSE
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
              # S3.1 S3.2 
              if (print) cat("S3.1 or 3.2: tr(A%*%inv(X))\n")
              inv_X <- invs[invs %in% mR]
              # res_str <- glue::glue("-t({target}%*% {deparse(other_left)} %*%{target})")
              # result <- parse(text = res_str)[[1]]
              res = paste0("-t(", inv_X, "%*%", oL, "%*%", inv_X,")")
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
          
          expr1 = deparse(expr)
          N <- length(most_right)
          if(N>2 & !FreeQ(most_right[[2]], X_) & most_right[[1]]=="*"){
            if (print) cat("P2: using product rule...\n")
            
            # expr1 = deparse(expr)
            if( debug ) printm(expr1)
            if (debug) printm(oL, mR)
            if (debug) show_ast(most_right)
            
            FX = deparse(most_right[[2]])
            GX = deparse(most_right[[3]])
            
            expr
            term1 <- gsub_expr(expr, GX, "G_X")
            term2 <- gsub_expr(expr, FX, "F_X")
            
            if(debug){
              printm(expr, term1, term2)
            }
            
            dterm1 <- mD0(term1, X_)
            dterm2 <- mD0(term2, X_)
            
            res_temp <- parse(text = glue::glue("{dterm1}+{dterm2}"))[[1]]
            res_temp
            res_temp <- gsub_expr(res_temp, "F_X", FX)
            res_temp <- gsub_expr(res_temp, "G_X", GX)
            
            res <- reduce_expr_sign(res_temp)
            return(deparse(res))
          }
          # Here, mR is not X_ nor inv(X_) but contains X_.
          # must use chain rule

          if (print) cat("C1: using chain rule...\n")

          expr1 = deparse(expr)
          if( debug ) printm(expr1)
          if (debug) printm(oL, mR)
          if (debug) show_ast(most_right)

          N <- length(most_right)
          FX = deparse(most_right[[N]])
          
          if(N>2 & !FreeQ(most_right[[2]], X_)){
            # C1yはF(X)以外にXが影響しているので使えない。
            cat("\n*** tr(A%*%(F(X)%*%G(X))) is not yet available.***\n")
            res=paste0("mD0(",deparse(expr),", ",X_,")")
            cat(res);cat("\n\n")
            return( res )
          }
          
          if (debug) printm(FX)

          expr1 = gsub(FX, "FX", expr1, fixed = TRUE)
          expr1 = gsub(" ", "", expr1)

          if (debug) printm(expr1)
          if (debug){
            cat("mD0(f(F(X)), X) = tr(t(mD0(f(FX), FX)) %*% F(X))\n")
            cat("f(FX) =", expr1, "\n")
            cat("F(X) =", FX, "\n\n")
          } 
          
          # mD0(f(FX), X)
          res1 = mD0(expr1, "FX")
          if (debug) printm(res1)
          
          
          res1FX = paste0("tr(t(RES1)%*%", FX, ")")
          res1FX = gsub(" ", "", res1FX)
          
          if (debug) printm(res1FX)
          
          res = mD0(res1FX, X_)
          
          if (debug) printm(res)
          
          res1 = gsub("RES1", res1, res, fixed = TRUE)
          res1 = gsub("FX", FX, res1, fixed = TRUE)
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
        # 2. FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) ->FALSE
        else{
          
          
          
          # Here, mR is either X_, inv(X_) or Hadamar Prod
          # and other_left contains X_, therefor, both factors contain X_
          if (print) cat("P1: using product rule.....\n")
          if (debug) cat("mD0(tr(oL %*% mR)) = mD0(tr(oL %*% mRc)) + mD0(tr(oLc %*% mR))\n")
          if (debug) printm(mR, oL)
          # first_term <- second_term <- call("mD0", expr, as.symbol(X_))
          # first_term[[2]][[2]][[3]] <- as.symbol("mRc")
          # second_term[[2]][[2]][[2]] <- as.symbol("oLc")
          first_term <- second_term <- expr
          first_term[[2]][[3]] <- as.symbol("mRc")
          second_term[[2]][[2]] <- as.symbol("oLc")
          if (debug) cat("the 1st term of P1 is: mD0(", deparse(first_term), ",", X_, ")\n")
          if (debug) cat("the 2nd term of P1 is: mD0(", deparse(second_term),",", X_, ")\n")
          # if (debug) cat("the 2nd term of P1 is:", paste0("mD0(tr(oLc%*%", mR, "),", X_, ")"),"\n")
          # if (debug) cat("the 2nd term of P1 is:", paste0("mD0(tr(oLc%*%", mR, "),", X_, ")"),"\n")
          if (debug) cat("*** processing the 1st term* ***\n")
          
          
          
          res1 = mD0(deparse(first_term), X_)
          res11 = gsub_expr(res1, "mRc", mR)
          # res11 = gsub("mRc", mR, res1, fixed = TRUE)
          # res11 = gsub(" ", "", res11)
          
          if (debug) printm(res1, res11)
          if (debug) cat("*** processing the 2nd term* ***\n")
          
          res2 =  mD0(deparse(second_term), X_)
          res22 = gsub_expr(res2, "oLc", oL)
          # res22 = gsub("oLc", oL, res2)
          # res22 = gsub(" ", "", res22)
          
          if (debug) printm(res2, res22)
          
          # res = paste0(res11, "+", res22)
          res = call("+", res11, res22)
          # res = gsub("+-", "-", res, fixed = TRUE)
          res = reduce_expr_sign(res)
          
          if (debug) printm("final result", "/", res)
          return(deparse(res, width.cutoff = 500))
          
        } # end of product rule
        
      } # end of matrix product
      else{
        default_expr <-  deparse(expr)
        expr[[2]] <- call("%*%", as.symbol("I"), expr[[2]])

        res <- mD0(expr, X_)
        if(!grepl("mD0", res)){
          if(print) cat("\nTechnic: add I %*% \n")
          if(debug){cat(glue::glue("{default_expr} -> {deparse(expr)}"));cat("\n\n") }
          res <- deparse(reduce_expr_I(res))

          return(res)
        }
        
        cat("\n*** Cannot differentiate the input expression.***\n")
        res = paste0("mD0(", deparse(expr), ", ", X_, ")")
        return(res)
        
      } # end of sorry!
      
    } # end of trace function
    else{
      
      # S4
      if (as.character(expr[[1]]) == "det"){
        if(expr[[2]] == expr_var){
          if (print) cat("S4 : mD0(det(X), X)")
          res_str <- glue::glue("det({X_})*inv(t({X_}))")
          # result <- parse(text = res_str)[[1]]
          return(res_str)
        }
      }
      
      
      # other scalar functions
      cat("\n**** Currently, trace and det are the only function available.***\n")
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


gradma <- function (expr, ..., values = NULL, dexpr = NULL,sym = 0, ntogoback = 1, 
          print = 0, debug = 0) 
{
  temp = analyze_3d_val(values = values, debug = debug, ntogoback = ntogoback, 
                        ...)
  if (0) {
    namename = temp$namename
    nname = temp$nname
    pat2 = temp$pat2
    arg = temp$arg
    dimarg = temp$dimarg
    argval = temp$argval
  }
  arg = temp$arg
  namename = temp$namename
  nname = temp$nname
  pat2 = temp$pat2
  for (i in 1:nname) {
    code = paste(namename[i], "=pat2[[i]]", sep = "")
    if (debug >= 2) 
      printm(i, code)
    eval(parse(text = code))
  }
  vv = c("arg", "atd", "code", "debug", "expr", "expr0", "i", 
         "constants", "print", "name", "namename", "nname", "ntogoback", 
         "pat2", "vv", "values", "sym", "dimarg", "argval", "temp", 
         arg)
  const = setdiff(ls(), vv)
  const0 = paste(const, collapse = ", ")
  if (is.null(dexpr)) {
    # dexpr = Dm_core(expr, arg, deparse_result = 1)
    # dexpr = mD0(expr, arg)
    dexpr = paste0(mD0(expr, arg), collapse ="") # exprは長すぎると勝手に改行する。deparseしたらその改行で別れた文字列ベクトルになってしまう。
  }
  dexpr = gsub("inv", "Inv", dexpr)
  gradma = Eval(dexpr, values = values, ..., fullsymb = 1, 
                check = 0)
  if (sym) {
    gradma = gradma + t(gradma) - Diag(gradma)
  }
  if (print) {
    cat("\nInput expression \"", expr, "\" was analytically differentiated", 
        sep = "")
    cat(" with respect to ", arg, ".\n", sep = "")
    printm(dexpr)
    cat("The above expression was evaluated with the following values:\n")
    print(pat2)
    cat("The result, with sym =", sym, ", is\n")
    printm(gradma)
  }
  return(gradma)
}




show_ast <- function( expr, indent_char=" ", nindent=1 ) {
  # AST (abstruct syntax tree) の構造を表示する
  # Shin-ichi Mayekawa with ChatGPT
  # 20250706cot
  # title added: 20250707
  # when expr is an expression: 20250708
  # nindent: 20250708
  #
  
  # expression を変換
  if( is.expression(expr) ) expr=as.character(expr)
  # 文字列を式に変換
  if (is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  cat("AST of", deparse(expr), "\n")
  # 再帰的に表示
  recurse <- function(expr, indent_char = " ", nindent=1) {
    if (is.call(expr)) {
      cat(indent_char, "call: ", deparse(expr[[1]]), "\n")
      for (i in 2:length(expr)) {
        recurse(expr[[i]]
                , paste0(indent_char, substr(indent_char,1,nindent)), nindent=nindent)
      }
    } else if (is.symbol(expr)) {
      cat(indent_char, "symbol: ", as.character(expr), "\n")
    } else {
      cat(indent_char, "const: ", expr, "\n")
    }
  } # end of recurse
  
  recurse(expr, indent_char, nindent)
  
  # return( expr )
  
} # end of show_ast

