
#' 
#' 
#' @export

mD0 <- function( expr, X_="X", trace_chain=1, debug=0){
  # product rule for trace
  # Shin-ichi Mayekawa
  # 20250705cot,06cot,07,08
  # modified by Dr.F
  # recursion depth: 20250714
  # add .tc to mD0 call: 20250713
  # add drop_parens safe_deparse and add I%*%: 20250712
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
  
  # the recursive depth
  depth=sys.nframe()
  # printm(depth)
  # if( depth > 1000 ){
  #   cat("\nerror1:(mD0) Too many recursive calls: Job Abandoned.\n\n")
  #   printm(expr)
  #   stop()
  # }
  
  # shortcut for trace_chain
  .tc=trace_chain
  
  
  
  if (is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  expr_var <- parse(text = X_)[[1]]
  
  result <- NULL
  
  
  
  expr_out <- drop_parens(expr)
  
  if(expr != expr_out){
    if (trace_chain) cat("drop parens: \n")
    if(debug) cat(paste0(safe_deparse(expr), " -> ", safe_deparse(expr_out), "\n"))
    expr <- expr_out
  }
  
  
  if (is.call(expr)) {
    # S1
    if(FreeQ(expr, X_)){
      # S1: no X
      if (trace_chain) cat("S1: tr(A)\n")
      return("O")
    }
    
    
    # distribute mD0 as mD0(A+B,X) = mD0(A,X) + mD0(B,X)
    # rule (23)
    sums <- c("+", "-")
    if (as.character(expr[[1]]) %in% sums) {
      if (trace_chain){
        cat("rule(23): mD0(A+B,X) = mD0(A,X) + mD0(B,X)\n")
        cat("mD of sum is converted to sum of mDs.\n")
      }
      expr[[2]] <- parse(text = mD0(expr[[2]], X_, trace_chain=.tc))[[1]]
      expr[[3]] <- parse(text = mD0(expr[[3]], X_, trace_chain=.tc))[[1]]
      if( debug ) show_ast(expr)
      res = safe_deparse(expr)
      return(res)
    }
    
    
    # rule (25)
    prods <- c("*")
    if(as.character(expr[[1]]) %in% prods){
      if (trace_chain) cat("rule(25): mD0(A*B,X) = mD0(A*Bc,X) + mD0(Ac*B,X)\n")
      op <- as.character(expr[[1]])
      lhand_deriv <- mD0(expr[[2]], X_, trace_chain=.tc)
      rhand_deriv <- mD0(expr[[3]], X_, trace_chain=.tc)
      lhand <- safe_deparse(expr[[2]])
      rhand <- safe_deparse(expr[[3]])
      res_str <-  glue::glue("{lhand_deriv} * {rhand} + {rhand_deriv} * {lhand}")
      expr <- parse(text = res_str)[[1]]
      if( debug ) show_ast(expr)
      
      res = safe_deparse(expr)
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
        if (trace_chain) cat("rule(24): the chain rule......")
        if(f1_op == "exp")
          df1_factor <- expr
        if(f1_op == "log")
          df1_factor <- call("/", 1, expr[[2]])
        
        df2_factor <- mD0(expr[[2]], X_, trace_chain=.tc)
        res = call("*", df1_factor, parse(text=df2_factor)[[1]])
        res = safe_deparse(res)
        return(res)
      }
      # else{
      #   if (trace_chain) cat("rule(26): the chain rule......")
      #   if (trace_chain) cat("In general, rule(26) is not yet available.")
      #   df1_factor <- parse(text="df1(f2)/df2")[[1]]
      #   df2_factor <- mD0(expr[[2]], X_)
      #   res = call("*", df1_factor, parse(text=df2_factor)[[1]])
      #   res = deparse(res)
      #   return(res)
      # }
    }
    
    
    
    if(0){
      # 開発中領域#########################################################################################
      
      if (trace_chain) cat("C1: using chain rule for all ...\n")
      
      expr1 = safe_deparse(expr)
      if( debug ) printm(expr1)
      if (debug) printm(oL, mR)
      if (debug) show_ast(most_right)
      
      N <- length(most_right)
      FX = safe_deparse(most_right[[N]])
      
      if(N>2 & !FreeQ(most_right[[2]], X_)){
        # C1yはF(X)以外にXが影響しているので使えない。
        cat("\n*** tr(A%*%(F(X)%*%G(X))) is not yet available.***\n")
        res=paste0("mD0(",safe_deparse(expr),", ",X_,")")
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
      res1 = mD0(expr1, "FX", trace_chain=.tc)
      if (debug) printm(res1)
      
      
      res1FX = paste0("tr(t(RES1)%*%", FX, ")")
      res1FX = gsub(" ", "", res1FX)
      
      if (debug) printm(res1FX)
      
      res = mD0(res1FX, X_, trace_chain=.tc)
      
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
      if(debug) cat("reordered trace: ", safe_deparse(expr), "\n\n")
      
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
        exprstr = safe_deparse(expr)
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
        
        mR = safe_deparse(most_right)
        if (debug) printm(mR, X_, mR == X_, hp)
        
        # それ以外の左のファクター
        other_left <- expr[[2]][[2]]
        oL = safe_deparse(other_left)
        oL = gsub(" ", "", oL)
        invs <- paste0(c("inv", "Inv", "ginv", "Ginv"), 
                       "(", X_, ")")
        if (debug) printm(invs, mR %in% invs)

        # Hadamar の特定公式フラグ        
        most_right_temp <- drop_parens(most_right)
        if(is.call(most_right_temp))
        if(length(most_right_temp)>2){
          if((as.character(most_right_temp[[1]]) %in% c("*", "%.%")) &  most_right_temp[[3]]==X_){
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
              if (trace_chain) cat("S2: tr(A%*%X)\n")
              res = paste0("t(", oL, ")")
            }
            else if (mR %in% invs) {
              # S3.1 S3.2 
              if (trace_chain) cat("S3.1 or 3.2: tr(A%*%inv(X))\n")
              inv_X <- invs[invs %in% mR]
              # res_str <- glue::glue("-t({target}%*% {deparse(other_left)} %*%{target})")
              # result <- parse(text = res_str)[[1]]
              res = paste0("-t(", inv_X, "%*%", oL, "%*%", inv_X,")")
            }
            else if (hp == 1) {
              # S6
              if (trace_chain) cat("S6: tr((A*X)%*%B)\n")
              # if( debug ) printm( deparse(most_right[[2]][[1]]) )
              AA = safe_deparse(most_right[[2]]) #modified##############################################
              # if( debug ) printm(mR,oL, AA)
              # if( oL == "I" ) res=paste0("diag(",AA,")")
              # else res=paste0(AA,"*t(",oL,")")
              hp_op <- as.character(most_right[[1]])
              res = paste0(AA, hp_op, "t(", oL, ")")
            }
            return(res)
          }
          
          expr1 = safe_deparse(expr)
          N <- length(most_right)
          if(N>2 & !FreeQ(most_right[[2]], X_) & most_right[[1]]=="*"){
            if  (trace_chain) cat("P2: using product rule...\n")
            
            
            depth=sys.nframe()
            # depth = ""
            GXplaceholder <- glue::glue("G_X{depth}")
            FXplaceholder <- glue::glue("F_X{depth}")
            
            # expr1 = safe_deparse(expr)
            if( debug ) printm(expr1)
            if (debug) printm(oL, mR)
            if (debug) show_ast(most_right)
            
            FX = safe_deparse(most_right[[2]])
            GX = safe_deparse(most_right[[3]])
            
            term1 <- gsub_expr(expr, GX, GXplaceholder)
            term2 <- gsub_expr(expr, FX, FXplaceholder)
            
            if(debug){
              printm(expr, term1, term2)
            }
            
            dterm1 <- mD0(term1, X_, trace_chain=.tc)
            dterm2 <- mD0(term2, X_, trace_chain=.tc)
            
            res_temp <- parse(text = glue::glue("{dterm1}+{dterm2}"))[[1]]
            res_temp
            res_temp <- gsub_expr(res_temp, GXplaceholder, GX)
            res_temp <- gsub_expr(res_temp, FXplaceholder, FX)
            
            res <- reduce_expr_sign(res_temp)
            return(safe_deparse(res))
          }
          # Here, mR is not X_ nor inv(X_) but contains X_.
          # must use chain rule

          if (trace_chain) cat("C1: using chain rule...\n")

          expr1 = safe_deparse(expr)
          if( debug ) printm(expr1)
          if (debug) printm(oL, mR)
          if (debug) show_ast(most_right)

          N <- length(most_right)
          FX = safe_deparse(most_right[[N]])
          
          if(N>2 & !FreeQ(most_right[[2]], X_)){
            # C1はF(X)以外にXが影響しているので使えない。
            cat("\n*** tr(A%*%(F(X)%*%G(X))) is not yet available.***\n")
            res=paste0("mD0(",safe_deparse(expr),", ",X_,")")
            cat(res);cat("\n\n")
            return( res )
          }
          
          if (debug) printm(FX)
          
          depth=sys.nframe()
          # depth = ""
          mD_fFXplaceholder <- glue::glue("mD_fFX{depth}")
          FXplaceholder <- glue::glue("FX{depth}")
          
          fFX <- gsub_expr(expr, FX, replacement = FXplaceholder)

          if (debug) {
            fFX_str <- safe_deparse(fFX)
            printm(fFX_str)
            cat("mD0(f(F(X)), X) = mD0(tr(t(mD0(f(FX), FX)) %*% F(X)), X)\n")
            cat("f(FX) =", fFX_str, "\n")
            cat("F(X) =", FX, "\n\n")
          } 
          
          # mD0(f(FX), X)
          mD_fFX = mD0(fFX, FXplaceholder, trace_chain=.tc)
          if (debug) printm(mD_fFX)
          
          res1FX = easy_parse(paste0("tr(t(", mD_fFXplaceholder, ")%*%", FXplaceholder, ")"))
          res1FX = gsub_expr(res1FX, FXplaceholder, FX)
          
          if (debug) printm(safe_deparse(res1FX))
          
          res_temp = mD0(res1FX, X_, trace_chain=.tc)
          
          if (debug) printm(res_temp)
          
          
          res = gsub(mD_fFXplaceholder, mD_fFX, res_temp, fixed = TRUE)
          res = gsub(FXplaceholder, FX, res, fixed = TRUE)
          res = gsub(" ", "", res)
          
          if (debug) printm(res)
          
          
          res <- safe_deparse(cancel_double_expr(res))
          
          return(res)
          
          # 
          # #     cat("\n*** chain rule not yet available.***\n")
          # #     res=paste0("mD0(",safe_deparse(expr),", ",X_,")")
          # #     return( res )
          # 
          
          # cat("\n*** the followling mD0 is not yet available.***\n")
          # res=paste0("mD0(",safe_deparse(expr),", ",X_,")")
          # cat(res);cat("\n\n")
          # return( res )

        } # end of specific rules and chani rules
        # 2. FreeQ(oL, X_) ->FALSE; FreeQ(mR, X_) ->FALSE
        else{
          
          
          
          # Here, mR is either X_, inv(X_) or Hadamar Prod
          # and other_left contains X_, therefor, both factors contain X_
          if (trace_chain) cat("P1: using product rule.....\n")
          if (debug) cat("mD0(tr(oL %*% mR)) = mD0(tr(oL %*% mRc)) + mD0(tr(oLc %*% mR))\n")
          if (debug) printm(mR, oL)
          # first_term <- second_term <- call("mD0", expr, as.symbol(X_))
          # first_term[[2]][[2]][[3]] <- as.symbol("mRc")
          # second_term[[2]][[2]][[2]] <- as.symbol("oLc")
          first_term <- second_term <- expr
          first_term[[2]][[3]] <- as.symbol("mRc")
          second_term[[2]][[2]] <- as.symbol("oLc")
          if (debug) cat("the 1st term of P1 is: mD0(", safe_deparse(first_term), ",", X_, ")\n")
          if (debug) cat("the 2nd term of P1 is: mD0(", safe_deparse(second_term),",", X_, ")\n")
          # if (debug) cat("the 2nd term of P1 is:", paste0("mD0(tr(oLc%*%", mR, "),", X_, ")"),"\n")
          # if (debug) cat("the 2nd term of P1 is:", paste0("mD0(tr(oLc%*%", mR, "),", X_, ")"),"\n")
          if (debug) cat("*** processing the 1st term* ***\n")
          
          
          
          res1 = mD0(safe_deparse(first_term), X_, trace_chain=.tc)
          res11 = gsub_expr(res1, "mRc", mR)
          # res11 = gsub("mRc", mR, res1, fixed = TRUE)
          # res11 = gsub(" ", "", res11)
          
          if (debug) printm(res1, res11)
          if (debug) cat("*** processing the 2nd term* ***\n")
          
          res2 =  mD0(safe_deparse(second_term), X_, trace_chain=.tc)
          res22 = gsub_expr(res2, "oLc", oL)
          # res22 = gsub("oLc", oL, res2)
          # res22 = gsub(" ", "", res22)
          
          if (debug) printm(res2, res22)
          
          # res = paste0(res11, "+", res22)
          res = call("+", res11, res22)
          # res = gsub("+-", "-", res, fixed = TRUE)
          res = reduce_expr_sign(res)
          
          if (debug) printm("final result", "/", res)
          return(safe_deparse(res))
          
        } # end of product rule
        
      } # end of matrix product
      else{
        default_expr <-  safe_deparse(expr)
        expr[[2]] <- call("%*%", as.symbol("I"), expr[[2]])

        res <- mD0(expr, X_, trace_chain=.tc)
        if(!grepl("mD0", res)){
          if (trace_chain) cat("\nTechnic: add I %*% \n")
          if(debug){cat(glue::glue("{default_expr} -> {deparse(expr)}"));cat("\n\n") }
          res <- safe_deparse(reduce_expr_I(res))

          return(res)
        }
        
        cat("\n*** Cannot differentiate the input expression.***\n")
        res = paste0("mD0(", safe_deparse(expr), ", ", X_, ")")
        return(res)
        
      } # end of sorry!
      
    } # end of trace function
    else{
      
      # S4
      if (as.character(expr[[1]]) == "det"){
        if(expr[[2]] == expr_var){
          if (trace_chain) cat("S4 : mD0(det(X), X)")
          res_str <- glue::glue("det({X_})*inv(t({X_}))")
          # result <- parse(text = res_str)[[1]]
          return(res_str)
        }
      }
      
      
      # other scalar functions
      cat("\n**** Currently, trace and det are the only functions available.***\n")
      res = paste0("mD0(", safe_deparse(expr), ", ", X_, ")")
      return(res)
      
    }
    
  } # end of is.call(expr)
  else{
    # expr is not a call
    cat("\n**** expr does not have a scalar function of X.***\n")
    res = paste0("mD0(", safe_deparse(expr), ", ", X_, ")")
    return(res)
  } # end of non-call
  
} # end of mD0
