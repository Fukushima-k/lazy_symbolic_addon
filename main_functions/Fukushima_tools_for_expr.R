
#' easy_parse
#' 
#' @examples
#' easy_parse("X")
#' easy_parse("tr(X%*%B)")
#' 

easy_parse <- function(text){
  parse(text=text)[[1]]
} # end of easy_parse


#' safe_deparse
#' 
#' @note deparseをただすると、長いexprは文字列ベクトルになってしまうので、
#' 
#' @examples
#' 
#' 
#'  
safe_deparse <- function(expr){
  deparse(expr, width.cutoff = 500)
} # end of safe_deparse


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
#' decompose_MatProd("(A%*%X)%*%B", "%*%")
#' decompose_MatProd("(A%*%X)%*%B", "%*%", flat = TRUE)
#' decompose_MatProd("A%*%((X%*%B))", "%*%", flat = TRUE)
#' decompose_MatProd("A%*%(((((X))%*%B)))", "%*%", flat = TRUE)
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
    i_range <- NULL
    if(flat & missing(target_X)){
      # i <- 1
      # while(i <= length(temp_current)){
      i_range <- seq_along(temp_current)  
    }
    if(!missing(target_X)){
      i <-which(grepl(paste("\\b",target_X,"\\b", sep=""), as.character(temp_current)))
      i_range <- i[1] # i_range must be 1 length
    }
    if(!is.null(i_range))
    for(i in i_range){
      if(is.call(temp_current[[i]]))
      if(temp_current[[i]][[1]] == "(")
      if(is.call(temp_current[[i]][[2]])){
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
        else if(as.character(temp_current[[i]][[2]][[1]]) == "("){
            continue <- TRUE
            temp_current[[i]] <- temp_current[[i]][[2]]
        }
      }
      else if(is.symbol(temp_current[[i]][[2]])){
        continue <- TRUE
        temp_current[[i]] <- temp_current[[i]][[2]]
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
#' @param all TRUE if all parens should be removed from ast. FALSE if 二項演算子同士の順序関係を明示したい場合。
#' @param in_biop flag for recursive process. 
#' 
#' @examples
#' # example code
#' drop_parens("X")
#' drop_parens("(X)")
#' drop_parens("((X))")
#' drop_parens("(t((X)))")
#' drop_parens("(t((tr((X)))))") %>% show_ast()
#' drop_parens("(t((tr((X)))))") %>% show_ast()
#' drop_parens("(A*B)")
#' 
#' # examples for all
#' drop_parens("((A%*%(B*((C%*%C)))))") %>% show_ast()
#' drop_parens("((A%*%(B*((C%*%C)))))", all = T)%>% show_ast()
#' 
#' drop_parens("((A%*%(B*((C%*%C)))))") %>% show_ast()
#' drop_parens("((A%*%(B*((C%*%C)))))", all = T)%>% show_ast()
#' 
#' 
#' 
#' expr <- easy_parse("(X*A)%*%C") ; expr %>% show_ast
#' drop_parens(expr) %>% show_ast()
#' expr <- easy_parse("C%*%(X*A)") ; expr %>% show_ast 
#' drop_parens(expr) %>% show_ast()
#' expr <- easy_parse("(X%*%A)*C") ; expr %>% show_ast
#' drop_parens(expr) %>% show_ast()
#' expr <- easy_parse("C*(X%*%A)") ; expr %>% show_ast 
#' drop_parens(expr) %>% show_ast()
#' expr <- easy_parse("C%*%(X%*%A)") ; expr %>% show_ast 
#' drop_parens(expr) %>% show_ast()
#' expr <- easy_parse("(C%*%X)%*%A") ; expr %>% show_ast 
#' drop_parens(expr) %>% show_ast()
#' 
#' # かっこに対する考察
#' easy_parse("X%*%B%*%C") %>% show_ast()
#' easy_parse("(X%*%B)%*%C") %>% show_ast()
#' easy_parse("(X*B)%*%C") %>% show_ast()
#' easy_parse("X*B%*%C") %>% show_ast() # %*%が＊よりも優先度高い
#' 
#' second factor
#' expr <- easy_parse("A%*%C") ; expr %>% show_ast
#' expr[[3]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X%*%B)"); show_ast(expr)
#' 
#' first factor
#' expr <- easy_parse("A%*%C") ; expr %>% show_ast
#' expr[[2]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X%*%B)"); show_ast(expr)
#' 
#' つまり
#' 1. 構文木に代入する場合は、
#'   1.1. 左から順にの計算順序から変わる場合は、見た目上の()がつく。そうでない場合は()なし。
#'   1.2. ()を明示的に入れた場合はちゃんと実際の構文木上も現れる。
#' 2. 必要不要問わず、"()"つきをパースすると、必ず()が構文木に現れる。
#' 
#' expr <- easy_parse("A*C") ; expr %>% show_ast
#' expr[[2]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X%*%B)"); show_ast(expr)
#' 
#' expr <- easy_parse("A*C") ; expr %>% show_ast
#' expr[[3]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X%*%B)"); show_ast(expr)
#' 
#' expr <- easy_parse("A%*%C") ; expr %>% show_ast
#' expr[[2]] <- easy_parse("X*B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X*B)"); show_ast(expr)
#' 
#' expr <- easy_parse("A%*%C") ; expr %>% show_ast
#' expr[[3]] <- easy_parse("X*B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X*B)"); show_ast(expr)
#' 
#' expr <- easy_parse("(X*A)%*%C") ; expr %>% show_ast
#' expr <- easy_parse("C%*%(X*A)") ; expr %>% show_ast 
#' 
#' ということは、構文木上では一度かっこをほぼすべて外しても問題ない。（はず）
#' 
#' 
#' 
#' easy_parse("X%*%(B*C)") %>% show_ast() # %*%が＊よりも優先度高い
#' easy_parse("X%*%B*C") %>% show_ast() # %*%が＊よりも優先度高い
#' easy_parse("X%*%B*C") %>% show_ast() # %*%が＊よりも優先度高い
#' 
#' 
#' @export 
#' 

drop_parens <- function(expr, all = FALSE, in_biop = FALSE){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  if(is.call(expr)){
    if(expr[[1]] == "("){
      if(in_biop & !all) { 
        # 温存
        expr_in <- expr[[2]]
        if(is.call(expr_in)){
          if(length(expr_in) == 3){
            expr_in[[2]] <- drop_parens(expr_in[[2]], in_biop = TRUE, all = all)
            expr_in[[3]] <- drop_parens(expr_in[[3]], in_biop = TRUE, all = all)
            expr[[2]] <- expr_in
            return(expr)
          }
        }
        
        return(drop_parens(expr[[2]], in_biop = TRUE, all = all))
      }
      return(drop_parens(expr[[2]], all = all))
      
    }else if(length(expr) == 2){
      # ignore unary operator 
      expr[[2]] <- drop_parens(expr[[2]], all = all)
    }else if(length(expr) == 3){
      expr[[2]] <- drop_parens(expr[[2]], in_biop = TRUE, all = all)
      expr[[3]] <- drop_parens(expr[[3]], in_biop = TRUE, all = all)
    }
  }
  return(expr)
}



# in_biop = T all = T !all= F -> 全外し
# in_biop = F all = T !all= F -> 全外し
# in_biop = T all = F !all= T -> 一部残し　
# in_biop = F all = F !all= T -> 全外し



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

#' reduce I in expr
#'
#' @examples
#' # example code
#' reduce_expr_I("A%*%I")
#' reduce_expr_I("A*I")
#' reduce_expr_I("I%*%A")
#' reduce_expr_I("I%*%I")
#' reduce_expr_I("A%*%CB%*%I%*%D*E")
#' reduce_expr_I("A%*%((CB%*%I)%*%D)*E")
#' reduce_expr_I("A%*%C%*%B%*%I%*%I*E")
#' 
#' reduce_expr_I("t(t(-t(inv(B%*%X)%*%t(t(-t(inv(t(inv(B%*%X)))%*%t(t(t(I)))%*%inv(t(inv(B%*%X))))))%*%inv(B%*%X)))%*%B)")
#' grep_expr("t(t(-t(inv(B%*%X)%*%t(t(-t(inv(t(inv(B%*%X)))%*%t(t(t(I)))%*%inv(t(inv(B%*%X))))))%*%inv(B%*%X)))%*%B)")
#' 
#' @export
#' 

reduce_expr_I <- function(expr){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  
  info_I <- grep_expr(expr, "I")
  if(length(info_I)==0) return(expr) 
  info_I <- info_I[[1]]
  if(is.null(info_I$parent)) return(expr)
  
  if((as.character(info_I$parent[[1]]) %in% c("%*%"))){
    depth <- length(info_I$path)
    target_path <- info_I$path[-depth]
    I_path <- info_I$path[depth]
    remain <- info_I$parent[[(2:3)[2:3!=I_path]]]
    expr <- assign_at_expr(expr, target_path, remain)
    expr <- reduce_expr_I(expr)
    return(expr)
    # expr <- reduce_expr_I(expr)
  }

  return(expr)
}



#' t(t(A)) -> A in expr
#'
#' @examples
#' example code
#' cancel_double_expr("t(t(A))")
#' cancel_double_expr("t(A)")
#' cancel_double_expr("A")
#' cancel_double_expr("t(t(t(A)))")
#' cancel_double_expr("t(t(t(A))*B)")
#' cancel_double_expr("t(t(t(t(A))*t(B)))")
#' cancel_double_expr("t(t(A))*inv(inv(t(B)))")
#' cancel_double_expr("inv(inv(t(B)))")
#' cancel_double_expr("t(I)", sym = "S")
#' cancel_double_expr("t(I)%*%B%*%t(S)%*%inv(S)", sym = "S")
#' cancel_double_expr("t(I)%*%B%*%t(S)%*%inv(S) * inv(I)", sym = "S")
#' 
#' @export
#' 

cancel_double_expr <- function(expr, sym, inv){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  sym_mats <- c("I")
  inv_mats <- c("I")
  
  double_op <- c("t", "inv")
  
  if(!missing(sym)) sym_mats <- c(sym_mats, sym)
  if(!missing(inv)) inv_mats <- c(inv_mats, inv)
  
  info_double_list <- grep_expr(expr, double_op)
  # info_double_list <- grep_expr(expr, "t")
  if(length(info_double_list)==0) return(expr) 
  for(i in seq_along(info_double_list)){
    
    info_double <- info_double_list[[i]]
    if(is.null(info_double$parent)) return(expr)
    
    target_op <- as.character(info_double$match)
    if(is.call(info_double$parent[[2]]))
      if((as.character(info_double$parent[[2]][[1]]) %in% target_op)){
        
        depth <- length(info_double$path)
        target_path <- info_double$path[-depth]
        remain <- info_double$parent[[2]][[2]]
        expr <- assign_at_expr(expr, target_path, remain)
        # expr <- cancel_double_expr(expr)
        expr <- cancel_double_expr(expr, sym = sym_mats, inv=inv_mats)
        return(expr)
        # expr <- reduce_expr_I(expr)
      }
    
    # t(S) = S  inv(I) = Iの処理
    sym_logic <- (target_op == "t"   )&&( safe_deparse(info_double$parent[[2]]) %in% sym_mats)
    inv_logic <- (target_op == "inv" )&&( safe_deparse(info_double$parent[[2]]) %in% inv_mats)
    if(sym_logic || inv_logic){
      
      depth <- length(info_double$path)
      target_path <- info_double$path[-depth]
      remain <- info_double$parent[[2]]
      # expr; assign_at_expr(expr, target_path)
      expr <- assign_at_expr(expr, target_path, remain)
      expr <- cancel_double_expr(expr, sym = sym_mats, inv=inv_mats)
      return(expr)
    }
  }
  
  return(expr)
}





#' grep for expr 
#' 
#' @examples 
#' 
#' expr <- "(tr(A %*% B) + A + t(C))"
#' grep_expr(expr, "A")
#' 
#' grep_expr(expr, "t(C)")
#' grep_expr(expr, "A%*%B")
#' 
#' grep_expr(expr, "+")
#' grep_expr(expr, "(")
#' grep_expr(expr, "t")
#' 
#' @export
#' 

grep_expr <- function(expr, varname) {
  
  # for(expr_name in c("expr", "varname")){
  #   expr_temp <- eval(parse(text=expr_name))
  #   if (is.character(expr_temp)){
  #     assign(expr_name, 
  #            tryCatch(parse(text = expr_temp)[[1]], error = function(e) {
  #              warning(glue::glue("{expr_name}への入力が有効な R 式ではありません"))
  #              return(NULL)
  #            })
  #     )
  #   }
  # }
  # 
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
  
  if(is.character(varname))
    varname <- lapply(varname, function(expr)tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    }))
  varname_post_parsed_str <- sapply(varname, deparse)
  
  matches <- list()
  
  find_var <- function(e, path = NULL, parent = NULL) {
    # 正確に一致（括弧含む式、関数呼び出し、演算も可）
    if ((paste0(deparse(e),collapse="") %in% varname_post_parsed_str)) {
      matches[[length(matches) + 1]] <<- list(
        path = path,
        parent = parent,
        match = e
      )
    }
    
    # 再帰探索（symbol は飛ばす）
    if (is.call(e)) {
      for (i in seq_along(e)) {
        find_var(e[[i]], c(path, i), e)
      }
    }
  }
  
  find_var(expr)
  return(matches)
}



#' assigne new expr at path
#' 
#' @examples
#' 
#' (expr <- quote(tr(A %*% B) + A + C))
#' assign_at_expr(expr, c(2, 2), quote(Z))
#' 
#' assign_at_expr(expr, grep_expr(expr, "A")[[1]]$path, quote(Z))
#' assign_at_expr(expr, grep_expr(expr, "A")[[2]]$path, quote(Z))
#'  
#' assign_at_expr(expr, c(2, 2))
#' 
#' assign_at_expr(quote(tr(A %*% B) + A), 2)
#' 
#' @export
#' 

assign_at_expr <- function(expr, path, value) {
  if(missing(value)) value = NULL
  for(expr_name in c("expr", "value")){
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
  
  if (length(path) == 0) return(value)  # expr 全体を置き換える場合
  
  # 再帰的に代入を適用する内部関数
  recursive_set <- function(e, p) {
    if(is.null(value)){
      if (length(p) == 1) {
        return(e[[p[1]]])
      } else {
        return(recursive_set(e[[p[1]]], p[-1]))
      }
      return(e)
    }else{
      if (length(p) == 1) {
        e[[p[1]]] <- value
      } else {
        e[[p[1]]] <- recursive_set(e[[p[1]]], p[-1])
      }
      return(e)
    }
  }
  
  recursive_set(expr, path)
} # end of assign_at_expr



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

