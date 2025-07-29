#' Parsing text w/o writing text= and trailing [[1]]
#'
#' @param text a string to be parsed
#'
#' @examples
#'
#' modify_math_operators()
#' easy_parse("X")
#' easy_parse("tr(X%*%B)")
#'
#' @return
#' an expression
#'
#' @export
#'

easy_parse <- function(text){
  if(is.character(text))
    parse(text=text)[[1]]
  else
    text
} # end of easy_parse


#' Deparse an expression with maximum width.cutoff
#'
#' @param expr and expression to be deparsed
#'
#' @details deparseをただすると、長いexprは文字列ベクトルになってしまうので、
#'
#' @examples
#'
#' modify_math_operators()
#' text=
#' "Diag(A)%*%B%*%Diag(C)%*%H-Diag(A)%*%B%*%E%*%H+Diag(A)%*%F%*%H+t(A)%*%G%*%H"
#' expr=parse(text=text)[[1]]
#' deparse(expr)
#' safe_deparse(expr)
#'
#' @return
#' a text string
#'
#' @export
#'

safe_deparse <- function(expr){

  gsub(" ", "", deparse(expr, width.cutoff = 500))

} # end of safe_deparse



#' Decompose Matrix Product
#'
#' Decompose a product of matrices into factors
#'
#' @param expr input expression or string
#' @param op the operator to be used as a delimiter
#' @param return_op = TRUE if the operator is required to be returned
#' @param flat ????
#' @param target_X ????
#'
#' @details
#' Currently, nested expressions may not be handled correctly in some cases,
#' so caution is advised.
#'
#' @examples
#'
#' modify_math_operators()
#' # example code
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "%*%")
#' decompose_MatProd("A%*%B%*%C%*%D%*%E", "*")
#'
#' decompose_MatProd("a+b-c+d+e", c("+"), return_op = TRUE)
#' decompose_MatProd("a+b-c+d+e", c("-", "+"), return_op = TRUE)
#'
#' decompose_MatProd("a+b-(c+d)+e", c("-", "+"), return_op =  TRUE)
#' decompose_MatProd("a+b-(c+d)+e", c("+"), return_op =  TRUE)
#'
#' decompose_MatProd("a+b--e", c("+"),  return_op =  TRUE)
#' decompose_MatProd("a+b-+e", c("-", "+"),  return_op =  TRUE)
#' decompose_MatProd("+a++b-+-c+-d-+e", c("-", "+"),  return_op =  TRUE)
#'
#' decompose_MatProd("A%*%B%*%((X%*%C)%*%D)", "%*%", flat = TRUE)
#' decompose_MatProd("A*B*((X%*%C)*D)", "%*%", flat = TRUE)
#' decompose_MatProd("A*B*((X*C)*D)", "*", flat = TRUE)
#'
#' drop_parens("A*B*((X*C)*D)") |> decompose_MatProd("*", flat = TRUE)
#'
#' decompose_MatProd("A+B-((X+C)+D)-E", c("+", "-"), flat = TRUE, return_op = TRUE)
#' decompose_MatProd("A+B-((X+C)+D)-E", c("+"), flat = TRUE, return_op = TRUE)
#' decompose_MatProd("A+B-((X+C)+D)-E", c("+"), flat = FALSE, return_op = TRUE)
#'
#' decompose_MatProd("A+(B+C)+(X+D)", "+", target_X = "X")
#' decompose_MatProd("A+(B+C)+((X+D)+E)", "+", target_X = "X")
#'
#' decompose_MatProd("(A%*%X)%*%B", "%*%")
#' decompose_MatProd("(A%*%X)%*%B", "%*%", flat = TRUE)
#' decompose_MatProd("A%*%((X%*%B))", "%*%", flat = TRUE)
#' decompose_MatProd("A%*%(((((X))%*%B)))", "%*%", flat = TRUE)
#'
#'
#' # flatの修正
#' decompose_MatProd("X-(A+B)", c("+", "-"), flat = TRUE, return_op=TRUE)
#' decompose_MatProd("X-(A+B)", c("+"), flat = TRUE, return_op=TRUE)
#'
#' @return
#' a list of terms/factors and, if requested, a vector of operators
#'
#' @export
#'

decompose_MatProd <- function(expr, op, return_op = FALSE
                              , flat = FALSE, target_X){

  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })


  if(identical(op, "-") | identical(op, c("-", "+") ) | identical(op, c("+", "-") )){
    op = "+"
  }

  if(flat | !missing(target_X))
  if(length(op) !=1){
    stop("もしかしたら分配法則うまくいかないかもしれないので、現versionでは止めます。。")
  }

  if(identical(op, "+")){
    expr <- make_minus_sign(expr)
  }

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
    # temp_current |> print()
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
      if(is.na(i_range)) i_range <- NULL
    }
    if(!is.null(i_range))
    for(i in i_range){
      temp_current_i <- drop_parens(temp_current[[i]])
      if(op == "+"){
        expr <- linear_expand_expr(temp_current_i, "-", most_out = "+")
        temp_current_i <- reduce_expr_sign(expr, minus_as_sign = TRUE)
      }

      if(is.call(temp_current_i)){
        if(as.character(temp_current_i[[1]]) %in% op){
          continue <- TRUE
          additional_op = as.character(temp_current_i[[1]])
          temp_current[[i]] <- c(temp_current_i[[2]], temp_current_i[[3]])
          temp_current <- unlist(temp_current)

          ops <- as.list(ops)
          if(i > length(ops)) {
            ops <- c(ops, additional_op)
          }else{
            ops[[i]] <- list(additional_op, ops[[i]])
          }
          ops <- unlist(ops)
        }else if(length(temp_current_i)==2){
          temp_current[[i]] <- temp_current_i
        }
      }else{
        temp_current[[i]] <- temp_current_i
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
#' @param terms a list of terms from decompose_MatProd
#' @param op a vector of operators
#'
#' @examples
#'
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
#' @return
#' an expression
#'
#' @export
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

} # end of compose_MatProd



#' Recompose MatProd
#'
#'
#' @param expr a string consisting of terms
#' @param op a string consisting of operators
#'
#' @examples
#' recompose_MatProd("A +(B-C)", c("+", "-"))
#'
#' recompose_MatProd(
#' " tr(t(Y) %*% Y)-tr(t(Y) %*% X)-(tr(t(X) %*% Y)-tr(t(X) %*%X))", c("-"))
#'
#' @return
#' an expression
#'
#' @export
#'

recompose_MatProd <- function(expr, op){
  terms <- decompose_MatProd(expr, op, return_op = TRUE, flat = TRUE)
  expr <- compose_MatProd(terms)
  expr <- reduce_expr_sign(expr)
  return(expr)
}





#' Smart Transposition
#'
#' This function takes care of double transposition or
#' enclosing parentheses when applying \code{t}  function.
#'
#' @param expr an expression or a string
#' @param deparse = TRUE if the result be deparsed
#'
#' @examples
#'
#' # modify_math_operators()
#' # library(tidyr)
#' easy_parse("X") |>  transpose_expr()
#' easy_parse("t(X)") |>  transpose_expr()
#' easy_parse("t(t(X)%*%B)") |>  transpose_expr()
#' easy_parse("(t(t(X)%*%B))") |>  transpose_expr()
#' easy_parse("(t(t(X)%*%B))") |> drop_parens()  |>  transpose_expr()
#'
#' @return
#' an expression or a string depending on \code{deparse} option.
#'
#' @export
#'

transpose_expr <- function( expr, deparse=0 ){

 if (is.character(expr))
  expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
   warning("入力が有効な R 式ではありません")
   return(NULL)
  })

  if(is.call(expr)){
    if(expr[[1]] == "t"){
     if( deparse ) return( safe_deparse(expr[[2]]) )
     else return(expr[[2]])
    }
  }

  res=call("t", expr)
  if( deparse )return( safe_deparse(res) )
  else return( res )

} # end of transpose_expr



#' drop parens
#'
#' Remove unnecessary parentheses from an expression#'
#'
#' @param expr an expression or a string
#' @param all = TRUE if all parens should be removed from ast. \cr
#'  = FALSE if 二項演算子同士の順序関係を明示したい場合。
#' @param in_biop flag for recursive process.
#'
#' @examples
#'
#' modify_math_operators()
#' # example code
#' drop_parens("X")
#' drop_parens("(X)")
#' drop_parens("((X))")
#' drop_parens("(t((X)))")
#' drop_parens("(t((tr((X)))))") |> show_ast()
#' drop_parens("(t((tr((X)))))") |> show_ast()
#' drop_parens("(A*B)")
#'
#' # examples for all
#' drop_parens("((A%*%(B*((C%*%C)))))") |> show_ast()
#' drop_parens("((A%*%(B*((C%*%C)))))", all = TRUE)|> show_ast()
#'
#' drop_parens("((A%*%(B*((C%*%C)))))") |> show_ast()
#' drop_parens("((A%*%(B*((C%*%C)))))", all = TRUE)|> show_ast()
#'
#'
#'
#' expr <- easy_parse("(X*A)%*%C") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#' expr <- easy_parse("C%*%(X*A)") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#' expr <- easy_parse("(X%*%A)*C") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#' expr <- easy_parse("C*(X%*%A)") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#' expr <- easy_parse("C%*%(X%*%A)") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#' expr <- easy_parse("(C%*%X)%*%A") ; expr |> show_ast()
#' drop_parens(expr) |> show_ast()
#'
#' # かっこに対する考察
#' easy_parse("X%*%B%*%C") |> show_ast()
#' easy_parse("(X%*%B)%*%C") |> show_ast()
#' easy_parse("(X*B)%*%C") |> show_ast()
#' easy_parse("X*B%*%C") |> show_ast() # %*%が＊よりも優先度高い
#'
#' # second factor
#' expr <- easy_parse("A%*%C") ; expr |> show_ast()
#' expr[[3]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X%*%B)"); show_ast(expr)
#'
#' # first factor
#' expr <- easy_parse("A%*%C") ; expr |> show_ast()
#' expr[[2]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X%*%B)"); show_ast(expr)
#'
#' #つまり
#' #1. 構文木に代入する場合は、
#' #   1.1. 左から順にの計算順序から変わる場合は、見た目上の () がつく。
#' # そうでない場合は()なし。
#' #  1.2. ()を明示的に入れた場合はちゃんと実際の構文木上も現れる。
#' #2. 必要不要問わず、"()" 付きをパースすると、必ず () が構文木に現れる。
#'
#' expr <- easy_parse("A*C") ; expr |> show_ast()
#' expr[[2]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X%*%B)"); show_ast(expr)
#'
#' expr <- easy_parse("A*C") ; expr |> show_ast()
#' expr[[3]] <- easy_parse("X%*%B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X%*%B)"); show_ast(expr)
#'
#' expr <- easy_parse("A%*%C") ; expr |> show_ast()
#' expr[[2]] <- easy_parse("X*B"); show_ast(expr)
#' expr[[2]] <- easy_parse("(X*B)"); show_ast(expr)
#'
#' expr <- easy_parse("A%*%C") ; expr |> show_ast()
#' expr[[3]] <- easy_parse("X*B"); show_ast(expr)
#' expr[[3]] <- easy_parse("(X*B)"); show_ast(expr)
#'
#' expr <- easy_parse("(X*A)%*%C") ; expr |> show_ast()
#' expr <- easy_parse("C%*%(X*A)") ; expr |> show_ast()
#'
#' # ということは、構文木上では一度かっこをほぼすべて外しても問題ない。（はず）
#'
#' easy_parse("X%*%(B*C)") |> show_ast() # %*%が＊よりも優先度高い
#' easy_parse("X%*%B*C") |> show_ast() # %*%が＊よりも優先度高い
#' easy_parse("X%*%B*C") |> show_ast() # %*%が＊よりも優先度高い
#'
#' @return
#' an expression
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
        # in_biop = T all = T !all= F -> 全外し
        # in_biop = F all = T !all= F -> 全外し
        # in_biop = T all = F !all= T -> 一部残し　
        # in_biop = F all = F !all= T -> 全外し
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

} # end of drop_parens





#' Reorder unary oparators
#'
#' This function is used to move unary operators (e.g., `inv`, `t`, `-`) to the outermost position in an expression, as much as possible.
#' It is useful for handling nested unary operators or when you want to prioritize moving specific operators outward.
#'
#' @param expr input expression or string
#' @param most_out The unary operator that should be moved to the outermost position.
#' @param add_exch_op (optional) An additional unary operator to be added to the set of exchangeable operators. This is useful if you have custom unary operators.
#' @param exchangable_ops A vector of unary operators that can be exchanged. The default is `c("inv", "(", "t", "-")`.
#'
#' @note 再帰解決系
#'
#' @examples
#'
#' modify_math_operators()
#' unary_reorder_expr("(t(inv(A)))", "inv")
#' unary_reorder_expr("(t(-(inv(A))))", "inv")
#' unary_reorder_expr("t(-(gune(A)))", "gune")
#' unary_reorder_expr("gune(t(-(inv(A))))", "inv")
#' unary_reorder_expr("(t(B + (inv(A))))", "inv")
#'
#' unary_reorder_expr("t(-(gune(A)))", "gune", add_exch_op = "gune")
#' unary_reorder_expr("gune(t(-(inv(A))))", "inv", add_exch_op = "gune")
#'
#' unary_reorder_expr("t(-(t(-(inv(A)))))", "inv"
#' , exchangable_ops = c("-", "(", "inv"))
#'
#' unary_reorder_expr("t(inv(A))", "t")
#' unary_reorder_expr("inv(t(-(B)) - A)", "-")
#'
#' @return
#' an expression
#'
#' @export
#'

unary_reorder_expr <- function(expr, most_out, add_exch_op, exchangable_ops = c("inv", "(", "t","-")){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  if(!missing(add_exch_op)) exchangable_ops <- c(exchangable_ops, add_exch_op)

  if(!(most_out %in% exchangable_ops)) return(expr)

  info_unary_list <- grep_expr(expr, most_out)

  if(length(info_unary_list)==0) return(expr)

  # choose the first unary op.
  length_expr <-sapply(info_unary_list, function(x){
    length(x$parent)
  })

  info_unary <- info_unary_list[[match(2, length_expr)]]
  if(is.null(info_unary$parent)) return(expr)

  paths_to_mostout <- info_unary$path

  maxdepth <- length(paths_to_mostout)

  ops_to_mostout <- sapply(1:maxdepth, function(depth){
    temp_path <- paths_to_mostout
    temp_path <- temp_path[1:depth]
    temp_path[depth] <- 1
    c(deparse(assign_at_expr(expr, temp_path))
      , length(assign_at_expr(expr, temp_path[-depth])))
  })

  # exchange
  continue <- TRUE
  for(depth in (maxdepth-1):0){
    if(continue){
      if(depth==0){
        expr[[1]] <- as.symbol(most_out)
      }else
      if((as.character(ops_to_mostout[1, depth]) %in% exchangable_ops) & ops_to_mostout[2, depth] !=3){
        temp_path <- paths_to_mostout
        temp_path <- c(temp_path[1:depth], 1)
        expr <- assign_at_expr(expr, temp_path, as.symbol(ops_to_mostout[1, depth]))

        if(depth == 1){
        }
      }else{
        temp_path <- paths_to_mostout
        temp_path <- c(temp_path[1:depth], 1)
        expr <- assign_at_expr(expr, temp_path, as.symbol(most_out))
        continue <- FALSE
      }
    }
  }

  return(expr)

} # end of unary_reorder_expr






#' Remove multiple consecutive signs from an expression
#'
#'
#' @param expr an expression
#' @param minus ????
#' @param as_sign ????
#'
#' @examples
#'
#' modify_math_operators()
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
#' reduce_expr_sign("-A+++B")
#' reduce_expr_sign("-A+----B")
#' reduce_expr_sign("-A+--++--B")
#' reduce_expr_sign("-A+++B")
#' reduce_expr_sign("-A--(A-B)")
#' reduce_expr_sign("(A--B)")
#' reduce_expr_sign("(A----t(B%*%-C))")
#'
#' reduce_expr_sign(" -A + -B", minus_as_sign = TRUE)
#' reduce_expr_sign(" -A --B + -C", minus_as_sign = FALSE)
#'
#' @return
#' an expression
#'
#' @export
#'

reduce_expr_sign <- function(expr, minus_as_sign = FALSE){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  drop_sign_plus <- function(expr){
    if(is.call(expr)){
      if(expr[[1]] == "+" & length(expr)==2){
        return(drop_sign_plus(expr[[2]]))
      } else if(length(expr)==2){
        expr[[2]] <- drop_sign_plus(expr[[2]])
        return(expr)
      } else if(length(expr)==3){
        expr[[2]] <- drop_sign_plus(expr[[2]])
        expr[[3]] <- drop_sign_plus(expr[[3]])
        return(expr)
      }
    }
    return(expr)
  }

  sign_exchange <- function(expr){
    if(is.call(expr)){
      if(length(expr)==3){

        if(is.call(expr[[3]])){
          if(length(expr[[3]])==2 & expr[[3]][[1]] == "-"){
            if(expr[[1]] == "+") {
              expr[[1]] <- as.symbol("-")
              expr[[2]] <- sign_exchange(expr[[2]])
              expr[[3]] <- sign_exchange(expr[[3]][[2]])
              return(expr)
            }
            if(expr[[1]] == "-") {
              expr[[1]] <- as.symbol("+")
              expr[[2]] <- sign_exchange(expr[[2]])
              expr[[3]] <- sign_exchange(expr[[3]][[2]])
              return(expr)
            }
          }
        }

        expr[[2]] <- sign_exchange(expr[[2]])
        expr[[3]] <- sign_exchange(expr[[3]])
        return(expr)
      } else if(length(expr)==2){
        expr[[2]] <- sign_exchange(expr[[2]])
        return(expr)
      }
    }
    return(expr)
  }

  # reduce only sign
  expr <- drop_sign_plus(expr)
  expr <- cancel_double_expr(expr, double_op = "-")
  expr <- sign_exchange(expr)
  if(minus_as_sign) expr <- make_minus_sign(expr)

  return(expr)

} # end of reduce_expr_sign



#' Remove multiplicative identity matrix I
#'
#' @param expr an expression or a string
#' @param doubpe_op ????
#' @param inv ????
#'
#' @examples
#'
#' modify_math_operators()
#' reduce_expr_I("A%*%I")
#' reduce_expr_I("A*I")
#' reduce_expr_I("I%*%A")
#' reduce_expr_I("I%*%I")
#' reduce_expr_I("A%*%CB%*%I%*%D*E")
#' reduce_expr_I("A%*%((CB%*%I)%*%D)*E")
#' reduce_expr_I("A%*%C%*%B%*%I%*%I*E")
#'
#' \dontrun{
#' reduce_expr_I("t(t(-t(inv(B%*%X)%*%t(t(-t(inv(t(inv(B%*%X)))%*%t(t(t(I)))%*%inv(t(inv(B%*%X))))))%*%inv(B%*%X)))%*%B)")
#' grep_expr("t(t(-t(inv(B%*%X)%*%t(t(-t(inv(t(inv(B%*%X)))%*%t(t(t(I)))%*%inv(t(inv(B%*%X))))))%*%inv(B%*%X)))%*%B)")
#' }
#'
#' @return
#' an expression
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
  }
  return(expr)

} # end of reduce_expr_I




#' Remove multiple consecutive t's or inv's from an expression
#'
#'
#' @param expr an expression of a string
#' @param sym  string vector of the variables to be assumed as symmetric
#' @param use_unary_reorder = TRUE to use use_unary_reorder functin
#'
#'
#' #' @note 再帰解決系
#'
#' @examples
#'
#' modify_math_operators()
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
#' cancel_double_expr("t(inv(t(inv(A)))) %*% -inv(t(-(B)))"
#' , use_unary_reorder=TRUE)
#' cancel_double_expr("-inv(t(-(B)))"
#' , use_unary_reorder=TRUE)
#'
#' cancel_double_expr("-inv(t(-(B)) - A)", use_unary_reorder=TRUE)
#' # これ期待通りの挙動ではないので要修正
#' # →　修正完了
#'
#' @return
#' an expression
#'
#' @export
#'

cancel_double_expr <- function(expr, double_op = c("t", "inv", "-"), sym, inv, use_unary_reorder = FALSE){
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  sym_mats <- c("I")
  inv_mats <- c("I")

  # double_op <- c("t", "inv", "-")


  if(!missing(sym)) sym_mats <- c(sym_mats, sym)
  if(!missing(inv)) inv_mats <- c(inv_mats, inv)

  info_double_list <- grep_expr(expr, double_op)
  length_expr <- lapply(info_double_list, function(x)length(x$parent))
  info_double_list <- info_double_list[length_expr==2]

  # info_double_list <- grep_expr(expr, "t")
  if(length(info_double_list)==0) return(expr)
  for(i in seq_along(info_double_list)){

    info_double <- info_double_list[[i]]
    if(is.null(info_double$parent)) return(expr)

    target_op <- as.character(info_double$match)
    if(is.call(info_double$parent[[2]])){
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
      if(use_unary_reorder){
        temp_expr <- unary_reorder_expr(info_double$parent[[2]], target_op)
        if(as.character(temp_expr[[1]])==target_op){

          depth <- length(info_double$path)
          target_path <- info_double$path[-depth]
          remain <- temp_expr[[2]]
          expr <- assign_at_expr(expr, target_path, remain)
          # expr <- cancel_double_expr(expr)
          expr <- cancel_double_expr(expr, sym = sym_mats, inv=inv_mats)
          return(expr)
          # expr <- reduce_expr_I(expr)
        }
      }
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

} # end of cancel_double_expr





#' grep for expr
#'
#'
#' @param expr input expression or string
#' @param varname  a string vector containing variable names to grep
#'
#' @examples
#' expr <- easy_parse("(tr(A %*% B) + A + t(C))")
#' res <- grep_expr(expr, "A")
#' length(res) # -> 2. exprの中に"A"はふたつ含まれているので、それぞれの結果
#' # 一つ目のAの結果
#' expr[[2]][[2]][[2]][[2]][[2]]  # res[[1]]$pathの意味
#' expr[[2]][[2]][[2]][[2]]       # res[[1]]$parentの意味
#' # ２つ目のAの結果
#' expr[[2]][[2]][[3]]　　　 # res[[2]]$pathの意味
#' expr[[2]][[2]] 　　　　　 # res[[2]]$parentの意味
#'
#' grep_expr(expr, "t(C)")
#' grep_expr(expr, "A%*%B")
#'
#' grep_expr(expr, "+")
#' grep_expr(expr, "(")
#' grep_expr(expr, "t")
#'
#' @return
#' a list of list consisting of
#' \preformatted{
#' path
#' parent
#' match
#' }
#'
#'
#' @export
#'

grep_expr <- function(expr, varname) {

  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  if(is.character(varname))
    varname <- lapply(varname, function(expr) {
      if(expr %in% c("+", "-", "(")) return(as.symbol(expr))
      tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })})
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

} # end of grep_expr



#' assign new expr at path
#'
#'
#' @param expr a quoted expression
#' @param path from grep_expr
#' @param value the value to be assigned
#'
#' @examples
#'
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
#' @return
#' an expression
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
               warning(sprintf(
                "%sへの入力が有効な R 式ではありません", expr_name))
               return(NULL)
             })
      )
    }
  }

  if (length(path) == 0 | is.null(path)){
    if(is.null(value)) return(expr)
    return(value)  # expr 全体を置き換える場合
  }

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



#' gsub for an expression
#'
#'
#' @param expr an expression or a string
#' @param object the symbol in the expression to be replaced by replacement
#' @param replacement the replacement
#'
#' @examples
#'
#' gsub_expr("t(A)%*%(B+C)+D","C","X")
#' gsub_expr("t(A)%*%(B+C)+C","C","X")
#' gsub_expr("t(A)%*%(B+C)+D","B+C","X")
#' gsub_expr("t(A)%*%(B+C)+D","t","inv")
#'
#' @return
#' an expression
#'
#' @export
#'
gsub_expr <- function(expr, object, replacement){

  for(expr_name in c("expr", "object", "replacement")){
    expr_temp <- eval(parse(text=expr_name))
    if (is.character(expr_temp)){
      assign(expr_name,
             tryCatch(parse(text = expr_temp)[[1]], error = function(e) {
               warning(sprintf(
                 "%sへの入力が有効な R 式ではありません", expr_name))
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
#'
#' @param expr an expression of a string
#'
#' @details
#' Currently, nested expressions may not be handled correctly
#'  in some cases, so caution is advised.
#'
#' @return
#'  an expression
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
    }else if(op %in% c("%.%", "*")){
      tbl_symbol <- table(symbols_past)
      length_encoding <-
       list(lengths = paste0("(", tbl_symbol, ")"), values = names(tbl_symbol))
    }
    temp_factors <-
     paste0(length_encoding$values,  "^", length_encoding$lengths)
    temp_factors <- gsub("\\^\\(*1\\)*", "", temp_factors)
    expr_str <- paste(temp_factors, collapse = op)

    parse(text=expr_str)[[1]]
  }

  powerize(powerize(expr, "*"), "%*%")

} # end of simplify_power



#' Reorder the arguments of the tr and * functions
#'
#'
#' Reorder the arguments of the tr and * functions
#' so that the objects specified by X_ is placed at the right most position.
#'
#' @param expr an expression of a string
#' @param X_ a string containing the object name to be moved
#' @param op a vector of operators to be used
#' @param attr = TRUE to use the flag for transposition
#'
#'
#' @examples
#' # see tests/mD0_test.R
#'
#' modify_math_operators()
#'
#' trace_reorder("tr(A%*%(X*C))", "X")
#'
#' trace_reorder("tr(A%*%(X*B%.%C))","X")
#' trace_reorder("tr(A%*%(X*B*C))","X")
#'
#' trace_reorder("X*B*C", "X")
#' trace_reorder("X%.%B*C", "X")
#' trace_reorder("A%*%(t(X*B)*C)","X")
#' trace_reorder("A%*%(t(t(X*B)*C))","X")
#'
#' trace_reorder("t(X)*B*C", "X", op = "*", attr = TRUE)
#'
#' @return
#' an epression
#'
#' @export
#'
#'
#'

trace_reorder <- function(expr, X_, op=c("both", "%*%", "*", "%.%")
                          , attr = FALSE){
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


    # grepl("C", as.character(expr))
    # as.character(expr) %in% "C"
    # # if(deparse(expr) %in% paste0("t(", X_, ")"))


    if(op == "both" & is.call(expr)){
      if(as.character(expr[[1]]) %in% c("%*%", "*", "%.%")){
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
      # targetだけ、必要に応じて先に転置してしまう。
      # t(A) %*% t(X) -> t(A) %*% X (neq; transposed = TRUE, temporary)
      if(deparse(target) == paste0("t(", X_, ")")){
        target <- transpose_expr(target)
        transposed = TRUE
      }else if(N>1){
        # reorder target factor *
        target_temp <- trace_reorder(target, X_, op = "*", attr = TRUE)
        target <- target_temp$expr
        transposed <- target_temp$transposed
        # reorder target factor %.%
        target_temp <- trace_reorder(target, X_, op = "%.%", attr = TRUE)
        target <- target_temp$expr
        transposed <- target_temp$transposed | transposed

        temp_current[[X_index]] <- target
      }


      # process transpose
      # t(A) %*% t(X) -> t(X %*% A)
      # 順番とtarget以外のtransposeを実行。
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



#' Convert diag(X) to I*X, the Hadamar product
#'
#' @param expr an expression or a string
#'
#' @examples
#' # example code
#'
#' diag_to_hp("tr(A%*%Diag(t(X)%*%X))")
#' diag_to_hp("tr(A%*%Diag(t(X)+X))")
#' diag_to_hp("tr(A%*%(t(X)%*%X))")
#'
#' diag_to_hp("tr(Diag(A)%*%diag(t(X)%*%Diag(X + diag(B))))")
#'
#' @export
#'

diag_to_hp <- function(expr){

  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  info_diag_list <- grep_expr(expr, c("diag", "Diag"))


  if(length(info_diag_list)==0) return(expr)


  for(i in seq_along(info_diag_list)){

    info_diag <- info_diag_list[[i]]
    if(is.null(info_diag$parent)) return(expr)

    path <- info_diag$path[-length(info_diag$path)]
    replacement <- call("*",
                        info_diag$parent[[2]],
                        as.symbol("I"))
    expr <- assign_at_expr(expr, path, replacement)
  }

  return(expr)

}# end of diag_to_hp


#' wrap Hadamar product with parens for apparance
#'
#' @param expr an expression or a string
#'
#' @examples
#' # example code
#'
#' res1 = "A%*%(B%.%C)" %>% drop_parens(all = TRUE)
#' res1 |> show_ast()
#' res1 |> wrap_hp() %>% show_ast()
#' 
#' res2 = "(B%.%C)%*%A" %>% drop_parens(all = TRUE)
#' res2 |> show_ast()
#' res2 |> wrap_hp() %>% show_ast()
#'
#' @export
#'
#'

wrap_hp <- function(expr){
  
  wrap_hp_1 <- function(e, in_prod = FALSE){
    if(is.call(e)){
      op = as.character(e[[1]])
      if(op == "%*%"){
        e[[2]] <- wrap_hp_1(e[[2]], in_prod = TRUE)
        e[[3]] <- wrap_hp_1(e[[3]], in_prod = TRUE)
        return(e)
      }
      if(op %in% c("*", "%.%") && in_prod){
        return(call("(", wrap_hp_1(e)))
      }
      e[[2]] <- wrap_hp_1(e[[2]])
      if(length(e) == 3) e[[3]] <- wrap_hp_1(e[[3]])
      return(e)
    }
    return(e)
  }
  
  wrap_hp_1(expr)
  
}# end of wrap_hp



#' Add + sign before unary - sign
#'
#'
#' @param expr an expression or a string
#'
#'
#' @examples
#' # example code
#'
#' make_minus_sign("A-B")
#' make_minus_sign("A-(B+C)")
#' easy_parse("A-(B-C)")
#' make_minus_sign("A-(B-C)")
#' make_minus_sign("a+b--e")
#'
#' make_minus_sign("t(X) - A -t(t(B) -t(C))")
#'
#' reduce_expr_sign("a+b--e")
#' reduce_expr_sign("a-(b+c)")
#'
#' expr <- easy_parse("A-(B-C)") |> print()
#' expr <- make_minus_sign(expr) |> print()
#' expr <- linear_expand_expr(expr, "-", most_out = "+") |> print()
#' expr <- recompose_MatProd(expr, "+") |> print()
#'
#' @export
#'

make_minus_sign <- function(expr){

  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  info_minus_list <- grep_expr(expr, c("-"))
  # if(length(info_minus_list)==0) return(expr)

  # choose the first **binary** op.
  length_expr <-sapply(info_minus_list, function(x){length(x$parent)})
  if(is.na(match(3, length_expr))) return(expr)

  info_minus <- info_minus_list[[match(3, length_expr)]]



  if(is.null(info_minus$parent)) return(expr)

  path <- info_minus$path[-length(info_minus$path)]
  replacement <- call("+",
                      info_minus$parent[[2]],
                      call("-", info_minus$parent[[3]]))
  expr <- assign_at_expr(expr, path, replacement)

  return(make_minus_sign(expr))

} # end of make_minus_sign



#' Lienarly Distribute Functions
#'
#'
#' @param expr an expression or a string
#' @param ... strings containing function names
#' @param most_out
#'
#' @note 再帰解決系
#'
#' @examples
#'
#' linear_expand_expr("tr(A-B)", "tr", "inv")
#' linear_expand_expr("inv(tr(A-B)) %*% C", "tr")
#' linear_expand_expr("tr(A-B+C)", "tr")
#' linear_expand_expr("A-B", "tr")
#'
#'
#' linear_expand_expr("A%*%(X-B)", "%*%")
#' linear_expand_expr("tr(A%*%(X-B))", "tr", "%*%", "(")
#'
#' linear_expand_expr("(A-B)*(A-B)", "*")
#'
#' @return
#' an expression
#'
#' @export
#'

linear_expand_expr <- function(expr, ... , most_out =c("+","-")){

  linear_expand_expr_core <- function(expr, ..., most_out ){

  fn_names <- as.character(list(...))
  if(is.character(expr))
    expr <- tryCatch(parse(text = expr)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })

  # most_out <- c("+", "-")
  # most_out <- c("+")
  exchangable_ops <- fn_names

  expr <- drop_parens(expr, all = TRUE)
  info_sum_list <- grep_expr(expr, most_out)

  if(length(info_sum_list)==0) return(expr)

  # choose the first **binary** op.
  length_expr <-sapply(info_sum_list, function(x){length(x$parent)})
  info_sum_list <- info_sum_list[length_expr==3]

  for(i in rev(seq_along(info_sum_list))){
    info_sum <- info_sum_list[[i]]
    if(is.null(info_sum$parent)) return(expr)

    paths_to_mostout <- info_sum$path

    maxdepth <- length(paths_to_mostout)

    ops_to_mostout <- sapply(1:maxdepth, function(depth){
      temp_path <- paths_to_mostout
      temp_path <- temp_path[1:depth]
      temp_path[depth] <- 1
      c(deparse(assign_at_expr(expr, temp_path))
        , length(assign_at_expr(expr, temp_path[-depth])))
    })

    # exchange
    continue <- TRUE
    for(depth in (maxdepth-1):1){
      if(continue){
        if(depth==0){
          return(expr) # +- is most out
        }else
          if(as.character(ops_to_mostout[1, depth]) %in% exchangable_ops){
            if(ops_to_mostout[2, depth] !=3){

              path_to_parent <- paths_to_mostout[1:depth]
              parent <- assign_at_expr(expr, path_to_parent)

              path_to_target <- path_to_parent[-length(path_to_parent)]
              target <- assign_at_expr(expr, path_to_target)

              parent[[2]] <- assign_at_expr(target, 2, parent[[2]])
              parent[[3]] <- assign_at_expr(target, 2, parent[[3]])

              expr <- assign_at_expr(expr, path_to_target, parent)
              expr <- linear_expand_expr_core(expr, ..., most_out = most_out)
              # return(recompose_MatProd(expr, most_out))
              return(expr)
            }else if(ops_to_mostout[2, depth] ==3){

              path_to_parent <- paths_to_mostout[1:depth]
              parent <- assign_at_expr(expr, path_to_parent)

              path_to_target <- path_to_parent[-length(path_to_parent)]
              target <- assign_at_expr(expr, path_to_target)

              parent[[2]] <- assign_at_expr(target, path_to_parent[depth], parent[[2]])
              parent[[3]] <- assign_at_expr(target, path_to_parent[depth], parent[[3]])

              expr <- assign_at_expr(expr, path_to_target, parent)
              expr <- linear_expand_expr_core(expr, ..., most_out = most_out)
              return(expr)
            }
          }else{
            continue <- FALSE
          }
      }
    }
  }

  # return(recompose_MatProd(expr, most_out))
  return(expr)

  }

  linear_expand_expr_core(expr, ..., most_out = most_out)

  # decompose_MatProd(expr, most_out)

}　# end of linear_expand_expr




