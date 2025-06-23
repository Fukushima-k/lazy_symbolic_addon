#' Express Summation Expressions in Matrix Form
#'
#' @param sexpr input summation expression
#' @param expand = 0 not to expand the input prior to processing
#' @param deparse_result = TRUE if the result as a string is needed.
#' @param print = 0 not to print the result
#' @param debug = 1 to print debug info
#'
#' @details
#' The summation of an expression over subscript k is expressed as \cr
#' \code{ s( exp, {sub} )}  \cr
#' or \cr
#' \code{ s( exp, {sub, from, to} ) }
#' \cr
#' However, in this function,
#' it is assumed that the subscript must be the abbreviated form \{sub\}
#' not the full form \{sub,from,to\}.
#'
#' The body part of sum must have only one term: Compare the following: \cr
#'  \code{sum2mat("s(a+b,{k})", expand=0)} \cr
#'  and \cr
#'  \code{sum2mat("s(a+b,{k})"} \cr
#' where the default \code{expand=1} option expands the body of a summation.
#'
#' When there are multiple summations in an expression,
#' use \code{sumMoveIn} prior to the call. See the last example below.
#'
#' When the input expression is very complex ,
#' the result may NOT be correct!!
#'
#' @return
#' Corresponding matrix expression with subscripts as a string
#' or an expression.
#'
#' @examples
#' sexpr="s(A[i,j]*b[j],{j})" |> sum2mat()
#' sexpr="s(A[j,i]*b[j],{j})" |> sum2mat()
#' sexpr="s(b[i]*A[i,j],{i})" |> sum2mat()
#' sexpr="s(b[j]*A[i,j],{j})" |> sum2mat()
#'
#' sexpr="s(A[i,k]*B[k,j],{k}) + s(A[i,k]*B[j,k],{k})" |> sum2mat()
#' sexpr="s(A[k,i]*B[k,j],{k}) + s(A[k,i]*B[j,k],{k})" |> sum2mat()
#' sexpr="s(A[i,k]*B[k,j] + A[i,k]*B[j,k],{k})" |> sum2mat()
#'
#' sexpr="A[i,j]" |> sum2mat()
#' sexpr="A[i,j]*D[j,j]" |> sum2mat()
#' sexpr="A[i,j]*D[i,i]" |> sum2mat()
#' sexpr="D[i,i]*A[i,j]" |> sum2mat()
#'
#' sexpr="A[i,j]+B[i,j]" |> sum2mat()
#' sexpr="-A[i,j]-B[i,j]+C[i,j]" |> sum2mat()
#'
#' sexpr="s(A[i,j],{j})" |> sum2mat()
#' sexpr="s(A[i,j],{i})" |> sum2mat()
#' sexpr="s(A[i,j]*D[j,j],{j})" |> sum2mat()
#'
#' sexpr="s(s(A[i,j]*B[j,k],{k}),{j})" |> sum2mat()
#' sexpr="s(s(A[i,j]*B[j,k],{k}),{j})" |> sumMoveIn() |> sum2mat()
#'
#'
#' \dontrun{
#' library(htmltools)
#' # to LaTeX
#' sexpr="s(A[i,j]*b[j],{j})" |> sum2mat()
#' |> to_latex(mat2sum=TRUE, print_html=TRUE)
#'
#' sexpr="s(A[i,j]*b[j],{j})" |> sum2mat(deparse_result=FALSE)
#' |> to_latex(mat2sum=TRUE, print_html=TRUE)
#' }
#'
#'
#' @author Contributed by Dr. Kentaro Fukushima at Osaka University.
#'
#'
#' @export
#'

# sum2mat -----------------------------------------------------------------
sum2mat <- function(expr_str, deparse_result = FALSE, print=0 ){
  
  expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
  expr <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })

  # とにかく、絶対行列にする関数
  sum2mat_convert <- function(expr, sum_var=NULL, in_bi=FALSE, debug = 1){

    if (is.symbol(expr)) {
      # 変数名の場合
      # 変数名を LaTeX のコマンドに変換 (tuika)
      expr <- return(expr)
      if(expr %in% chars)
        return(paste0("\\", expr))
      else
        return(expr)
    } else if (is.numeric(expr)) {
      return(expr)
    } else if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if(op == "s"){
      # s( **** , {i})
      # summationは、対象変数を引数に入れて再帰的に処理
      sum_var <- expr[[3]][[2]]
      expr[[2]] <- sum2mat_convert(expr[[2]]) # sum_varが不要な処理だけ、優先的に済ませる。
      result <- sum2mat_convert(expr[[2]], sum_var)

    }else if(op %in% c("*", "+", "-")){
      # a[i,j] * b[j,k]
      # a[i,j] + b[j,k]
      # a[i,j] - b[j,k]

      if(length(expr) == 3)
        # 2項演算子の場合
        result <- BinOper_sum2mat(expr, sum_var)
      else
        # 1項演算子の場合
        result <- call(op, sum2mat_convert(expr[[2]], sum_var))
    }else if(op =="["){
      # a[i,j]

      result <- sq_brackets_sum2mat(expr, sum_var, in_bi)

    }else{
      # print("素通りさせます。")
      result <- (expr)
    }
    }
    if(debug){
      print(glue::glue("{deparse(expr)} -> ")); cat("   ")
      print(result)
    }
    return(result)
  } # end of sum2mat_convert



  sq_brackets_sum2mat <- function(expr, sum_var = NULL, in_bi=FALSE){
    if(is.null(sum_var) | in_bi) {
      return(expr)
    }else{
      # print("[] in summation")
      expr_new <- expr
      sum_var_index <- 0
      for(i in 3:4){
        if(expr_new[[i]] == sum_var){
          expr_new[[i]] <- 1
          sum_var_index <- i-2
        }
      }
      if(sum_var_index == 1){
        Mat_symbol <- call("t", expr[[2]])
        expr_new[3:4] <- rev(expr_new[3:4])
      }else{
        Mat_symbol <- expr[[2]]
      }

      expr_new[[2]] <- call("(", call("%*%", Mat_symbol, as.symbol("one")))

      return(expr_new)
    }
  } # end of sq_brackets_sum2mat



  exchange_sign <- function(expr){
    op_temp <- as.character(expr[[1]])
    if(op_temp %in% c("-", "+")){
      root <- expr[[2]]
      root[[2]] <- call(op_temp, root[[2]])
      return(root)
    }else{
      return(expr)
    }
  } # end of exchange_sign



  BinOper_unify <- function(Mat_symbol_list, sublist_new, operator){
    operator_mat <- operator
    if(operator_mat == "*") operator_mat <- "%@%"
    call("[",
         call(operator_mat, Mat_symbol_list[[1]], Mat_symbol_list[[2]]),
         sublist_new[[1]],
         sublist_new[[2]])
  } # end of BinOper_unify



  BinOper_sum2mat <- function(expr, sum_var=NULL){
    operator <- as.character(expr[[1]])
    arguments_BinOperator <- expr[-1] |>
      lapply(function(x) exchange_sign(sum2mat_convert(x, sum_var=sum_var, in_bi = TRUE)))
    Mat_symbol_list <- arguments_BinOperator |> sapply(function(x)x[[2]])
    sublist_list <- arguments_BinOperator |> lapply(function(x){
      sapply(3:length(x), function(i){
        x[[i]]
      })
    })
    
    
    if(is.call(sublist_list[[1]][[1]])){
      return(expr)
    }

    # 条件分岐の整理
    if(is.null(sum_var)){
      sum_logic <- TRUE
    }else if(!(as.character(sum_var) %in% (as.character(unlist(sublist_list))))){
      sum_logic <- TRUE
    }else{
      sum_logic <- FALSE
    }

    if(sum_logic){

      # 行列がサイズが異なるなら、summation無しは飛ばす。
      if((length(sublist_list[[1]]) !=2) | (length(sublist_list[[2]]) != 2)){
        return(expr)
      }
      
      # 要素積、要素和(operator_mat定義済み)
      Mat_symbol_list_mod <- Mat_symbol_list |> lapply(function(x){
        # 余計なかっこ()の削除
        if(as.character(x)[1]=="[") x[[2]] else x
      })
      if(identical(sublist_list[[1]], sublist_list[[2]])){
        # print("添え字の順が一致")
        return(BinOper_unify(Mat_symbol_list_mod, sublist_list[[1]], operator))
      }else if(identical(sublist_list[[1]], rev(sublist_list[[2]]))){
        # print("添え字の順が反対")
        Mat_symbol_list_mod[[2]] <- call("t", Mat_symbol_list_mod[[2]])
        return(BinOper_unify(Mat_symbol_list_mod, sublist_list[[1]], operator))

        # 対角行列
      }else if(sublist_list[[1]][[1]]==sublist_list[[1]][[2]] |
               sublist_list[[2]][[1]]==sublist_list[[2]][[2]]){
        for(i in 1:2){
          if(sublist_list[[i]][[1]]==sublist_list[[i]][[2]]){
            if(which(sublist_list[[-i]]== sublist_list[[i]][[1]]) != i){
              # 対角行列とは逆の行列の添え字が逆転しているか否か。
              Mat_symbol_list[[-i]] <- call("t", Mat_symbol_list[[-i]])
              sublist_new<- rev(sublist_list[[-i]])
            }else{
              sublist_new<- sublist_list[[-i]]
            }
            Mat_symbol_list[[i]] <- call("diag", Mat_symbol_list[[i]])
          }
        }
        return(BinOper_unify(Mat_symbol_list, sublist_new, "%*%"))

        # その他
      }else {
        expr[[2]] <- arguments_BinOperator[[1]]
        expr[[3]] <- arguments_BinOperator[[2]]
        # cat("想定外(想定内)（素通り）")
        return(expr)
      }

      # sum_varがある場合
    }else {
      ## 行列同士
      # if(all(sapply(sublist_list, length) == 2)){
      # sublist_list; Mat_symbol_list; sum_var; operator
      sub_logical_list <- lapply(sublist_list, function(sublist)as.character(sublist)==sum_var)
      
      
      if(!all(sapply(sub_logical_list, any))){
      # sum_varが両方の行列に含まれないばあい。
        return(expr)
      }
      
      # 右行列と左行列で処理を反転させる
      logic_flip <- list(function(x)x,function(x)!x)
      Mat_symbol_list_mod <- sublist_new <- res <- list()
      for(i in 1:2){
        sublist_new[[i]] <-  sublist_list[[i]][!sub_logical_list[[i]]]
        Mat_symbol_list_mod[[i]] <- Mat_symbol_list[[i]]
        
        if(length(sublist_list[[i]]) == 2){
          if(logic_flip[[i]](identical(sub_logical_list[[i]], c(TRUE, FALSE)))){
            Mat_symbol_list_mod[[i]] <- call("t", Mat_symbol_list[[i]])
          }else if(logic_flip[[i]](identical(sub_logical_list[[i]], c(FALSE, TRUE)))){
            Mat_symbol_list_mod[[i]] <- Mat_symbol_list[[i]]
          }
        }else if(length(sublist_list[[i]]) == 1){
          if(sub_logical_list[[i]]){
            if(i == 1)
              Mat_symbol_list_mod[[i]] <- call("t", Mat_symbol_list[[i]])
          }else{
            stop("想定外です。6")
          }
        }else{
          stop("想定外です。5")
        }
        if(length(sublist_new[[i]]) == 0)
          sublist_new[[i]] <- list(1) # [1,] の列引数に相当するexpression要素の作成
      }
      result <- BinOper_unify(Mat_symbol_list_mod,
                              lapply(sublist_new,function(x)x[[1]]),
                              "%*%")
    }
    return(result)
  } # end of BinOper_sum2mat


  expr_result <- sum2mat_convert(expr)

  if(deparse_result){
    expr_result <- deparse(expr_result)
    if( print ) printm(expr_result)
  }

  return(expr_result)



} # end of sum2mat




