# library(tidyverse)
library(lazy.symbolic)

str_replace_all <- function( string, pattern, replacement ){
  gsub( pattern, replacement, string )
}

to_latex <- function(expr_str, doller = TRUE,
                     mat2sum = FALSE, simple_mat2sum = FALSE,
                     print_html = FALSE, use_tidyverse = TRUE) {
  save_expr_str <- expr_str
  
  if(mat2sum||simple_mat2sum){
    if(use_tidyverse)
      expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
    else
      expr_str <- gsub("}", ")", gsub("{", "chu_kakko(", expr_str, fixed = TRUE), fixed = TRUE)
  }
  
  
  # 入力文字列を R のexpressionとしてパース
  expr <- e <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })
  if (is.null(expr)) return(expr_str)
  
  op_supsc <- c("t", "T", "ginv", "inv")
  supsc <- c("T", "T", "-", "-1")
  
  chars <- c("theta", "alpha", "beta", "gamma", "delta", "epsilon",
             "zeta", "eta", "theta", "iota", "kappa", "lambda",
             "mu", "nu", "xi", "omicron", "pi", "rho",
             "sigma", "tau", "upsilon", "phi", "chi",
             "psi", "omega")
  
  # 再帰的に式を LaTeX 文字列に変換する内部関数
  rec_convert <- function(e) {
    if (is.symbol(e)) {
      # 変数名の場合
      # 変数名を LaTeX のコマンドに変換 (tuika)
      e <- as.character(e)
      if(e %in% chars)
        return(paste0("\\", e))
      else
        return(e)
    } else if (is.numeric(e)) {
      return(as.character(e))
    } else if (is.call(e)) {
      # 呼び出しの場合：演算子や関数呼び出し
      op <- as.character(e[[1]])
      
      if(mat2sum||simple_mat2sum){
        if(e[[1]] == "s"){
          # sum_scripts <- e[[3]] |> as.character()
          ss <- e[[3]]  # sum_scripts
          if(length(ss) <= 2)
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "}", rec_convert(e[[2]])))
          else
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "=",rec_convert(ss[[3]]), "}^{", rec_convert(ss[[4]]), "}", rec_convert(e[[2]])))
        }else if(e[[1]] == "*"){
          return(paste0(rec_convert(e[[2]]), rec_convert(e[[3]])))
          # return(paste0(e[[2]], " \\cdot ", e[[3]]))
        }else if(e[[1]] == "["){
          subscripts_elements <- as.character(e)
          return(paste0(subscripts_elements[2], "_{", paste(subscripts_elements[-(1:2)], collapse = ","), "}"))
        }else if((op %in% c("nrow", "ncol"))){
          if(simple_mat2sum)
            return(eval(e))
          else
            return(paste0(substr(op, 2, 2), "(", rec_convert(e[[2]]), ")"))
        }
      }
      
      if (op == "/") {
        # 分数：a / b を \frac{a}{b} に変換
        return(paste0("\\frac{", rec_convert(e[[2]]), "}{", rec_convert(e[[3]]), "}"))
      } else if (op == "*") {
        # 乗算：a * b を a \cdot b に変換
        return(paste0(rec_convert(e[[2]]), " \\cdot ", rec_convert(e[[3]])))
      } else if (op == "+") {
        return(paste0(rec_convert(e[[2]]), " + ", rec_convert(e[[3]])))
      } else if (op == "-") {
        if (length(e) == 2) {
          # 単項マイナスの場合
          return(paste0("-", rec_convert(e[[2]])))
        } else {
          return(paste0(rec_convert(e[[2]]), " - ", rec_convert(e[[3]])))
        }
      } else if (op == "^") {
        # 累乗：a ^ b を {a}^{b} に変換
        return(paste0("{", rec_convert(e[[2]]), "}^{", rec_convert(e[[3]]), "}"))
        
        # 追加開始
      } else if (op == "(") {
        # 括弧：() を \left( \right) に変換
        return(paste0("\\left(", rec_convert(e[[2]]), "\\right)"))
      } else if (op == "%*%") {
        return(paste0(rec_convert(e[[2]]), rec_convert(e[[3]])))
      } else if (op %in% op_supsc) {
        return(paste0("{", rec_convert(e[[2]]), "}^{", supsc[op == op_supsc], "}"))
        
        # 追加終了
        
      } else {
        # 関数呼び出しの場合（例: sqrt, sin, cos など）
        fun_name <- op
        args <- sapply(as.list(e[-1]), rec_convert)
        # 一般には \fun{arg1, arg2, ...} 形式にする（必要に応じて書式を調整）
        return(paste0("\\", fun_name, "{", paste(args, collapse = ", "), "}"))
      }
    } else {
      # その他のケースは deparse して返す
      return(paste(deparse(e), collapse = " "))
    }
  }
  
  expr <- rec_convert(expr)
  
  if(simple_mat2sum){
    # gsub() の replacement に関数を渡す方法
    if(use_tidyverse){
      expr <- expr |>
        str_replace_all("s1", "k") |>
        str_replace_all("s2", "l") |>
        str_replace_all("s3", "m") |>
        str_replace_all("s4", "n") |>
        str_replace_all("s5", "o")
    }else{
      expr <- c("s1 test s2 string", "another s3 and s4 test s5")
      
      # 置換ルールの定義 (パターンと置換後の文字列のペア)
      patterns <- c("s1", "s2", "s3", "s4", "s5")
      replacements <- c("k", "l", "m", "n", "o")
      
      # ループで順番に gsub を適用
      for (i in seq_along(patterns)) {
        expr <- gsub(patterns[i], replacements[i], expr, fixed = TRUE)
      }
    }
  }
  
  # subscriptの処理
  expr <- gsub("([a-zA-Z0-9]+)_([a-zA-Z0-9]+)", "{{\\1}_{\\2}}", expr)
  # expr <- gsub("([a-zA-Z]+)([0-9]+)", "\\1_{\\2}", expr)
  expr <- gsub("(?<!_)([A-Za-z]+)([0-9]+)(?!}_)", "{\\1_{\\2}}", expr, perl = TRUE)
  
  
  
  if(doller|print_html){
    expr <- paste0("$$", expr, "$$")
  }
  # 変換結果を返す
  if(print_html) print_tex_as_html(expr, save_expr_str)
  return(expr)
}



to_tex_matrix <- function(df, type =c("matrix"), print_html = FALSE) {
  type = match.arg(type)
  
  if(type == "matrix"){
    # データフレームの各要素を変換
    tex_matrix <- apply(df, c(1,2), to_latex, doller = FALSE)
    
    # 行列を LaTeX の bmatrix 形式で構築
    tex_code <- paste0(apply(tex_matrix, 1, paste, collapse = " & "), collapse = " \\\\\n")
    tex_code <- paste0("\\begin{bmatrix}\n", tex_code, "\n\\end{bmatrix}")
    
    if(print_html) print_tex_as_html(tex_code)
    return(tex_code)
  }
}



print_tex_as_html <- function(TeX_code, input){
  if(!require(htmltools)){
    stop("package 'htmltools' is required for print_tex_as_html()")
  }
  
  
  if(missing(input)) input <- ""
  
  Ps <- NULL
  for(i in seq_along(TeX_code)){
    Ps[[i]]  <- p(sprintf("%s%s", input[[i]], TeX_code[[i]]))
  }
  # Ps <- map2(input, output, function(x,y)p(x,y))
  
  html_math <- tags$html(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
    do.call(tags$body, Ps)
    # tags$body(
    #   # h2("MathJax Test in HTML"),
    #   # p("Inline TeX: ", HTML("\\( a^2 + b^2 = c^2 \\)")),
    #   p("Result: ", HTML(TeX_code))
    # )
  )
  
  # View()の代わりにブラウザで表示（HTMLレンダリングされる）
  html_print(html_math)
}





if(0)expr_str <- "s( a[i,j]*b[j,k]*c[k,l], {k})"
sum_move_in <- function(expr){
  # if(mat2sum||simple_mat2sum)
  expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
  
  # 入力文字列を R の式にパース
  expr <- e <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })
  if (is.null(expr)) return(expr_str)
  
  converter <- function(e){
    
    if (is.symbol(e)) {
      # 変数名の場合
      # 変数名を LaTeX のコマンドに変換 (tuika)
      e <- as.character(e)
      if(e %in% chars)
        return(paste0("\\", e))
      else
        return(e)
    } else if (is.numeric(e)) {
      return(as.character(e))
    } else if (is.call(e)) {
      op <- as.character(e[[1]])
      
      if(op == "s"){
        ss <- as.character(e[[3]])[2]  # sum_scripts
        
        check_sub <- function(expr, ss){
          
        }
        
        e[[2]]
      }
      
    }
  }
}


# sum2mat -----------------------------------------------------------------


sum2mat <- function(expr_str, deparse_result = FALSE){
  expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
  expr <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
    warning("入力が有効な R 式ではありません")
    return(NULL)
  })
  
  # とにかく、絶対行列にする関数
  sum2mat_convert <- function(expr, sum_var=NULL){
    op <- as.character(expr[[1]])
    if(op == "s"){
      # s( **** , {i})
      # summationは、対象変数を引数に入れて再帰的に処理
      sum_var <- expr[[3]][[2]]
      return(sum2mat_convert(expr[[2]], sum_var))
      
    }else if(op %in% c("*", "+", "-")){
      # a[i,j] * b[j,k]
      # a[i,j] + b[j,k]
      # a[i,j] - b[j,k]
      
      result <- BinOper_sum2mat(expr, sum_var)
      return(result)
    }else if(op =="["){
      # a[i,j]
      
      result <- sq_brackets_sum2mat(expr, sum_var)
      return(result)
      
    }else{
      print("素通りさせます。")
      return(expr)
    }
  }
  
  sq_brackets_sum2mat <- function(expr, sum_var = NULL){
    if(is.null(sum_var)) {
      return(expr)
    }else{
      # print("[] in summation")
      expr_new <- expr
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
  }
  
  BinOper_sum2mat <- function(expr, sum_var=NULL){
    operator <- as.character(expr[[1]])
    arguments_BinOperator <- expr[-1] |> lapply(function(x) sum2mat_convert(x))
    Mat_symbol_list <- arguments_BinOperator |> sapply(function(x)x[[2]])
    sublist_list <- arguments_BinOperator |> lapply(function(x){
      sapply(3:length(x), function(i){
        x[[i]]
      })
    })
    
    BinOper_unify <- function(Mat_symbol_list, sublist_new, operator){
      operator_mat <- operator
      if(operator_mat == "*") operator_mat <- "%@%"
      call("[",
           call(operator_mat, Mat_symbol_list[[1]], Mat_symbol_list[[2]]),
           sublist_new[[1]],
           sublist_new[[2]])
    }
    
    if(is.null(sum_var)){
      
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
        cat("想定外(想定内)（素通り）")
        return(expr)
      }
      
      # sum_varがある場合
    }else {
      ## 行列同士
      # if(all(sapply(sublist_list, length) == 2)){
      # sublist_list; Mat_symbol_list; sum_var; operator
      sub_logical_list <- lapply(sublist_list, function(sublist)as.character(sublist)==sum_var)
      
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
  }
  
  expr_result <- sum2mat_convert(expr)
  if(deparse_result)
    expr_result <- deparse(expr_result)
  
  return(expr_result)
}




