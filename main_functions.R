
to_latex <- function(expr_str, doller = TRUE,
                     mat2sum = FALSE, simple_mat2sum = FALSE) {
  
  if(mat2sum||simple_mat2sum)
    expr_str <- expr_str %>%str_replace_all("\\{", "chu_kakko\\(") %>% str_replace_all("\\}", "\\)")
  
  # 入力文字列を R の式にパース
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
          # sum_scripts <- e[[3]] %>% as.character()
          ss <- e[[3]]  # sum_scripts
          if(length(ss) <= 2)
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "}", rec_convert(e[[2]])))
          else
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "=",rec_convert(ss[[3]]), "}^{", rec_convert(ss[[4]]), "}", rec_convert(e[[2]])))
        }else if(e[[1]] == "*"){
          return(paste0(rec_convert(e[[2]]), rec_convert(e[[3]])))
          # return(paste0(e[[2]], " \\cdot ", e[[3]]))
        }else if(e[[1]] == "["){
          subscripts_elements <- e %>% as.character()
          return(paste0(subscripts_elements[2], "_{", subscripts_elements[-(1:2)] %>% paste(collapse = ","), "}"))
        }else if((op %in% c("nrow", "ncol"))){
          if(simple_mat2sum)
            return(eval(e))
          else 
            return(paste0(op %>% str_sub(2,2), "(", rec_convert(e[[2]]), ")"))
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
      expr <- expr %>% 
        str_replace_all("s1", "k") %>% 
        str_replace_all("s2", "l") %>% 
        str_replace_all("s3", "m") %>% 
        str_replace_all("s4", "n") %>% 
        str_replace_all("s5", "o")
  }
  
  # subscriptの処理
  expr <- gsub("([a-zA-Z0-9]+)_([a-zA-Z0-9]+)", "{{\\1}_{\\2}}", expr)
  # expr <- gsub("([a-zA-Z]+)([0-9]+)", "\\1_{\\2}", expr)
  expr <- gsub("(?<!_)([A-Za-z]+)([0-9]+)(?!}_)", "{\\1_{\\2}}", expr, perl = TRUE)
  
 
  
  if(doller){
    expr <- paste0("$$", expr, "$$")
  }
  # 変換結果を返す
  return(expr)
}



to_tex_matrix <- function(df, type =c("matrix")) {
  type = match.arg(type)
  
  if(type == "matrix"){
    # データフレームの各要素を変換
    tex_matrix <- apply(df, c(1,2), to_latex, doller = FALSE)
    
    # 行列を LaTeX の bmatrix 形式で構築
    tex_code <- paste0(apply(tex_matrix, 1, paste, collapse = " & "), collapse = " \\\\\n")
    tex_code <- paste0("\\begin{bmatrix}\n", tex_code, "\n\\end{bmatrix}")
    
    return(tex_code)
  }
}
