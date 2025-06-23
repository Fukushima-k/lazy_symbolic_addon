#' Convert the input string to LaTex and display it on the RStudio viewer
#'
#'
#' @param expr_str The input expression as a string
#' @param dollar = FALSE to omit the encompassing dollar signs
#' @param mat2sum = TRUE if the input expression contains summation
#' @param simple_mat2sum = TRUE if the input expression contains summation
#' and the simpler LaTeX is prefered
#' @param print_html = TRUE to display the result in RStudio's Viewer pane.
#' @param undef_Greek Character string specifying how to handle undefined Greek macros (e.g., \Zeta).
#'   Use `"strip"` to print them as plain strings (e.g., `\Zeta` → `"Zeta"`), or `"initial"` to print only their initial character (e.g., `"Z"`).
#' 
#' @details
#' This function requires \code{htmltools} package when \code{print_html=TRUE}.
#'
#' @return
#' LaTeX expression of \code{expr}
#'
#' @examples
#' modify_math_operators()
#' A <- demomat(2,3, root="a")
#' B <- demomat(3,2, root="b")
#' AB=A%*%B
#' printm(AB)
#' AB_tex <- to_latex( AB )
#' printm(AB_tex)
#'
#' sexpr <- mat2sum( "A%*%B+C" )
#' printm(sexpr)
#' sexpr_tex <- to_latex( sexpr, mat2sum=TRUE )
#'
#' \dontrun{
#' library(htmltools)
#' print_tex_as_html(AB_tex)
#' AB_tex <- to_latex( t(A)+B, print_html=TRUE )
#' sexpr_tex <- to_latex( sexpr, mat2sum=TRUE, print_html=TRUE )
#' sexpr_tex <- to_latex( sexpr, simple_mat2sum=TRUE, print_html=TRUE )
#' }
#'
#'
#' @author Contributed by Dr. Kentaro Fukushima of Osaka University.
#'
#' @export
#'

to_latex <- function(expr_str, dollar = TRUE,
                     mat2sum = FALSE, simple_mat2sum = FALSE,
                     print_html = FALSE, undef_Greek =c("strip", "keep", "initial")){
  
  if(is.matrix(expr_str)){
    # データフレームの各要素を変換
    tex_matrix <- apply(expr_str, c(1,2), to_latex_core, dollar = FALSE,
                        undef_Greek = undef_Greek)
    
    # 行列を LaTeX の bmatrix 形式で構築
    tex_code <- paste0(apply(tex_matrix, 1, paste, collapse = " & "), collapse = " \\\\\n")
    tex_code <- paste0("\\begin{bmatrix}\n", tex_code, "\n\\end{bmatrix}")
    
    if(print_html) print_tex_as_html(tex_code)
    return(tex_code)
  }else{
    return(to_latex_core(expr_str,
                         dollar=dollar,
                         mat2sum = mat2sum,
                         simple_mat2sum = simple_mat2sum,
                         print_html = print_html,
                         undef_Greek = undef_Greek))
  }
} # end of to_latex
 


#' Print the LaTex as html in RStudio Viewer pane or a RMarkdown (Quarto) document
#'
#'
#' @param TeX_code The input string vector containing LaTeX expressions
#' @param annotation (Optional) A string vector containing annotations for the expressions. 
#'
#'
#' @examples
#'
#' \dontrun{
#' library(htmltools)
#' modify_math_operators()
#' A <- demomat(2,3, root="a")
#' B <- demomat(3,2, root="b")
#' AB=A%*%B
#' printm(AB)
#' AB_tex <- to_latex( AB )
#' printm(AB_tex)
#' print_tex_as_html(AB_tex)
#' AB_tex <- to_latex( t(A)+B, print_html=TRUE )
#' sexpr_tex <- to_latex( sexpr, mat2sum=TRUE )
#' print_tex_as_html(sexpr_tex)
#' }
#'
#'
#'
#'
#' @author Contributed by Dr. Kentaro Fukushima at Osaka University.
#'
#'
#'
#'
#' @export
#'

print_tex_as_html <- function(TeX_code, annotation){
  
  if(missing(annotation)) annotation <- ""
  
  if (isTRUE(getOption("knitr.in.progress"))) {
    # mdに出力する。
    message("Document is being knitted via knitr, so expressions will appear in the output document, not in the Viewer.")
    cat(annotation)
    return(knitr::asis_output(TeX_code))
  } 
  
  if(!require(htmltools)){
    stop("package 'htmltools' is required for print_tex_as_html()")
  }

  Ps <- lapply(paste0(annotation, TeX_code), p)
  
  html_math <- tags$html(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
    do.call(tags$body, Ps)
  )
  
  # View()の代わりにブラウザで表示（HTMLレンダリングされる）
  html_print(html_math)
} # end of print_tex_as_html



#' Simulate str_replace_all of tidyverse.
str_replace_all <- function( string, pattern, replacement ){
  gsub( pattern, replacement, string )
}


#' The core part of to_latex functions by Dr. Fukushima
to_latex_core <- function(expr_str, dollar = TRUE,
                          mat2sum = FALSE, simple_mat2sum = FALSE,
                          print_html = FALSE, undef_Greek =c("strip", "keep", "initial")) {
  if(is.call(expr_str)){
    save_expr_str <- deparse(expr_str)
    expr <- expr_str
  }else{
    save_expr_str <- expr_str
    
    if(mat2sum||simple_mat2sum){
      expr_str <- expr_str |> str_replace_all("\\{", "chu_kakko\\(") |> str_replace_all("\\}", "\\)")
    }
    
    # 入力文字列を R のexpressionとしてパース
    expr <- e <- tryCatch(parse(text = expr_str)[[1]], error = function(e) {
      warning("入力が有効な R 式ではありません")
      return(NULL)
    })
    if (is.null(expr)) return(expr_str)
  }
  
  
  op_supsc <- c("t", "T", "ginv", "inv")
  supsc <- c("T", "T", "-", "-1")
  
  # 再帰的に式を LaTeX 文字列に変換する内部関数
  rec_convert <- function(e) {
    if (is.symbol(e)) {
      # 変数名の場合
      e <- as.character(e)
      # 変数名を LaTeX のコマンドに変換 (tuika)
      e <- greeknum(e, undef_Greek=undef_Greek)
      return(e)
    } else if (is.numeric(e)) {
      return(as.character(e))
    } else if (is.call(e)) {
      # 呼び出しの場合：演算子や関数呼び出し
      op <- as.character(e[[1]])
      
      if(mat2sum||simple_mat2sum){
        if(op == "s"){
          # sum_scripts <- e[[3]] |> as.character()
          ss <- e[[3]]  # sum_scripts
          if(length(ss) <= 2)
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "}", rec_convert(e[[2]])))
          else
            return(paste0("\\sum_{", rec_convert(ss[[2]]), "=",rec_convert(ss[[3]]), "}^{", rec_convert(ss[[4]]), "}", rec_convert(e[[2]])))
        }else if(op == "*"){
          return(paste0(rec_convert(e[[2]]), rec_convert(e[[3]])))
          # return(paste0(e[[2]], " \\cdot ", e[[3]]))
        }else if(op == "["){
          subscripts_elements <- as.character(e)
          if(is.call(e[[2]])){
            return(paste0("\\left(", rec_convert(e[[2]]), "\\right)_{"
                          , paste(subscripts_elements[-(1:2)], collapse = ","), "}"))
          }else{
            return(paste0(rec_convert(e[[2]]), "_{", paste(subscripts_elements[-(1:2)], collapse = ","), "}"))
          }
        }else if(op == "+"){
          return(paste0(rec_convert(e[[2]]), "+", rec_convert(e[[3]])))
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
        if(is.call(e[[2]])){
          return(paste0(
            "{\\left(", rec_convert(e[[2]]), "\\right)}^{"
            , supsc[op == op_supsc], "}"))
        }else{
          return(paste0("{", rec_convert(e[[2]]), "}^{"
                        , supsc[op == op_supsc], "}"))
        }
      }else if(op == "["){
        stop("作成中。mat2sumの場合はmat2sum=TRUEにして実行してください。")
        
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
    expr <- expr |>
      str_replace_all("s1", "k") |>
      str_replace_all("s2", "l") |>
      str_replace_all("s3", "m") |>
      str_replace_all("s4", "n") |>
      str_replace_all("s5", "o")
  }
  
  # subscriptの処理
  expr <- gsub("([a-zA-Z0-9]+)_([a-zA-Z0-9]+)", "{{\\1}_{\\2}}", expr)
  # expr <- gsub("([a-zA-Z]+)([0-9]+)", "\\1_{\\2}", expr)
  expr <- gsub("(?<!_)(\\\\?[A-Za-z]+)([0-9]+)(?!}_)", "{\\1_{\\2}}", expr, perl = TRUE)
  
  
  
  if(dollar|print_html){
    expr <- paste0("$$", expr, "$$")
  }
  
  # greek chars with subs: sm20250610
  # expr=greeknum(expr)
  
  
  # 変換結果を返す
  if(print_html) print_tex_as_html(expr, save_expr_str)
  return(expr)
  
} # end of to_latex_core

#' convert subscripted Greek characters
greeknum <- function( texpr, debug=0, undef_Greek =c("strip", "keep", "initial")){
  # convert subscripted Greek characters
  # Shin-ichi Mayekawa
  # 20250610
  #
  
  #
  # Args:
  #
  #  texpr A LaTeX expression from to_latex function
  #
  
  # order matters!!
  GreekChars=c("Alpha",  "Beta",  "Gamma",  "Delta",  "Epsilon"
               , "Zeta",  "Theta", "Eta"
               , "Iota",  "Kappa",  "Lambda",  "Mu",  "Nu",  "Xi",  "Omicron",  "Pi",  "Rho"
               , "Sigma",  "Tau",  "Upsilon",  "Phi",  "Chi",  "Psi",  "Omega" )
  greekChars=tolower(GreekChars)
  
  for( i in 1:length(GreekChars) ){
    from=paste("(?<!\\\\)",GreekChars[i],sep="")
    to=paste("\\\\",GreekChars[i],sep="")
    texpr=gsub(from,to,texpr,perl=TRUE)
    from=paste("(?<!\\\\)",greekChars[i],sep="")
    to=paste("\\\\",greekChars[i],sep="")
    texpr=gsub(from,to,texpr,perl=TRUE)
    if( debug ) printm(i,from,to,texpr)
    
    if(0){
      from=paste(GreekChars[i],"([0-9]+)",sep="")
      to=paste("\\\\",GreekChars[i],"_\\{\\1\\}",sep="")
      texpr=gsub(from,to,texpr)
      from=paste(greekChars[i],"([0-9]+)",sep="")
      to=paste("\\\\",greekChars[i],"_\\{\\1\\}",sep="")
      texpr=gsub(from,to,texpr)
    }
  }
  
  # take care of zeta and eta.
  texpr=gsub("\\\\z\\\\eta","\\\\zeta",texpr)
  texpr=gsub("\\\\Z\\\\eta","\\\\Zeta",texpr)
  texpr=gsub("\\\\th\\\\eta","\\\\theta",texpr)
  texpr=gsub("\\\\Th\\\\eta","\\\\Theta",texpr)

  texpr=gsub("\\\\B\\\\eta","\\\\Beta",texpr)
  texpr=gsub("\\\\b\\\\eta","\\\\beta",texpr)
  
  texpr=gsub("\\\\E\\\\psi","\\\\Epsi",texpr)
  texpr=gsub("\\\\e\\\\psi","\\\\epsi",texpr)
  texpr=gsub("\\\\U\\\\psi","\\\\Upsi",texpr)
  texpr=gsub("\\\\u\\\\psi","\\\\upsi",texpr)
  
  
  # 
  undefined_Greeks <-  c("Alpha",  "Beta",  "Epsilon", "Zeta", "Eta"
            , "Iota",  "Kappa",  "Mu",  "Nu", "Omicron",  "Rho"
            ,  "Tau",  "Chi")
  undef_Greek <- match.arg(undef_Greek)
  if(undef_Greek == "strip"){
    for(ug in undefined_Greeks){
      texpr <-
        gsub(sprintf("\\\\(%s)", ug), ug, texpr)
    }
  }else if(undef_Greek == "initial"){
    for(ug in undefined_Greeks){
      texpr <-
        gsub(sprintf("\\\\(%s)", ug), substr(ug,1,1), texpr)
    }
  }
  
  return( texpr)
  
} # end of greeknum




