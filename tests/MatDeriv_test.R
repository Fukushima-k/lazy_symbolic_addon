
if(0){
  library(lazy.symbolic)
  library(tidyr)
  source("main_functions/gradmn3.R")
  
  testthat::test_file("tests/MatDeriv_test.R")
  
  
  # * や + など、交換可能な演算において、指定したX_を一番右まで移動させる関数
  # trace内の%*%の移動に対応させるため、円順序的に交換する。
  trace_reorder("A%*%B%*%C", "A") 
  trace_reorder("A%*%B%*%C", "B")%>% to_latex(print_html = TRUE)
  trace_reorder("A%*%B%*%C", "C")%>% to_latex(print_html = TRUE)
  trace_reorder("A%*%B%*%C", "D")%>% to_latex(print_html = TRUE)
  trace_reorder("A%*%B%*%(C+D)", "A")%>% to_latex(print_html = TRUE)
  trace_reorder("AX+SJD*tr(A)", "A")%>% to_latex(print_html = TRUE)
  
  # 第二世代
  trace_reorder("A%*%B %*% inv(X)", "X")
  trace_reorder("B %*% inv(X)%*%A", "X")
  trace_reorder("(X*A)%*%B", "X")
  
  # 第三世代
  trace_reorder("A%*%t(X)", "X")
  trace_reorder("t(X)%*%B", "X")
  trace_reorder("A%*%t(X)%*%C", "X")
  
  Dm_core("f(A)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%X)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%inv(X))", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(t(X)%*%B)", "X")
  Dm_core("det(X)", "X") %>% to_latex(print_html = TRUE)
  
  # reorderが必要
  Dm_core("tr(X%*%C)", "X")%>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%X%*%C)", "X") %>% to_latex(print_html = TRUE)
  
  # reorderをf(X)に対応
  Dm_core("tr(inv(X)%*% A)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(inv(X)%*% inv(A) %*% B %*% C)", "X") %>% to_latex(print_html = TRUE)
   
  # べき乗の処理
  simplify_power("X%*%X%*%A%*%C%*%A%*%A%*%A") %>% to_latex(print_html = TRUE)
  simplify_power("X*X*A*C*A*A*A") %>% to_latex(print_html = TRUE)
  # 現状は、入れ子がうまく処理されない場合があるので要注意。
  simplify_power("A*X%*%A*A") 
  simplify_power("A%*%X*A%*%A") 
  # 要素のべき乗行列は微分可能
  Dm_core("tr(X^(3)%*%B)", "X") %>% to_latex(print_html = TRUE) # 数字、文字両方対応
  Dm_core("tr(X^(p)%*%B)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(B%*%X^(e))", "X")
  # 通常のべき乗行列はundefined
  Dm_core("tr(X^3%*%B)", "X") %>% to_latex(print_html = TRUE) # 数字、文字両方対応
  Dm_core("tr(X^d%*%B)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(B%*%X^p)", "X")
  
  # Hadamar積を含む場合
  # to_latexがあだマール積を\odotに変換していないので、今後要修正。でも、スカラと行列をを区別できないので難しそう。
  # 　　　-> 簡単に解決　二項演算子*の第一引数と第二引数に大文字が含まれている場合に\odotへ変換
  trace_reorder("(X*A)%*%B", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr((X*A)%*%B)", "X")  %>% to_latex(print_html = TRUE)
  Dm_core("tr((A*X)%*%B)", "X")
  Dm_core("tr((A*X*C)%*%B)", "X")
  Dm_core("tr((X*(A%*%C))%*%B)", "X")  %>% to_latex(print_html = TRUE)
  Dm_core("tr((X*A%*%C)%*%B)", "X")  # たぶんこれはダメ？？？
  Dm_core("tr((B%*%X*A%*%C)%*%B)", "X")
  # "X*A*B"  %>% trace_reorder("X", "*")
  # "X*A%*%B"%>% trace_reorder("X", "*")  
  # "X%*%D*A%*%B"  %>% trace_reorder("X", "*")
  
  # *のlatex出力の調整。
  
  c("A*B", "a*b", "3*5", "a*A", "3*D_a", "-5*b",  "+3*v", "a_A * c_D") %>% 
    sapply(to_latex) %>% 
    print_tex_as_html()
  c("A*B", "a*b", "3*5", "a*A", "3*D_a", "-5*b",  "+3*v", "a_A * c_D") %>% 
    sapply(to_latex, safe_prod = TRUE) %>% 
    print_tex_as_html()
  
  
  # f(X) + g(X)の微分
  Dm_core("tr(A%*%X)+tr(B%*%X)", "X")
  Dm_core("tr(A%*%X)*tr(B%*%X)", "X")
  Dm_core("tr(A%*%X)+tr(t(X)%*%B)+tr(C%*%B)", "X")
  Dm_core("exp(tr(A%*%X)) + exp(tr(B%*%X))", "X")
  Dm_core("log(tr(A%*%X))", "X")

  

  
  # 構造木の確認
  expr_str <- "tr((inv(X)%*% inv(A)) %*% B %*% C)"
  expr_str <- "tr(inv(X) + inv(A) + B + C)"
  expr_str <- "X*A %*% B"
  
  expr_str <- "X%*%A + D%*%B - A*A + C*C"
  eval(parse(text = glue::glue("lobstr::ast({expr_str})") ))
}



# core のみで成立

# expr_str <- "f(A)"
# result_str <- "O"
# testthat::expect_equal(Dm_core(expr_str, "X"), parse(text=result_str)[[1]])

check_numerical_identity <- function(funcs, seed = 123){
  if(seed == "r"){
    seed <- runif(1) * 1e+8
  }
  sapply(funcs, function(func){
    set.seed(seed)
    Xn <- matrix(rnorm(3*3), 3)
    An <- 3*matrix(rnorm(3*3), 3)
    Bn <- 0.3*matrix(rnorm(3*3), 3)
    Cn <- -1*matrix(rnorm(3*3), 3)
    O <- 0
    p <- sample(2:10, size = 1)
    Gradmn <- gradmn(func, X=Xn, A=An, B=Bn, C=Cn, O=O, p=p, print=0, debug=0 )
    Gradma <- gradma(func, X=Xn, A=An, B=Bn, C=Cn, O=O, p=p, print=0, debug=0 )
    max(abs(Gradmn - Gradma))
  })
}

easy_parse <- function(text){
  parse(text=text)[[1]]
}

library(testthat)

test_that("trace_reorder basic functionality", {
  expect_equal(trace_reorder("A%*%B%*%C", "A"),     easy_parse("B %*% C %*% A"))
  expect_equal(trace_reorder("A%*%B%*%C", "A"),     easy_parse("B %*% C %*% A"))
  expect_equal(trace_reorder("A%*%B%*%C", "B"),     easy_parse("C %*% A %*% B"))
  expect_equal(trace_reorder("A%*%B%*%C", "C"),     easy_parse("A %*% B %*% C"))
  expect_equal(trace_reorder("A%*%B%*%C", "D"),     easy_parse("A %*% B %*% C"))
  expect_equal(trace_reorder("A%*%B%*%(C+D)", "A"), easy_parse("B %*% (C + D) %*% A"))
  expect_equal(trace_reorder("AX+SJD*tr(A)", "A"),  easy_parse("AX + SJD * tr(A)"))
})

test_that("trace_reorder advanced expressions", {
  expect_equal(trace_reorder("A%*%B %*% inv(X)", "X"), easy_parse("A %*% B %*% inv(X)"))
  expect_equal(trace_reorder("B %*% inv(X)%*%A", "X"), easy_parse("A %*% B %*% inv(X)"))
})

test_that("Dm_core derivatives", {
  expect_equal(Dm_core("f(A)", "X"),                             easy_parse("O"))
  expect_equal(Dm_core("tr(A%*%X)", "X"),                        easy_parse("t(A)"))
  expect_equal(Dm_core("tr(A%*%inv(X))", "X"),                   easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(Dm_core("tr(A%*%ginv(X))", "X"),                   easy_parse("-t(ginv(X) %*% A %*% ginv(X))"))
  expect_equal(Dm_core("det(X)", "X"),                           easy_parse("det(X) * inv(t(X))"))
  
  # reorderが必要
  expect_equal(Dm_core("tr(X%*%C)", "X"),                        easy_parse("t(C)"))
  expect_equal(Dm_core("tr(A%*%X%*%C)", "X"),                    easy_parse("t(C %*% A)"))
  
  # reorderをf(X)に対応
  expect_equal(Dm_core("tr(inv(X)%*% A)", "X"),                  easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(Dm_core("tr(inv(X)%*% inv(A) %*% B %*% C)", "X"), easy_parse("-t(inv(X) %*% inv(A) %*% B %*% C %*% inv(X))"))
})


# -------------------------------------------------------------------------
# Powers of X under the trace
# -------------------------------------------------------------------------

test_that("powers of X", {
  expect_equal(simplify_power("X%*%X%*%A%*%C%*%A%*%A%*%A") ,　easy_parse("X^2 %*% A %*% C %*% A^3"))
  expect_equal(Dm_core("tr(X^(3)%*%B)", "X"),               easy_parse("3 * (X^(2) * t(B))"))
  expect_equal(Dm_core("tr(X^(p)%*%B)", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
  expect_equal(Dm_core("tr(B%*%X^(p))", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
})

# -------------------------------------------------------------------------
# Hadamard‑product cases
# -------------------------------------------------------------------------

test_that("Hadamard product under trace", {
  expect_equal(Dm_core("tr((X*A)%*%B)", "X"),               easy_parse("A * t(B)"))
  expect_equal(Dm_core("tr((A*X)%*%B)", "X"),               easy_parse("A * t(B)"))
  expect_equal(Dm_core("tr((A*X*C)%*%B)", "X"),               easy_parse("C * A * t(B)"))
  expect_equal(Dm_core("tr((X*(A%*%C))%*%B)", "X"),               easy_parse("(A %*% C) * t(B)"))
})


# -------------------------------------------------------------------------
# transpose and basic fomula
# -------------------------------------------------------------------------

test_that("transpose", {
  expect_equal(Dm_core("tr(A%*%t(X))", "X")             ,easy_parse("t(t(A))"))
  expect_equal(Dm_core("tr(t(X)%*% B)", "X")            ,easy_parse("t(t(B))"))
  expect_equal(Dm_core("tr(A%*%t(X)%*%C%*%t(B))", "X")  ,easy_parse("t(t(A) %*% B %*% t(C))"))
})

test_that("basic fomula", {
  expect_equal(Dm_core("tr(A%*%X)+tr(B%*%X)", "X")                ,easy_parse("t(A) + t(B)"))
  expect_equal(Dm_core("tr(A%*%X)*tr(B%*%X)", "X")                ,easy_parse("t(A) * tr(B %*% X) + t(B) * tr(A %*% X)"))
  expect_equal(Dm_core("tr(A%*%X)+tr(t(X)%*%B)+tr(C%*%B)", "X")   ,easy_parse("t(A) + t(t(B)) + O"))
  expect_equal(Dm_core("exp(tr(A%*%X)) + exp(tr(B%*%X))", "X")    ,easy_parse("exp(tr(A %*% X)) * t(A) + exp(tr(B %*% X)) * t(B)"))
  expect_equal(Dm_core("log(tr(A%*%X))", "X")                     ,easy_parse("1/(tr(A %*% X)) * t(A)"))
})


# -------------------------------------------------------------------------
# compared with numerical gradients
# -------------------------------------------------------------------------

c("tr(A%*%inv(X))",
  "det(X)",
  "tr(inv(X)%*% inv(A) %*% B %*% C)",
  "tr(X^(3)%*%B)",
  "tr(B%*%X^(p))",
  "tr((X*A)%*%B)",
  "tr((X*(A%*%C))%*%B)",
  "tr(A%*%X)+tr(B%*%X)",
  "tr(A%*%X)+tr(X%*%B)+tr(C%*%B)",
  "tr(A%*%X)*tr(X%*%B)",
  "exp(tr(A%*%X)) * tr(B%*%X)",
  "tr(A%*%t(X))",
  "tr(A%*%t(X)%*%C%*%B)",
  "tr(A%*%t(X)%*%C%*%t(B))",
  # "exp(tr(A%*%X)) * exp(tr(B%*%X))", # どこかで代入に失敗している模様。
  "exp(tr(A%*%X)) + exp(tr(B%*%X))"
) %>% 
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, 0.00001)










