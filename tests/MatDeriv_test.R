if(0){
  setwd("lazy_symbolic_addon/")
  library(lazy.symbolic)
  library(tidyr)
  
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
  
  Dm_core("f(A)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%X)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%inv(X))", "X") %>% to_latex(print_html = TRUE)
  Dm_core("det(X)", "X") %>% to_latex(print_html = TRUE)
  
  # reorderが必要
  Dm_core("tr(X%*%C)", "X")%>% to_latex(print_html = TRUE)
  Dm_core("tr(A%*%X%*%C)", "X") %>% to_latex(print_html = TRUE)
  
  # reorderをf(X)に対応
  Dm_core("tr(inv(X)%*% A)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(inv(X)%*% inv(A) %*% B %*% C)", "X") %>% to_latex(print_html = TRUE)
   
  # べき乗の処理
  simplify_power("X%*%X%*%A%*%C%*%A%*%A%*%A") %>% to_latex(print_html = TRUE)
  Dm_core("tr(X^3%*%B)", "X") %>% to_latex(print_html = TRUE) # 数字、文字両方対応
  Dm_core("tr(X^p%*%B)", "X") %>% to_latex(print_html = TRUE)
  Dm_core("tr(B%*%X^p)", "X")
  
  # Hadamar積を含む場合
  # to_latexがあだマール積を\bigodotに変換していないので、今後要修正。でも、スカラと行列をを区別できないので難しそう。
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
  
  # 構造木の確認
  expr_str <- "tr((inv(X)%*% inv(A)) %*% B %*% C)"
  expr_str <- "tr(inv(X) + inv(A) + B + C)"
  expr_str <- "X*A %*% B"
  eval(parse(text = glue::glue("ast({expr_str})") ))
}



# core のみで成立

# expr_str <- "f(A)"
# result_str <- "O"
# testthat::expect_equal(Dm_core(expr_str, "X"), parse(text=result_str)[[1]])


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
  expect_equal(Dm_core("det(X)", "X"),                           easy_parse("det(X) %*% inv(t(X))"))
  
  # reorderが必要
  expect_equal(Dm_core("tr(X%*%C)", "X"),                        easy_parse("t(C)"))
  expect_equal(Dm_core("tr(A%*%X%*%C)", "X"),                    easy_parse("t(C %*% A)"))
  
  # reorderをf(X)に対応
  expect_equal(Dm_core("tr(inv(X)%*% A)", "X"),                  easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(Dm_core("tr(inv(X)%*% inv(A) %*% B %*% C)", "X"), easy_parse("-t(inv(X) %*% inv(A) %*% B %*% C %*% inv(X))"))
})

test_that("Dm_core derivatives", {
  expect_equal(Dm_core("f(A)", "X"),                             easy_parse("O"))
  expect_equal(Dm_core("tr(A%*%X)", "X"),                        easy_parse("t(A)"))
  expect_equal(Dm_core("tr(A%*%inv(X))", "X"),                   easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(Dm_core("tr(A%*%ginv(X))", "X"),                   easy_parse("-t(ginv(X) %*% A %*% ginv(X))"))
  expect_equal(Dm_core("det(X)", "X"),                           easy_parse("det(X) %*% inv(t(X))"))
  
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
  expect_equal(Dm_core("tr(X^3%*%B)", "X"),               easy_parse("3 * (X^2 * t(B))"))
  expect_equal(Dm_core("tr(X^p%*%B)", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
  expect_equal(Dm_core("tr(B%*%X^p)", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
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










