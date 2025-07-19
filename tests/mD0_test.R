criteria = 0.00001
criteria = 0.01

library(lazy.symbolic)
library(tidyr)
modify_math_operators()

if(0){
  
  source("main_functions/Fukushima_MatDeriv.R")
  source("main_functions/Fukushima_main_functions.R")
  source("main_functions/Fukushima_tools_for_expr.R")
  
  testthat::test_file("tests/mD0_test.R")
  
  
  # mD0("tr((inv(X)*t(X))%*%A)","X") の前に、まずmD0("tr((inv(X)*X)%*%A)","X")を見ます。
  # mD0("tr((inv(X)*X)%*%A)","X")は内部的に、mD0("tr(A%*%(FX))", "FX")を計算しているわけですが、
  # mD0("tr(A%*%(X))", "X")自体がそもそも計算できないことが問題なので、drop_parens()によって
  # かっこを外す対処しました。伴って、S6の部分を微修正以下のように修正しました。
  # AA = deparse(most_right[[2]][[2]])
  # -> AA = deparse(most_right[[2]])
  #   
  # これで、
  # mD0("tr((inv(X)*X)%*%A)","X")
  
  mD0("tr((inv(X)*t(X))%*%A)","X", debug = 1)
  # 解析解
  "t(t(t(inv(X)*t(A))))+mD0(tr(t(A)*t(t(t(t(X))))%*%inv(X)),X)"
  # 解析解の数値的評価
  # t(t(t(Inv(Xn)*t(An)))) + gradmn("tr(t(A)*t(t(t(t(X))))%*%inv(X))", X=Xn, A=An)
  
  # 数値解
  gradmn("tr((inv(X)*t(X))%*%A)", X = Xn, A = An)
  
  # mD0("tr((inv(X)*t(X))%*%A)","X", debug = 1, trace_chain = 0)
  # 解析解
  "t(inv(X)*t(A))"
  # 解析解の数値的評価
  t(Inv(Xn)*t(An))
  
  # これは、(55)ではなく(57)だったんで、P2の実装が必要。
  # P２には、tr(A%*%(F(X)* B))の微分が必要。
  check_numerical_identity("tr(A%*%(X*B))", seed="r")
  check_numerical_identity("tr(A%*%(t(X)*B))", seed="r")
  
  check_numerical_identity("tr(A%*%(inv(X)*B))", seed="r")
    mD0("tr((inv(X)*B)%*%A)")
    gradmn("tr((inv(X)*B)%*%A)", X = Xn, A = An, B=Bn)
  
  mD0("tr(A %*% (B * inv(X)))")
  mD0("tr(A %*% (FX * inv(X)))")
  
  # 後は、この分解ができるように誘導  
  mD0("tr((inv(X)*t(X))%*%A)")
  check_numerical_identity("tr((inv(X)*t(X))%*%A)", seed = "r")
  # gradmn("tr((inv(X)*t(X))%*%A)", X = Xn, A = An)
  
  
  
  
  mD0("tr((inv(X)%*%t(X))%*%A)")
  mD0("tr((inv(X)%*%t(X))%*%A)", "X")
  
  
  
  
  inv <- function(d){Inv(d)}
  
  mD0("tr(inv(X)%*%t(X)%*%A)") %>% to_latex(print_html = TRUE)
  # 解析解の評価
  An_mD0 = t(t(Inv(Xn)) %*% t(An)) - t(Inv(Xn) %*% (t(Xn) %*% An) %*% Inv(Xn))
  An_GPT = An%*%inv(Xn) - t(inv(Xn))%*%t(An)%*%t(inv(Xn))
  Nm_myk = gradmn("tr(inv(X)%*%t(X)%*%A)", X=Xn,A=An)
  printm(An_mD0,An_GPT,Nm_myk)
  
  Xn <- Xn+0.00001
  tr(inv(Xn)%*%t(Xn)%*%An)
  
  
  # Chain Rule
  # functions to be differentiated
  func = "tr(A %*% inv(t(X))) "
  
  
  func <- "tr(A %*% inv(t(X))) "
  func %>% mD0("X", trace_chain = 1)
  func %>% mD0("X", trace_chain = 0)
  
  func <- "tr(A%*%t(inv(X)))"  
  func %>% mD0("X", trace_chain = 1)
  func %>% mD0("X", trace_chain = 0)
  
  
  func <- "tr(inv(B%*%X%*%C)%*%A)"
  func %>% mD0("X", trace_chain = 1)
  func %>% mD0("X", trace_chain = 0)
  
  func <- "tr(A%*%inv(t(X)%*%B%*%X)%*%C)"
  func %>% mD0("X", trace_chain = 1)
  func %>% mD0("X", trace_chain = 0)
  
  func <- "tr((inv(X)*t(X))%*%A)"
  # func %>% mD0("X", trace_chain = 1)
  func %>% mD0("X", trace_chain = 0)
  
  
  
  
  
  
  　　　
  # mD0("tr(inv(t(X))%*%t(inv(t(X))))","X")
  # mD0("tr(inv(t(X))) ","X")
  # 
  
  
  
  # tr(A%*%B)以外の対処

  # I%*%をつけ足せば、理論上計算できるケースも多い。
  mD0("tr(inv(t(X))) ","X", debug=1)
  mD0("tr(I%*%inv(t(X))) ","X")
  
  mD0("tr(A*X) ","X")
  # mD0("tr(A%*%X) ","X")
  mD0("tr(I%*%(A*X)) ","X")
  
  expr <- 
    call("%*%", as.symbol("A"), call("*", as.symbol("B"), as.symbol("X")))
  trace_reorder(expr, "X")
    
  # この二つは微分計算できないので、要対処
  trace_reorder("t(A) %*% t(B) %*% t(X * A)", "X")
  mD0("t(A) %*% t(B) %*% t(X * A)", "X")
  trace_reorder("t(A) %*% t(B) %*% t((X) * A)", "X")
  mD0("t(A) %*% t(B) %*% t((X) * A)", "X")
  
  
  
  
  
  # to do 
  # 転置をadd_transposeに　#実装
  # t(t(A)) -> A ; inv(inv(A)) -> A ; A %*% I -> Aに。 # 実装
  # mD0_parsed("tr(A*(X))", "X", debug = 1)    #
  mD0_parsed("tr(A*(X))", "X", debug = 1)    #
  decompose_MatProd("A*(X)", "X")    #
  decompose_MatProd("(A*(X))", "X")    #
  decompose_MatProd("tr((A*(X)))", "X")    #
  # 構文木上の無駄かっこを外す。
  # 　decompose("A%*%((X%*%B))")を分解できるように。 # 対応済み
  #   decompose_MatProd("(A*(X))", "X")    #
  #   decompose_MatProd("tr((A*(X)))", "X")    #
  # decomposeで外れるかっこ
    decompose_MatProd("A*(((X)))", "*")    #
    decompose_MatProd("((A*((X))))", "*", target_X = "X")    #
  
  # 外れないかっこ
    expr <- "((tr(A*((X)))))"
    expr <- "((tr((A*(((X)))))))"
    expr <- "(tr(((X))*A))"
    expr <- "exp((tr(((X))*A)))"
    # expr <- "tr(((X))*A)"
    expr %>% decompose_MatProd("*", target_X = "X")    #
    expr %>% trace_reorder("X")    #
    expr %>% mD0(debug = 1)    #
    
    
    
    
  # cancel_double_expr(t(t(A)%*%t(B))) -> B%*%Aにできるように。
  # cancel_double_expr(t(-t(A))) -> -Aにできるように。# できるようになりました。
    cancel_double_expr("t(-t(A))", use_unary_reorder = TRUE)
    
  # cancel_double_expr("-inv(t(-(B)) - A)", use_unary_reorder=TRUE) # これ期待通りの挙動ではないので要修正
    # 修正完了
    unary_reorder_expr("inv(t(-(B)) - A)", "-") # good 
    cancel_double_expr("-inv(t(-(B)) - A)", use_unary_reorder=TRUE) # g
  
  
    
    
    
    
}

# assign_at_expr(expr, numeric(0))

# core のみで成立

# expr_str <- "f(A)"
# result_str <- "O"
# testthat::expect_equal(Dm_core(expr_str, "X"), parse(text=result_str)[[1]])


X <- matrix(rnorm(3*3), 3)
A <- 3*matrix(rnorm(3*3), 3)
B <- 0.3*matrix(rnorm(3*3), 3)
C <- -1*matrix(rnorm(3*3), 3)
O<- X*0
p <- sample(2:10, size = 1)
I <- diag(3)

b <- rnorm(3)
Y <- rnorm(3)

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
    O <- Xn*0
    p <- sample(2:10, size = 1)
    In <- diag(3)
    Gradmn <- gradmn(func, X=Xn, A=An, B=Bn, C=Cn, O=O, I = In, p=p, print=0, debug=0 )
    Gradma <- gradma(func, X=Xn, A=An, B=Bn, C=Cn, O=O, I = In, p=p, print=0, debug=0 )
    max(abs(Gradmn - Gradma))
  })
}


check_numerical_identity_by_b <- function(funcs, seed = 123){
  if(seed == "r"){
    seed <- runif(1) * 1e+8
  }
  sapply(funcs, function(func){
    set.seed(seed)
    
    bn <- rnorm(3)
    Yn <- rnorm(3)
    Xn <- matrix(rnorm(3*3), 3)
    An <- 3*matrix(rnorm(3*3), 3)
    Bn <- 0.3*matrix(rnorm(3*3), 3)
    Cn <- -1*matrix(rnorm(3*3), 3)
    O <- Xn*0
    p <- sample(2:10, size = 1)
    In <- diag(3)
    Gradmn <- gradmn(func, b=bn, Y=Yn, X=Xn, A=An, B=Bn, C=Cn, O=O, I = In, p=p, print=0, debug=0 )
    Gradma <- gradma(func, b=bn, Y=Yn, X=Xn, A=An, B=Bn, C=Cn, O=O, I = In, p=p, print=0, debug=0 )
    max(abs(Gradmn - Gradma))
  })
}

mD0_parsed <- function(...){
  easy_parse(mD0(...))
}

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
    dexpr = mD0(expr, arg)
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

library(testthat)

test_that("trace_reorder basic functionality", {
  expect_equal(trace_reorder("A%*%B%*%C", "A"),     easy_parse("B %*% C %*% A"))
  expect_equal(trace_reorder("C%*%A%*%B", "A"),     easy_parse("B %*% C %*% A"))
  expect_equal(trace_reorder("B%*%C%*%A", "A"),     easy_parse("B %*% C %*% A"))
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

test_that("trace_reorder both * a and  %*%", {
  # Hadamar product
  expect_equal(trace_reorder("A * B * X", "X"), easy_parse("A*B*X"))
  expect_equal(trace_reorder("B * X * A", "X"), easy_parse("A*B*X"))
  expect_equal(trace_reorder("X * A * B", "X"), easy_parse("A*B*X"))

  # # hirarchy
  expect_equal(trace_reorder("A%*%(X%*%B)%*%C", "X"), easy_parse("B %*% C %*%  A %*% X "))
  expect_equal(trace_reorder("A*(X*B)*C", "X"),       easy_parse("B  *  C  *   A  *  X "))
  expect_equal(trace_reorder("A%*%(X*B)%*%C", "X"),   easy_parse("C %*% A %*% (B  *  X)"))
  expect_equal(trace_reorder("A*(X%*%B)*C", "X"),     easy_parse("C  *  A  *  (X %*% B)")) # この場合は、どう処理をするべきか。
  expect_equal(trace_reorder("A%*%B%*%(F%*%((E%*%(X%*%C))%*%D))", "X") , easy_parse("C %*% D %*% A %*% B %*% F %*% E %*% X"))
  expect_equal(trace_reorder("(A%*%B)%*%(X%*%C)", "X") , easy_parse("C %*% (A %*% B) %*% X"))
     drop_parens("(A%*%B)%*%(X%*%C)")

  # decompose_MatProd("A%*%B%*%((X%*%C)%*%D)", "%*%", flat = TRUE)
  # decompose_MatProd("A*B*((X%*%C)*D)", "%*%", flat = TRUE)
  # decompose_MatProd("A*B*((X*C)*D)", "*", flat = TRUE)
  # decompose_MatProd("A+B-((X+C)+D)-E", c("+", "-"), flat = TRUE, return_op = TRUE)


  # trace
  expect_equal(trace_reorder("t(A) * t(B) * t(X)", "X"), easy_parse("B*A*X"))
  expect_equal(trace_reorder("t(B) * t(X) * t(A)", "X"), easy_parse("B*A*X"))
  expect_equal(trace_reorder("t(X) * t(A) * t(B)", "X"), easy_parse("B*A*X"))

  expect_equal(trace_reorder("t(A) %*% t(B) %*% t(X)", "X"), easy_parse("B%*%A%*%X"))
  expect_equal(trace_reorder("t(B) %*% t(X) %*% t(A)", "X"), easy_parse("B%*%A%*%X"))
  expect_equal(trace_reorder("t(X) %*% t(A) %*% t(B)", "X"), easy_parse("B%*%A%*%X"))

  expect_equal(trace_reorder("t(A) %*% t(B) %*% (t(X) %*% A)", "X"), easy_parse("B %*% A %*%  t(A) %*% X "))
  expect_equal(trace_reorder("t(A) %*% t(B) %*% (t(X) * A)", "X"),   easy_parse("B %*% A %*% (t(A)  *  X)"))

  # 複数存在する場合は、先頭を後ろに持ってくる。, easy_parse())
  expect_equal(trace_reorder("A%*%X%*%B%*%X%*%C", "X"), easy_parse("B %*% X %*% C %*% A %*% X"))
  expect_equal(trace_reorder("(A*X)%*%B%*%X%*%C", "X"), easy_parse("B %*% X %*% C %*% (A * X)"))

  # 多重かっこ外し。
  expect_equal(trace_reorder("tr(A%*%((((X%*%B)))))", "X"), easy_parse("tr(B %*% A %*% X)"))
  expect_equal(trace_reorder("tr(A%*%((((X*B)))))", "X"), easy_parse("tr(A %*% (B*X))"))
  expect_equal(trace_reorder("tr(A*((((X%*%B)))))", "X"), easy_parse("tr(A * (X %*% B))"))
})

test_that("mD0_parsed derivatives", {
  expect_equal(mD0_parsed("f(A)", "X"),                             easy_parse("O"))
  expect_equal(mD0_parsed("tr(A%*%X)", "X"),                        easy_parse("t(A)"))
  expect_equal(mD0_parsed("tr(A%*%inv(X))", "X"),                   easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(mD0_parsed("tr(A%*%ginv(X))", "X"),                  easy_parse("-t(ginv(X) %*% A %*% ginv(X))"))
  expect_equal(mD0_parsed("det(X)", "X"),                           easy_parse("det(X) * inv(t(X))"))

  # reorderが必要
  expect_equal(mD0_parsed("tr(X%*%C)", "X"),                        easy_parse("t(C)"))
  expect_equal(mD0_parsed("tr(A%*%X%*%C)", "X"),                    easy_parse("t(C %*% A)"))

  # reorderをf(X)に対応
  expect_equal(mD0_parsed("tr(inv(X)%*% A)", "X"),                  easy_parse("-t(inv(X) %*% A %*% inv(X))"))
  expect_equal(mD0_parsed("tr(inv(X)%*% inv(A) %*% B %*% C)", "X"), easy_parse("-t(inv(X) %*% inv(A) %*% B %*% C %*% inv(X))"))
})


# -------------------------------------------------------------------------
# Powers of X under the trace
# -------------------------------------------------------------------------

# test_that("powers of X", {
#   expect_equal(simplify_power("X%*%X%*%A%*%C%*%A%*%A%*%A") ,　easy_parse("X^2 %*% A %*% C %*% A^3"))
#   expect_equal(mD0_parsed("tr(X^(3)%*%B)", "X"),               easy_parse("3 * (X^(2) * t(B))"))
#   expect_equal(mD0_parsed("tr(X^(p)%*%B)", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
#   expect_equal(mD0_parsed("tr(B%*%X^(p))", "X"),               easy_parse("p * (X^(p - 1) * t(B))"))
# })

# -------------------------------------------------------------------------
# Hadamard‑product cases
# -------------------------------------------------------------------------

test_that("Hadamard product under trace", {
  expect_equal(mD0_parsed("tr((X*A)%*%B)", "X"),               easy_parse("A * t(B)"))
  expect_equal(mD0_parsed("tr((A*X)%*%B)", "X"),               easy_parse("A * t(B)"))
  expect_equal(mD0_parsed("tr((A*X*C)%*%B)", "X"),               easy_parse("C * A * t(B)"))
  expect_equal(mD0_parsed("tr((X*(A%*%C))%*%B)", "X"),               easy_parse("(A %*% C) * t(B)"))
})


# -------------------------------------------------------------------------
# transpose and basic fomula
# -------------------------------------------------------------------------

test_that("transpose", {
  expect_equal(mD0_parsed("tr(A%*%t(X))", "X")             ,easy_parse("t(t(A))"))
  expect_equal(mD0_parsed("tr(t(X)%*% B)", "X")            ,easy_parse("t(t(B))"))
  expect_equal(mD0_parsed("tr(A%*%t(X)%*%C%*%t(B))", "X")  ,easy_parse("t(t(A) %*% B %*% t(C))"))
})

test_that("basic fomula", {
  expect_equal(mD0_parsed("tr(A%*%X)+tr(B%*%X)", "X")                ,easy_parse("t(A) + t(B)"))
  expect_equal(mD0_parsed("tr(A%*%X)*tr(B%*%X)", "X")                ,easy_parse("t(A) * tr(B %*% X) + t(B) * tr(A %*% X)"))
  expect_equal(mD0_parsed("tr(A%*%X)+tr(t(X)%*%B)+tr(C%*%B)", "X")   ,easy_parse("t(A) + t(t(B)) + O"))
  expect_equal(mD0_parsed("exp(tr(A%*%X)) + exp(tr(B%*%X))", "X")    ,easy_parse("exp(tr(A %*% X)) * t(A) + exp(tr(B %*% X)) * t(B)"))
  expect_equal(mD0_parsed("exp(tr(A%*%X))", "X")                     ,easy_parse("exp(tr(A %*% X)) * t(A)"))
  expect_equal(mD0_parsed("log(tr(A%*%X))", "X")                     ,easy_parse("1/tr(A %*% X) * t(A)"))
})

test_that("chain rules ", {
  expect_equal(mD0_parsed("tr(A%*%t(B%*%X))", "X", debug = 1)                   , easy_parse("t(t(A) %*% B)"))
  expect_equal(mD0_parsed("tr(A*(B%*%X))", "X", debug = 1)                      , easy_parse("t(t(A * I) %*% B)"))
  expect_equal(mD0_parsed("tr(A*X)", "X", debug = 1)                            , easy_parse("A * t(I)"))
  expect_equal(mD0_parsed("tr(A%*%inv(B%*%X))", "X", debug = 1)                 , easy_parse("t(t(-t(inv(B %*% X) %*% A %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(inv(B%*%X))", "X", debug = 1)                     , easy_parse("t(t(-t(inv(B %*% X) %*% inv(B %*% X))) %*% B)"))

  expect_equal(mD0_parsed("tr(t(inv(B%*%X)))", "X", debug = 1)                  , easy_parse("t(t(-t(inv(B %*% X) %*% inv(B %*% X))) %*% B)"))

  expect_equal(mD0_parsed("tr(t(inv(t(inv(B%*%X)))))", "X", debug = 1)          , easy_parse("t(t(-t(inv(B %*% X) %*% -t(inv(t(inv(B %*% X))) %*%
    inv(t(inv(B %*% X)))) %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(t(inv(t(inv(B%*%X)))%*%C))", "X", debug = 1)      , easy_parse("t(t(-t(inv(B %*% X) %*% -t(inv(t(inv(B %*% X))) %*% C %*%
    inv(t(inv(B %*% X)))) %*% inv(B %*% X))) %*% B)"))


  expect_equal(mD0_parsed("tr(t(A)%*%t(B)%*%t(X*A))")   , easy_parse("A *t(B%*% A)"))
  expect_equal(mD0_parsed("tr(t(A)%*%t(B)%*%t(t(X)*A))"), easy_parse("t(A)*B%*%A"))
})

c(
  "tr(A%*%t(B%*%X))",
  "tr(A*(B%*%X))",
  "tr(A*(B%*%X))",
      "tr(t(A*t(I)) %*% B%*%X)",
      "tr(A*X)",
  "tr(A%*%inv(B%*%X))",
  "tr(inv(B%*%X))",
  "tr(t(inv(B%*%X)))",
  "tr(t(inv(t(inv(B%*%X)))))",
  "tr(t(inv(t(inv(B%*%X)))%*%C))",
  "tr(t(A)%*%t(B)%*%t(X*A))",
  "tr(t(A)%*%t(B)%*%t(t(X)*A))",
  "tr(A%*%X)"
) %>%
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)



# ignore_parens

test_that("chain rules ", {
  expect_equal(mD0_parsed("tr(A%*%t(B%*%X))", "X", debug = 1)                   , easy_parse("t(t(A) %*% B)"))
  expect_equal(mD0_parsed("tr(A*(B%*%X))", "X", debug = 1)                      , easy_parse("t(t(A * I) %*% B)"))
  expect_equal(mD0_parsed("tr(A*X)", "X", debug = 1)                            , easy_parse("A * t(I)"))
  expect_equal(mD0_parsed("tr(A%*%inv(B%*%X))", "X", debug = 1)                 , easy_parse("t(t(-t(inv(B %*% X) %*% A %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(inv(B%*%X))", "X", debug = 1)                     , easy_parse("t(t(-t(inv(B %*% X) %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(t(inv(B%*%X)))", "X", debug = 1)                  , easy_parse("t(t(-t(inv(B %*% X) %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(t(inv(t(inv(B%*%X)))))", "X", debug = 1)          , easy_parse("t(t(-t(inv(B %*% X) %*% -t(inv(t(inv(B %*% X))) %*%
    inv(t(inv(B %*% X)))) %*% inv(B %*% X))) %*% B)"))
  expect_equal(mD0_parsed("tr(t(inv(t(inv(B%*%X)))%*%C))", "X", debug = 1)      , easy_parse("t(t(-t(inv(B %*% X) %*% -t(inv(t(inv(B %*% X))) %*% C %*%
    inv(t(inv(B %*% X)))) %*% inv(B %*% X))) %*% B)"))
})

# t(t(A)) -> A ; inv(inv(A)) -> A ; A %*% I -> Aに。 # 実装
# mD0_parsed("tr(A*(X))", "X", debug = 1)    #
mD0_parsed("tr(A*(X))", "X", debug = 1)    #

# A*X decompose_MatProd()で外す。
expect_equal(decompose_MatProd("A*(X)", "*", target_X = "X"), decompose_MatProd("A*X", "*", target_X = "X"))
expect_equal(decompose_MatProd("A*(((X)))", "*", target_X = "X"), decompose_MatProd("A*X", "*", target_X = "X"))
expect_equal(decompose_MatProd("((A*(((X)))))", "*", target_X = "X"), decompose_MatProd("A*X", "*", target_X = "X"))

# mD0実行時の最初のdrop_parensで外れる部分。
expect_equal(mD0("tr(A%*%t(X))"),    mD0("(( tr( A %*% (t((X)) ) ) ))"))
expect_equal(mD0("tr(A%*%t(inv(X)%*%C))"),    mD0("(( tr( A %*% (t(( inv(X) %*% C ))) ) ))"))
expect_equal(mD0("tr(A%*%t(inv(X)))"),    mD0("(( tr( A %*% (t(( inv(X) ))) ) ))"))

expr <- "exp((tr(((X))*A)))"
drop_parens(expr)
mD0(expr)

expr <- "exp(tr(X*A))"
mD0(expr)

# X以外のsymbolについたかっこは、事後的にdrop_parensしないと外れない。
# expect_equal(
  mD0("tr(A%*%t(X))")
  # ,
  mD0("(( tr( ((A)) %*% (t((X)) ) ) ))") %>% drop_parens()
  # mD("(( tr( ((A)) %*% (t((X)) ) ) ))")
  # )




c(
  # "tr(A%*%(t(t(X*B)*C)))",
  "((tr((A*(inv((X)%*%C))))))", 
  "tr(A%*%X)"
) %>%
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)

# -------------------------------------------------------------------------
# compared with numerical gradients
# -------------------------------------------------------------------------
#
c(
  "tr(A%*%inv(X))",
  "det(X)",
  "tr(inv(X)%*% inv(A) %*% B %*% C)",
  # "tr(X^(3)%*%B)",
  # "tr(B%*%X^(p))",
  "tr((X*A)%*%B)",
  "tr((X*(A%*%C))%*%B)",
  "tr(A%*%X)+tr(B%*%X)",
  "tr(A%*%X)+tr(X%*%B)+tr(C%*%B)",
  "tr(A%*%X)*tr(X%*%B)",
  "exp(tr(A%*%X)) * tr(B%*%X)",
  "tr(A%*%t(X))",
  "tr(A%*%t(X)%*%C%*%B)",
  "tr(A%*%t(X)%*%C%*%t(B))",
  "exp(tr(A%*%X)) * exp(tr(B%*%X))",
  "exp(tr(A%*%X)) + exp(tr(B%*%X))",

  "tr(A%*%X)"
) %>%
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)

c(
  # mD3のテスト
  # product rule
  "tr(A%*%Inv(X)%*%B%*%X%*%C)",
  "tr(t(X)%*%A%*%X%*%t(X))",

  # Hadamar Product
  "tr((A*X)%*%B)",
  "tr((A*X)%*%B%*%X)",
  "tr((A*X)%*%B%*%inv(X))",

  # Chain Rule
  "tr(A %*% inv(t(X))) ",
  "tr(A%*%t(inv(X)))",
  "tr(inv(B%*%X%*%C)%*%A)",
  "tr(A%*%inv(t(X)%*%B%*%X)%*%C)",
  # "tr((inv(X)*t(X))%*%A)",

  "tr(A%*%X)"
) %>%
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)


c(
  "tr(inv(t(X)))",
  "tr(I%*%inv(t(X)))",

  "tr(A*X) ",
  "tr(I%*%(A*X)) ",
  "tr(A%*%X)"
) %>%
  check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)



########################################
# 0715 %.%も処理
########################################

mD0("tr(A%*%(t(t(X*B)*C)))")


trace_reorder("(t(C) * (t(B) %.% X))", "X")
trace_reorder("tr(C %*% (B * t(X)))", "X")
trace_reorder("tr(C %*% (B %.% t(X)))", "X")

trace_reorder("tr(C %*% (C * B %.% t(X)))", "X")
trace_reorder("tr(C %*% (C %.% B * t(X)))", "X")
trace_reorder("tr(C %*% (C*(B * t(X))))", "X")


# トレース内 * %.%の確認中
# comparison: version *, version %.%, and reorderd version
test_that(" check %.%  ", {
  
  c("tr(C %*% (C*(B %.% t(X))))",
    "tr(C %*% (C*(B * t(X))))",
    "tr(t(C) %*% (t(C) * (t(B) %.% X)))" #trace_reorder("tr(C %*% (C*(B %.% t(X))))", "X")
  ) %>% 
    lapply(function(x)eval(parse(text = x)))
  
  
  # mD0("tr(A%*%(B%.%X))")
  # mD0("(t(C) * (t(B) %.% X))")
  
  c(
    "tr(A%*%(t(t(X*B)*C)))",
    "tr(A%*%(B%.%X))",
    "tr(C %*% (C * B %.% t(X)))",
    # 
    "tr(t(C) * (t(B) %.% X))",
    "tr(C %*% (B * t(X)))",
    "tr(C %*% (B %.% t(X)))",
    "tr(C %*% (C * B %.% t(X)))",
    "tr(C %*% (C %.% B * t(X)))",
    "tr(C %*% (C*(B * t(X))))",
    "tr(A%*%X)"
  ) %>%
    check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)
  })

test_that(" check placeholder  ", {
  
  
  # chain ruleのplaceholderについて
  # P2の場合のplaceholderをFXに設定すると、下の処理はうまくいかない。
  # おそらくC1のplaceholderと干渉している。
  expect_equal(
    mD0_parsed("tr((inv(X)*t(X))%*%A)") ,
    easy_parse("t(inv(X)) * t(t(A)) - t(inv(X) %*% t(t(X) * t(A)) %*% inv(X))"))
  # if FX, then "t(inv(X)) * t(t(A)) - t(inv(X) %*% t(inv(X) * t(A)) %*% inv(X))"
  
  "tr((inv(X)*t(X))%*%A)" %>%
    check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)
  
  
  # P2二回使う場合などもうまくいかなくなりそうなので、再帰の深さもplaceholder名に加えた。
  # 以下の関数は、depth = ""にすると、代入が完成せずうまくいかなくなる。
  # 条件文は、mD0内の修正をした箇所。
  if(0){
    # depth=sys.nframe() # こちらならOK
    depth = ""　 # こちら（つまり従来通り）だとだめ
    mD_fFXplaceholder <- glue::glue("mD_fFX{depth}")
    FXplaceholder <- glue::glue("FX{depth}")
  }
  expect_equal(mD0_parsed("tr(X%*%t(X%*%t(X%*%C%*%t(X))))"), 
               easy_parse("t(t(C) %*% t(X) %*% t(t(X) %*% X)) + t(C %*% t(X) %*% (t(X) %*% 
    X)) + t(t(X %*% C %*% t(X)) %*% t(X)) + t(t(X %*% t(X %*% 
    C %*% t(X))))"))
  
  
  # eval(parse(text="tr(X%*%t(X%*%t(X%*%C%*%t(X)*A)))"))
  
  
  "tr(t(X%*%C%*%t(X)*A))" %>% mD0
  
  # 内側で使われている、微分の確認。
  c(
    "tr(X%*%t(X%*%t(X%*%C%*%t(X)*A)))", 
    "tr(C%*% t(X) %*% t(A) %*% X)",
    "tr(t(C) %*% (A*X))",
    "tr(t(C) %*% (A*X%*%C%*%t(X)))", # これが原因で、gsubを用いていたのが原因だった。
    "tr(A)"
  ) %>%
    # "tr(t(C) %*% (A*X%*%C%*%t(X)))" %>%  # これが原因
    check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)
  # "tr(C%*% t(X) %*% t(A) %*% X)" %>% 
  c(
    "tr(C%*% t(X) %*% t(A) %*% X)",
    "tr(t(C) %*% (A*X))",
    "tr(t(C) %*% (A*X%*%C%*%t(X)))", # これが原因で、gsubを用いていたのが原因だった。
    "tr(A)"
  ) %>%  sapply(mD0) # "t(C%*%t(X)%*%t(A))+t(t(C)%*%t(X)%*%A)"
  
  # gsubはgsub_exprに置き換えて
  # gsub(pattern, replacement, x)
  # gsub_expr(expr = x, object = pattern, replacement)
})


test_that(" 前川先生からの0717", {
  
  c(
    "tr(A%*%(I*t(X)%*%X))",
    "tr(A%*%(I%.%t(X)%*%X))",
    
    # 対角行列のアダマール化
    "tr(A%*%Diag(t(X)%*%X))",
    "tr(Diag(A)%*%Diag(t(X)%*%Diag(X + Diag(B))))",
    "tr(A)"
  ) %>%
    check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)
  
})

test_that(" 0719", {
  
  
  
  
  expect_equal(linear_expand_expr("tr(A-B)", "tr", "inv")               ,easy_parse("tr(A)-tr(B)"))
  expect_equal(linear_expand_expr("inv(tr(A-B)) %*% C", "tr")           ,easy_parse(" inv(tr(A) - tr(B)) %*% C   "))
  expect_equal(linear_expand_expr("tr(A-B+C)", "tr")                    ,easy_parse("  tr(A) - tr(B) + tr(C)  "))
  expect_equal(linear_expand_expr("A-B", "tr")                          ,easy_parse(" A-B    "))
  expect_equal(linear_expand_expr("A%*%(X-B)", "%*%")                   ,easy_parse("  A %*% X - A %*% B  "))
  expect_equal(linear_expand_expr("tr(A%*%(X-B))", "tr", "%*%", "(")    ,easy_parse(" tr(A %*% X) - tr(A %*% B)   "))
  expect_equal(linear_expand_expr("tr(t(Y-X)%*%(Y-X))","t","%*%","tr")    ,
  easy_parse(" tr(t(Y) %*% Y)- tr(t(X) %*% Y)  - (tr(t(Y) %*% X)  - tr(t(X) %*%X))"))
  
  
  (A-B)*C
  (A*C-B*C)
  
  
  "tr(t(Y) %*% Y) - tr(t(Y) %*% X) - (tr(t(X) %*% Y) - tr(t(X) %*%X))" %>% 
    recompose_MatProd(c("+"))
  
  
  linear_expand_expr("tr(t(Y-X)%*%(Y-X))","t","%*%","tr")  %>% 
    decompose_MatProd(op = c("+"))
  
  mD0("tr(A%*%(B-X))")
  mD0("tr(A%*%B-A%*%X)")
  mD0("tr(A%*%B)-tr(A%*%X)")
  mD0("tr(t(Y - X %*% b) %*% (Y - X %*% b))", "b")
  
  
  linear_expand_expr("tr(t(Y - X %*% b) %*% (Y - X %*% b))", "t", "%*%", "tr") 
  linear_expand_expr("tr(t(Y - X) %*% (Y - X))", "t", "%*%", "tr") 
  
  
  # linear_expand_expr("tr(t(Y) %*% (Y - X)) - tr(t(X) %*% (Y - X))", "t", "%*%", "tr") 
  # %>% 
  #   linear_expand_expr("tr", "%*%")
  
  "tr(A%*%(X-B))" %>% 
    easy_parse() %>% 
    drop_parens(all = TRUE) %>% 
    show_ast() %>% 
    linear_expand("tr")
  

  dexpr <-   mD0( "tr(t(C - A%*%X) %*% (C - A%*%X))")
  
  eval(parse(text = dexpr))
  eval(parse(text = "-2*t(A)%*% (C + A%*%X)"))
  
  c(
    # 最小二乗法をしたい。
    # "tr(t(Y - X %*% b) %*% (Y - X %*% b))",
    "tr(A %*% (B - X))", # これがループする -> 解決
    
    "tr(t(C - A%*%X) %*% (C - A%*%X))",　# 　Y = AX (本当はY=Xbだが、Xで微分がデフォなので)
    # 通るようになったが、mD0( "tr(t(C - A%*%X) %*% (C - A%*%X))")が"-2*t(A)%*% (C + A%*%X)"になってしまう。
  　# 原因は、linea_expand(A-B)*(A-B) が A*A - A*B - B*A ** - ** B*Bになってしまうから
  　# recompose_MatProd()をoffにしたので、いったんOK！
  　
    "tr(A)"
  ) %>%
    check_numerical_identity(seed="r") %>% sapply(testthat::expect_lt, criteria)
  
  
  # decompose_MatProd(flat = TRUE)を修正する必要がある。
  # 一回A-BはA+-Bに変換したい。
  
  # to do 
  # decompose_MatProd() で分配法則を実現したい。
  # A - (B+C) = A -B -C
  
  
})


  
  