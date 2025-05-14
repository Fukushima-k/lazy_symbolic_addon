
setwd("lazy_symbolic_addon/")
library(lazy.symbolic)
source("main_functions.R")

# 行列の準備--------
# set output line width
options(width=500)

# 数値演算子の拡張:
modify_math_operators()

# set output line width
options(width=500)
# 行列とベクトルの定義
# 
# ベクトル
v <- demomat(3,1,root="v",vec=1)   # 3 x 1 の縦ベクトル
u <- demomat(3,1,root="u",vec=1)
u4 <- demomat(4,1,root="u",vec=1)  # 4 x 1 の縦ベクトル
v4 <- demomat(4,1,root="v",vec=1)


# 定数
one3 <- matrix("1",3,1) # col vector of length 3 consisting of 1s.
one4 <- matrix("1",4,1) # col vector of length 4 consisting of 1s.
zero3 <- matrix("0",3,1) # col vector of length 3 consisting of 0s.
zero4 <- matrix("0",4,1) # col vector of length 4 consisting of 0s.


# 行列
A <- demomat(4,3,root="a")    # 4 x 3 の行列
B <- demomat(4,3,root="b")

X <- demomat(3,2,root="x")    # 3 x 2 の行列
Y <- demomat(3,2,root="y")
Z <- demomat(3,2,root="z")


Y2 <- demomat(2,3,root="y")    # 2 x 3 の行列
Z2 <- demomat(3,3,root="y")    # 3 x 3 の行列

# 直接に要素を定義して 2 x 3 の行列を作る
X2 <- matrix( paste("x",outer(1:2,1:3, paste, sep=""), sep=""), 2,3 )

# 対角行列
D3 <- demomat(3,3,root="d", shape="diag")    # 3 x 3 の対角行列
D4 <- demomat(4,4,root="d", shape="diag")    # 4 x 4 の対角行列

# 対称行列
S <- demomat(3,3,root="s", shape="sym")    # 3 x 3 の対称行列
Su <- demomat(3,3,root="s", shape="symU")    # 3 x 3 の対称行列

# 三角行列
L <- demomat(3,3,root="s", shape="lowert")    # 3 x 3 の下三角行列
U <- demomat(3,3,root="s", shape="uppert", nodiag=1)    # 3 x 3 の上三角行列

# 単位行列
I3 <- diag(3)
I4 <- diag(4)

# 零行列
Zero3 <- matrix("0",3,3)

# set output line width
options(width=500)
# 中心化行列:  J = I_n - (1/n) * one_n %*% t(one_n)
J3=I3 - "(1/3)" * one3 %*% t(one3) 


L1=demomat(3,3,root="L1_",shape="lowert")
L2=demomat(3,3,root="L2_",shape="lowert")


A2=demomat(2,2,root="a")

detA2="a11*a22-a21*a12"
invA2d=(paste("1/(",detA2,")",sep=""))*matrix(c("a22","-a21","-a12","a11"),2,2)


# 完全記号行列

bigA=demomat(2,2,root="A", fullsymb=1)  
bigB=demomat(2,2,root="B", fullsymb=1)  
invbigA=matSwp(bigA)


# -----

使い方の説明

- 関数一覧
  - to_latex()
    - 要素ごとにLatex記法に変換する関数
    - mat2sum = TRUEにすると、総和記号による表記もLaTeX記法に変換できる。
    - simple_mat2sum = TRUEにすると、さらにシンプルな表記になる。
  - 使い方
    - 変換したい当該オブジェクトを引数に上記関数を使用すると、Latex記法の文字列が返ってくる。
    - QuartoのRチャンクにおいて、チャンクオプションをresults:"asis"にすると、Rの返した結果をmarkdown上に直接記述されたものと解釈してコンパイルできる。
    - 注：このオプションをつけないと、あくまでRチャンクの結果としての文字列を返すのみになり、これをさらにコンパイルすることが出来なくなる。


## to_latex() ---------
# オブジェクトをLatex記法に変換できる
Print(S)
TeX_S <- to_latex(S) 

# コンソールに表示
cat(TeX_S)

# ViewerにMathJaxを使って表示 print_tex_as_html()
print_tex_as_html(TeX_S)

# to_latexの引数指定で直接Viewerに表示することもできる。
to_latex(S, print_html = TRUE)

# mat2sumのオブジェクトであれば、引数mat2sumかsimple_mat2sumをTRUEに
expr <- mat2sum("t(v) %*% S %*% v", root="k",simple=0)
print(expr)
to_latex(expr, mat2sum = TRUE, print_html=TRUE)
to_latex(expr, simple_mat2sum = TRUE, print_html=TRUE)


## to_latex_core()------
# to_latex()の内部で機能している関数。直接ユーザーが扱う必要はない。

## print_tex_as_html()-----
# to_latex()の返り値をViewerにMathJaxを使って表示 
# to_latex(., print_html=TRUE)で使えるので、この関数を直接使わなくてもいい。



## sum2mat()------
# 総和表記のオブジェクトを行列表記に変換する関数。

sum2mat("s(a[i,k]*b[k,j],{k})")
sum2mat("s(a[i,j],{j})")
sum2mat("s(a[i,k]*s(b[k,l]*c[l,j],{l}),{k})")

# 何もしないと出力はexpressionで返すので、文字列にしたいときはdeparse_result=TRUE
A = sum2mat("s(a[i,k]*b[k,j],{k})")
B = sum2mat("s(a[i,k]*b[k,j],{k})", deparse_result = TRUE)
print(A); class(A)
print(B); class(B)

# テストファイルを用意しているので、これを実行してみてもよい。ファイルの中身を見ていろいろいじってみてもいい。
if(!require(testthat)){
  stop("package 'testthat' is required for print_tex_as_html()")
}
testthat::test_file("test_sum2mat.R")
