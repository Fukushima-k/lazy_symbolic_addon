source("main_functions.R")
if(0){
  testthat::test_file("test_sum2mat.R")
}


# sum over k
expr_str <- "s(a[i,k]*b[k,j],{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%b)[i,j]")[[1]])
expr_str <- "s(a[k,i]*b[k,j],{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%b)[i,j]")[[1]])
expr_str <- "s(a[i,k]*b[j,k],{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%t(b))[i,j]")[[1]])
expr_str <- "s(a[k,i]*b[j,k],{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%t(b))[i,j]")[[1]])

# sum over j (or i)
expr_str <- "s(a[i,j]*b[j], {j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%b)[i,1]")[[1]]) # 列ベクトルです
expr_str <- "s(a[i]*b[i,j], {i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%b)[1,j]")[[1]]) # 行ベクトルです

expr_str <- "s(a[i,j],{j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%one)[i,1]")[[1]])
expr_str <- "s(a[j,i],{j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%one)[i,1]")[[1]])
expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%one)[j,1]")[[1]])
expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%one)[j,1]")[[1]])
## この関数ではiとjを特別視していないので、下記のように結果が変わるようにはしていません。
## 再帰的計算をすれば、結果が行or列野どちらであるかにとくべつ配慮を行わなくても、うまくいくんじゃないかなと思ってます。
# expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(one)%*%A)[1,j]")[[1]])
# expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(one)%*%t(A)[1,j]")[[1]])


# no summation 
expr_str <- "a[i,j]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a       %*% diag(b))[i,j]")[[1]])
expr_str <- "a[j,i]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)    %*% diag(b))[i,j]")[[1]])

expr_str <- "a[i,i]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(diag(a) %*%      b )[i,j]")[[1]])
expr_str <- "a[i,i]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(diag(a) %*%    t(b))[i,j]")[[1]])

## この関数ではiとjを特別視していないので、下記でも上と同様に結果が変わるようにはしていません。
expr_str <- "a[i,j]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%@%b)[i,j]")[[1]])
expr_str <- "a[i,j]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%@%t(b))[i,j]")[[1]])
expr_str <- "a[j,i]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%@%t(b))[j,i]")[[1]])
expr_str <- "a[j,i]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%@%b)[j,i]")[[1]])

expr_str <- "a[i,j]+b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a+b)[i,j]")[[1]])
expr_str <- "a[i,j]+b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a+t(b))[i,j]")[[1]])
expr_str <- "a[j,i]+b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a+t(b))[j,i]")[[1]])
expr_str <- "a[j,i]+b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a+b)[j,i]")[[1]])

# sの入れ子と行列表現にできない場合の確認
expr_str <- "s(a[i,k]*s(b[k,l]*c[l,j],{l}),{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%(b%*%c))[i,j]")[[1]])
expr_str <- "a[i, k] * s(b[k, l] * c[l, j], {l})"; testthat::expect_equal(sum2mat(expr_str), parse(text="a[i, k] * (b %*% c)[k, j]")[[1]])




# その他、自由なテスト箇所
expr_str <- "-A[i,j]-B[i,j]-C[i,j]-D[i,i]*T[j,i] "; testthat::expect_equal(sum2mat(expr_str), parse(text="(-A-B-C-diag(D)%*%t(T))[i,j]")[[1]])
expr_str <- "s(A[i,s1]*B[s1,j],{s1})+s(A[i,s1]*C[s1,j]*D[j,j],{s1})-s(A[i,s1]*s(C[s1,s2]*T[j,s2],{s2}),{s1}) "; testthat::expect_equal(sum2mat(expr_str), parse(text="(A%*%B+A%*%C%*%diag(D)-A%*%(C%*%t(T)))[i,j]")[[1]])
