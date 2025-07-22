if(0){
  # sum2mat()のテストファイル-----
  
  ## テストの実行-----
  if(!require(testthat)){
    stop("package 'testthat' is required for print_tex_as_html()")
  }
  testthat::test_file("tests/sum2mat_test.R")
  
  source("main_functions/Fukushima_sum2mat.R")
  
  ## 実行例
  sum2mat("s(a[i,k]*b[k,j],{k})")
  sum2mat("s(a[i,j],{j})")
  sum2mat("s(a[i,k]*s(b[k,l]*c[l,j],{l}),{k})")
  
}

# sum over k
expr_str <- "s(a[i,k]*b[k,j],{k})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%b)[i,j]")
expr_str <- "s(a[k,i]*b[k,j],{k})"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%b)[i,j]")
expr_str <- "s(a[i,k]*b[j,k],{k})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%t(b))[i,j]")
expr_str <- "s(a[k,i]*b[j,k],{k})"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%t(b))[i,j]")

# sum over j (or i)
expr_str <- "s(a[i,j]*b[j], {j})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%b)[i,1]") # 列ベクトルです
expr_str <- "s(a[i]*b[i,j], {i})"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%b)[1,j]") # 行ベクトルです

expr_str <- "s(a[i,j],{j})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%one)[i,1]")
expr_str <- "s(a[j,i],{j})"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%one)[i,1]")
expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%one)[j,1]")
expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%one)[j,1]")
## この関数ではiとjを特別視していないので、下記のように結果が変わるようにはしていません。
## 再帰的計算をすれば、結果が行or列野どちらであるかにとくべつ配慮を行わなくても、うまくいくんじゃないかなと思ってます。
# expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), "(t(one)%*%A)[1,j]")
# expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), "(t(one)%*%t(A)[1,j]")


# no summation 
expr_str <- "a[i,j]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), "(a%*%diag(b))[i,j]")
expr_str <- "a[j,i]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), "(t(a)%*%diag(b))[i,j]")

expr_str <- "a[i,i]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), "(diag(a)%*%b)[i,j]")
expr_str <- "a[i,i]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), "(diag(a)%*%t(b))[i,j]")

## この関数ではiとjを特別視していないので、下記でも上と同様に結果が変わるようにはしていません。
expr_str <- "a[i,j]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), "(a%@%b)[i,j]")
expr_str <- "a[i,j]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), "(a%@%t(b))[i,j]")
expr_str <- "a[j,i]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), "(a%@%t(b))[j,i]")
expr_str <- "a[j,i]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), "(a%@%b)[j,i]")

expr_str <- "a[i,j]+b[i,j]"; testthat::expect_equal(sum2mat(expr_str), "(a+b)[i,j]")
expr_str <- "a[i,j]+b[j,i]"; testthat::expect_equal(sum2mat(expr_str), "(a+t(b))[i,j]")
expr_str <- "a[j,i]+b[i,j]"; testthat::expect_equal(sum2mat(expr_str), "(a+t(b))[j,i]")
expr_str <- "a[j,i]+b[j,i]"; testthat::expect_equal(sum2mat(expr_str), "(a+b)[j,i]")

# sの入れ子と行列表現にできない場合の確認
expr_str <- "s(a[i,k]*s(b[k,l]*c[l,j],{l}),{k})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%(b%*%c))[i,j]")
expr_str <- "a[i, k] * s(b[k, l] * c[l, j], {l})"; testthat::expect_equal(sum2mat(expr_str), "a[i,k]*(b%*%c)[k,j]")




# その他、自由なテスト箇所
expr_str <- "-A[i,j]-B[i,j]-C[i,j]-D[i,i]*T[j,i] "; testthat::expect_equal(sum2mat(expr_str), "(-A-B-C-diag(D)%*%t(T))[i,j]")
expr_str <- "s(A[i,s1]*B[s1,j],{s1})+s(A[i,s1]*C[s1,j]*D[j,j],{s1})-s(A[i,s1]*s(C[s1,s2]*T[j,s2],{s2}),{s1}) "; testthat::expect_equal(sum2mat(expr_str), "(A%*%B+A%*%C%*%diag(D)-A%*%(C%*%t(T)))[i,j]")



# diagとsummationに関する追加テスト
expr_str <- "s(C[i,k]*D[k,k]*E[k,j],{k})"; testthat::expect_equal(sum2mat(expr_str), "(C%*%diag(D)%*%E)[i,j]")
expr_str <- "s(a[i,k]*b[k,k],{k})"; testthat::expect_equal(sum2mat(expr_str), "(a%*%diag(b)%*%one)[i,1]")

testthat::expect_equal(sum2mat("s(A[i,s1]*B[s1,j]+C[i,s1]*D[s1,j],{s1})")   , "(A%*%B+C%*%D)[i,j]")
testthat::expect_equal(sum2mat("s(-A[i,s1]*B[s1,j],{s1})")                  , "(-A%*%B)[i,j]")
testthat::expect_equal(sum2mat("s(-A[i,s1]*B[s1,j]+C[i,s1]*D[s1,j],{s1})")  , "(-A%*%B+C%*%D)[i,j]")



# まだうまくいっていないもの
if(0){
  expr_str <- "s(s(C[i,k]*D[k,l]*E[l,j],{k}),{l})"; sum2mat(expr_str)
  expr_str <- "s(s(C[i,k]*E[l,j]*D[k,l],{k}),{l})"; sum2mat(expr_str)
  expr_str <- "s(C[i,k]*E[l,j]*D[k,l],{k})"; sum2mat(expr_str)
  
  expr_str <- "s(s(C[i,k]*E[l,j]*D[k,l],{k}),{l})"; sum2mat(expr_str)
  
  
  
  # t(A)%*%diag(W)%*%A
  # This works
  "s(A[s1,i] * W[s1,s1] * A[s1,j],{s1})" %>% sum2mat
  # This does NOT
  "s(A[s1,i] * A[s1,j] * W[s1,s1],{s1})" %>% sum2mat
  
  # t(Y-X) %*% W %*% (Y-X)
  # This does not work
  sum_res <- "t(Y-X) %*% W %*% (Y-X)" %>% mat2sum
  # This does not work
  "s(Y[s1,i]-X[s1,i]*s(W[s1,s2]*(Y[s2,j]-X[s2,j]),{s2}),{s1})" %>% sum2mat
  
}
