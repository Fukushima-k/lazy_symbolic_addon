
source("main_functions.R")


testthat::expect_equal(sum2mat("s(a[i,k]*b[k,j],{k})" ), parse(text="(a%*%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[k,i]*b[k,j],{k})" ), parse(text="(t(a)%*%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[i,k]*b[j,k],{k})" ), parse(text="(a%*%t(b))[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[k,i]*b[j,k],{k})" ), parse(text="(t(a)%*%t(b))[i,j]")[[1]])

testthat::expect_equal(sum2mat("a[i,j]*b[i,j]"), parse(text="(a%@%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[i,j]*b[j,i]"), parse(text="(a%@%t(b))[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]*b[i,j]"), parse(text="(a%@%t(b))[j,i]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]*b[j,i]"), parse(text="(a%@%b)[j,i]")[[1]])

testthat::expect_equal(sum2mat("a[i,j]+b[i,j]"), parse(text="(a+b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[i,j]+b[j,i]"), parse(text="(a+t(b))[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]+b[i,j]"), parse(text="(a+t(b))[j,i]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]+b[j,i]"), parse(text="(a+b)[j,i]")[[1]])

expr_str <- "s(a[i,k]*s(b[k,l]*c[l,j],{l}),{k})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%(b%*%c))[i,j]")[[1]])
expr_str <- "a[i, k] * s(b[k, l] * c[l, j], {l})"; testthat::expect_equal(sum2mat(expr_str), parse(text="a[i, k] * (b %*% c)[k, j]")[[1]])

expr_str <- "s(a[i,j]*b[j], {j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%b)[i,1]")[[1]])
expr_str <- "s(a[i]*b[i,j], {i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%b)[1,j]")[[1]])

# A
expr_str <- "s(a[i,j],{j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%one)[i,1]")[[1]])
expr_str <- "s(a[j,i],{j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%one)[i,1]")[[1]])
expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%one)[j,1]")[[1]])
expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%one)[j,1]")[[1]])
# expr_str <- "s(a[i,j],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(one)%*%A)[1,j]")[[1]])
# expr_str <- "s(a[j,i],{i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(one)%*%t(A)[1,j]")[[1]])

expr_str <- "a[i,j]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a       %*% diag(b))[i,j]")[[1]])
expr_str <- "a[j,i]*b[j,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)    %*% diag(b))[i,j]")[[1]])
expr_str <- "a[i,i]*b[i,j]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(diag(a) %*%      b )[i,j]")[[1]])
expr_str <- "a[i,i]*b[j,i]"; testthat::expect_equal(sum2mat(expr_str), parse(text="(diag(a) %*%    t(b))[i,j]")[[1]])

sum2mat(expr_str)








# 
# sum over j
# a[i,j] -->         ( A%*%one )[i]          col vec
# a[j,i] -->         ( t(A)%*%one )[i]       col vec
# 
# sum over i
# a[i,j] -->         ( t(one)%*%A )[j]       row vec 上の二つと区別がつかない
# a[j,i] -->         ( t(one)%*%t(A) )[j]    row vec
# 
# no summation
# a[i,j]*b[j,j] -->  ( A%*%diag(B) )[i,j]
# a[j,i]*b[j,j] -->  ( t(A)%*%diag(B) )[i,j]
# 
# a[i,i]*b[i,j] -->  ( diag(A)%*%B )[i,j]
# a[i,i]*b[j,i] -->  ( diag(A)%*%t(B) )[i,j]
# 


