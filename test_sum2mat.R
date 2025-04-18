
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
expr_str <- "a[i, k] * s(b[k, l] * c[l, j], {l})"; testthat::expect_equal(sum2mat(expr_str), parse(text="a[i, k] * (b %*% c)[i, k]")[[1]])

expr_str <- "s(a[i,j]*b[j], {j})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(a%*%b)[i,1]")[[1]])
expr_str <- "s(a[i]*b[i,j], {i})"; testthat::expect_equal(sum2mat(expr_str), parse(text="(t(a)%*%b)[1,j]")[[1]])
