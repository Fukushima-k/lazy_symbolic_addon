
testthat::expect_equal(sum2mat("s(a[i,k]*b[k,j],{k})" ), parse(text="(a%*%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[k,i]*b[k,j],{k})" ), parse(text="(t(a)%*%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[i,k]*b[j,k],{k})" ), parse(text="(a%*%t(b))[i,j]")[[1]])
testthat::expect_equal(sum2mat("s(a[k,i]*b[j,k],{k})" ), parse(text="(t(a)%*%t(b))[i,j]")[[1]])






testthat::expect_equal(sum2mat("a[i,j]*b[i,j]"), parse(text="(a%@%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[i,j]*b[j,i]"), parse(text="(a%@%t(b))[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]*b[i,j]"), parse(text="(t(a)%@%b)[i,j]")[[1]])
testthat::expect_equal(sum2mat("a[j,i]*b[j,i]"), parse(text="(t(a)%@%t(b))[i,j]")[[1]])