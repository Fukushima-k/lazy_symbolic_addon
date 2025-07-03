library(lazy.symbolic)

analyze_3d_val <- function( values=NULL, ntogoback=1, debug=0, ... ){
 # analyze three dots and values to obtain the contents
 # Shin-ichi Mayekawa
 # 20250701
 #

 # get pat1 and its value, pat2 from ...
 atd=analyze_three_dots(...)
 pat2=atd$val
 name=as.list(atd$name2)
 names(name)=atd$name

 # store ...
 # name <- as.list(substitute(list(...)))[-1L]
 # pat2 <- list(...)

 if( !is.null(values) ){

  # values  given
  values=gsub(" ","",values)
  res=strsplit(values,";")[[1]]
  res=res[res != ""]
  nv=strsplit(res,"=")
  name=NULL; pat2=NULL
  for( i in 1:length(nv) ){
   name=append(name,list(nv[[i]][2]))
   pat2=append(pat2,list(nv[[i]][1]))
  }
  names(name)=unlist(pat2)
  names(pat2)=unlist(pat2)
  nname=length(name)

  # parent env
  sysparent=sys.parent(n=ntogoback)
  if( debug >= 2 ){
   printm(sysparent)
   printm(name,pat2,names(name))
   printm(atd$name,atd$name2, atd$val)
  }

  # get the values of pat1 from the parent (calling) environment
  for( i in 1:nname ){
   temp=name[[i]]
   if( debug ) printm(i,temp)
   if( exists( temp, where=sys.frame(sys.parent(n=ntogoback)) ) ){
    if( debug ) printm(i,pat2[[i]])
    pat2[[i]]=get(temp,pos=parent.frame(n=ntogoback))
   } # end of exists
   else{
    # Here, temp does not exist in the parent frame.
    # cat("\nerror1:(matReplace) Object ", temp, " does not exist.\n")
    pat2[[i]]=temp
   } # end of does not exist
  } # end of i

 } # end of values


 # This stores "pat1"
 namename=names(name)
 nname=length(namename)

 # arg is the first element
 arg=namename[1]
 dimarg=paste("c(", paste(dim(pat2[[1]]),collapse=","), ")", sep="")
 argval=pat2[[1]]

 if( debug ){
  cat("\n(analyze_3d_val): name:\n"); print(name)
  cat("\n(analyze_3d_val): pat2:\n"); print(pat2)
  cat("\n(analyze_3d_val): namename:\n"); print(namename)
  cat("\n(analyze_3d_val): nname:\n"); print(nname)
 }

 return( list(nname=nname, namename=namename, pat2=pat2
              , arg=arg, dimarg=dimarg, argval=argval) )

} # end of analyze_3d_val



gradma <- function( expr, ..., values=NULL, dexpr=NULL, sym=0
                    , ntogoback=1, print=0, debug=0 ){
 # analytic first derivative of string expr w.r.t  X matrix
 # where X is the first element of ... or values.
 # Shin-ichi Mayekawa
 # 20250701
 # dexpr as argument: 20250702
 # sym: 20250702
 #

 # analyze ... and values
 temp=analyze_3d_val( values=values, debug=debug, ntogoback=ntogoback, ... )
 if(0){
  namename=temp$namename; nname=temp$nname; pat2=temp$pat2
  arg=temp$arg;  dimarg=temp$dimarg;  argval=temp$argval
 }
 arg=temp$arg; namename=temp$namename;  nname=temp$nname;  pat2=temp$pat2

 # define the matrices to be used in this environment
 for( i in 1:nname ){
  code=paste(namename[i],"=pat2[[i]]",sep="")
  if( debug >= 2 ) printm(i,code)
  eval(parse(text=code))
 }

 vv=c("arg","atd","code","debug","expr","expr0","i","constants","print"
      ,"name","namename","nname","ntogoback","pat2","vv","values", "sym"
      , "dimarg", "argval", "temp", arg)
 const=setdiff( ls(), vv )
 const0=paste(const,collapse=", ")

 if( is.null(dexpr) ){
  # analytic derivative
  dexpr=Dm_core(expr, arg, deparse_result=1)
 }

 # replace inv
 dexpr=gsub("inv","Inv",dexpr)


 # eval
 gradma=Eval( dexpr, values=values, ..., fullsymb=1, check=0 )

 # symmetric X
 if( sym ){
  gradma=gradma+t(gradma)-Diag(gradma)
 }


 if( print ){
  cat("\nInput expression \"", expr
      , "\" was analytically differentiated", sep="")
  cat(" with respect to ", arg,".\n", sep="")
  printm(dexpr)
  cat("The above expression was evaluated with the following values:\n")
  print(pat2)
  cat("The result, with sym =",sym, ", is\n")
  printm(gradma)
 }

 return( gradma )

} # end of gradma



gradmn <- function( expr, ..., values=NULL, sym=0
                    , ntogoback=1, print=0, debug=0 ){
 # numerical first derivative of string expr w.r.t  X matrix
 # where X is the first element of ... or values.
 # Shin-ichi Mayekawa
 # 20250701
 # sym: 20250702
 #

 # replace inv
 expr=gsub("inv","Inv",expr)

 # analyze ... and values
 temp=analyze_3d_val( values=values, debug=debug, ntogoback=ntogoback, ... )
 namename=temp$namename;  nname=temp$nname;  pat2=temp$pat2
 arg=temp$arg;  dimarg=temp$dimarg;  argval=temp$argval

 # define the matrices to be used in this environment
 for( i in 1:nname ){
  code=paste(namename[i],"=pat2[[i]]",sep="")
  if( debug >= 2 ) printm(i,code)
  eval(parse(text=code))
 }

 vv=c("arg","atd","code","debug","expr","expr0","i","constants","print"
      ,"name","namename","nname","ntogoback","pat2","vv","values", "sym"
      , "dimarg", "argval", "temp", arg)
 const=setdiff( ls(), vv )
 const0=paste(const,collapse=", ")

 # generate function (function factory)
 #
 # The function generated below has an access to the pat1 objects
 # which have pat2 as their values.
 #
 code="function( vec ){"
 if( sym ) code=c( code, paste(arg,"=vechinv(vec)",sep="") )
 else code=c( code, paste(arg,"=array(vec,dim=",dimarg,")",sep="") )
 code=c( code, paste("return(", expr, ") }", sep="") )
 Func <- eval(parse(text=code))

 if( print ){
  cat("\nInput expression \"", expr, "\" was converted to a function\n", sep="")
  cat(" with the argument c(", arg,") and the constatns ", const0,".\n", sep="")
  printm(code, .lj.=1)
  cat("The values of the argument and the constants are: \n\n")
  print(pat2)
  cat("The body of the function, with sym =", sym, "\n")
  printm(code, .lj.=1)
 }

 # numerical differentiation
 if( sym ){
  grad=JacobianMat( vech(argval), Func )
  Grad=vechinv(grad)
 }
 else{
  grad=JacobianMat( c(argval), Func )
  Grad=array(grad,dim=dim(pat2[[1]]))
 }

 return( Grad )

} # end of gradmn






if(0){



# functions to be differentiated
func1="tr(A%*%X)"

# numerical values of the variables
Xn=demomat(3,3, shape="sym")
An=demomat(3,3)

Gradmn1s <- gradmn( func1, X=Xn, A=An, sym=1, print=1, debug=0 )
Gradma1s <- gradma( func1, X=Xn, A=An, sym=1, print=1, debug=0 )
printm(Gradmn1s,Gradma1s, fmt="9.5")
printm(max(abs(Gradmn1s-Gradma1s)))





# functions to be differentiated
func5="tr(A%*%t(X)%*%B%*%X%*%C)"

# analytic derivative w.r.t. X
dexpr5="B%*%X%*%C%*%A+t(B)%*%X%*%t(A)%*%t(C)"

# numerical values of the variables
set.seed(1701)
Xn=matrix(rnorm(3*3),3,3)
Xn=Xn+t(Xn)
An=matrix(rnorm(3*3),3,3)
Bn=matrix(rnorm(3*3),3,3)
Cn=matrix(rnorm(3*3),3,3)

Gradmn5s <- gradmn( func5, X=Xn, A=An, B=Bn, C=Cn, sym=1, print=1, debug=0 )
Gradma5s <- gradma( func5, dexpr=dexpr5, X=Xn, A=An, B=Bn, C=Cn, sym=1
                   , print=1, debug=0 )
printm(Gradmn5s,Gradma5s, fmt="9.5")
printm(max(abs(Gradmn5s-Gradma5s)))





# functions to be differentiated
func1="tr(A%*%X)"

# numerical values of the variables
Xn=demomat(3,2)
An=demomat(2,3)

Gradmn1 <- gradmn( func1, X=Xn, A=An, print=1, debug=0 )
Gradma1 <- gradma( func1, X=Xn, A=An, print=1, debug=0 )
printm(Gradmn1,Gradma1, fmt="9.5")
printm(max(abs(Gradmn1-Gradma1)))



# functions to be differentiated
func2="tr(inv(X)%*%A)"

# numerical values of the variables
Xn=matrix(c(1,2,4,3,2,1,5,1,2),3,3)
An=matrix(c(1,2,3,3,2,1,5,1,2),3,3)

Gradmn2 <- gradmn( func2, X=Xn, A=An, print=1, debug=0 )
Gradma2 <- gradma( func2, X=Xn, A=An, print=1, debug=0 )
printm(Gradmn2,Gradma2, fmt="9.5")
printm(max(abs(Gradmn2-Gradma2)))



# functions to be differentiated
func3="tr((A*X)%*%B)"

# numerical values of the variables
Xn=matrix(c(1,2,4,3,2,1,5,1,2),3,3)
An=matrix(c(1,2,3,3,2,1,5,1,2),3,3)
Bn=matrix(c(1,2,5,3,2,1,5,1,2),3,3)

Gradmn3 <- gradmn( func3, X=Xn, A=An, B=Bn, print=1, debug=0 )
Gradma3 <- gradma( func3, X=Xn, A=An, B=Bn, print=1, debug=0 )
printm(Gradmn3,Gradma3, fmt="9.5")
printm(max(abs(Gradmn3-Gradma3)))


# functions to be differentiated
func4="tr(X^3%*%B)"

# numerical values of the variables
Xn=matrix(c(1,2,4,3,2,1,5,1,2),3,3)
Bn=matrix(c(1,2,5,3,2,1,5,1,2),3,3)

Gradmn4 <- gradmn( func4, X=Xn, B=Bn, print=1, debug=0 )
Gradma4 <- gradma( func4, X=Xn, B=Bn, print=1, debug=0 )
printm(Gradmn4,Gradma4, fmt="9.5")
printm(max(abs(Gradmn4-Gradma4)))


# functions to be differentiated
func5="tr(A%*%t(X)%*%B%*%X%*%C)"

# analytic derivative w.r.t. X
dexpr5="B%*%X%*%C%*%A+t(B)%*%X%*%t(A)%*%t(C)"

# numerical values of the variables
set.seed(1701)
Xn=matrix(rnorm(3*2),3,2)
An=matrix(rnorm(3*2),3,2)
Bn=matrix(rnorm(3*3),3,3)
Cn=matrix(rnorm(3*2),2,3)

Gradmn5 <- gradmn( func5, X=Xn, A=An, B=Bn, C=Cn, print=1, debug=0 )
Gradma5 <- gradma( func5, dexpr=dexpr5, X=Xn, A=An, B=Bn, C=Cn
                   , print=1, debug=0 )
printm(Gradmn5,Gradma5, fmt="9.5")
printm(max(abs(Gradmn5-Gradma5)))



# functions to be differentiated
func6="tr(A%*%inv(t(X)%*%B%*%X)%*%C)"

# analytic derivative w.r.t. X
dexpr6=
 "-B.X.inv(t(X).B.X).C.A.inv(t(X).B.X)-t(B).X.t(inv(t(X).B.X)).t(A).t(C).t(inv(t(X).B.X))"
dexpr6=gsub(".", "%*%", dexpr6, fixed=1)

# numerical values of the variables
set.seed(1701)
Xn=matrix(rnorm(3*2),3,2)
An=matrix(rnorm(3*2),3,2)
Bn=matrix(rnorm(3*3),3,3)
Cn=matrix(rnorm(3*2),2,3)

Gradmn6 <- gradmn( func6, X=Xn, A=An, B=Bn, C=Cn, print=1, debug=0 )
Gradma6 <- gradma( func6, dexpr=dexpr6, X=Xn, A=An, B=Bn, C=Cn
                   , print=1, debug=0 )
printm(Gradmn6,Gradma6, fmt="9.5")
printm(max(abs(Gradmn6-Gradma6)))



}















if(0){




 func1="tr(A%*%X)"
 resDm1=Dm_core(func1, "X", deparse_result=1)
 func2="tr(inv(X)%*%inv(A) %*% B)"
 resDm2=Dm_core(func2, "X", deparse_result=1)
 resDm22=gsub("inv","Inv",resDm2)
 func22=gsub("inv","Inv",func2)

 printm(func1,resDm1)
 printm(func2,resDm2)

 Xn=diag(2)
 An=demomat(2,2)
 Bn=demomat(2,2)*10
 resDm1n=Eval( resDm1, A=An, X=Xn, fullsymb=1 )
 resDm2n=Eval( resDm22, A=An, B=Bn, X=Xn , fullsymb=1, check=0 )





f1 <- function( x, y, ... ){

 mcall=match.call()
 mcall=deparse(mcall)
 printm(mcall)
 str(mcall)

}

f1( X="1+ 5",YY=2,a=3)



}
