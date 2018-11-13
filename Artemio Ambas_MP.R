######################################
# Machine Problem - Assignment 2     #
# Submitted by: Artemio R. Ambas Jr. #
######################################

#- Problem number 1 - Remove NA in a vector
remove.na<-function(v1) {
    v2<-vector(length=length(v1))
    j<-0
    for (i in 1:length(v1)) {
      if (!is.na(v1[i])) {
        j<-j+1
        v2[j]<-v1[i]
      }
      i<-i+1
    }
    v2<-v2[1:j]
    v1<-v2
    return(v1)
  }
#- End of number 1

#- Problem number 3 - Compute determinant of a given matrix
get.determinant<-function(dmatrix) 
{
  mdim<-dim(dmatrix)
  matrix.determinant<-0
  matrix.determinant2<-0
  
  if (mdim[1] == mdim[2]) 
  {
    if (mdim[1] == 2 && mdim[2]==2) 
    {
      matrix.determinant <- (dmatrix[1,1]*dmatrix[2,2])-(dmatrix[2,1]*dmatrix[1,2])
      return(matrix.determinant)
    }
    
    if (mdim[1] == 3 && mdim[2]==3) 
    {
      matrix.determinant <-dmatrix[1,1]*((dmatrix[2,2]-dmatrix[3,3]))-dmatrix[1,2]*((dmatrix[3,3]-dmatrix[2,3]))-dmatrix[1,3]*((dmatrix[2,1]-dmatrix[3,1]))
      return(matrix.determinant)
    }
    if (mdim[1] == 4 && mdim[2]==4) 
    {
      a<-matrix(1:9,3,3)
      a[1,1]<-dmatrix[2,2]
      a[1,2]<-dmatrix[3,2]
      a[1,3]<-dmatrix[4,2]
      a[2,1]<-dmatrix[2,3]
      a[2,2]<-dmatrix[3,3]
      a[2,3]<-dmatrix[4,3]
      a[3,1]<-dmatrix[2,4]
      a[3,2]<-dmatrix[3,4]
      a[3,3]<-dmatrix[4,4]
      
      b<-matrix(1:9,3,3)
      b[1,1]<-dmatrix[2,1]
      b[1,2]<-dmatrix[3,1]
      b[1,3]<-dmatrix[4,1]
      b[2,1]<-dmatrix[2,3]
      b[2,2]<-dmatrix[3,3]
      b[2,3]<-dmatrix[4,3]
      b[3,1]<-dmatrix[2,4]
      b[3,2]<-dmatrix[3,4]
      b[3,3]<-dmatrix[4,4]
      
      c<-matrix(1:9,3,3)
      c[1,1]<-dmatrix[2,1]
      c[1,2]<-dmatrix[3,1]
      c[1,3]<-dmatrix[4,1]
      c[2,1]<-dmatrix[2,2]
      c[2,2]<-dmatrix[3,2]
      c[2,3]<-dmatrix[4,2]
      c[3,1]<-dmatrix[2,4]
      c[3,2]<-dmatrix[3,4]
      c[3,3]<-dmatrix[4,4]
      
      d<-matrix(1:9,3,3)
      d[1,1]<-dmatrix[2,1]
      d[1,2]<-dmatrix[3,1]
      d[1,3]<-dmatrix[4,1]
      d[2,1]<-dmatrix[2,2]
      d[2,2]<-dmatrix[3,2]
      d[2,3]<-dmatrix[4,2]
      d[3,1]<-dmatrix[2,3]
      d[3,2]<-dmatrix[3,3]
      d[3,3]<-dmatrix[4,3]
      
      matrix.determinant2 <-(dmatrix[1,1]*(get.determinant(a)))-(dmatrix[1,2]*(get.determinant(b)))-(dmatrix[1,3]*(get.determinant(c)))-(dmatrix[1,4]*(get.determinant(d)))
      return(matrix.determinant2)
    }
  }
}
#- End of number 3

#- Problem number 5 - Accept date in POSIXct and output day of week in character
get.weekday<-function(date.input){
  date.input<-as.POSIXct(date.input)
  char.weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  current.year<-as.character(date.input)
  current.year<-substr(current.year,1,4)
  year.first.day<-paste(current.year,"-01-01",sep="")
  year.first.day<-as.POSIXct(year.first.day)
  days.total<-date.input - year.first.day
  days.total<-as.integer(days.total)
  weekday.num <- days.total %% 7
  return(char.weekday[weekday.num+1])
}
#- End of number 5

#- Problem number 6 - Function to compute net pay at work
compute.netpay<-function(gross.salary,taxable.benefits,nontaxable.benefits)
{  
  taxable.income<-gross.salary+taxable.benefits
  
  sss<-581.30
  philhealth<-437.50
  pagibig<-100.00
  
  tax.table<-matrix(1:24,6,4)
  
  # level1
  tax.table[1,2]<-0
  tax.table[1,3]<-20833
  tax.table[1,4]<-1
  
  # level2
  tax.table[2,2]<-20834
  tax.table[2,3]<-33332
  tax.table[2,4]<-.20
  
  # level3
  tax.table[3,2]<-33333
  tax.table[3,3]<-66666
  tax.table[3,4]<-.25
  
  # level4
  tax.table[4,2]<-66667
  tax.table[4,3]<-166666
  tax.table[4,4]<-.30
  
  # level5
  tax.table[5,2]<-166667
  tax.table[5,3]<-666666
  tax.table[5,4]<-.32
  
  # level6
  tax.table[6,2]<-666667
  tax.table[6,3]<-666667
  tax.table[6,4]<-.35
  
  i=1
  findtax<-FALSE
  tax.rate<-0
  
  while (findtax==FALSE) {
    if (taxable.income>=666667) {
      findtax<-TRUE
      tax.rate<-.35
    } else if (taxable.income>=tax.table[i,2] && taxable.income<=tax.table[i,3]) {
      findtax<-TRUE
      tax.rate<-tax.table[i,4]
    }
    i=i+1
  }
  net.pay<-(((gross.salary+taxable.benefits)*(1-tax.rate))+nontaxable.benefits)-(sss+philhealth+pagibig)
  return(net.pay)
}
#- End of number 6

#- Problem number 8 - Computes for the compound interest
get.compounded<-function(principal.amount,interest.rate,number.of.times,time) {
  interest.rate<-interest.rate/100
  compounded.amount<-principal.amount*((1+(interest.rate/number.of.times))^(time*number.of.times))
  return(compounded.amount)
}
#- End of number 8


