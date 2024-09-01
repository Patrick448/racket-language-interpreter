#lang dcc019/classes

class c1 extends object
  field i
  field j

  method initialize (x) 1
  
  method number(n) -(n,2)

class c2 extends object
  field zz
  field yy

  method initialize (x) 1
  
  method number(n) -(n,2)


%let t1 = 0 in begin let t2 = -(t1,2) in -(t2,5) in let t4 = new c2() in 1
let minhaVar = new c2() in 5

% Result: -2
