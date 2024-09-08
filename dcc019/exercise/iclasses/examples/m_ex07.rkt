#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() i=4545
    method test(x) {print x}


class c2 extends c1
    field j
    field m
    method initialize(l) {j=888;m=999; print -(-(m,j), l)}
    method print44() {print 44}
    method testsuper(x) super test(-(x,1))
    method getm() return m


{

    var k;{
        k = new c2(20)
    }


}
