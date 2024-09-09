#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() i=4545
    method test(x) return x


class c2 extends c1
    field j
    field m
    method initialize(l) {j=888;m=999; print -(-(m,j), l)}
    method print44() {print 44}
    method testsuper(x) {print super test(-(x,1))}
    method getm() return m


{

    var k;{
        k = new c2(20);
        send k testsuper(555);
        print send k getm()
    };
    var i;{
        i = 1000;
        print i
    }


}
