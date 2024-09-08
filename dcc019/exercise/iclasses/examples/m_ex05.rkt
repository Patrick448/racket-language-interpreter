#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() i=4545
    method test(x) {print x}


class c2 extends c1
    field j
    method initialize() j=888
    method testsuper(x) super test(-(x,1))


{

    var k;{
        k = new c2();
        send k testsuper(14)
    }

}
