#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() i=4545
    method test(x) {i = 21212212;print 5555555; print x}

{

    var k;{
        k = new c1();
        send k test(2)
    }

}
