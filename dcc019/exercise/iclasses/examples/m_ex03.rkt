#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() return 1
    method test() {i = 21212212;print 5555555; print i}
    method printi() {print i}
    method seti() {i = 9999}

{

    var k;{
        k = new c1();
        print k;
        var i;{
            i=3;
            k=i;
            print k
        }
    }

}
