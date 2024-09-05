#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() return 1
    method test() {i = 21212212;print 5555555; print i}
    method printi() {print i}
    method seti() {i = 9999}

{
    {
        print 150;
        print 888;
        print 444
    };
    var k;{
        k = new c1();
        send k seti();
        send k printi()
    };
    {
        print if zero?(0) then 333 else 311;
        if zero?(1) print 111 print 777;
        var i; {
            i=3
        }
    }
}
