#lang dcc019/exercise/iclasses

class c1 extends object
    field i
    method initialize() return 1


{
    {
        print 150;
        print 888;
        print 444
    };

    {
        print if zero?(0) then 333 else 311;
        if zero?(1) print 111 print 777;
        var i; {
            i=3
        }
    }
}
