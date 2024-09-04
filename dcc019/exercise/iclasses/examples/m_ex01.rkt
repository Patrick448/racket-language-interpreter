#lang dcc019/exercise/iclasses



{
    var i; {
    i=100;
    print i;
    while ¬zero?(i) {print i; i=-(i,1)}
    };

    var i; {
    i=0;
    print i;
    while ¬zero?(-(i,100)) {print i; i=-(i,-(0,1))}
    }
}
