load("tr/patterns.js");


if (true) {
    replace(
        q.equivalentClassesMatch("?baseClass",
                                 q.objectSomeValuesFromMatch(o.has_part, 
                                                             q.intersectionOfMatch(o.concentration_of, 
                                                                                   q_abnormal(),
                                                                                   q_towards("?substance")
                                                                                   ))),
        function(m,owl) {
            print("Checking "+m.baseClass+" owl="+m.substance);
            //return owl.equivalentClasses(m.baseClass, y.call(m,owl));
            return owl.equivalentClasses(m.baseClass,
                                         owl.someValuesFrom(o.has_part,
                                                            owl.intersectionOf(o.concentration_of,
                                                                               abnormal(),
                                                                               inheres_in(m.substance))));
        }
    );
    

}

if (false) {
tr(
    [
        o.concentration_of,
        q_abnormal(),
        q_towards("?x")
    ],
    function(m,owl) 
    { 
        return [o.count,
                inheres_in(m.x),
                abnormal()]
    }
);
}
print("Done!");
