load("tr/patterns.js");
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.concentration_of}, 
                                                                               q_abnormal(),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(m.q,
                                                                           abnormal(),
                                                                           inheres_in(m.substance))));
    }
);
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.concentration_of}, 
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(m.q,
                                                                           inheres_in(m.substance))));
    }
);
