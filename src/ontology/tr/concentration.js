load("tr/patterns.js");


// abnormal X concentration
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
// inc/dec X concentration
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

// abnormal X concentration in Y
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.concentration_of}, 
                                                                               q_abnormal(),
                                                                               q_inheres_in("?in"),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(m.q,
                                                                           abnormal(),
                                                                           inheres_in(
                                                                               partOf(m.substance, m.in)
                                                                           ))));
    }
);

// inc/dec X concentration in Y
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.concentration_of}, 
                                                                               q_inheres_in("?in"),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(m.q,
                                                                           inheres_in(
                                                                               partOf(m.substance, m.in)
                                                                           ))));
    }
);


test(o.abnormal_pancreas_iron_level,
     owl.someValuesFrom(o.has_part,
                        owl.intersectionOf(o.concentration_of,
                                           owl.someValuesFrom(o.has_component, o.abnormal),
                                           owl.someValuesFrom(o.inheres_in,
                                                              partOf(o.iron_atom, o.pancreas)
                                           )))
    );
