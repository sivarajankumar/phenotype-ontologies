load("tr/patterns.js");

// abnormal X concentration
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch(o.having_extra_processual_parts, 
                                                                               q_abnormal(),
                                                                               q_inheres_in("?location"),
                                                                               q_towards("?process")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(o.present,
                                                                           abnormal(),
                                                                           inheres_in( occursIn(m.process,m.location) ))))
    }
);
