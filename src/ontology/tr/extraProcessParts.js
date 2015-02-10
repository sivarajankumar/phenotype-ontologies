load("tr/patterns.js");

needs(o.has_part, "has part")
needs(o.count, "count")
needs(o.having_extra_processual_parts, "+p")

// clones from numberOf

function mapPato(c) {
    if (c.equals(o.extra_or_missing_processual_parts)) {
        return o.count;
    }
    if (c.equals(o.lacking_processual_parts)) {
        return o.absent;
    }
    if (c.equals(o.having_decreased_processual_parts)) {
        return o.present_in_fewer_numbers_in_organism;
    }
    if (c.equals(o.having_extra_processual_parts)) {
        return o.present_in_greater_numbers_in_organism;
    }
    print("NO MAPPING: "+c);
    return null;
}

// abnormal X amount
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.extra_or_missing_processual_parts}, 
                                                                               q_abnormal(),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(mapPato(m.q),
                                                                           abnormal(),
                                                                           inheres_in(m.substance))));
    }
);

// decreased/increased X amount
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.extra_or_missing_processual_parts}, 
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        print("MAPPING: "+m.substance+" "+m.q);
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(mapPato(m.q),
                                                                           inheres_in(m.substance))));
    }
);

// abnormal X amount in Y
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.extra_or_missing_processual_parts}, 
                                                                               q_abnormal(),
                                                                               q_inheres_in("?in"),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(mapPato(m.q),
                                                                           abnormal(),
                                                                           inheres_in(
                                                                               partOf(m.substance, m.in)
                                                                           ))));
    }
);

// inc/dec X amount in Y
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.extra_or_missing_processual_parts}, 
                                                                               q_inheres_in("?in"),
                                                                               q_towards("?substance")
                                                                              ))),
    function(m,owl) {
        return owl.equivalentClasses(m.baseClass,
                                     owl.someValuesFrom(o.has_part,
                                                        owl.intersectionOf(mapPato(m.q),
                                                                           inheres_in(
                                                                               partOf(m.substance, m.in)
                                                                           ))));
    }
);



/*
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
*/
