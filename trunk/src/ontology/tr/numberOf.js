load("tr/patterns.js");

function mapPato(c) {
    if (c.equals(o.has_number_of)) {
        return o.count;
    }
    if (c.equals(o.altered_number_of)) {
        return o.count;
    }
    if (c.equals(o.lacks_all_parts_of_type)) {
        return o.absent;
    }
    if (c.equals(o.has_fewer_parts_of_type)) {
        return o.present_in_fewer_numbers_in_organism;
    }
    if (c.equals(o.has_extra_parts_of_type)) {
        return o.present_in_greater_numbers_in_organism;
    }
    print("NO MAPPING: "+c);
    return null;
}

// abnormal X amount
replace(
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.has_number_of}, 
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
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.has_number_of}, 
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
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.has_number_of}, 
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
                                                         q.intersectionOfMatch({var: "q", subClassOf: o.has_number_of}, 
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

test(o.abnormal_leukocyte_cell_number,
     owl.someValuesFrom(o.has_part,
                        owl.intersectionOf(o.count,
                                           owl.someValuesFrom(o.has_component, o.abnormal),
                                           owl.someValuesFrom(o.inheres_in, o.leukocyte)
                                           ))
    );
