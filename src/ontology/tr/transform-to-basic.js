axioms = owl.getAllAxioms();

console.log("#Axioms = "+axioms.length);

owl.loadOntology("http://purl.obolibrary.org/obo/upheno/imports/ro_import.owl", {addToImports: true})

owlinit(owl);

//q.config = {logLevel : 5};
function eqHasPart() {
    return q.equivalentClassesMatch("?phenotype", 
                                    q.someValuesFromMatch("?subqRel",
                                                          q.intersectionOfMatch(arguments)));
}

print("INH="+o.inheres_in);
trs =
    [
        {
            name: "T",
            match: q.equivalentClassesMatch("?x","?y"),
            gen: function(m,owl) {
                var eca =
                    owl.equivalentClasses(m.y,
                                          m.x
                                         );
                return eca;              
            }
        },

        {
            name: "EQ",
            match: eqHasPart("?quality",
                             q.someValuesFromMatch(o.inheres_in, "?e")),
            gen: function(m,owl) {
                var eca =
                equivalentClasses(m.phenotype,
                                  m.e
                                 );
                return eca;
                              
            }
        },

        {
            name: "IPO",
            match: eqHasPart("?quality",
                             q.someValuesFromMatch(o.inheres_in, "?part"),
                             q.someValuesFromMatch(o.inheres_in_part_of, "?whole"),
                             "?qualifier"),
            gen: function(m,owl) {
                var eca =
                equivalentClasses(m.phenotype,
                                  m.part
                                 );
                return eca;
                              
            }
        },


    ];

//axioms = owl.getAllAxioms().filter(function(ax) { ax.containsNamedEquivalentClass != null })
num = 0;
trs.forEach(function(tr) {
    pp(tr);
    var nu = q.findAndReplaceOrExtendInAxioms(tr.match, tr.gen, true, axioms);
    console.log("# = "+nu.length);
    num += nu.length;
});
console.log("Total="+num);
    

                                                                     
