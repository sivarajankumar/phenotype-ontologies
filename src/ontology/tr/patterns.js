var q = new DLMatch(owl);
//q.config = { logLevel : 1 };

// convenience wrapper for outer part of expressions
var eqHasPart = function(ops) {
    //print(arguments);
    var x =
    q.equivalentClassesMatch("?baseClass",
                             q.objectSomeValuesFromMatch(o.has_part, 
                                                         q.intersectionOfMatch.apply(q, ops)));
    print("EHP="+x);
    return x;
}

var totalReplacements = 0;
var totalNewAxioms = 0;
function replace(x,y) {
    print("REPLACING: "+x +" ===> "+y);
    print("===");
    var rmap = 
        q.findAndReplace(x,y);
    rmap.replacements.forEach(function(r) {
        print("REPLACE: ");
        r.rmAxioms.forEach(pp);
        print("WITH: ");
        r.newAxioms.forEach(pp);
        print("---");
    });
    print("Replacements: "+rmap.replacements.length);    
    print("New Axioms: "+rmap.newAxioms.length);    
    totalReplacements += rmap.replacements.length;
    totalNewAxioms += rmap.newAxioms.length;
    print("Total Replacements: "+totalReplacements);    
    print("Total New Axioms: "+totalNewAxioms);    
    var outfile = "tr/out.owl";
    owl.useFunctionalSyntax();
    owl.save(outfile);
    print("Saved to: "+outfile);
}


function tr(x,y) {
    print("Translating...");
    var axioms = 
        q.findAndReplace(
            eqHasPart(x),
            function(m,owl) {
                //print("Checking "+m+" owl="+owl);
                //return owl.equivalentClasses(m.baseClass, y.call(m,owl));
                return []
            }
        );
    print("New axioms: "+axioms.length);
    return axioms;
}

function test(c,x) {
    owl.getReasoner().flush();
    var ecs = owl.getInferredEquivalentClasses(x);
    print("ECS="+ecs);
    var ok = false;
    ecs.forEach(function(ec) {
        print("TESTING '"+ec+"' == '"+c+"'");
        if(ec.equals(c)) { ok = true }
    });
    if (!ok) {
        print("FAILED: "+c+" != "+x);
        quit(1);
    }
    else {
        print("Test passed");
    }
}

function partOf(p,w) {
    return owl.intersectionOf(p,
                              owl.someValuesFrom(o.part_of, w));
}

function abnormal() {
    return owl.someValuesFrom(o.has_component, o.abnormal);
}
function q_abnormal() {
    return q.someValuesFromMatch(o.has_component, o.abnormal);
}
function towards(x) {
    return owl.someValuesFrom(o.towards, x);
}
function q_towards(x) {
    return q.someValuesFromMatch(o.towards, x);
}
function inheres_in(x) {
    return owl.someValuesFrom(o.inheres_in, x);
}
function q_inheres_in(x) {
    return q.someValuesFromMatch(o.inheres_in, x);
}
//function isa(v,c) {
//    return {var: v, 
//}

print("Loaded patterns library");
