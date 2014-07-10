//var fn = "mammal.owl";
//var fn = "wbphenotype/wbphenotype-importer.owl";
//var fn = "imports/mpath_import.owl";
var fn = "imports/uberon_import.owl";

blacklist = JSON.parse(fs.read("util/blacklist.js"));

var nsmap =
    {
        "UBERON" : "imports/uberon_import.owl",
        "GO" : "imports/go_import.owl",
        "WBbt" : "imports/wbbt_import.owl",
        "MPATH" : "imports/mpath_import.owl",
        "NBO" : "imports/nbo_import.owl",
        "FBbt" : "imports/fbbt_import.owl",
    };
var alts = 
    {
        "UBERON": ["CL"]
    }

owl.loadFile("imports/ro_extra.owl");
var roImp = owl.getOntology();

for (var ns in nsmap) {
    var fn = nsmap[ns];

    var outfn = fn.replace("_import.owl","_phenotype.owl");
    if (outfn == fn) {
        console.warn(fn+" <-- non-standard");
        continue;
        //outfn = fn + "-pheno.owl";
    }

    // ----------------------------------------
    // LOAD AND CREATE STUB
    // ----------------------------------------
    console.log("Loading: "+fn);
    owl.addCatalog();
    owl.loadFile(fn);
    src = owl.getOntology();


    console.log("Creating ont");
    owl.createOntology( "http://purl.obolibrary.org/obo/upheno/"+outfn, [src, roImp]);
    console.log("Created ont: " + owl);
    


    // ----------------------------------------
    // SETUP
    // ----------------------------------------
    var entityRoots = ["cell", "biological_process", "anatomical structure"];
    //var namespaces = ["UBERON","CL","MPATH", "GO"];
    
    addMode(true);
    var prop = o.is_variation_of;
    if (prop == null) {
        console.warn("Making substitute");
        prop = mkObjectProperty({id:"http://purl.obolibrary.org/obo/UPHENO_0000001", label:"has phenotype affecting"});
    }
    
    // ----------------------------------------
    // ITERATE
    // ----------------------------------------
    cs = owl.getClasses(true);
    console.log("#Classes = " + cs.length);
    cs.forEach(function(c) {
        var iri = owl.getIRI(c) + "";
        if (blacklist[iri]) {
            console.log("  Skipping: "+c);
            return;
        }
        //if (namespaces.filter(function(ns) {return iri.indexOf(ns) > -1}).length == 0) {
        //console.log("  Skipping: "+c);
        //return;
        //}

        if (iri.indexOf(ns) == -1) {
            if (alts[ns] != null &&
                alts[ns].filter(function(alt) {return iri.indexOf(alt) > -1}).length > 0) {
                // OK
            }
            else {
                return;
            }
        }
        var piri = iri + "PHENOTYPE";
        var pc = owl.class(piri);
        var plabel = owl.getLabel(c) + " phenotype";
        labelAssertion(pc, plabel);
        equivalentClasses(pc, someValuesFrom(prop,c));
    });
    
    save(outfn);
}
