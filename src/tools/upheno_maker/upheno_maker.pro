:- use_module(library(thea2/owl2_popl)).
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_util)).
:- use_module(library(thea2/owl2_reasoner)).
:- use_module(library(thea2/owl2_io)).
:- use_module(library(thea2/owl2_profiles)).
:- use_module(library(thea2/owl2_text_display)).
:- use_module(library(thea2/util/memoization)).

/*
  ---+ upheno_maker

  Creates the uberpheno ontology from the combination of multiple ontologies

  See the makefile in src/ontology/uberpheno
  
  Input:

  Either: hp-mp/mp-hp-ext-merged-uberon.owl
  Or: {mp,hp,mp-equivalence-axioms,hp-equivalence-axioms,uberon-with-isa-for-FMA-FMA-MA,pato}

  [the former is the merge of the latter]
  
  Algorithm:

  Every phenotype class from {mp,hp,zpheno} is merged into the upheno ontology; a phenoclass is omitted
  if it has an equivalent representative class.

  during merging, FMA,MA,ZFA are all mapped to UBERON classes. This makes an ontology
  called upheno-asserted.

  After merging, the simple reference closure is computed for all
  classes used in phenotype logical definitions. This makes an
  ontology called upheno-asserted-full.

  Then a DL reasoner is executed to compute additional SubClassOf
  axioms. This makes an ontology called upheno-inferred.

  Current equivalence axioms between named classes are left in the
  ontology
  
  */

% ----------------------------------------
% CONFIG
% ----------------------------------------

conf(ontology,'http://purl.obolibrary.org/obo/upheno').
conf(asserted_file,'upheno-asserted.owl').
conf(asserted_file_full,'upheno-asserted-full.owl').
conf(inferred_file,'upheno-inferred.owl').

% ----------------------------------------
% DB
% ----------------------------------------

:- dynamic cached_axiom/1.
cache_axiom(A) :- assert(cached_axiom(A)).

assert_cached_axioms(O):- findall(A,cached_axiom(A),As),forall(member(A,As),assert_axiom_if_valid(A,O)).

% wrapper for assert_axiom/2 - avoid asserting reflexive equivalence
assert_axiom_if_valid(equivalentClasses([X,X]),_) :- !.
assert_axiom_if_valid(A2,O) :- assert_axiom(A2,O).


% ----------------------------------------
% TOP-LEVEL
% ----------------------------------------

% iterate through every phenotype class (e.g. c in {MP,HP}),
% and transform-slurp this into new ontology O
make_upheno :-
        table_pred(owl2_model:equivalent_to/2), % optimization
        conf(ontology,O),
        conf(inferred_file,IF),
        conf(asserted_file,AF),
        conf(asserted_file_full,AFF),
        slurp_all_phenotype_classes(O),
        assert_cached_axioms(O),
        % todo - fix - this clears rdf graph and loses input axioms
        %save_axioms(AF,owl,[ontology(O)]),
        mireot(O),
        save_axioms(AFF,owl,[ontology(O)]),
        add_direct_inferred_links(O),
        save_axioms(IF,owl,[ontology(O)]).

slurp_all_phenotype_classes :-
        slurp_all_phenotype_classes('http://pur.obolibrary.org/obo/upheno').
slurp_all_phenotype_classes(O) :-
        assert(ontology(O)),
        debug(upheno,'Getting phenotype classes',[]),
        setof(C,phenotype_class(C),Cs),
        debug(upheno,'Merging phenotype classes into upheno',[]),
        forall(member(C,Cs),
               slurp_class_into(C)),
        debug(upheno,'Slurpd all classes into: ~w',[O]),
        add_property_axioms(O).

add_property_axioms(O) :-
        findall(A,(axiom_about_property(A),axiom_profile(A,owl2_EL)),As),
        forall(member(A,As),
               assert_axiom(A,O)).

axiom_about_property(A) :- A=objectProperty(_),A.
axiom_about_property(A) :- propertyAxiom(A).
axiom_about_property(A) :- A=annotationAssertion(_,P,_),objectProperty(P),A.

% ----------------------------------------
% SLURPING/MERGING
% ----------------------------------------

% slurp:
%  * if the class is an an equivalence then ignore non-representative member
%  * otherwise slurp class into upheno
slurp_class_into(C) :-
        annotationAssertion('http://www.w3.org/2002/07/owl#deprecated',C,literal(type(_,true))),
        print_message(informational,owlfmt('Ignore Obsolete: ~w',[C])),
        !.
slurp_class_into(C) :-
        has_representative(C,C2),
        print_message(informational,owlfmt('Ignore: ~w (will instead use ~w as representative)',[C,C2])),
        !.
slurp_class_into(C) :-
        % map class IRI and make a declaration
        rewrite_as_upheno_uri(C,CX),
        print_message(informational,owlfmt('Slurping: ~w as ~w',[C,CX])),
        cache_axiom(class(CX)),
        % TODO: xref

        % get set of equivalents
        findall(EC,nc_equivalent_to(C,EC),ECs1),
        sort(ECs1,ECs),
        print_message(informational,owlfmt('Equivs: ~w',[ECs])),

        % bring in all axioms, after merging
        debug(upheno,' Merging axioms (direct):',[]),
        forall(class_has_slurpable_axiom(C,A),
               slurp_axiom(A)),
        debug(upheno,' Merging axioms (for equivs):',[]),
        forall((member(EC,ECs),class_has_slurpable_axiom(EC,A)),
               slurp_axiom(A)).

% we only slurp a simple subset of axioms into upheno
class_has_slurpable_axiom(C,A) :- A=subClassOf(C,_),A.
class_has_slurpable_axiom(C,equivalentClasses([C,D])) :- equivalent_to(C,D).
class_has_slurpable_axiom(C,A) :- A=annotationAssertion(_,C,_),A.
class_has_slurpable_axiom(C,A) :- A=propertyAssertion(_,C,_),A.

%slurp_axiom(A,_RefC,O) :-
%        ontologyAxiom(O,A), % already have
%        !.
slurp_axiom(A) :-
        map_IRIs( replace_entity_with_generic, A, A2),
        cache_axiom(A2).


% ----------------------------------------
% MAPPING LOGIC
% ----------------------------------------

% equivalence between named classes; also, make asym
nc_equivalent_to(A,B) :- atom(A),equivalent_to(A,B),atom(B).
nc_equivalent_to(A,B) :- atom(A),equivalent_to(B,A),atom(B).

% B is a representative class for A if:
% * A and B are equivalent AND
% * B is alphabetically greater (arbitrary criterion, favours MP over HP)
has_representative(A,B) :- nc_equivalent_to(A,B),A@<B,\+((nc_equivalent_to(B,C),B@<C)).


% map to uberon OR to canonical member of equivalence set.
% TODO: also map HP,MP->UPHENO
replace_entity_with_generic(X,Y) :- ubermap(X,Y),!.
replace_entity_with_generic(X,Y) :- has_representative(X,Z),!,replace_entity_with_generic(Z,Y).
replace_entity_with_generic(X,Y) :- is_phenotype_class(X),rewrite_as_upheno_uri(X,Y),!.
replace_entity_with_generic(X,X).

ubermap(X,Y) :-
        subClassOf(X,Y),
        \+ in_uberon(X),
        in_uberon(Y).

in_uberon(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/UBERON_',_,X).

phenotype_class(C) :- class(C),is_phenotype_class(C).
phenotype_class(C) :- equivalent_to(C,_),\+class(C),is_phenotype_class(C). % HACK to make up for bug in ZFIN OWL
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/HP_',_,X).
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/MP_',_,X).
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/_ZPHEN',_,X).
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/UPHENO_',_,X).

rewrite_as_upheno_uri(X,NewURI) :-
        atom(X),
        atom_concat('http://purl.obolibrary.org/obo/',ID,X),
        atomic_list_concat(Toks,'_',ID), % Typically: [Prefix,Num]
        atomic_list_concat(['http://purl.obolibrary.org/obo/UPHENO_'|Toks],NewURI).

% ----------------------------------------
% MIREOT
% ----------------------------------------

% a simple mireoting procedure to bring in a simple reference closure into O.
% here the reference closure spans over SubClassOf, EquivalentClasses,
% SomeValuesFrom (with selected relationships)
mireot(O) :-
        setof(X,A^(ontologyAxiom(O,A),paxiom_refs(A,X),\+is_phenotype_class(X)),Xs),
        length(Xs,NumXs),
        debug(upheno,'num referenced classes: ~w',[NumXs]),
        mireot_axioms(Xs,[],[],Axioms),
        length(Axioms,NumAxioms),
        debug(upheno,'num imported axioms: ~w',[NumAxioms]),
        forall(member(A,Axioms),
               assert_axiom(A,O)).

%% mireot_axioms(+Stack:list, Visiteds:list, +AxiomsAccum:list ,?FinalAxions:list) is det
% closure - depth first - traverse until stack is empty. on completion, unify
% accumulator with final set
mireot_axioms([],_,Axs,Axs).
mireot_axioms([X|Xs],Vs,Axs,FinalAxs) :-
        member(X,Vs),
        !,
        mireot_axioms(Xs,Vs,Axs,FinalAxs).
mireot_axioms([X|Xs],Vs,Axs,FinalAxs) :-
        debug(upheno,'   X: ~w',[X]),
        findall(A,mireot_axiom(X,A),NewAxs),
        findall(Y,(mireot_traverse(X,Y),\+is_phenotype_class(Y),\+member(Y,Vs)),NewXs),
        append(Xs,NewXs,AllXs),
        append(Axs,NewAxs,AllAxs),
        mireot_axioms(AllXs,[X|Vs],AllAxs,FinalAxs).

% import these axioms:
mireot_axiom(X,A) :- A=subClassOf(X,_),A.
mireot_axiom(X,equivalentClasses([X,Y])) :- equivalent_to(X,Y).
mireot_axiom(X,A) :- A=annotationAssertion(_,X,_),A.

% traverse these axioms and expressions:
mireot_traverse(someValuesFrom(P,Y),Y) :- mireot_property(P).
mireot_traverse(intersectionOf(L),Y) :- member(Y,L).
mireot_traverse(X,Y) :- subClassOf(X,Y).
mireot_traverse(X,Y) :- equivalent_to(X,Y).

% only traverse these properties:
mireot_property('http://purl.obolibrary.org/obo/BFO_0000050'). % part_of // '0
mireot_property('http://purl.obolibrary.org/obo/BFO_0000052'). % inheres_in
mireot_property('http://purl.obolibrary.org/obo/hp/hp-logical-definitions_inheres_in_part_of'). 
mireot_property('http://purl.obolibrary.org/obo/hp/hp-logical-definitions_qualifier'). 

% seed:
paxiom_refs(equivalentClasses(ECL),Y) :-
        member(intersectionOf(L),ECL),
        member(Y,L).


% ----------------------------------------
% REASONING
% ----------------------------------------

add_direct_inferred_links(O) :-
        initialize_reasoner(pellet,
                            RE,
                            [ontology(O),
                             filter(A,(axiom_profile(A,owl2_EL),\+individualAxiom(A)))
                            ]),
        debug(upheno,'done initializing reasoner',[]),

        % query for inferred SubClassOfs
        AT=subClassOf(_,_),
        findall(AT,reasoner_ask(RE,AT,true),Axs),
        debug(upheno,'got axioms',[]),
        length(Axs,NumAxs),
        debug(upheno,'num axioms = ~w',[NumAxs]),
        print_message(informational,tagval('number of inferred axioms',NumAxs)),
        forall(member(Ax,Axs),
               print_message(informational,owlfmt('  Inferred axiom: ~w',[Ax]))),

        % assert implied links
        debug(upheno,'asserting = ~w',[NumAxs]),
        forall(member(Ax,Axs),
               assert_axiom(Ax,O)),

        % query for equivalent classes:
        findall(X-Y,reasoner_ask(RE,equivalentClasses([X,Y])),EquivPairs),
        length(EquivPairs,NumEquivs),
        print_message(informational,tagval('number of equivalencies',NumEquivs)),
        forall(member(X-Y,EquivPairs),
               print_message(informational,owlfmt('  Inferred: ~w EquivalentTo ~w',[X,Y]))),
        handle_equiv_pairs(EquivPairs).

% do nothing for now
handle_equiv_pairs(_).

/*
handle_equiv_pairs([]).
handle_equiv_pairs([P|Ps]) :-
        handle_equiv_pair(P),
        handle_equiv_pairs(Ps).
*/

:- multifile prolog:message//1.
prolog:message(tagval(T,V)) --> [T,':',V].



