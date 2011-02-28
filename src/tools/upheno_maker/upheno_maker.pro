:- use_module(library(thea2/owl2_popl)).
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_util)).
:- use_module(library(thea2/owl2_reasoner)).
:- use_module(library(thea2/owl2_io)).
:- use_module(library(thea2/owl2_text_display)).

% ----------------------------------------
% CONFIG
% ----------------------------------------

conf(ontology,'http://purl.obolibrary.org/obo/upheno').
conf(asserted_file,'upheno-asserted.owl').
conf(inferred_file,'upheno-inferred.owl').


% ----------------------------------------
% TOP-LEVEL
% ----------------------------------------

% iterate through every phenotype class (e.g. c in {MP,HP}),
% and transform-slurp this into new ontology O
make_upheno :-
        conf(ontology,O),
        conf(inferred_file,IF),
        conf(asserted_file,AF),
        slurp_all_phenotype_classes(O),
        save_axioms(AF,owl,[ontology(O)]),
        add_direct_inferred_links(O),
        save_axioms(IF,owl,[ontology(O)]).

slurp_all_phenotype_classes :-
        slurp_all_phenotype_classes('http://pur.obolibrary.org/obo/upheno').
slurp_all_phenotype_classes(O) :-
        assert(ontology(O)),
        debug(upheno,'Getting phenotype classes',[]),
        setof(C,(class(C),
                is_phenotype_class(C)),
              Cs),
        debug(upheno,'Merging phenotype classes into upheno',[]),
        forall(member(C,Cs),
               slurp_class_into(C,O)),
        debug(upheno,'Slurpd all classes into: ~w',[O]),
        add_property_axioms(O).


add_property_axioms(O) :-
        findall(A,propertyAxiom(A),As),
        forall(member(A,As),
               assert_axiom(A,O)).

% ----------------------------------------
% SLURPING
% ----------------------------------------


% slurp:
%  * if the class is an an equivalence then ignore non-representative member
%  * otherwise slurp class into upheno
slurp_class_into(C,_) :-
        has_representative(C,C2),
        print_message(informational,owlfmt('Ignore: ~w (will instead use ~w as representative)',[C,C2])),
        !.
slurp_class_into(C,O) :-
        % map class IRI and make a declaration
        rewrite_as_upheno_uri(C,CX),
        print_message(informational,owlfmt('Slurping: ~w as ~w',[C,CX])),
        assert_axiom(class(CX),O),
        % TODO: xref

        % get set of equivalents
        findall(EC,nc_equivalent_to(C,EC),ECs1),
        sort(ECs1,ECs),
        print_message(informational,owlfmt('Equivs: ~w',[ECs])),

        % bring in all axioms, after merging
        debug(upheno,' Merging axioms (direct):',[]),
        forall(class_has_slurpable_axiom(C,A),
               slurp_axiom(A,CX,O)),
        debug(upheno,' Merging axioms (for equivs):',[]),
        forall((member(EC,ECs),class_has_slurpable_axiom(EC,A)),
               slurp_axiom(A,CX,O)).

class_has_slurpable_axiom(C,A) :- A=subClassOf(C,_),A.
class_has_slurpable_axiom(C,equivalentClasses([C,D])) :- equivalent_to(C,D).
class_has_slurpable_axiom(C,A) :- A=annotationAssertion(_,C,_),A.
class_has_slurpable_axiom(C,A) :- A=propertyAssertion(_,C,_),A.

slurp_axiom(A,_RefC,O) :-
        ontologyAxiom(O,A), % already have
        !.
slurp_axiom(A,_RefC,O) :-
        %print_message(informational,owlfmt('   Slurping axiom: ~w',[A])),
        map_IRIs( replace_entity_with_generic, A, A2),
        %print_message(informational,owlfmt('      Rewritten as: ~w',[A2])),
        assert_axiom_if_valid(A2,O).

assert_axiom_if_valid(equivalentClasses([X,X]),_) :- !.
assert_axiom_if_valid(A2,O) :-        assert_axiom(A2,O).


% equivalence between named classes; also, make asym
nc_equivalent_to(A,B) :- atom(A),equivalent_to(A,B),atom(B).
nc_equivalent_to(A,B) :- atom(A),equivalent_to(B,A),atom(B).

% B is a representative class for A if:
% * A and B are equivalent AND
% * B is alphabetically greater (arbitrary criterion, favours MP over HP)
has_representative(A,B) :- nc_equivalent_to(A,B),A@<B.


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

is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/HP_',_,X).
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/MP_',_,X).
is_phenotype_class(X) :- atom(X),atom_concat('http://purl.obolibrary.org/obo/_ZPHEN',_,X).

rewrite_as_upheno_uri(X,NewURI) :-
        atom(X),
        atom_concat('http://purl.obolibrary.org/obo/',ID,X),
        atomic_list_concat(Toks,'_',ID), % Typically: [Prefix,Num]
        atomic_list_concat(['http://purl.obolibrary.org/obo/UPHENO_'|Toks],NewURI).

% ----------------------------------------
% REASONING
% ----------------------------------------

add_direct_inferred_links(O) :-
        initialize_reasoner(pellet,
                            RE,
                            [ontology(O),
                             filter(A,(axiom_profile(A,owl2_EL),\+individualAxiom(A)))
                            ]),
        debug(upheno,'done reasoning',[]),
        AT=subClassOf(_,_),
        findall(AT,reasoner_ask(RE,AT,true),Axs),
        debug(upheno,'got axioms',[]),
        length(Axs,NumAxs),
        debug(upheno,'num axioms = ~w',[NumAxs]),
        print_message(informational,tagval('number of inferred axioms',NumAxs)),
        debug(upheno,'asserting = ~w',[NumAxs]),
        forall(member(Ax,Axs),
               assert_axiom(Ax,O)),
        findall(X-Y,reasoner_ask(RE,equivalentClasses([X,Y])),EquivPairs),
        length(EquivPairs,NumEquivs),
        print_message(informational,tagval('number of equivalencies',NumEquivs)),
        handle_equiv_pairs(EquivPairs).

handle_equiv_pairs([]).
handle_equiv_pairs([P|Ps]) :-
        handle_equiv_pair(P),
        handle_equiv_pairs(Ps).

handle_equiv_pair(X-Y) :-
        true.                   % TODO


        


        





:- multifile prolog:message//1.
prolog:message(tagval(T,V)) --> [T,':',V].
