:- use_module(bio(bioprolog_util)).

% TODO: use skeleton-of for morphology

wall:-
        wall(_).

wall(Ont):-
        write_header(Ont),
        call_unique(parent(P,R,S)),
        mk(Ont,P,R,S),
        fail.



% todo - increased rate should be same as positively regulates
rule(go,results_in_assembly_of,rate,count).
rule(go,results_in_assembly_of,'increased rate','present in greater numbers in organism').
rule(go,results_in_assembly_of,'decreased rate','present in fewer numbers in organism').

rule(go,results_in_disassembly_of,rate,count).
rule(go,results_in_disassembly_of,'decreased rate','present in greater numbers in organism').
rule(go,results_in_disassembly_of,'increased rate','present in fewer numbers in organism').

rule(go,results_in_development_of,quality,morphology).
rule(go,results_in_development_of,absent,aplastic).
rule(go,results_in_development_of,abolished,aplastic).

rule(go,results_in_morphogenesis_of,quality,shape).
rule(go,results_in_organization_of,quality,structure).
rule(go,results_in_formation_of,quality,structure).
rule(go,results_in_maturation_of,quality,maturity).   % TODO - check

rule(go,results_in_growth_of,rate,size).
rule(go,results_in_growth_of,'increased rate','increased size').
rule(go,results_in_growth_of,'decreased rate','decreased size').

rule(go,results_in_fusion_of,quality,'unfused from').  % TODO - implicit abnormality
rule(go,results_in_fission_of,quality,'fused with').  % TODO - implicit abnormality
rule(go,results_in_division_of,quality,'fused with').  % TODO - implicit abnormality

%rule(go,results_in_determination_of,quality,morphology).  % TODO - new pato?
%rule(go,results_in_specification_of,quality,morphology).  % TODO - new pato?
%rule(go,results_in_commitment_to,quality,morphology).  % TODO - new pato?
rule(go,results_in_aquisition_of_features_of,quality,morphology).  % TODO - new pato?

% TODO - do something more sophisticated with these; e.g. quantity in
rule(go,transports_or_maintains_localization_of,quality,position).
rule(go,results_in_movement_of,quality,position).

rule(go,regulates_levels_of,quality,count).

% TODO: use more specific relations for bioynthesis vs catabolism
rule(go,has_output,rate,count).
rule(go,has_output,'increased rate','present in greater numbers in organism').
rule(go,has_output,'decreased rate','present in fewer numbers in organism').
% TODO: inverse for has_input

rule(go,acts_on_population_of,rate,count).
rule(go,acts_on_population_of,'increased rate','present in greater numbers in organism').
rule(go,acts_on_population_of,'decreased rate','present in fewer numbers in organism').

rule(go,occurs_in,quality,quality). % TODO

rule(uberon,contributes_to_morphology_of,morphology,morphology).

rule(uberon,skeleton_of,morphology,morphology).
rule(uberon,skeleton_of,size,size).

% e.g. gland functionality causes abnormal gland process; TODO - reverse?
rule(uberon,capable_of,functionality,quality).  
rule(uberon,capable_of,'decreased functionality','decreased rate'). % TODO - efficiency?
rule(uberon,capable_of,'increased functionality','increased rate'). % TODO - efficiency?
rule(uberon,capable_of,absent,abolished). % too strong?

rule(uberon,capable_of_part_of,functionality,quality).  
rule(uberon,capable_of_part_of,'decreased functionality','decreased rate'). % TODO - efficiency?
rule(uberon,capable_of_part_of,'increased functionality','increased rate'). % TODO - efficiency?

q(organ,mass).
q(organ,size).


assay(glycosaminoglycan,urine).


mk(Ont,P,R,S) :-
        rule(Ont,R,PGN,SGN),
        class(SG,SGN),
        class(PG,PGN),
        phname(S,SG,SQ,SQN),
        phname(P,PG,PQ,PQN),
        mkclass(SQ,SQN,SG,inheres_in,S),
        mkclass(PQ,PQN,PG,inheres_in,P),
        mkrel(PQ,PQN,causes,SQ,SQN).

% TODO - what about plain 'cholesterol import' - can we assume it removes it from blood?
%transport(exports,has_target_end_location,'increased rate','present in greater numbers in organism').
%transport(exports,has_target_end_location,'decreased rate','present in fewer numbers in organism').
transport(exports,has_target_start_location,'increased rate','present in fewer numbers in organism').
transport(exports,has_target_start_location,'decreased rate','present in greater numbers in organism').
transport(imports,has_target_end_location,'increased rate','present in greater numbers in organism').
transport(imports,has_target_end_location,'decreased rate','present in fewer numbers in organism').
%transport(imports,has_target_start_location,'increased rate','present in fewer numbers in organism').
%transport(imports,has_target_start_location,'decreased rate','present in greater numbers in organism').

mk(transport,P,R,S) :-
        transport(R,LocRel,PGN,SGN),
        parent(P,LocRel,Loc),

        class(PG,PGN),
        phname(P,PG,PQ,PQN),
        mkclass(PQ,PQN,PG,inheres_in,P),

        %class(S,SN),
        phname(Loc,S,LocS,LocSN),
        mkclass(LocS,LocSN,S,part_of,Loc),
        
        class(SG,SGN),
        phid(LocS,SG,SQ),
        concat_atom([LocSN,SGN],' ',SQN),
        mkclass(SQ,SQN,SG,inheres_in,S),

        mkrel(PQ,PQN,causes,SQ,SQN).

:- dynamic done/1.
:- dynamic done/2.

% Generates a class stanza, if one has not already been written for
% this skolemized ID
mkclass(ID,_N,_G,_R,_Y) :-
        done(ID),
        debug(pheno,'Done: ~w',[ID]),
        !.
mkclass(ID,N,G,R,Y) :-
        class(G,GN),
        class(Y,YN),
        \+ entity_partition(G,goantislim_grouping),
        \+ entity_partition(Y,goantislim_grouping),
        format('[Term]~n'),
        format('id: ~w~n',[ID]),
        format('name: ~w~n',[N]),
        format('intersection_of: ~w ! ~w~n',[G,GN]),
        format('intersection_of: ~w ~w ! ~w~n',[R,Y,YN]),
        %maplist(writerel,Rels),
        assert(done(ID)),
        nl,
        !.

% Generates a relationship/bridge stanza, if one has not already been
% written for this skolemized pair of subject and object
mkrel(ID,_N,_R,Y,_YN) :-
        done(ID,Y),
        !.
mkrel(ID,N,R,Y,YN) :-
        format('[Term]~n'),
        format('id: ~w ! ~w~n',[ID,N]),
        format('relationship: ~w ~w ! ~w~n',[R,Y,YN]),
        nl,
        assert(done(ID,Y)),
        !.

% @Deprecated
writerel(rel(R,Z,ZN)) :-
        format('relationship: ~w ~w ! ~w~n',[R,Z,ZN]).


%% phname(+DiffClass, +G:genus, ?DefinedClassID, DefinedClassName)
phname(C,G,ID,CN) :-
        phid(C,G,ID),
        class(C,N),
        class(G,GN),
        concat_atom([N,GN],' ',CN).

phid(C,G,ID) :-
        concat_atom([Ont,Local],':',C),
        concat_atom([OntG,LocalG],':',G),
        concat_atom(['UPHENO:',Ont,Local,'-',OntG,LocalG],ID).
        
        
:- dynamic written_header/0.

write_header(_Ont) :-
        written_header,
        !.
write_header(Ont) :-
        format('ontology: upheno/~w~n',[Ont]),
        nl,
        assert(written_header),
        !.
