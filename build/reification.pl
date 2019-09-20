%% reification table taken from
%%        https://github.com/goodmami/norman/blob/master/maps/reifications-full.tsv
%% explained in 
% @inproceedings{Goodman:2019,
%   title     = "{AMR} Normalization for Fairer Evaluation",
%   author    = "Goodman, Michael Wayne",
%   booktitle = "Proceedings of the 33rd Pacific Asia Conference on Language, Information, and Computation",
%   year      = "2019",
%   address   = "Hakodate"
% }

%% reification(Role,Concept,:Source,:Target)
%% used in transforming 
%%       (A Role B)    <==> (A :Source-of (c/Concept :Target B)) [regular role]
dereify([A,Var|Roles],[A,Var,[Role,B]|Roles2]):-
    reification(Role,Concept,Source,Target),invRole(Source,SourceOf),
    select([SourceOf,[Concept,_,[Target,B]]],Roles,Roles1), % must not be involved in other roles
    dereify(Roles1,Roles2).
%%       (A Role-of B) <==> (A :Target-of (c/Concept :Source B)) [inverted role]
dereify([A,Var|Roles],[A,Var,[RoleOf,B]|Roles2]):-
    reification(Role,Concept,Source,Target),invRole(Target,TargetOf),
    select([TargetOf,[Concept,_,[Source,B]]],Roles,Roles1), % must not be involved in other roles
    dereify(Roles1,Roles2),
    invRole(Role,RoleOf).

dereify([A,Var|Roles],[A,Var|Roles1]):-
    maplist(dereifyRole,Roles,Roles1).
dereify(Var,Var).

dereifyRole([Role,AMRin],[Role,AMRout]):-
    dereify(AMRin,AMRout).


reification(':accompanier','accompany-01',':ARG0',':ARG1').
reification(':age','age-01',':ARG1',':ARG2').
reification(':beneficiary','benefit-01',':ARG0',':ARG1').
reification(':beneficiary','receive-01',':ARG2',':ARG0').
reification(':cause','cause-01',':ARG1',':ARG0').
reification(':concession','have-concession-91',':ARG1',':ARG2').
reification(':condition','have-condition-91',':ARG1',':ARG2').
% reification(':cost','cost-01',':ARG1',':ARG2'). %% unprocessed role
reification(':degree','have-degree-92',':ARG1',':ARG2').
reification(':destination','be-destined-for-91',':ARG1',':ARG2').
reification(':domain','have-mod-91',':ARG2',':ARG1').
reification(':duration','last-01',':ARG1',':ARG2').
% reification(':employed-by','have-org-role-91',':ARG0',':ARG1'). %% unprocessed role
reification(':example','exemplify-01',':ARG0',':ARG1').
% reification(':extent','have-extent-91',':ARG1',':ARG2').  %% unprocessed role
reification(':frequency','have-frequency-91',':ARG1',':ARG2').
reification(':instrument','have-instrument-91',':ARG1',':ARG2').
reification(':li','have-li-91',':ARG1',':ARG2').
reification(':location','be-located-at-91',':ARG1',':ARG2').
reification(':manner','have-manner-91',':ARG1',':ARG2').
reification(':meaning','mean-01',':ARG1',':ARG2'). %% unprocessed role
reification(':mod','have-mod-91',':ARG1',':ARG2').
reification(':name','have-name-91',':ARG1',':ARG2').
reification(':ord','have-ord-91',':ARG1',':ARG2').
% reification(':part','have-part-91',':ARG1',':ARG2'). %% unprocessed role but :part-of
reification(':polarity','have-polarity-91',':ARG1',':ARG2').
reification(':poss','own-01',':ARG0',':ARG1').
reification(':poss','have-03',':ARG0',':ARG1').
reification(':purpose','have-purpose-91',':ARG1',':ARG2').
reification(':quant','have-quant-91',':ARG1',':ARG2').
reification(':role','have-org-role-91',':ARG0',':ARG2').
reification(':source','be-from-91',':ARG1',':ARG2').
reification(':subevent','have-subevent-91',':ARG1',':ARG2').
% reification(':subset','include-91',':ARG2',':ARG1'). %% unprocessed role
% reification(':superset','include-91',':ARG1',':ARG2'). %% unprocessed role
reification(':time','be-temporally-at-91',':ARG1',':ARG2').
reification(':topic','concern-02',':ARG0',':ARG1').
reification(':value','have-value-91',':ARG1',':ARG2'). %% unprocessed role

invRole(Role,RoleInv):-atomic_concat(Role,'-of',RoleInv).

testDereify(AMRstring):-
    amrParse(AMRstring,AMR),
    pprint(AMR),
    dereify(AMR,AMR1),
    (AMR=AMR1->write("no dereification"),nl;pprint(AMR1)).

testD1:-dereifyEx(ko,AMR),testDereify(AMR).
testD2:-dereifyEx(ok,AMR),testDereify(AMR).

% ex6.re.amr (should not be dereified)
dereifyEx(ko,'
# ::snt "He did not ride the bicycle carefully."
(r / ride-01
   :ARG0 (h / he)
   :ARG1 (b / bicycle)
   :ARG1-of (h2 / have-manner-91
                :ARG2 (c / care-04)
                :polarity -)
)').

% ex6.re2.amr (should be dereified)
dereifyEx(ok,'
# ::snt "He rode the bicycle carelessly."
(r / ride-01
   :ARG0 (h / he)
   :ARG1 (b / bicycle)
   :ARG1-of (h2 / have-manner-91
                :ARG2 (c / care-04 :polarity -)
            )
)').

dereifyEx(ok1,'
# ::snt "He did not ride the bicycle carefully."
(r / ride-01
   :ARG0 (h / he)
   :ARG1 (b / bicycle)
   :ARG1-of (h2 / have-manner-91
                :ARG2 (c / care-04))
   :polarity -
)').

