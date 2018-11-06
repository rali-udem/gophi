%% remove inverse arguments by adding :*Role
%% example:
%      [girl,g,
%            [':ARG1-of',['see-01',s,
%                                  [':ARG0',[boy,b]]]]]],
% becomes
%      [girl,\g,
%            [':*:ARG1',['see-01',s,
%                                 [':ARG1',g],
%                                 [':ARG0',[boy,b]]]]]],
% if the new introduced variable (here g) is not projected, it is marked as projected

elimInv([Concept, Var | Roles],[Concept, Var1 | Roles1]):-!,
    elimInvRoles(Concept,Var,Roles,Var1,Roles1).
elimInv(Var,Var).

% elimInvRoles(+Concept,+Var,+Roles,-VarOut,-Roles)
elimInvRoles(ConceptA,VarA,[[RoleInv,AMR]|Roles],VarAP,[[StarRole,AMRout]|Roles1]):-
    atom_concat(Role,'-of',RoleInv),
    %% check for "ordinary" roles terminating by "-of"
    \+memberchk(RoleInv,[':consist-of',':domain-of',':part-of',':polarity-of',':subevent-of']),!,
    atom_concat(':*',Role,StarRole),
    elimInv(AMR,AMR1),
    addInvRole(RoleInv,VarA,AMR1,AMRout),
    projectVar(VarA,VarAP),
    elimInvRoles(ConceptA,VarA,Roles,_Var,Roles1).
% continue if no inverse role
elimInvRoles(_,Var,[],Var,[]).
elimInvRoles(Concept,Var,[[Role,AMR]|Roles],Var1,[[Role,AMR1]|Roles1]):-
    elimInv(AMR,AMR1),
    elimInvRoles(Concept,Var,Roles,Var1,Roles1).

addInvRole(RoleInv,VarA,[ConceptB,VarB             |Roles],
                        [ConceptB,VarB,[Role,UVarA]|Roles]):-
    re_matchsub('^(.*)-of',RoleInv,Sub,[]),
    atom_string(Role,Sub.1),
    unProjectVar(VarA,UVarA).
addInvRole(_,_,AMR,AMR):- % "strange" case of an inverse role on a variable 
    writeln('** inverse role on strange AMR':AMR).

% project a variable is not already projected 
projectVar(\V,\V):-!. 
projectVar(V,\V).
% do not project an already projected variable
unProjectVar(\V,V):-!. 
unProjectVar(V,V).
