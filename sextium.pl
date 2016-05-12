lexer(Tokens) -->
   white_space,
   (  (  "(*", !, comment, { Token = tokComment}
      ;  ":=",      !, { Token = tokAssgn }
      ;  ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  ",",	     !, { Token = tokComma }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (and, tokAnd),
                                     (begin, tokBegin),
                                     (call, tokCall),
                                     (div, tokDiv),
                                     (do, tokDo),
                                     (done, tokDone),
                                     (else, tokElse),
                                     (end, tokEnd),
                                     (fi, tokFi),
                                     (if, tokIf),
                                     (local, tokLocal),
                                     (mod, tokMod),
                                     (not, tokNot),
                                     (or, tokOr),
                                     (procedure,tokProcedure),
                                     (program, tokProgram),
                                     (read, tokRead),
                                     (return, tokReturn),
                                     (skip, tokSkip),
                                     (then, tokThen),
                                     (value, tokValue),
                                     (while, tokWhile),
                                     (write, tokWrite)]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Token \= tokComment, !,
           Tokens = [Token | TokList];
           Tokens = TokList },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).
 
white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { A = 39; A = 95 ; code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

comment --> "*)";
   [_], comment.


/*
<program> ::= program <identyfikator> <blok>
<blok> ::= <deklaracje> begin <instrukcja zlozona> end
<deklaracje> ::= <puste> | <deklaracje> <deklaracja>
<deklaracja> ::= <deklarator> | <procedura>
<deklarator> ::= local <zmienne>
<zmienne> ::= <zmienna> | <zmienne> , <zmienna>
<zmienna> ::= <identyfikator>
<procedura> ::= procedure <nazwa procedury> ( <argumenty formalne> ) <blok>
<nazwa procedury> ::= <identyfikator>
<argumenty formalne> ::= <puste> | <ciag argumentow formalnych>
<ciag argumentow formalnych> ::= <argument formalny> | <ciag argumentow formalnych>,<argument formalny>
<argument formalny> ::= <zmienna> | value <zmienna>
<instrukcja złozona> ::= <instrukcja> | <instrukcja zlozona> ; <instrukcja>
<instrukcja> ::= <zmienna> := <wyrazenie arytmetyczne>
| if <wyrazenie logiczne> then <instrukcja zlozona> fi
| if <wyrazenie logiczne> then <instrukcja zlozona> else <instrukcja zlozona> fi
| while <wyrazenie logiczne> do <instrukcja zlozona> done
| call <wywolanie procedury>
| return <wyrazenie arytmetyczne>
| read <zmienna>
| write <wyrazenie arytmetyczne>
<wyrazenie arytmetyczne> ::= <skladnik> | <wyrazenie arytmetyczne> <operator addytywny> <skladnik>
<operator addytywny> ::= +|-
<skladnik> ::= <czynnik> | <skladnik> <operator multiplikatywny> <czynnik>
<operator multiplikatywny> ::= * | div | mod
<czynnik> ::= <wyrazenie proste> | - <wyrazenie proste>
<wyrazenie proste> ::= <wyrazenie atomowe> | ( <wyrazenie arytmetryczne> )
<wyrazenie atomowe> ::= <zmienna> | <wywolanie procedury> | <literal calkowitoliczbowy>
<wywolanie procedury> ::= <nazwa procedury> ( <argumenty faktyczne> )
<argumenty faktyczne> ::= <puste> | <ciag argumentow faktycznych>
<ciag argumentow faktycznych> ::= <argument faktyczny> | <ciag argumentow faktycznych> , <argument faktyczny>
<argument faktyczny> ::= <wyrazenie arytmetyczne>
<wyrazenie logiczne> ::= <koniunkcja> | <wyrazenie logiczne> or <koniunkcja>
<koniunkcja> ::= <warunek> | <koniunkcja> and <warunek>
<warunek> ::= <wyrazenie relacyjne> | not <wyrazenie relacyjne>
<wyrazenie relacyjne> ::= <wyrazenie arytmetyczne> <operator relacyjny> <wyrazenie arytmetyczne> | ( <wyrazenie logiczne> )
<operator relacyjny> ::= < | <= | > | >= | = | <>
<puste> ::=
*/

:- op(990, xfy, ';;').
:- op(900, xfy, :=).
:- op(820, xfy, and).
:- op(840, xfy, or).
:- op(700, xfy, <=).
:- op(700, xfy, <>).

program(Ast) --> [tokProgram], [tokVar(_)], blok(Blok), { Ast =  Blok }.
blok(Blok) --> deklaracje(Decl), [tokBegin], instr_zlozona(InsZlo), [tokEnd], {Blok = (Decl ';;' InsZlo)}.
deklaracje(Deklaracje) --> ( deklaracja(Decl), !, deklaracje(Rest), {Rest \= [],!, Deklaracje = (Decl ';;' Rest);Deklaracje = Decl } ) ; ( [], { Deklaracje = [] } ) .
deklaracja(Deklaracja) --> ( deklarator(Decl), !, { Deklaracja = Decl } ) ; ( procedura(Proc), { Deklaracja = Proc } ).
deklarator(Deklarator) --> [tokLocal], zmienne(Zmienne), { Deklarator = local(Zmienne) }.
zmienne(Zmienne) --> ( zmienna(Zmienna), [tokComma], zmienne(Rest),! ,{ Zmienne = (Zmienna ';;' Rest) } );( zmienna(Zmienna), { Zmienne = Zmienna } ).
zmienna(Zmienna) --> [tokVar(Id)], { Zmienna = variable(Id) }.
procedura(Procedura) --> [tokProcedure], [tokVar(Id)], [tokLParen], argumenty_formalne(ArgFor),[tokRParen], blok(Blok), { Procedura = procdecl(Id , ArgFor, Blok) }.
argumenty_formalne(ArgFor) --> (ciag_arg_for(ArgFor)) ; ({ArgFor = []}).
ciag_arg_for(ArgFor) --> argument_formalny(ArgFor1), ([tokComma], (ciag_arg_for(Rest), {ArgFor = (ArgFor1 ';;' Rest) } ); {ArgFor = ArgFor1}).
argument_formalny(ArgFor) --> ([tokValue], !, [tokVar(Id)], {ArgFor = argv(Id)}) ; ( [tokVar(Id)], { ArgFor = arg(Id)} ).
instr_zlozona(InsZlo) --> instrukcja(Ins), (( [tokSColon], instr_zlozona(Rest), !, {InsZlo = (Ins ';;' Rest)} ) ; ([], { InsZlo = Ins })).
instrukcja(Ins) -->  ( [tokVar(Id)], !, [tokAssgn], arith_expr(ArithTree), { Ins = (Id := ArithTree) } );
( [tokIf], bool_expr(LogTree), [tokThen], instr_zlozona(InsZlo), [tokElse], !, instr_zlozona(InsZlo2), [tokFi], {Ins = if(LogTree, InsZlo, InsZlo2) } ) ;
( [tokIf], !, bool_expr(LogTree), [tokThen], instr_zlozona(InsZlo), [tokFi], {Ins = if(LogTree, InsZlo) } ) ;
( [tokWhile], !, bool_expr(LogTree) ,[tokDo] , instr_zlozona(InsZlo), [tokDone], { Ins = while(LogTree, InsZlo) } ) ;
( [tokCall], !, wywolanie_procedury(WywProc), {Ins = call(WywProc)} ) ;
( [tokReturn], !, arith_expr(ArithTree), { Ins = return(ArithTree) } ) ;
( [tokRead], !, [tokVar(Id)], {Ins = read(Id)} ) ;
( [tokWrite], arith_expr(ArithTree), { Ins = write(ArithTree) } ) .
wywolanie_procedury(WywProc) --> [tokVar(Id)], [tokLParen], argumenty_faktyczne(AFe), [tokRParen], {AFe\=[], !, WywProc = call(Id , AFe); WywProc = call(Id) } .
argumenty_faktyczne(AFe) --> argument_faktyczny(AF), ([tokComma], !, argumenty_faktyczne(Rest), {Rest\=[], !, AFe = (AF ';;' Rest)} ; {AFe = AF} ); [], { AFe = [] } . 

argument_faktyczny(AF) --> arith_expr(Expr), { AF = Expr }.

arith_expr(Expr) -->
   summand(Summand), arith_expr(Summand, Expr1),{Expr=arith(Expr1)}.

arith_expr(Acc, Expr) -->
   additive_op(Op), !, summand(Summand),
      { Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

summand(Expr) -->
   factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
      { Acc1 =.. [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

factor(Expr) -->
   (  [tokLParen], !, arith_expr(Expr), [tokRParen]
   ;  [tokNumber(N)], !, { Expr = constant(N) }
   ;  wywolanie_procedury(Proc), !, {Expr = Proc}
   ;  [tokVar(Var)], { Expr = variable(Var) }
   ).

bool_expr(Bool) -->
   disjunct(Disjunct), bool_expr(Disjunct, Bool1), {Bool = bool(Bool1)}.

bool_expr(Acc, Bool) -->
   [tokOr], !, disjunct(Disjunct),
      { Acc1 =.. [or, Acc, Disjunct] }, bool_expr(Acc1, Bool).
bool_expr(Acc, Acc) -->
   [].

disjunct(Disjunct) -->
   conjunct(Conjunct), disjunct(Conjunct, Disjunct).

disjunct(Acc, Disjunct) -->
   [tokAnd], !, conjunct(Conjunct),
      { Acc1 =.. [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) -->
   [].

conjunct(Conjunct) -->
   (  [tokLParen], !, bool_expr(Conjunct), [tokRParen]
   ;  [tokNot], !, conjunct(NotConjunct),
         { Conjunct = not(NotConjunct) }
   ;  [tokTrue], !,
         { Conjunct = true }
   ;  [tokFalse], !,
         { Conjunct = false }
   ;  arith_expr(LExpr), rel_op(Op), arith_expr(RExpr),
         { Conjunct =.. [Op, LExpr, RExpr] }
   ).

additive_op(+) -->
   [tokPlus], !.
additive_op(-) -->
   [tokMinus].

multiplicative_op(*) -->
   [tokTimes], !.
multiplicative_op(//) -->
   [tokDiv], !.
multiplicative_op(mod) -->
   [tokMod].

rel_op(=) -->
   [tokEq], !.
rel_op(<>) -->
   [tokNeq], !.
rel_op(<) -->
   [tokLt], !.
rel_op(<=) -->
   [tokLeq], !.
rel_op(>) -->
   [tokGt], !.
rel_op(>=) -->
   [tokGeq].

parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   %write(TokList),
   phrase(program(Absynt), TokList).
   
/*
sextium processor commands
NOP do nothing
SYSCALL takes code from acc 1 - halt 2 - read (IO -> acc) 3 - write (dr -> IO)
LOAD mem[ar] -> acc
STORE acc -> mem[ar]
SWAPA acc <-> ar
SWAPD acc <-> dr
BRANCHZ if acc==0 then pc=ar
BRANCHN if acc<0 then pc=ar
JUMP acc -> pc
CONST mem[pc++] -> acc
ADD acc += dr
SUB acc -= dr
MUL acc *= dr
DIV (acch = accl 'mod' dr), accl = accl 'div' dr 
SHIFT if dr<0 acc>>-dr else acc<<dr
NAND ~(acc & dr)

opn arithmetic, opn logical, dictv for variables, dictp for procedures
*/

%arithmetic_opn(ArithTree=arith(Left,op,Right), OPN):-arithmetic_opn(Left, OPNLeft), arithmetic_opn(Right, OPNRight), OPN=[OPNLeft,OPNRight,op].
%na podobnym zalozeniu bool_opn

apply(_,[],[]):-!.
apply(Scope,[X|Rest],[(X,Scope)|R1]):-apply(Scope,Rest,R1).

peel([],[]):-!.
peel([arg(X)|Rest],[X|RWyn]):-!,peel(Rest,RWyn).
peel([variable(X)|Rest],[X|RWyn]):-peel(Rest,RWyn).

arg_correct([],[]):-!.
arg_correct([X|Rest],[arg(X)|R2]):-arg_correct(Rest,R2).

var_correct([],[]):-!.
var_correct([X|Rest],[variable(X)|R2]):-var_correct(Rest,R2).

to_dicts(ProgTree,Dictv,Dictp,X):-!,instr_zl_list(ProgTree,ProgList),to_dicts(ProgList,Dictv1,Dictp1,[],X),flatten(Dictv1,Dictv),flatten(Dictp1,Dictp).
to_dicts([],[],[],_,[]):-!.
to_dicts([args(Args)],Dictv,_,Scope,ArgCorrected):-!,instr_zl_list(Args,Dictvt),peel(Dictvt,Dictvt2),apply(Scope,Dictvt2,Dictv),arg_correct(Dictv,ArgCorrected).
to_dicts([local(X)|Rest],Dictv,Dictp,Scope,[local(Corrected)|CRest]):-!,instr_zl_list(X,Dictvt),peel(Dictvt,Dictvt2),apply(Scope,Dictvt2,Dictv1),var_correct(Dictv1,Corrected),to_dicts(Rest,Dictv2,Dictp,Scope,CRest),Dictv=[Dictv1,Dictv2].
to_dicts([procdecl(Name,Args,Blok)|Rest],Dictv,Dictp,Scope,[X|CRest]):-!,to_dicts([args(Args)],Dictv1,_,[Name|Scope],ArgCorr),to_dicts(Blok,Dictv2,Dictp1,[Name|Scope],BlokCorr),X=procdecl(Name,ArgCorr,BlokCorr),to_dicts(Rest,Dictv3,Dictp2,Scope,CRest),Dictv=[Dictv1,Dictv2,Dictv3],Dictp=[Name,Dictp1,Dictp2].
to_dicts((A ';;' B),Dictv,Dictp,Scope,Corrected):-!,instr_zl_list((A ';;' B),List),to_dicts(List,Dictv,Dictp,Scope,Corrected).
to_dicts([A := arith(X)|Rest],Dictv,Dictp,Scope,[(A,Scope) := arith(CX)|CRest]):-!,arith_correct(X,CX,Scope),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([call(Name)|Rest],Dictv,Dictp,Scope,[call((Name,Scope))|CRest]):-!,to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([call(Name,Args)|Rest],Dictv,Dictp,Scope,[call((Name,Scope),CArgs)|CRest]):-!,to_dicts(Args,_,_,Scope,CArgs),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([if(Bool,Then,Else)|Rest],Dictv,Dictp,Scope,[if(CBool,CThen,CElse)|CRest]):-!,to_dicts([Bool],_,_,Scope,[CBool]),to_dicts(Then,_,_,Scope,CThen),to_dicts(Else,_,_,Scope,CElse),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([if(Bool,Then)|Rest],Dictv,Dictp,Scope,[if(CBool,CThen)|CRest]):-!,to_dicts([Bool],_,_,Scope,[CBool]),to_dicts(Then,_,_,Scope,CThen),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([return(Arith)|Rest],Dictv,Dictp,Scope,[return(CArith)|CRest]):-!,to_dicts([Arith],_,_,Scope,[CArith]),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([read(Var)|Rest],Dictv,Dictp,Scope,[read((Var,Scope))|CRest]):-!,to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([while(Bool,Blok)|Rest],Dictv,Dictp,Scope,[while(CBool,CBlok)|CRest]):-!,to_dicts([Bool],_,_,Scope,[CBool]),to_dicts(Blok,_,_,Scope,CBlok),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([write(Arith)|Rest],Dictv,Dictp,Scope,[write(CArith)|CRest]):-!,to_dicts([Arith],_,_,Scope,[CArith]),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([arith(X)|Rest],Dictv,Dictp,Scope,[arith(CX)|CRest]):-!,arith_correct(X,CX,Scope),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([bool(X)|Rest],Dictv,Dictp,Scope,[bool(CX)|CRest]):-!,bool_correct(X,CX,Scope),to_dicts(Rest,Dictv,Dictp,Scope,CRest).
to_dicts([X|Rest],Dictv,Dictp,Scope,[X|CRest]):-to_dicts(Rest,Dictv,Dictp,Scope,CRest).

%if 3 if 2 return read while
%arith correct dla funkcji

arith_opn(arith(X),OPN):-process_arith((X),OPNtf),flatten(OPNtf,OPN).

bool_opn_final(X,OPN):-bool_opn(X,OPNtf),flatten(OPNtf,OPN).

bool_opn(not(X), OPN):-bool_opn(X,OPNBeg),OPN=[OPNBeg,not],!.
bool_opn(bool(X),OPN):-bool_opn(X,OPN),!.
bool_opn(X and Y,OPN):-bool_opn(X,Left),bool_opn(Y,Right),OPN=[Left,Right,and],!.
bool_opn(X or Y,OPN):-bool_opn(X,Left),bool_opn(Y,Right),OPN=[Left,Right,or],!.
bool_opn(X,OPN):-process_bool((X),OPN).%,flatten(OPNtf,OPN).

arith_correct(call(X),call((X,Scope)),Scope):-!.
arith_correct(call(X,Y),Ccall,Scope):-!,to_dicts([call(X,Y)],[],[],Scope,[Ccall]).
arith_correct(variable(X),variable((X,Scope)),Scope):-!.
arith_correct(constant(X),constant(X),_):-!.
arith_correct(X * Y,CX * CY,Scope):-arith_correct(X,CX,Scope),arith_correct(Y,CY,Scope).
arith_correct(X // Y,CX // CY,Scope):-arith_correct(X,CX,Scope),arith_correct(Y,CY,Scope).
arith_correct(X mod Y,CX mod CY,Scope):-arith_correct(X,CX,Scope),arith_correct(Y,CY,Scope).
arith_correct(X + Y,CX + CY,Scope):-arith_correct(X,CX,Scope),arith_correct(Y,CY,Scope).
arith_correct(X - Y,CX - CY,Scope):-arith_correct(X,CX,Scope),arith_correct(Y,CY,Scope).

bool_correct(call(X),call((X,Scope)),Scope):-!.
bool_correct(call(X,Y),Ccall,Scope):-!,to_dicts([call(X,Y)],[],[],Scope,[Ccall]).
bool_correct(variable(X),variable((X,Scope)),Scope):-!.
bool_correct(constant(X),constant(X),_):-!.
bool_correct(arith(X),CX,Scope):-!,arith_correct(X,CX,Scope).
bool_correct(X < Y,CX < CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).
bool_correct(X <= Y,CX <= CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).
bool_correct(X > Y,CX > CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).
bool_correct(X >= Y,CX >= CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).
bool_correct(X = Y,CX = CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).
bool_correct(X <> Y,CX <> CY,Scope):-bool_correct(X,CX,Scope),bool_correct(Y,CY,Scope).

con_var_call(X):- X=variable(_) ; X=constant(_); X=call(_);X=call(_,_).

process_arith(X,X):-con_var_call(X),!.
process_arith((X * Y),OPN):-process_arith(X,OPNLeft),process_arith(Y,OPNRight),OPN=[OPNLeft,OPNRight,* ].
process_arith((X // Y),OPN):-process_arith(X,OPNLeft),process_arith(Y,OPNRight),OPN=[OPNLeft,OPNRight,//].
process_arith((X mod Y),OPN):-process_arith(X,OPNLeft),process_arith(Y,OPNRight),OPN=[OPNLeft,OPNRight,mod].
process_arith((X + Y),OPN):-process_arith(X,OPNLeft),process_arith(Y,OPNRight),OPN=[OPNLeft,OPNRight,+].
process_arith((X - Y),OPN):-process_arith(X,OPNLeft),process_arith(Y,OPNRight),OPN=[OPNLeft,OPNRight,-].

process_bool(X,X):-con_var_call(X),!.
process_bool(arith(X),OPN):-arithmetic_opn(arith(X),OPN),!.
process_bool(X < Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,<].
process_bool(X <= Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,<=].
process_bool(X > Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,>].
process_bool(X >= Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,>=].
process_bool(X = Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,=].
process_bool(X <> Y,OPN):-process_bool(X,OPNLeft),process_bool(Y,OPNRight),OPN=[OPNLeft,OPNRight,<>].

%createdict(Tree):-
%to_prolog_asm(Tree,Dict,ProAsm):-process_deklproc(Deklaracje_Procedur,Dict_Part1,ProAsm_Part1),process_body(Body,Dict_Part2,ProAsm_Part2),Dict=[Dict_Part1,Dict_Part2],ProAsm=[ProAsm_Part1,ProAsm_Part2].
%process_deklproc((Deklaracja ';;' Deklaracje),Dict,ProAsm):-to_prolog_asm(Deklaracja,D1,PA1),process_deklproc(Deklaracje,D2,PA2),ProAsm=[PA1,PA2],Dict=[D1,D2].
%process_body((local(Something) ';;' Rest),Dict,ProAsm):-process_local_var(Something,Dict1),!,Dict=[Dict1,Dict2],process_body(Rest,Dict2,ProAsm).
%process_body((Something ';;' Rest),Dict,ProAsm):-process_commands(Something,Dict,ProAsm)



instr_zl_list(X,List):-instr_zl_list2(X,L1),flatten(L1,List).
instr_zl_list2((X ';;' Y),List):-!,instr_zl_list2(X,Left),instr_zl_list2(Y,Right),List=[Left,Right].
instr_zl_list2(X,X).

opn_to_pasm([],[]):-!.
opn_to_pasm([stos],[]):-!.
opn_to_pasm([constant(X)],[const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(ffff),swapa,store,swapa,const(X),store]):-!.
opn_to_pasm([variable(X)],[const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(ffff),swapa,store,swapd,const(X),swapa,load,swapa,swapd,swapa,store,nop]):-!.
opn_to_pasm([call(X)|Rest],PAsm):-process_to_pasm([call(X)],PAsm1),PAsm=[PAsm1|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([call(X,Y)|Rest],PAsm):-process_to_pasm([call(X,Y)],PAsm1),PAsm=[PAsm1|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([*|Rest],PAsm):-PAsm=[mul,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store,nop|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([//|Rest],PAsm):-PAsm=[div,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store,nop|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([mod|Rest],PAsm):-PAsm=[div,swapd,const(fff0),swapd,shift,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store,nop|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([+|Rest],PAsm):-PAsm=[add,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store,nop|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([-|Rest],PAsm):-PAsm=[sub,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store,nop|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([<|Rest],PAsm):-PAsm=[swapa,const(pc,6),swapa,branchn,swapd,swapa,const(pc,13),swapa,branchn,swapd,const(pc,8),jump,(6),swapa,const(0000),sub,swapa,(8),const(pc,16),swapa,branchn,swapd,const(0000),sub,swapd,swapa,(12),sub,swapa,const(pc,12),swapa,branchn,nop,nop,nop,(15),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(24),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(32)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([<=|Rest],PAsm):-PAsm=[swapa,const(pc,6),swapa,branchn,swapd,swapa,const(pc,13),swapa,branchn,swapd,const(pc,8),jump,(6),swapa,const(0000),sub,swapa,(8),const(pc,16),swapa,branchn,swapd,const(0000),sub,swapd,swapa,(12),sub,swapa,const(pc,12),swapa,branchn,branchz,nop,nop,(15),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(24),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(32)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([>|Rest],PAsm):-PAsm=[swapd,nop,nop,nop,swapa,const(pc,6),swapa,branchn,swapd,swapa,const(pc,13),swapa,branchn,swapd,const(pc,8),jump,(6),swapa,const(0000),sub,swapa,(8),const(pc,16),swapa,branchn,swapd,const(0000),sub,swapd,swapa,(12),sub,swapa,const(pc,12),swapa,branchn,nop,nop,nop,(15),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(24),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(32)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([>=|Rest],PAsm):-PAsm=[swapd,nop,nop,nop,swapa,const(pc,6),swapa,branchn,swapd,swapa,const(pc,13),swapa,branchn,swapd,const(pc,8),jump,(6),swapa,const(0000),sub,swapa,(8),const(pc,16),swapa,branchn,swapd,const(0000),sub,swapd,swapa,(12),sub,swapa,const(pc,12),swapa,branchn,branchz,nop,nop,(15),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(24),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(32)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([=|Rest],PAsm):-PAsm=[sub,swapa,const(pc,12),swapa,branchz,nop,nop,nop,(3),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(12),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(20)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([<>|Rest],PAsm):-opn_to_pasm([=,not|Rest],PAsm).
opn_to_pasm([and|Rest],PAsm):-PAsm=[add,swapd,const(0002),sub,swapa,const(pc,11),swapa,branchz,(4),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(13),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(21)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([or|Rest],PAsm):-PAsm=[add,swapd,const(0000),sub,swapa,const(pc,11),swapa,branchn,(4),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0000),store,const(ffff),swapa,store,const(pc,10),jump,nop,(13),const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(0001),store,const(ffff),swapa,store,nop,nop,nop,(21)|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([not|Rest],PAsm):-PAsm=[const(0001),sub,swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,sub,swapa,store,const(ffff),swapa,store|PAR],opn_to_pasm([stos|Rest],PAR).
opn_to_pasm([stos|Rest],PAsm):-PAsm=[const(ffff),swapa,load,swapd,swapa,const(0001),add,swapa,swapd,const(ffff),swapa,store,swapd,swapa,const(0001),swapd,sub,swapa,swapd,load,swapd,nop,nop,nop|PAR],opn_to_pasm(Rest,PAR).
opn_to_pasm([variable(X)|Rest],PAsm):-PAsm=[const(X),swapa,load,swapd|PAR],opn_to_pasm(Rest,PAR).
opn_to_pasm([constant(X)|Rest],PAsm):-PAsm=[const(X),swapd,nop,nop|PAR],opn_to_pasm(Rest,PAR).

%swapd,const(ffff),swapa,load,swapd,swapa,const(0001),swapd,add,swapd,const(ffff),swapa,swapd,store,swapd,swapa,const(0001),swapd,sub,swapa,swapd,load - poprzedni, niepoprawny stos zamieniajacy argumenty
% dodac wywolywanie wariablesów? - done
% upewnic sie zeby wrzucalo na stos wynik -done
% dodac wywolywanie funkcji w arytmetyce - done
% inlajnowe? - nope
% great, i have written so much sextium code that its too easy for me already
% add to the procdecl return 0 for every function that doesnt return

process_to_pasm([],[]):-!.
process_to_pasm([local(X)|Rest],ProAsm):-!,local_to_pasm(X,PAsm1),process_to_pasm(Rest,PAsm2),ProAsm=[PAsm1|PAsm2].
process_to_pasm([procdecl(Name,Locale,Body)|Rest],ProAsm):-!,load_all_args(Locale,PAsm1),process_to_pasm(Body,PAsm2),ProAsm=[procbegin(Name),PAsm1,PAsm2,procend(Name),const(ffff),swapa,load,swapd,const(0001),add,store,swapd,const(0001),swapd,sub,swapa,load,swapa,swapd,const(0001),add,swapa,swapd,load,swapd,load,swapd,store,swapd,jump|PAR],process_to_pasm(Rest,PAR).
process_to_pasm([while(Bool,Body)|Rest],ProAsm):-!,bool_opn(Bool,OPNBool),opn_to_pasm(OPNBool,PAsm1),process_to_pasm(Body,PAsm2),ProAsm=[start(Body),PAsm1,const(ffff),swapa,load,swapa,load,swapa,swapd,const(0001),swapd,add,swapa,swapd,const(ffff),swapa,store,swapd,swapa,const(end(Body)),swapa,branchz,PAsm2,const(start(Body)),jmp,nop,nop,end(Body)|PAR],process_to_pasm(Rest,PAR).
process_to_pasm([if(Bool,Body,Else)|Rest],ProAsm):-!,bool_opn(Bool,OPNBool),opn_to_pasm(OPNBool,PAsm1),process_to_pasm(Body,PAsm2),process_to_pasm(Else,PAsm3),ProAsm=[PAsm1,const(ffff),swapa,load,swapa,load,swapa,swapd,const(0001),swapd,add,swapa,swapd,const(ffff),swapa,store,swapd,swapa,const(end(Body)),swapa,branchz,PAsm2,end(Body),const(end(Else)),jump,nop,nop,PAsm3,end(Else)|PAR],process_to_pasm(Rest,PAR).
process_to_pasm([if(Bool,Body)|Rest],ProAsm):-!,bool_opn(Bool,OPNBool),opn_to_pasm(OPNBool,PAsm1),process_to_pasm(Body,PAsm2),ProAsm=[PAsm1,const(ffff),swapa,load,swapa,load,swapa,swapd,const(0001),swapd,add,swapa,swapd,const(ffff),swapa,store,swapd,swapa,const(end(Body)),swapa,branchz,PAsm2,end(Body)|PAR],process_to_pasm(Rest,PAR).
process_to_pasm([call(Name)|Rest],ProAsm):-!,process_to_pasm(Rest,PAR),ProAsm=[const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(ffff),swapa,store,swapa,const(pc,1),store,const(procbeg(Name)),jump|PAR].
process_to_pasm([call(Name,Args)|Rest],ProAsm):-!,reverse(Args,RevA),save_args(RevA,PAsm1),process_to_pasm(Rest,PAR),ProAsm=[const(ffff),swapa,load,swapd,const(0001),swapd,sub,swapa,const(ffff),swapa,store,swapa,const(pc,1),store,nop,nop,PAsm1,const(procbeg(Name)),jump,nop,nop|PAR].
process_to_pasm([return(Arith)|Rest],ProAsm):-!,arith_opn(Arith,OPNArith),ProAsm=[OPNArith,const(procend),jump,nop,nop|Rest].
process_to_pasm([read(Var)|Rest],ProAsm):-!,process_to_pasm(Rest,PAR),ProAsm=[const(0002),syscall,swapa,const(Var),swapa,store,nop,nop|PAR].
process_to_pasm([write(Arith)|Rest],ProAsm):-!,arith_opn(Arith,OPNArith),process_to_pasm(Rest,PAR),ProAsm=[OPNArith,const(ffff),swapa,load,swapd,const(0001),add,store,swapd,const(0001),swapd,sub,swapa,load,swapd,const(0003),syscall|PAR].
process_to_pasm([Id := Arith|Rest],ProAsm):-!,arith_opn(Arith,OPNArith),process_to_pasm(Rest,PAR),ProAsm=[OPNArith,const(ffff),swapa,load,swapd,const(0001),add,store,swapd,const(0001),swapd,sub,swapa,load,swapa,const(Id),swapa,store,nop,nop,nop|PAR].

local_to_pasm([],[]):-!.
local_to_pasm([variable(X)|Rest],[const(X),swapa,const(0000),store|PAR]):-local_to_pasm(Rest,PAR).

load_all_args([],[]):-!.
load_all_args([arg(Arg)|Rest],[const(ffff),swapa,load,swapd,const(0001),add,store,swapd,const(0001),swapd,sub,swapa,load,swapa,const(Arg),swapa,store,nop,nop,nop|PAR]):-load_all_args(Rest,PAR).

save_args([],[]):-!.
save_args([arith(Arith)|Rest],[PAsm1|PAR]):-arith_opn(arith(Arith),OPNArith),opn_to_pasm(OPNArith,PAsm1),save_args(Rest,PAR).

%local_pasm((X ';;' Y),[Xs,Ys]):-!,local_pasm(X,Xs),local_pasm(Y,Ys).
%local_pasm(X,[X]).


%counter_implementer(List,PC,Dictv,Dictp,Repaired)-obsolete
%counter_implementer(List,PC,Acc,Dictofendsbegins) - no changes only removes notim portant stuff


is_command(X):- X=nop;X=syscall;X=load;X=store;X=swapa;X=swapd;X=branchz;X=branchn;X=jump;X=const(_);X=const(_,_);X=add;X=sub;X=mul;X=div;X=shift;X=nand.
is_const(X):-X=const(_);X=const(_,_).

is_label(start(X),start(X)).
is_label(end(X),end(X)).
is_label(procbegin(Name),Name).
is_label(procend(_),procend).
% counter_implementer(Pasm,0,0,DDict,Stripped)
counter_implementer([],PC,_,[end(PC)],[]):-!.
counter_implementer(List,PC1,4,DirectionsDict,Stripped):-!,PC is PC1+1,counter_implementer(List,PC,0,DirectionsDict,Stripped).
counter_implementer([X|Rest],PC1,Acc1,DirDict,[X|StRest]):-is_const(X),!,Acc is Acc1+1,PC is PC1+1,counter_implementer(Rest,PC,Acc,DirDict,StRest).
counter_implementer([X|Rest],PC,Acc1,DirDict,[X|StRest]):-is_command(X),!,Acc is Acc1+1,counter_implementer(Rest,PC,Acc,DirDict,StRest).
counter_implementer([X|Rest],PC,Acc,[(Label,PC)|DirDict],StRest):-is_label(X,Label),!,counter_implementer(Rest,PC,Acc,DirDict,StRest).
counter_implementer([_|Rest],PC,Acc,DirDict,StRest):-counter_implementer(Rest,PC,Acc,DirDict,StRest).

%const_filler(List,PC,Acc,Dictv,DirDict,PCEnd,Corrected)

%dict_to_numbered_dict(Dictv,Acc,NumDictv) Acc=0 or 1 na start

dict_to_numbered_dict([],_,[]):-!.
dict_to_numbered_dict([Var|Rest],Acc,[(Var,Acc)|NumRest]):-Acc1 is Acc+1,dict_to_numbered_dict(Rest,Acc1,NumRest).

const_filler([],_,_,_,_,_,[]):-!.
const_filler(List,PC1,4,Dictv,DirDict,PCEnd,Corrected):-!,PC is PC1+1,const_filler(List,PC,0,Dictv,DirDict,PCEnd,Corrected).
const_filler([const(_,Num)|Rest],PC1,Acc1,Dictv,DirDict,PCEnd,[const(PC2)|CRest]):-!,PC2 is PC1+Num,PC is PC1+1,Acc is Acc1+1,const_filler(Rest,PC,Acc,Dictv,DirDict,PCEnd,CRest).
const_filler([const(procend)|Rest],PC1,Acc1,Dictv,DirDict,PCEnd,[const(PC2)|CRest]):-!,member((procend(_),Num),DirDict),Num>PC1,!,PC2=Num,PC is PC1+1,Acc is Acc1+1,const_filler(Rest,PC,Acc,Dictv,DirDict,PCEnd,CRest).
const_filler([const(X)|Rest],PC1,Acc1,Dictv,DirDict,PCEnd,[const(PC2)|CRest]):-!,{member((X,Num),Dictv),PC2=Num;member((X,Num),DirDict),PC2=Num},PC is PC1+1,Acc is Acc1+1,const_filler(Rest,PC,Acc,Dictv,DirDict,PCEnd,CRest).
const_filler([X|Rest],PC,Acc1,Dictv,DirDict,PCEnd,[X|CRest]):-Acc is Acc1+1,const_filler(Rest,PC,Acc,Dictv,DirDict,PCEnd,CRest).


parse_and_tree(CharList,PAsm):-parse(CharList,X),tree_to_pasm(X,PAsm).
tree_to_pasm(Tree,PAsm):-to_dicts(Tree,Dictv,Dictp,X),process_to_pasm(X,PAsm1),flatten(PAsm1,PAsm).
tree_to_counter(Tree,Stripped):-to_dicts(Tree,Dictv,Dictp,AfterDicts),process_to_pasm(AfterDicts,PAsm),counter_implementer(PAsm,0,0,DirDict,Stripped).
parse_to_counter(CharList,Stripped):-parse(CharList,Absynt),tree_to_counter(Absynt,Stripped).

%what in the name of Lord is this
%last_iteration(List,Dictv,Dictp,Repaired)
%to_bytecode(List,Bytecode)

%
/*NOP do nothing
SYSCALL takes code from acc 1 - halt 2 - read (IO -> acc) 3 - write (dr -> IO)
LOAD mem[ar] -> acc
STORE acc -> mem[ar]
SWAPA acc <-> ar
SWAPD acc <-> dr
BRANCHZ if acc==0 then pc=ar
BRANCHN if acc<0 then pc=ar
JUMP acc -> pc
CONST mem[pc++] -> acc
ADD acc += dr
SUB acc -= dr
MUL acc *= dr
DIV (acch = accl 'mod' dr), accl = accl 'div' dr 
SHIFT if dr<0 acc>>-dr else acc<<dr
NAND ~(acc & dr)
*/
