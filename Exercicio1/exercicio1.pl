%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% consult('/Users/rafaelarodrigues/Desktop/Trabalhos/SRCR/Exercicio1/exe1.pl').
% AQUI SÓ REPRESENTAMOS AS SOLUÇÕES QUE EXISTEM
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

utente(1, 'André', 20, 'Espanha').
utente(2, 'João', 20, 'Braga').
utente(3, 'Luís', 21, 'Braga').
utente(4, 'Rafaela', 20, 'Braga').

sexo('M',1).
sexo('M',2).
sexo('M',3).
sexo('F',4).

servico(1, 'Ortodontia', 'Hospital', 'Braga').
servico(2, 'Medicina Geral', 'Clínica', 'Braga').
servico(3, 'Oftalmologia', 'Privado', 'Barcelos').
servico(4, 'Ortopedia', 'Hospital', 'Lisboa').

consulta('19/3/19', 1, 3, 25).
consulta('23/4/19', 2, 1, 3).
consulta('3/3/19', 3, 3, 33).
consulta('7/6/19', 3, 2, 15).
consulta('7/6/19', 1, 2, 15).

%Questão 1

  % Extensao do predicado registarU: IdUtente, NomeUtente, Idade, Cidade -> {V,F}
  % Só pode registar se não existir utente com o mesmo ID
  registarU(ID, N, I, C) :- nao(utente(ID,_,_,_)),
    assert(utente(ID,N,I,C)).
  % Extensao do predicado registarS: IdServico, Descricao, Instituicao, Cidade -> {V,F}
  % Só pode registar serviço se não existir serviço com o mesmo ID
  registarS(ID,D,I,C) :- nao(servico(A,_,_,_)),
    assert(servico(ID,D,I,C)).
  % Extensao do predicado registarC: Data, IdUtente, IdServico, Custo -> {V,F}
  % Só pode registar consulta se não existir para o mesmo serviço no mesmo Dia
  % Se o Custo for maior que 0, se existir o utente e se existir o serviço
  registarC(D,IdU,IdS,C) :-
    nao(consulta(D,IdU,IdS,_)),
    C > 0,
    utente(IdU,_,_,_),
    servico(IdS,_,_,_),
    assert(consulta(D,IdU,IdS,C)).

%Questão 2

  % Extensao do predicado apagarU: IdUtente -> {V,F}
  % Só pode remover utente se existir o utente e não existirem consultas
  % Com esse utente.
  apagarU(ID) :- utente(ID,_,_,_), nao(consulta(_,ID,_,_)), retract(utente(ID,_,_,_)).
  % Extensao do predicado apagarS: IdServico -> {V,F}
  % Só pode remover serviço se existir o serviço e não existirem consultas
  % Desse serviço
  apagarS(ID) :- servico(ID,_,_,_), nao(consulta(_,_,ID,_)), retract(servico(IdServico,_,_,_)).
  % Extensao do predicado apagarC: Data -> IdUtente -> IdServiço -> {V,F}
  % Só pode apagar uma consulta se existir essa consulta
  apagarC(D,ID,IDS) :- consulta(D,ID,IDS,_), retract(consulta(D,ID,IDS,_)).


%Questão 3
  %Extensao do predicado TodasInst: Lista -> {V,F}
  todasInst(S) :- solucoes(Inst,servico(_,_,Inst,_),X),
                  semRepetidos(X,S).


%Questão 4
  % Extensao do predicado utentePorSexo: Sexo do Utente, Lista de Utentes  -> {V,F}
  utentePorSexo(S,L) :- solucoes(Id,sexo(S,Id),L).
  % Extensao do predicado utentePorCidade: Cidade do Utente, Lista de Utentes  -> {V,F}
  utentePorCidade(C,L) :- solucoes(Nome,utente(_,Nome,_,C),L).
  % Extensao do predicado utentePorIdade: Idade do Utente, Lista de Utentes  -> {V,F}
  utentePorIdade(I,L) :- solucoes(Nome,utente(_,Nome,I,_),L).

  % Extensao do predicado servicosEmCidade: Cidade, Lista de Servicos  -> {V,F}
  servicosEmCidade(C,L):- solucoes(S, servico(_,S,_,C),L).
  % Extensao do predicado todosServicos: Lista de Servicos  -> {V,F}
  todosServicos(L):- solucoes(S, servico(_,S,_,_),L).
  % Extensao do predicado servicoInstituicaoLocal: Instituicao, Lista de Servicos  -> {V,F}
  servicoInstituicaoLocal(I,L):- solucoes((C,S), servico(_,S,I,C),L).

  % Extensao do predicado consultasPorDia: Dia, Lista de Consultas  -> {V,F}
  consultasPorDia(D,L) :- solucoes(Id , consulta(D,Id,_,_) , X),
                         getId(X,L).
  % Extensao do predicado consultasServiço: Data, Serviço, NumConsultas  -> {V,F}
  consultasServico(D,S,N) :-  servico(IdS,S,_,_),
                              solucoes(Id , consulta(D,Id,IdS,_), L),
                              countElements(L,N).
  % Extensao do predicado ganhoPorDia: Data, Valor  -> {V,F}
  ganhoPorDia(D,T) :-  solucoes(Valor , consulta(D,_,_,Valor), L),
                       sumElements(L,T).
%Questão 5

%Questão 6

%Questão 7

%Questão 8






% Extensão do predicado pertence: Elemento, Lista -> {V,F}
pertence(X, [X|T]).
pertence(X, [H|T]) :-  X \= H, pertence(X, T).


% Extensão do predicado não: T -> {V,F}
nao(T) :- T, !,fail.
nao(T).

% Extensão do predicado solucoes: Formato, Prova, LSolucoes -> {V,F}
%solucoes(F,P,L) :- findall(F,P,L).
solucoes(F,P,S) :- P , assert(tmp(F)), fail.
solucoes(F,P,S) :- construir(S,[]).

% Extensão do predicado construir: Solucao, Lista-> {V,F}
construir(S,L) :- retract(tmp(X)), ! ,construir(S,[X|L]).
construir(S,S).

% Extensão do predicado semRepetidos:: ListaComRepetidos, ListaSemRepetidos -> {V,F}
semRepetidos([],[]).
semRepetidos([H|T],S) :-  pertence(H,T) , semRepetidos(T,S).
semRepetidos([H|T],[H|S]) :- nao(pertence(H,T)) , semRepetidos(T,S).

% Extensão do predicado getId:: ListaId, ListaNomes -> {V,F}
getId([],[]).
getId([H1|C1],[N|C]) :- utente(H1,N,_,_), getId(C1,C).

% Extensão do predicado countElements:: ListaId, NumConsultas -> {V,F}
countElements([],0).
countElements([H|B],N) :- countElements(B,M), N is M+1.

% Extensão do predicado sumElements:: ListaValores, Total -> {V,F}
sumElements([],0).
sumElements([H|B],T) :- sumElements(B,M), T is M+H.
