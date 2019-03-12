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

utente(1, andre, 20, espanha).
utente(2, joao, 20, braga).
utente(3, luis, 21, braga).
utente(4, rafaela, 20, braga).

sexo('M',1).
sexo('M',2).
sexo('M',3).
sexo('F',4).

servico(1, dentista, 'hospital', braga).
servico(2, familia, 'clinica', braga).
servico(3, cirurgia, 'privado', braga).
servico(4, ortopedia, 'hospital', braga).

consulta(19/3/19, 1, 3, 25).
consulta(23/4/19, 2, 1, 3).
consulta(3/3/19, 3, 3, 33).
consulta(7/6/19, 4, 2, 15).

%Questão 1

  %Extensao do predicado registarU: IdUtente, NomeUtente, Idade, Cidade -> {V,F}
  registarU(ID, N, I, C) :- \+ utente(ID,_,_,_) , assert(utente(ID,N,I,C)).
  %Extensao do predicado registarS: IdServico, Descricao, Instituicao, Cidade -> {V,F}
  registarS(ID,D,I,C) :- \+ servico(A,_,_,_) , assert(servico(ID,D,I,C)).
  %Extensao do predicado registarC: Data, IdUtente, IdServico, Custo -> {V,F}
  registarC(D,IdU,IdS,C) :- \+ utente(D,_,_,_), \+ servico(D,_,_,_), assert(consulta(D,IdU,IdS,C)).

%Questão 2

  %Extensao do predicado apagarU: IdUtente -> {V,F}
  apagarU(ID) :- utente(ID,_,_,_), retract(utente(ID,_,_,_)).
  %Extensao do predicado apagarS: IdServico -> {V,F}
  apagarS(ID) :- servico(IdServico,_,_,_), retract(servico(IdServico,_,_,_)).
  %Extensao do predicado apagarC: Data -> IdUtente -> {V,F}
  apagarC(D,ID) :- consulta(D,ID,_,_), retract(consulta(D,ID,_,_)).


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

solucoes(F,P,L) :- findall(F,P,L).
%solucoes(F,P,S) :- P, assert(tmp(F)),fail.
%solucoes(F,P,S) :- construir([],S).

% Extensão do predicado construir: LInicial, Solucao-> {V,F}
construir(LI,S) :- retract(tmp(X)), ! ,construir([X|S],S).
construir(S,S).

% Extensão do predicado semRepetidos:: ListaComRepetidos, ListaSemRepetidos -> {V,F}
semRepetidos([],[]).
semRepetidos([H|T],S) :-  pertence(H,T) , semRepetidos(T,S).
semRepetidos([H|T],[H|S]) :- nao(pertence(H,T)) , semRepetidos(T,S).
