% consult('~/Documents/SRCR/SRCR/Exercicio2/exercicio2.pl').
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento Perfeito Positivo
% Extensão do predicado utente: Id Utente, Nome, Idade, Morada -> {V,F}
utente( 0 , 'Luis Miguel' , 21 , 'Rua das Palmeiras' ).
utente( 1 , 'Rafaela Rodrigues' , 20 , 'Rua das Nespereiras').
utente( 2 , 'Andre Goncalves' , 20 , 'Rua dos Pessegueiros').
utente( 3 , 'Joao Queiros' , 20 , 'Rua das Laranjeiras').
utente( 4 , 'Joao Miguel' , 35, 'Rua das Pereiras' ).

% Invariante Estrutural
% Não permitir a inserção de conhecimento positivo repetido sobre o mesmo ID
+utente(Id,_,_,_) :: (
    solucoes(Ids, utente(Id,_,_,_),L),
    comprimento(L,N),
    N == 1
).

% Invariante Estrutural
% Não permitir a inserção de utentes com idades inferiores a 0 e superiores a 130
+utente(_,_,Idade,_) :: (
    Idade >= 0,
    Idade =< 130
).

% Extensão do predicado prestador: Id Prestador, Nome, Especialidade, Instituição -> {V,F}
prestador( 0 , 'Joaquim da Graça' , 'Urologia' , 'Hospital de Braga').
prestador( 1 , 'Marafa da Derme' , 'Dermatologia' , 'Hospital de Braga').
prestador( 2 , 'Manel da Costa' , 'Pediatria' , 'Hospital de Guimaraes').
prestador( 3 , 'Serafim Peixoto' , 'Gastrenterologia' , 'Hospital da Luz').
prestador( 4 , 'Miguel Dourado' , 'Pediatria' , 'Hospital da Luz').
prestador( 5 , 'Estevao Esteves' , 'Cardiologia' , 'Hospital de Faro').
prestador( 6 , 'Sousa Costa' , 'Gastrenterologia' , 'Hospital de Faro').
prestador( 6 , 'Sousa Costa' , 'Otorrinolaringologia' , 'Hospital de Faro').
prestador( 7 , 'Dolores da Cunha' , 'Cardiologia' , 'Hospital de Faro').
prestador( 8 , 'Manuel Oliveira' , 'Cardiologia' , 'Hospital de Faro').
prestador( 9 , 'Diogo Teixeira' , 'Cardiologia' , 'Hospital de Faro').


% Invariante Estrutural
% Não permitir a inserção de conhecimento positivo repetido sobre o mesmo ID
+prestador(Id,_,_,_) :: (
    solucoes(Id, prestador(Id,_,_,_), L),
    comprimento(L,N),
    N == 1
).

% Invariante Estrutural
% Uma Instituição apenas pode ter 5 prestadores, no máximo, de cada Especialidade
% São tidos em consideração casos de especialidades/instituicoes imprecisa e incerta, que poderão
% ser da especialidade que se deseja adicionar, causando potenciais contradições
% na base de conhecimento.
+prestador(_,_,Especialidade,Instituicao) :: (
    solucoes(Id, prestador(Id,_,Especialidade,Instituicao), L),
    comprimento(L,N),
    N =< 5,
    nao(excecao(prestador(Id,_,Especialidade,Instituicao), nulo(especialidade_imprecisa))),
    nao(excecao(prestador(Id,_,_,Instituicao), nulo(especialidade_incerta))),
    nao(excecao(prestador(Id,_,Especialidade,Instituicao), nulo(instituicao_imprecisa))),
    nao(excecao(prestador(Id,_,Especialidade,_), nulo(instituicao_incerta)))
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento Perfeito Negativo

% É mentira que os utentes que fazem parte da base de conhecimento
% tenham parâmetros diferentes do que os que estão guardados
-utente( Id , Nome , Idade , Morada) :- utente(Id,_,_,_) , nao(utente( Id , Nome , Idade , Morada)).

% É mentira que os prestadores que fazem parte da base de conhecimento tenham
% um nome diferente do que o que lhes foi associado
-prestador( Id , Nome , _ , _) :- prestador(Id,_,_,_) , nao(prestador(Id,Nome,_,_)).

% O Joaquim da Graça não presta Urologia no Hospital de Guimarães
-prestador( 0 , 'Joaquim da Graça' , 'Urologia' , 'Hospital de Guimaraes').

% O Miguel Dourado não presta Gastrenterologia em lado algum
-prestador( 4 , 'Miguel Dourado' , 'Gastrenterologia', Instituicao).

% O Sousa Costa apenas presta cuidados de saúde no Hospital de Faro
-prestador( 6 , 'Sousa Costa' , _ , Instituicao) :-
    nao(Instituicao == 'Hospital de Faro').

% O Marafa da Derme apenas presta Dermatologia no Hospital de Braga
-prestador( 1 , 'Marafa da Derme' , _ , Instituicao) :-
    nao(Instituicao == 'Hospital de Braga').
-prestador( 1 , 'Marafa da Derme' , Especialidade , _ ) :-
    nao(Especialidade == 'Dermatologia').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento Imperfeito

% Tipo 1 - Conhecimento Incerto
% Não se sabe a morada do utente Rui Pedro, apesar de se saber a sua idade.
utente( 5 , 'Rui Pedro', 32 , nulo(incerto) ).
excecao(utente(Id,Nome,Idade,Morada),nulo(incerto)) :- utente(Id,Nome,Idade,nulo(incerto)).

% Tipo 3 - Conhecimento Interdito
% Não é possível adicionar prestadores de Homeopatia, Acupuntura e Aromaterapia
interdito('Homeopatia').
interdito('Acupuntura').
interdito('Aromaterapia').

% Invariante estrutural
% Não é possível adicionar prestadores de Especialidades interditas.
+prestador(Id,Nome,Especialidade,Instituicao) :: (nao(interdito(Especialidade))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extens�o do predicado que permite a involucao do conhecimento
involucao( T ) :- T,
                solucoes(I, -T::I, LInv),
                remocao(T),
                teste(LInv).

remocao(T) :- retract(T).
remocao(T) :- assert(T), !, fail.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado si: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,incerto,impreciso, desconhecido }

si( Questao,verdadeiro ) :-
    Questao.
si( Questao,falso ) :-
    -Questao.
si( Questao,incerto ) :-
    nao(Questao),
    nao(-Questao),
    excecao(Questao,nulo(incerto)).
si( Questao,impreciso ) :-
    nao(Questao),
    nao(-Questao),
    excecao(Questao,nulo(impreciso)).
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ),
    nao(excecao(Questao,nulo(incerto))),
    nao(excecao(Questao,nulo(impreciso))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

% Extensão do predicado comprimento, que dada uma lista, devolve o seu
% comprimento. comprimento: Lista, Tamanho -> {V,F}
comprimento( [],0 ).
comprimento( [H|T],N ) :- comprimento(T,M), N is M+1.
