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
+utente(Id,Nome,Idade,Morada) :: (
    solucoes(Id, utente(Id,Nome,Idade,Morada),L),
    comprimento(L,N),
    N == 1
).

% Invariante Referencial
% Não permitir a remoção de utentes que tenham cuidados associados
-utente(Id,_,_,_) :: nao(cuidado(_,Id,_,_,_)).

% Extensão do predicado prestador: Id Prestador, Nome, Especialidade, Instituição -> {V,F}
prestador( 0 , 'Joaquim da Graca' , 'Urologia' , 'Hospital de Braga').
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
+prestador(Id,Nome,Especialidade,Instituicao) :: (
    solucoes(Id, prestador(Id,Nome,Especialidade,Instituicao), L),
    comprimento(L,N),
    N == 1
).

% Extensão do predicado cuidado: Data, Id Utente, Id Prestador, Descrição, Custo -> {V,F}
cuidado(data(5,10,2018) , 0, 1 , 'cuidado ao Coracao' , 20).
cuidado(data(19,3,2019), 1, 2,'cuidado pediatrico' , 25).
cuidado(data(23,4,2019), 2, 1,'cuidado demartologico' , 3).
cuidado(data(3,3,2019), 1, 3,'cuidado ao Coracao' , 33).
cuidado(data(7,7,2019), 3, 2,'cuidado pediatrico' , 15).
cuidado(data(7,7,2019), 1, 2,'cuidado pediatrico' , 15).
cuidado(data(8,3,2019),5,7,'cuidado ao Coracao' ,30).
cuidado(data(25,4,2019),4,8,'cuidado ao Coracao' ,8).
cuidado(data(3,3,2019),4,5,'cuidado ao Coracao' ,40).
cuidado(data(4,9,2019),1,7,'cuidado ao Coracao' ,50).
cuidado(data(24,12,2019),0,5,'cuidado ao Coracao' ,20).
cuidado(data(1,1,2020),1,1,'cuidado demartologico' ,70).
cuidado(data(4,5,2019),4,1,'cuidado demartologico' ,25).
cuidado(data(8,8,2020),1,4,'cuidado ao Coracao' ,35).
cuidado(data(1,1,2020),1,6,'cuidado ao Coracao' ,60).
cuidado(data(18,6,2010),3,6,'cuidado ao Coracao' ,30).

% Invariante estrutural
% Não permitir a inserção de conhecimento repetido de cuidados
+cuidado(Data,IdU,IdS,Descricao,Custo) :: (solucoes(Ls, cuidado(Data,IdU,IdS,Descricao,Custo),S),
                                  comprimento(S,N),
                                  N == 1).

% Invariante referencial
% Não permitir a inserção de novos cuidados se não existir o utente com
% esse Id
+cuidado(_,IdU,_,_,_) :: utente(IdU,_,_,_).

% Invariante referencial
% Não permitir a inserção de novos cuidados se não existir o prestador
% com esse Id
+cuidado(_,_,IdS,_,_) :: prestador(IdS,_,_,_).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento Perfeito Negativo

% É mentira que os utentes que fazem parte da base de conhecimento
% tenham parâmetros diferentes do que os que estão guardados
-utente( Id , Nome , Idade , Morada) :- utente(Id,_,_,_) ,
                                      nao(utente(Id,Nome,Idade,Morada)),
                                      nao(excecao(utente(Id,Nome,Idade,Morada))).


% Um utente registado tem uma idade incerta mas sabemos que não são 70 anos
utente( 5,'Joaquina',nulo(incerto),'Av.da Liberdade, Lisboa').
-utente( 5,'Joaquina',70,'Av.da Liberdade, Lisboa').

% É mentira que os cuidados que fazem parte da base de conhecimento
% tenham parâmetros diferentes do que os que estão guardados
-cuidado( Data, IdUtente, IdPrestador,Descricao, Custo) :- cuidado(_,IdUtente,IdPrestador,_,_) ,
                                      nao(cuidado( Data, IdUtente, IdPrestador,Descricao, Custo)),
                                      nao(excecao(cuidado(Data,IdUtente,IdPrestador,Descricao,Custo))).

% Sabe-se que o custo do cuidado não foi de 40
-cuidado(data(19,3,2019), 1, 3,'Consulta breve', 40).

% É mentira que os prestadores que fazem parte da base de conhecimento tenham
% um nome diferente do que o que lhes foi associado
-prestador( Id , Nome , _ , _) :- prestador(Id,_,_,_) ,
                                nao(prestador(Id,Nome,_,_)),
                                nao(excecao(prestador(Id,Nome,_,_))).

% O Joaquim da Graça não presta Urologia no Hospital de Guimarães
-prestador( 0 , 'Joaquim da Graca' , 'Urologia' , 'Hospital de Guimaraes').

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

% Não se sabe o nome do utente
utente( 6 , nulo(incerto), 30 , 'Rua dos Pomares' ).
excecao(utente(Id,Nome,Idade,Morada)) :- utente(Id,nulo(incerto),Idade,Morada).

% Não se sabe a morada do utente Rui Pedro, apesar de se saber a sua idade.
utente( 7 , 'Rui Pedro', 32 , nulo(incerto) ).
excecao(utente(Id,Nome,Idade,Morada)) :- utente(Id,Nome,Idade,nulo(incerto)).

% Não se sabe a idade do utente
utente( 8 , 'Jorge', nulo(incerto) , 'Rua Pinheiral' ).
excecao(utente(Id,Nome,Idade,Morada)) :- utente(Id,Nome,nulo(incerto),Morada).

% Não se sabe qual a especialidade que o Hugo Anibal presta, uma vez que
% não foi preenchido o formulário 37423_B aquando da sua entrada a prestador
% no hospital
prestador( 10 , 'Hugo Anibal' , nulo(incerto) , 'Hospital da Luz').
excecao(prestador(Id,Nome,Especialidade,Instituicao)) :- prestador(Id,Nome,nulo(incerto), Instituicao).

% Não se sabe qual o hospital em que o Andre presta cuidado
prestador( 11 , 'Andre' , 'Genecologia' , nulo(incerto)).
excecao(prestador(Id,Nome,Especialidade,Instituicao)) :- prestador(Id,Nome,nulo(incerto), Instituicao).

% Não se sabe qual a data em que o cuidado foi prestado
cuidado(nulo(incerto),4,6,'Consulta breve',30).
excecao(cuidado( Data, IdUtente, IdPrestador,Descricao, Custo)) :- cuidado(nulo(incerto), IdUtente, IdPrestador,Descricao, Custo).

% Não se sabe qual o custo do cuidado que foi prestado
cuidado((data(7,9,2019),4,6,'Consulta breve',nulo(incerto))).
excecao(cuidado( Data, IdUtente, IdPrestador,Descricao, Custo)):- cuidado(Data, IdUtente, IdPrestador,Descricao, nulo(incerto)).

% Tipo 2 - Conhecimento Impreciso
% Um cuidado pode ter custado 40 ou 60 euros
excecao(cuidado(data(10,7,2019), 3, 2,'Consulta breve', 40)).
excecao(cuidado(data(10,7,2019), 3, 2,'Consulta breve', 60)).


% um cuidado pode ter sido prestado pelo Dr.João ou pelo Dr.Alberto
excecao(prestador( 10 , 'João Teixeira' , 'Cardiologia' , 'Hospital de Faro')).
excecao(prestador( 10 , 'Alberto Ricardo' , 'Cardiologia' , 'Hospital de Faro')).

% um medico pode ter prestado cuidados em dois hospitais distintos
excecao(prestador( 13,'Pediatria','Hospital Privado do Algarve','Faro')).
excecao(prestador( 13,'Pediatria','Hospital de Faro','Faro')).

% um utente com o mesmo ID pode estar registado com diferentes nomes do seu nome completo
excecao(utente( 15 , 'Joao Miguel' , 35, 'Rua das Amoreiras' )).
excecao(utente( 15 , 'Joao Miguel Abreu' , 35, 'Rua das Amoreiras' )).

% O utente com o id nº20 tem uma idade compreendida entre 18 e 23
excecao(utente( 20,'Alexandra',C,'Avenida 25 de Abril','Santarém','935694789')) :- C >= 18 , C =< 25.
excecao(utente( 16 , 'Miguel Joao' , I, 'Rua Casal Garcia ' )) :- I >= 18 , I =< 23.



% Tipo 3 - Conhecimento Interdito
% Não é possível adicionar prestadores de Homeopatia, Acupuntura e Aromaterapia
interdito('Homeopatia').
interdito('Acupuntura').
interdito('Aromaterapia').

% Invariante estrutural
% Não é possível adicionar prestadores de Especialidades interditas.
+prestador(Id,Nome,Especialidade,Instituicao) :: (nao(interdito(Especialidade))).

% cuidado do dia 17-04-2019 tem um utente que ninguém pode conhecer
interdito('3').
cuidado(data(17,04,2019), interdito, 2, 60).


% Invariante estrutural
% Não é possível adicionar ID utente de determinado cuidado.
+cuidado( Data, IdUtente, IdPrestador, Custo) :: (nao(interdito(IdUtente))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução do conhecimento
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
si( Questao,impreciso ) :-
    nao(Questao),
    nao(-Questao),
    solucoes(Excecoes, excecao(Questao), L),
    comprimento(L,N),
    N > 1.
si( Questao,incerto ) :-
    nao(Questao),
    nao(-Questao),
    solucoes(Excecoes, excecao(Questao), L),
    comprimento(L,N),
    N == 1.
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

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

% Extensão do predicado pertence: Elemento, Lista -> {V,F}
pertence(X, [X|T]).
pertence(X, [H|T]) :-  nao(X == H), pertence(X, T).

% Extensão do predicado dataValida: Data -> {V,F}
data(Dia,Mes,Ano) :- anoValido(Ano), mesValido(Mes), diaValido(Dia,Mes).

% Extensão do predicado natural: Num -> {V,F}
natural(1).
natural(N) :- N > 1, M is N-1, natural(M).

% Extensão do predicado anoValido: Ano -> {V,F}
anoValido(Ano) :- natural(Ano), Ano > 1890 , Ano < 2020.

% Extensão do predicado mesValido: Mes -> {V,F}
mesValido(Mes) :- natural(Mes), Mes < 13.

% Extensão do predicado diaValido: Dia, Mes, Ano -> {V,F}
diaValido(Dia,Mes) :- pertence(Mes,[1,3,5,7,8,10,12]),
                       natural(Dia),
                       Dia < 32.
diaValido(Dia,Mes) :- pertence(Mes,[4,6,9,11]),
                         natural(Dia),
                         Dia < 31.
% Ninguém gosta de bissextos
diaValido(Dia,2) :- natural(Dia), Dia < 29.
