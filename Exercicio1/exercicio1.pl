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
:- dynamic sexo/2.

utente(1, 'Andre', 20, 'Espanha').
utente(2, 'Joao', 20, 'Braga').
utente(3, 'Luis', 21, 'Braga').
utente(4, 'Rafaela', 20, 'Braga').

sexo('M',1).
sexo('M',2).
sexo('M',3).
sexo('F',4).

servico(1, 'Ortodontia', 'Hospital', 'Braga').
servico(2, 'Medicina Geral', 'Clinica', 'Braga').
servico(3, 'Oftalmologia', 'Privado', 'Barcelos').
servico(4, 'Ortopedia', 'Hospital', 'Lisboa').

consulta('19/3/19', 1, 3, 25).
consulta('23/4/19', 2, 1, 3).
consulta('3/3/19', 1, 3, 33).
consulta('7/6/19', 3, 2, 15).
consulta('7/6/19', 1, 2, 15).

% Questão 1 - Registar utentes, serviços e consultas;

  % Extensao do predicado registarU: IdUtente, NomeUtente, Idade, Cidade, Sexo -> {V,F}
  % Só pode registar se não existir utente com o mesmo ID e sexo for válido
  registarU(ID, N, I, C, S) :- nao(utente(ID,_,_,_)),
    nao(sexo(S,ID)),
    assert(utente(ID,N,I,C)),
    assert(sexo(S,ID)).
  % Extensao do predicado registarS: IdServico, Descricao, Instituicao, Cidade -> {V,F}
  % Só pode registar serviço se não existir serviço com o mesmo ID e se não existir
  % serviço com o mesmo nome na mesma instituição
  registarS(ID,D,I,C) :- nao(servico(ID,_,_,_)),
    nao(servico(_,D,I,_)),
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

% Questão 2 - Remover utentes, serviços e consultas;

  % Extensao do predicado apagarU: IdUtente -> {V,F}
  apagarU(ID) :- utente(ID,_,_,_),
    nao(consulta(_,ID,_,_)),
    sexo(_,ID),
    retract(utente(ID,_,_,_)),
    retract(sexo(_,ID)).
  % Extensao do predicado apagarS: IdServico -> {V,F}
  % Só pode remover serviço se existir o serviço e não existirem consultas
  % Desse serviço
  apagarS(ID) :- servico(ID,_,_,_), nao(consulta(_,_,ID,_)), retract(servico(IdServico,_,_,_)).
  % Extensao do predicado apagarC: Data -> IdUtente -> IdServiço -> {V,F}
  % Só pode apagar uma consulta se existir essa consulta
  apagarC(D,ID,IDS) :- consulta(D,ID,IDS,_), retract(consulta(D,ID,IDS,_)).


% Questão 3 - Identificar as instituições prestadoras de serviços;
  %Extensao do predicado todasInst: Lista -> {V,F}
  todasInst(S) :- solucoes(Inst,servico(_,_,Inst,_),X),
                  semRepetidos(X,S).


% Questão 4 - Identificar utentes/serviços/consultas por critérios de seleção;
  % Extensao do predicado utentePorSexo: Sexo do Utente, Lista de Utentes  -> {V,F}
  utentePorSexo(Sexo,L) :- solucoes(Nome, (sexo(Sexo,Id),utente(Id,Nome,_,_) ),L).
  % Extensao do predicado utentePorCidade: Cidade do Utente, Lista de Utentes  -> {V,F}
  utentePorCidade(C,L) :- solucoes(Nome,utente(_,Nome,_,C),L).
  % Extensao do predicado utentePorIdade: Idade do Utente, Lista de Utentes  -> {V,F}
  utentePorIdade(I,L) :- solucoes(Nome,utente(_,Nome,I,_),L).


  % Extensao do predicado todosServicos: Lista de Servicos  -> {V,F}
  todosServicos(L):- solucoes(S, servico(_,S,_,_),L).

  % Extensao do predicado consultasPorDia: Dia, Lista de Consultas  -> {V,F}
  % Identifica o par Nome de Utente, Descrição do Serviço efetuadas num dado dia.
  % Assume-se que ninguém efetua mais que uma consulta do mesmo serviço por dia.
  consultasPorDia(D,L) :- solucoes((Nome,Desc), (consulta(D,IdU,IdS,_) , utente(IdU,Nome,_,_), servico(IdS,Desc,_,_)) , L).
  % Extensao do predicado consultasServiço: Data, Serviço, NumConsultas  -> {V,F}
  consultasServico(D,S,N) :-  servico(IdS,S,_,_),
                              solucoes(Id , consulta(D,Id,IdS,_), L),
                              countElements(L,N).
  % Extensao do predicado ganhoPorDia: Data, Valor  -> {V,F}
  ganhoPorDia(D,T) :-  solucoes(Valor , consulta(D,_,_,Valor), L),
                       sumElements(L,T).

% Questão 5 - Identificar serviços prestados por instituição/cidade/datas/custo;

  % Extensao do predicado servicosPorCidade: Cidade, Lista de Servicos  -> {V,F}
  servicosPorCidade(C,L) :- solucoes(S, servico(_,S,_,C),R), semRepetidos(R,L).

  % Extensao do predicado servicosPorData: Data, Lista de Servicos  -> {V,F}
  % Identifica os serviços para os quais foram registadas consultas numa dada data.
  servicosPorData(Data,L) :- solucoes(Desc, (consulta(Data,_,IdServico,_) , servico(IdServico,Desc,_,_)),R),
                          semRepetidos(R,L).

  % Extensão do predicado servicosPorCusto: Custo, Lista de Serviços -> {V,F}
  % Identifica os serviços para os quais foram registadas consultas com um dado custo
  servicosPorCusto(Custo,L) :- solucoes(Desc, (consulta(_,_,IdServico,Custo) , servico(IdServico,Desc,_,_)), R),
                          semRepetidos(R,L).

%Questão 6 - Identificar os utentes de um serviço/instituição;

  % Extensão do predicado utentesPorServico: Serviço, Lista de Utentes -> {V,F}
  % Identifica os utentes para os quais já foi prestado um dado serviço
  utentesPorServico(IdS,L) :- solucoes(Nome, (consulta(_,IdU,IdS,_) , servico(IdS,_,_,_) , utente(IdU,Nome,_,_)), R),
                          semRepetidos(R,L).

  % Extensão do predicado utentesPorInstituicao: Instituicao, Lista de Utentes -> {V,F}
  % Identifica os utentes para os quais já foram prestados serviços numa dada instituição
  utentesPorInstituicao(Ins,L) :- solucoes(Nome, (consulta(_,IdU,IdS,_) , servico(IdS,_,Ins,_) , utente(IdU,Nome,_,_)), R),
                            semRepetidos(R,L).

%Questão 7 - Identificar serviços realizados por utente/instituição/cidade;

  % Extensão do predicado servicosPorUtente: Utente, Lista de Serviços -> {V,F}
  % Identifica os serviços realizados por um utente
  servicosPorUtente(IdU,L) :- solucoes(Desc, (consulta(_,IdU,IdS,_) , servico(IdS, Desc,_,_)), R),
                            semRepetidos(R,L).

  % Extensão do predicado servicosPorUtente: Instituicao, Lista de Serviços -> {V,F}
  % Identifica os serviços realizados por um utente
  servicosPorInstituicao(Inst,L) :- solucoes(Desc, (consulta(_,IdU,IdS,_) , servico(IdS, Desc,_,_)), R),
                            semRepetidos(R,L).

  % Extensão do predicado servicosUIC: IdUtente, Instituicao, Cidade, Lista de Serviços -> {V,F}
  % Identifica os serviços realizados ou por um dado id de utente, numa dada instituição ou cidade
  servicosUIC(IdU,Inst,Cid,L) :- solucoes((Nome,Desc,Inst,Cid) , (consulta(_,IdU,IdS,_), servico(IdS,Desc,Inst,Cid), utente(IdU,Nome,_,_)), R),
                              semRepetidos(R,L).

%Questão 8 - Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data

  % Extensão do predicado custoUSID: IdUtente, Serviço, Instituicao, Data, Custo Total -> {V,F}
  % Calcula o valor gasto dado um Id de utente, serviço, uma instituição e/ou data
  custoUSID(IdU,IdS,Inst,Data,R) :- solucoes(C, (consulta(Data,IdU,IdS,C) , servico(IdS,_,Inst,_)), L) ,
                                sumElements(L,R).


% Extensão do predicado pertence: Elemento, Lista -> {V,F}
pertence(X, [X|T]).
pertence(X, [H|T]) :-  X \= H, pertence(X, T).


% Extensão do predicado não: T -> {V,F}
nao(T) :- T, !,fail.
nao(T).

% Extensão do predicado solucoes: Formato, Prova, LSolucoes -> {V,F}
solucoes(F,P,L) :- P , assert(tmp(F)), fail.
solucoes(F,P,L) :- construir(L,[]).

% Extensão do predicado construir: Solucao, Lista-> {V,F}
construir(S,L) :- retract(tmp(X)), ! ,construir(S,[X|L]).
construir(S,S).

% Extensão do predicado semRepetidos:: ListaComRepetidos, ListaSemRepetidos -> {V,F}
semRepetidos([],[]).
semRepetidos([H|T],S) :-  pertence(H,T) , semRepetidos(T,S).
semRepetidos([H|T],[H|S]) :- nao(pertence(H,T)) , semRepetidos(T,S).

% Extensão do predicado countElements:: ListaId, NumConsultas -> {V,F}
countElements([],0).
countElements([H|B],N) :- countElements(B,M), N is M+1.

% Extensão do predicado sumElements:: ListaValores, Total -> {V,F}
sumElements([],0).
sumElements([H|B],T) :- sumElements(B,M), T is M+H.

% Extensão do predicado que permite a evolucao do conhecimento
% evolução: Termo -> {V,F}
evolucao( T ) :- solucoes( I, +T::I, LInv),
                 insercao(T),
                 teste(LInv).

teste([]).
teste([I|L]) :- I, teste(L).

insercao(T) :- assert(T).
insercao(T) :- retract(T), !, fail.

% Extensão do predicado que permite a involução do conhecimento
% involução: Termo -> {V,F}
involucao( T ) :- T,
                solucoes(I, -T::I, LInv),
                remocao(T),
                teste(LInv).

remocao(T) :- retract(T).
remocao(T) :- assert(T), !, fail.

% Invariante Referencial
% Só pode remover utente se existir o utente e não existirem consultas
% Com esse utente e existir registo do seu sexo.
-utente(Id,No,In,Ci) :: (nao(consulta(_,Id,_,_)) , sexo(_,Id)).
