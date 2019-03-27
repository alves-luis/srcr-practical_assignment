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

% Extensão do predicado Utente: Id, Nome, Idade, Cidade -> {V,F}
utente(1, 'Andre', 20, 'Esposende').
utente(2, 'Joao', 20, 'Braga').
utente(3, 'Luis', 21, 'Braga').
utente(4, 'Rafaela', 20, 'Braga').
utente(5, 'Joaquim', 23, 'Lisboa').
utente(6, 'Anabela', 27, 'Portalegre').
utente(7, 'Vitorino', 30, 'Lisboa').
utente(8, 'Zeferino', 37, 'Faro').
utente(9, 'Miguelito', 42, 'Outeiro').
utente(10, 'Jacinto', 43, 'Faro').
utente(11, 'Orlando', 50, 'Braga').
utente(12, 'Nestor', 45, 'Esposende').
utente(13, 'Moura', 55, 'Viana do Castelo').
utente(14, 'Barros', 33, 'Paredes de Coura').
utente(15, 'Proenca', 60, 'Figueira da Foz').

% Extensão do predicado Sexo: Sexo, Id -> {V,F}
sexo('M',1).
sexo('M',2).
sexo('M',3).
sexo('F',4).
sexo('M',5).
sexo('F',6).
sexo('M',7).
sexo('M',8).
sexo('M',9).
sexo('M',10).
sexo('M',11).
sexo('M',12).
sexo('M',13).
sexo('M',14).
sexo('M',15).

% Extensão do predicado sexoValido: Sexo -> {V,F}
sexoValido('M').
sexoValido('F').
sexoValido('O').

% Extensão do predicado servico: IdServ, Descricao, Instituicao, Cidade -> {V,F}
servico(1, 'Ortodontia', 'Hospital de S.Marcos', 'Braga').
servico(2, 'Medicina Geral', 'Clinica de Santa Tecla', 'Braga').
servico(3, 'Oftalmologia', 'Hospital Privado XPTO', 'Barcelos').
servico(4, 'Ortopedia', 'Hospital de Ourique', 'Lisboa').
servico(5, 'Psicologia', 'Hospital da Nossa Senhora do 20', 'Guimaraes').
servico(6, 'Medicina Dentaria', 'Hospital de S.Marcos', 'Braga').
servico(7, 'Oncologia', 'Hospital da Luz', 'Lisboa').
servico(8, 'Psiquiatria', 'Clinica de Santa Tecla', 'Braga').
servico(9, 'Dermatologia', 'Hospital Privado XPTO', 'Barcelos').

% Extensão do predicado consulta: Data, IdUtente, IdServiço, Custo -> {V,F}
consulta(data(19,3,2019), 1, 3, 25).
consulta(data(23,4,2019), 2, 1, 3).
consulta(data(3,3,2019), 1, 3, 33).
consulta(data(7,7,2019), 3, 2, 15).
consulta(data(7,7,2019), 1, 2, 15).
consulta(data(8,3,2019),5,7,30).
consulta(data(25,4,2019),8,8,8).
consulta(data(3,3,2019),15,5,40).
consulta(data(4,9,2019),12,7,50).
consulta(data(24,12,2019),9,5,20).
consulta(data(1,1,2020),13,6,70).
consulta(data(4,5,2019),14,3,25).
consulta(data(8,8,2020),10,4,35).
consulta(data(1,1,2020),11,6,60).
consulta(data(18,6,2010),8,6,30).

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

% Questão 1 - Registar utentes, serviços e consultas;

% Extensao do predicado registarU: IdUtente, NomeUtente, Idade, Cidade, Sexo -> {V,F}
% Só pode registar se não existir utente com o mesmo ID e sexo for válido
registarU(Id, Nome, Idade, Cidade, Sexo) :-
  evolucao(utente(Id,Nome,Idade,Cidade)), evolucao(sexo(Sexo,Id)).

% Invariante estrutural
% Não permitir a inserção de conhecimento repetido sobre utente
+utente(Id,_,_,_) :: (solucoes(Ls, utente(Id,_,_,_),S) ,
                countElements(S,N) ,
                N == 1).

% Invariante referencial
% Não permitir a inserção de utentes com idades não válidas
+utente(_,_,Id,_) :: (natural(Id), Id < 135).

% Invariante estrutural
% Não permitir a inserção de conhecimento repetido sobre sexo
+sexo(Sexo,Id) :: (solucoes(Ls, sexo(_,Id),S),
                countElements(S,N),
                N == 1).
% Invariante referencial
% Só pode inserir sexo se for sexoValido.
+sexo(S,_) :: sexoValido(S).


% Extensao do predicado registarS: IdServico, Descricao, Instituicao, Cidade -> {V,F}
% Só pode registar serviço se não existir serviço com o mesmo ID e se não existir
% serviço com o mesmo nome na mesma instituição
registarS(Id,Desc,Inst,Cidade) :- evolucao(servico(Id,Desc,Inst,Cidade)).

% Invariante estrutural
% Não permitir a inserção de conhecimento repetido sobre servico
+servico(Id,_,_,_) :: (solucoes(Ls, servico(Id,_,_,_), S),
                    countElements(S,N),
                    N == 1).
% Invariante estrutural
% Não permitir a inserção de conhecimento se existir um serviço
% com o mesmo nome na mesma instituição
+servico(_,Desc,Inst,_) :: (solucoes(Ls, servico(_,Desc,Inst,_), S),
                            countElements(S,N),
                            N == 1).

% Extensao do predicado registarC: Data, IdUtente, IdServico, Custo -> {V,F}
% Só pode registar consulta se não existir para o mesmo serviço no mesmo Dia
% Se o Custo for maior que 0, se existir o utente e se existir o serviço
registarC(Data,IdU,IdS,Custo) :- evolucao(consulta(Data,IdU,IdS,Custo)).

% Invariante estrutural
% Não permitir a inserção de conhecimento repetido de consulta
+consulta(Data,IdU,IdS,Custo) :: (solucoes(Ls, consulta(Data,IdU,IdS,Custo),S),
                                  countElements(S,N),
                                  N == 1).
% Invariante estrutural
% Não permitir a inserção de conhecimento em que o Custo é inferior a 0
+consulta(Data,IdU,IdS,Custo) :: Custo > 0.

% Invariante referencial
% Não permitir a inserção de novas consultas se não existir o utente com
% esse Id
+consulta(_,IdU,_,_) :: utente(IdU,_,_,_).

% Invariante referencial
% Não permitir a inserção de novas consultas se não existir o serviço
% com esse Id
+consulta(_,_,IdS,_) :: servico(IdS,_,_,_).

% Invariante referencial
% Não permitir a inserção de novas consultas se a data não for válida
+consulta(data(Dia,Mes,Ano),_,_,_) :: data(Dia,Mes,Ano).

% Questão 2 - Remover utentes, serviços e consultas;

% Extensao do predicado apagarU: IdUtente -> {V,F}
apagarU(ID) :- involucao(utente(ID,_,_,_)),
               involucao(sexo(_,ID)).

% Invariante Referencial
% Só pode remover utente se não existirem consultas
-utente(Id,No,In,Ci) :: nao(consulta(_,Id,_,_)).

% Extensao do predicado apagarS: IdServico -> {V,F}
% Só pode remover serviço se existir o serviço e não existirem consultas
% Desse serviço
apagarS(Id) :- involucao(servico(Id,_,_,_)).

% Invariante referencial
% Só pode remover servico se não existir consulta com esse Id
-servico(Id,_,_,_) :: nao(consulta(_,_,Id,_)).

% Extensao do predicado apagarC: Data, IdUtente, IdServiço, Custo -> {V,F}
apagarC(Data,ID,IDS,C) :- involucao(consulta(Data,ID,IDS,C)).


% Questão 3 - Identificar as instituições prestadoras de serviços;
  % Extensao do predicado todasInst: Lista -> {V,F}
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
  todosServicos(L):- solucoes((S,I), servico(_,S,I,_),L).

  % Extensao do predicado consultasPorDia: Dia, Lista de Consultas  -> {V,F}
  % Identifica o par Nome de Utente, Descrição do Serviço efetuadas num dado dia.
  % Assume-se que ninguém efetua mais que uma consulta do mesmo serviço por dia.
  consultasPorDia(D,L) :- solucoes((Nome,Desc), (consulta(D,IdU,IdS,_) , utente(IdU,Nome,_,_), servico(IdS,Desc,_,_)) , L).
  % Extensao do predicado consultasServiço: Data, Serviço, NumConsultas  -> {V,F}
  consultasServico(D,S,N) :-  servico(IdS,S,_,_),
                              solucoes(Id , consulta(D,Id,IdS,_), L),
                              countElements(L,N).

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

  % Extensão do predicado servicosPorInstituição: Instituicao, Lista de Serviços -> {V,F}
  % Identifica os serviços realizados numa instituição
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

  % Extensao do predicado ganhoPorDia: Data, Valor  -> {V,F}
  ganhoPorDia(D,T) :-  solucoes(Valor , consulta(D,_,_,Valor), L),
                    sumElements(L,T).


% Extensão do predicado pertence: Elemento, Lista -> {V,F}
pertence(X, [X|T]).
pertence(X, [H|T]) :-  X \= H, pertence(X, T).


% Extensão do predicado não: T -> {V,F}
nao(T) :- T, !,fail.
nao(T).

% Extensão do predicado solucoes: Formato, Prova, LSolucoes -> {V,F}
solucoes(F,P,L) :- findall(F,P,L).

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

% Extensão do predicado testa: Lista -> {V,F}
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
