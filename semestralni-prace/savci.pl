main :- identify.

identify:-
  retractall(known(_,_,_)),         % clear stored information
  mammal(X),
  write('Je to '),write(X),nl.
identify:-
  write('Tohoto savce neznam'),nl.


% Bunny classification
mammal(kralik) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(seda),
    bilozravec,
    vaha_pod_4kg.

% Polar bear classification
mammal("ledni medved") :-
    zije("ve vode"),
    zije("na sousi"),
    drapy,
    srst,
    barva(bila).

% Harp seal classification
mammal("tulen gronsky") :-
    zije("na sousi"),
    zije("ve vode"),
    srst,
    barva(bila).

% Seal classification
mammal(tulen) :-
    zije("ve vode"),
    zije("na sousi"),
    barva(seda).

% Dolphin classification
mammal(delfin) :-
    zije("ve vode"),
    barva(seda).

% Beaver classification
mammal(bobr) :-
    zije("na sousi"),
    zije("ve vode"),
    drapy,
    srst,
    bilozravec,
    barva(hneda).

% Giraffe classification
mammal(zirafa) :-
    zije("na sousi"),
    srst,
    barva(jina),
    bilozravec,
    krk_delsi_nez_2m.

% Cheetah classification
mammal(gepard) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(jina).

% Bat classification
mammal(netopyr) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(cerna),
    vaha_pod_4kg.

% Sloth classification
mammal(lenochod) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(seda),
    bilozravec.

% Panda classification
mammal(panda) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(cerno_bila),
    bilozravec.

% Zebra classification
mammal(zebra) :-
    zije("na sousi"),
    srst,
    barva(cerno_bila),
    bilozravec.

% Bear classification
mammal(medved) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(hneda).

% Chimpanzee classification
mammal(simpanz) :-
    zije("na sousi"),
    srst,
    barva(cerna).

% Hedgehog classification
mammal(jezek) :-
    zije("na sousi"),
    drapy,
    barva(seda),
    srst,
    vaha_pod_4kg.

% Wolf classification
mammal(vlk) :-
    zije("na sousi"),
    drapy,
    srst,
    barva(seda).

% Pig classification
mammal(prase) :-
    zije("na sousi"),
    srst,
    barva(ruzova),
    bilozravec.

% Elephant classification
mammal(slon) :-
    zije("na sousi"),
    bilozravec,
    barva(seda).

% Llama classification
mammal(lama) :-
    zije("na sousi"),
    srst,
    barva(jina).

% Killer whale classification
mammal(kosatka) :-
    zije("ve vode"),
    barva(cerno_bila).


% Ask the user
zije(X) :- ask(zije, X).
barva(X) :- menuask(barva, X, [cerna, cerno_bila, seda, hneda, bila, ruzova, jina]).
drapy :- ask(drapy).
srst :- ask(srst).
bilozravec :- ask(bilozravec).
vaha_pod_4kg :- ask(vaha_pod_4kg).
krk_delsi_nez_2m :- ask(krk_delsi_nez_2m).


% 'ask' is responsible for getting information from the user, and remembering
% the users response. If it doesn't already know the answer to a question
% it will ask the user. It then asserts the answer. It recognizes two
% cases of knowledge: 1) the attribute-value is known to be true,
% 2) the attribute-value is known to be false.

% This means an attribute might have multiple values. A third test to
% see if the attribute has another value could be used to enforce
% single valued attributes. (This test is commented out below)

% For this system the menuask is used for attributes which are single
% valued

% 'ask' only deals with simple yes or no answers. a 'yes' is the only
% yes value. any other response is considered a 'no'.

ask(Attribute,Value):-
  known(ano,Attribute,Value),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),         % fail if we know its false
  !, fail.

%ask(Attribute,_):-
%  known(ano,Attribute,_),           % fail if we know its some other value.
%  !, fail.                          % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                       % if we get here, we need to ask.
  write('? (ano nebo ne): '),
  read(Y),                          % get the answer
  asserta(known(Y,A,V)),            % remember it so we dont ask again.
  Y = ano.                          % succeed or fail based on answer.

ask(Attribute):-
  known(ano,Attribute,nothird),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute):-
  known(_,Attribute,nothird),         % fail if we know its false
  !, fail.

ask(A):-
    write(A),
    write('? (ano nebo ne): '),
    read(Y),
    asserta(known(Y,A,nothird)),
    Y = ano.



% 'menuask' is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer. In this case there is no
% need to check for a negative since 'menuask' ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(ano,Attribute,Value),       % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(ano,Attribute,_),           % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('Jaka je hodnota pro '),write(Attribute),write('?'),nl,
  display_menu(Menu),
  write('Zadejte cislo vyberu> '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(ano,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-        % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                       % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.        % start at one
  pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                    % what they entered as the value

pic_menu(_,_,none_of_the_above,[]). % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).      % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,               % try the next one
  pic_menu(NextCtr, N, Val, Rest).
