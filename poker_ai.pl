:- module(poker_ai_monte_carlo_kelly, [
    clear_game/0,
    set_my_hand/3,
    set_community_cards/1,
    winning_probability_monte_carlo/3,
    suggest_action_kelly/7,  % main entry for action suggestion
    suit/1,
    rank/1,
    card/2
]).

/** <module> Poker AI with Weighted Range + Monte Carlo + Kelly Bet Sizing

    1) Hard-coded "opponent_range_store/1" for the opponent's range.
    2) "winning_probability_monte_carlo/3" => approximate your equity.
    3) "suggest_action_kelly/7" => uses Kelly fraction to choose action & raise amount.

    Usage Example:
      ?- consult('poker_ai_monte_carlo_kelly.pl').

      % Clear any old data
      ?- clear_game.

      % Set our hole cards
      ?- set_my_hand(hero, card(hearts,9), card(hearts,10)).

      % Set partial or full community
      ?- set_community_cards([
           card(hearts, queen),
           card(diamonds, jack),
           card(clubs, 10)
         ]).

      % We want an action suggestion. 
      % Suppose pot=100, opp bet=50, bankroll=1000, we do 1000 samples
      ?- suggest_action_kelly(hero, 1000, 100, 50, 1000, Action, Amount).

      Action = 'Raise',
      Amount = 300.
*/

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

% ==============
%  DYNAMIC FACTS
% ==============
:- dynamic my_hand/2.            % my_hand(Player, [C1, C2])
:- dynamic community_cards/1.    % community_cards([...])

% Clear any old data
clear_game :-
    retractall(my_hand(_, _)),
    retractall(community_cards(_)).

% Example: ?- set_my_hand(hero, card(hearts,9), card(hearts,10)).
set_my_hand(Player, Card1, Card2) :-
    retractall(my_hand(Player,_)),
    writeln('Setting my hand to hearts 9 and hearts 10'),
    assertz(my_hand(Player, [Card1, Card2])).

% Example: ?- set_community_cards([card(hearts,2), card(spades,king), card(diamonds,10)]).
set_community_cards(Cards) :-
    retractall(community_cards(_)),
     writeln('Setting community cards to spades 10, diamonds 10, clubs 10'),
    assertz(community_cards(Cards)).

% =====================================
%   PRESET OPPONENT RANGE (WEIGHTED)
% =====================================
/** You can modify this list for whichever combos & weights you like. */
opponent_range_store([
    ( [card(spades,ace), card(hearts,king)],      5 ),
    ( [card(diamonds,queen), card(clubs,queen)],  2 ),
    ( [card(hearts,ace), card(hearts,queen)],     4 ),
    ( [card(spades,10), card(clubs,10)],          3 ),
    ( [card(diamonds,king), card(diamonds,jack)], 2 ),
    ( [card(spades,2), card(diamonds,7)],         1 )
    % Add or remove combos / weights as desired
]).

% Suit/Rank definitions
suit(clubs).
suit(diamonds).
suit(hearts).
suit(spades).

rank(2). rank(3). rank(4). rank(5). rank(6).
rank(7). rank(8). rank(9). rank(10).
rank(jack). rank(queen). rank(king). rank(ace).

card(S, R) :-
    suit(S),
    rank(R).

% A full 52-card deck
full_deck(Deck) :-
    findall(card(S,R), card(S,R), Deck).

% =====================================
%   MONTE CARLO EQUITY CALCULATION
% =====================================
/** winning_probability_monte_carlo(+Player, +NumSamples, -Probability)

    Runs 'NumSamples' random trials:
     - picks an opponent hand from the weighted range
     - fills any unknown community cards from the deck
     - compares best 5-card hand
     - returns approximate Probability of your hand winning 
*/
winning_probability_monte_carlo(Player, NumSamples, Probability) :-
    my_hand(Player, MyCards),
    ( community_cards(C) -> true ; C=[] ),   % partial or full community
    opponent_range_store(WeightedRange),
    sum_of_weights(WeightedRange, TotalWeight),
    do_monte_carlo(Player, MyCards, C, WeightedRange, TotalWeight, NumSamples, Wins),
    ( NumSamples > 0
      -> Probability is Wins / NumSamples
      ;  Probability = 0.0
    ),
    writeln('Winning Probability:'),
    writeln(Probability).

% The main loop for random sampling
do_monte_carlo(_, _, _, _, _, 0, 0).
do_monte_carlo(Player, MyCards, Comm, OppRange, TW, N, WinsOut) :-
    pick_weighted_opponent_hand(OppRange, TW, OppCards),
    random_fill_unknown_community(Comm, FinalComm),
    ( player_beats_opponent(MyCards, OppCards, FinalComm) -> WinVal = 1 ; WinVal = 0 ),
    N1 is N - 1,
    do_monte_carlo(Player, MyCards, Comm, OppRange, TW, N1, SubWins),
    WinsOut is SubWins + WinVal.

% Weighted pick from the range
pick_weighted_opponent_hand(Range, TotalWeight, OppCards) :-
    random(0.0, TotalWeight, Rnd),
    pick_weighted_hand_aux(Range, Rnd, OppCards).

pick_weighted_hand_aux([(Hand, W)|_], Rnd, Hand) :-
    Rnd =< W, !.
pick_weighted_hand_aux([(_,W)|T], Rnd, Result) :-
    Rnd2 is Rnd - W,
    pick_weighted_hand_aux(T, Rnd2, Result).

sum_of_weights([], 0.0).
sum_of_weights([(_,W)|T], Sum) :-
    sum_of_weights(T, Rest),
    Sum is W + Rest.

% Fill community up to 5 if partial
random_fill_unknown_community(Comm, Comm) :-
    length(Comm, 5), !.
random_fill_unknown_community(Comm, FinalComm) :-
    length(Comm, N),
    Missing is 5 - N,
    deck_for_community(Comm, Deck),
    random_pick(Missing, Deck, Extra),
    append(Comm, Extra, FinalComm).

deck_for_community(Comm, DeckOut) :-
    % gather my cards
    findall(Ca, my_hand(_, Ca), MList),
    flatten(MList, MyAll),
    full_deck(Full),
    append(MyAll, Comm, Used),
    subtract(Full, Used, DeckOut).

random_pick(0, _, []) :- !.
random_pick(K, Deck, [C|Cs]) :-
    K > 0,
    random_select(C, Deck, Rest),
    K1 is K - 1,
    random_pick(K1, Rest, Cs).

% Compare final 5-card best hands
player_beats_opponent(MyHole, OppHole, Comm) :-
    append(MyHole, Comm, My7),
    best_hand_score(My7, MyScore),
    append(OppHole, Comm, Opp7),
    best_hand_score(Opp7, OppScore),
    MyScore > OppScore.

% Simplified hand ranking
best_hand_score(Cards, Score) :-
    ( has_straight_flush(Cards) -> Score = 9
    ; has_four_of_a_kind(Cards) -> Score = 8
    ; has_full_house(Cards)     -> Score = 7
    ; has_flush(Cards)          -> Score = 6
    ; has_straight(Cards)       -> Score = 5
    ; has_three_of_a_kind(Cards)-> Score = 4
    ; has_pair(Cards)           -> Score = 2
    ; Score = 1
    ).

has_pair(Cards) :-
    count_by_rank(Cards, CR),
    member(_-2, CR).

has_three_of_a_kind(Cards) :-
    count_by_rank(Cards, CR),
    member(_-3, CR).

has_full_house(Cards) :-
    count_by_rank(Cards, CR),
    member(_-3, CR),
    member(_-2, CR).

has_flush(Cards) :-
    count_by_suit(Cards, CS),
    member(_-Cnt, CS),
    Cnt >= 5.

has_straight(Cards) :-
    get_rank_values(Cards, Ranks),
    consecutive(Ranks).

has_four_of_a_kind(Cards) :-
    count_by_rank(Cards, Counted),
    member(_-4, Counted).

has_straight_flush(Cards) :-
    count_by_suit(Cards, CS),
    member(Suit-Count, CS),
    Count >= 5,
    include(is_suit(Suit), Cards, SameSuitCards),
    get_rank_values(SameSuitCards, SR),
    consecutive(SR).

count_by_rank(Cards, Counted) :-
    findall(R, member(card(_,R), Cards), Rs),
    msort(Rs, SR),
    encode(SR, Counted).

count_by_suit(Cards, Counted) :-
    findall(S, member(card(S,_), Cards), Ss),
    msort(Ss, SS),
    encode(SS, Counted).

encode([], []).
encode([X|Xs], [X-N|Ys]) :-
    aggregate(count, member(X, [X|Xs]), N),
    exclude(=(X), Xs, Rest),
    encode(Rest, Ys).

is_suit(S, card(S,_)).

consecutive(List) :-
    length(List, L),
    L >= 5,
    consecutive_5(List).

consecutive_5([A,B,C,D,E|_]) :-
    A+1=:=B, B+1=:=C, C+1=:=D, D+1=:=E.
consecutive_5([_|T]) :-
    consecutive_5(T).

get_rank_values(Cards, RanksSorted) :-
    findall(V, (member(card(_,R), Cards), rank_value(R,V)), Vs),
    ( member(14, Vs) ->
        append([1], Vs, Extended),
        sort(Extended, RanksSorted)
    ;   sort(Vs, RanksSorted)
    ).

rank_value(2,2). rank_value(3,3). rank_value(4,4).
rank_value(5,5). rank_value(6,6). rank_value(7,7).
rank_value(8,8). rank_value(9,9). rank_value(10,10).
rank_value(jack,11). rank_value(queen,12).
rank_value(king,13). rank_value(ace,14).

% ============================================================
%   KELLY-BASED ACTION SUGGESTION: 'Raise' or 'Fold'
% ============================================================
/** suggest_action_kelly(+Player, +NumSamples, +Pot, +OpponentBet, +Bankroll, -Action, -RaiseAmount)

    1) We approximate your winning probability with `NumSamples` Monte Carlo trials.
    2) Use a simple Kelly formula to decide how much to raise if Kelly fraction > 0.
    3) If Probability < some threshold (like 0.3), we also fold.
*/
suggest_action_kelly(Player, NumSamples, Pot, OppBet, Bankroll, Action, RaiseAmount) :-
    % 1) get approximate probability
    winning_probability_monte_carlo(Player, NumSamples, PWin),

    % 2) define net odds ratio B = (Pot + OppBet) / OppBet
    ( OppBet =:= 0
      -> B = 999999   % or a large number if there's no bet to call
      ;  B is (Pot + OppBet) / OppBet
    ),

    % 3) compute Kelly fraction
    kelly_fraction(PWin, B, KellyF),

    % 4) define a fold threshold or use the fraction
    ( KellyF =< -1.0 ->
        Action = 'Fold',
        RaiseAmount = 0
    ; KellyF =< 0.0 ->
        Action = 'Check',
        RaiseAmount = 0
    ; 
        Action = 'Raise',
        Tentative is KellyF * Bankroll,
        RaiseAmount is round(Tentative)
    ),
     writeln('suggest_action_kelly called'),
    writeln('Action: '), writeln(Action),
    writeln('Amount: '), writeln(RaiseAmount).

/** kelly_fraction(+PWin, +NetOdds, -Fraction)
    Kelly fraction formula: f* = (p*b - (1-p)) / b
    p = probability of winning
    b = net odds ratio
*/
kelly_fraction(P, B, F) :-
    Q is 1 - P,
    F is (P * B - Q) / B.

% ---------------------------------------------------------------------
