
# :- use_module(library(scasp)).  % 载入 s(CASP)

# % 牌的花色
# suit(clubs). suit(diamonds). suit(hearts). suit(spades).

# % 牌的点数
# rank(2). rank(3). rank(4). rank(5). rank(6).
# rank(7). rank(8). rank(9). rank(10).
# rank(jack). rank(queen). rank(king). rank(ace).

# % 避免 `rank/1` 和 `suit/1` 交错
# :- discontiguous rank/1.
# :- discontiguous suit/1.

# % 定义扑克牌结构
# card(Suit, Rank) :- suit(Suit), rank(Rank).

# % 存储玩家手牌
# hand(player, card(hearts, ace), card(spades, king)).  % 玩家手牌 A♥ K♠

# % 让 AI 记住已揭示的公共牌
# :- dynamic revealed_board/1.

# % 生成可能的公共牌（排除玩家手牌）
# possible_board(Card) :-
#     suit(Suit), rank(Rank),
#     \+ hand(player, card(Suit, Rank)),  % 不能是玩家手牌
#     \+ revealed_board(card(Suit, Rank)). % 不能是已揭示的公共牌

# % 逐步揭示 Flop（3 张）
# reveal_flop :-
#     findall(Card, possible_board(Card), PossibleCards),
#     random_permutation(PossibleCards, Shuffled),
#     first_n(3, Shuffled, FlopCards),
#     assert_revealed_board(FlopCards),
#     update_ai_decision.

# % 逐步揭示 Turn（1 张）
# reveal_turn :-
#     findall(Card, possible_board(Card), PossibleCards),
#     random_permutation(PossibleCards, Shuffled),
#     first_n(1, Shuffled, [TurnCard]),
#     assert_revealed_board([TurnCard]),
#     update_ai_decision.

# % 逐步揭示 River（1 张）
# reveal_river :-
#     findall(Card, possible_board(Card), PossibleCards),
#     random_permutation(PossibleCards, Shuffled),
#     first_n(1, Shuffled, [RiverCard]),
#     assert_revealed_board([RiverCard]),
#     update_ai_decision.

# % 让 AI 记住揭示的公共牌
# assert_revealed_board([]).
# assert_revealed_board([Card | Rest]) :-
#     assertz(revealed_board(Card)),  % 让 AI 记住这张牌
#     assert_revealed_board(Rest).

# % 计算最佳 5 张手牌（玩家手牌 + 已揭示的公共牌）
# best_hand(Player, BestHand) :-
#     findall(Card, (hand(Player, Card); revealed_board(Card)), AllCards),
#     pick_best_five(AllCards, BestHand).

# % 10 种牌型规则
# has_high_card(Player) :-
#     \+ has_pair(Player), \+ has_flush(Player), \+ has_straight(Player).

# has_pair(Player) :-
#     findall(Rank, (hand(Player, card(_, Rank)); revealed_board(card(_, Rank))), Ranks),
#     sort(Ranks, UniqueRanks),
#     length(Ranks, L), length(UniqueRanks, U),
#     L > U.

# has_two_pair(Player) :-
#     findall(Rank, (hand(Player, card(_, Rank)); revealed_board(card(_, Rank))), Ranks),
#     findall(R, (member(R, Ranks), count_occurrences(R, Ranks, 2)), Pairs),
#     sort(Pairs, UniquePairs),
#     length(UniquePairs, 2).

# has_three_of_a_kind(Player) :-
#     findall(Rank, (hand(Player, card(_, Rank)); revealed_board(card(_, Rank))), Ranks),
#     member(R, Ranks),
#     count_occurrences(R, Ranks, 3).

# has_straight(Player) :-
#     findall(Rank, (hand(Player, card(_, Rank)); revealed_board(card(_, Rank))), Ranks),
#     sort(Ranks, UniqueRanks),
#     has_consecutive_five(UniqueRanks).

# has_flush(Player) :-
#     findall(Suit, (hand(Player, card(Suit, _)); revealed_board(card(Suit, _))), Suits),
#     member(S, Suits),
#     count_occurrences(S, Suits, N),
#     N >= 5.

# has_full_house(Player) :-
#     has_three_of_a_kind(Player),
#     has_pair(Player).

# has_four_of_a_kind(Player) :-
#     findall(Rank, (hand(Player, card(_, Rank)); revealed_board(card(_, Rank))), Ranks),
#     member(R, Ranks),
#     count_occurrences(R, Ranks, 4).

# has_straight_flush(Player) :-
#     has_straight(Player),
#     has_flush(Player).

# has_royal_flush(Player) :-
#     findall(card(Suit, Rank), (hand(Player, card(Suit, Rank)); revealed_board(card(Suit, Rank))), Cards),
#     has_flush_with_ranks(Cards, [10, jack, queen, king, ace]).

# % 辅助函数
# count_occurrences(_, [], 0).
# count_occurrences(E, [E | T], N) :- count_occurrences(E, T, N1), N is N1 + 1.
# count_occurrences(E, [_ | T], N) :- count_occurrences(E, T, N).

# has_consecutive_five([A,B,C,D,E | _]) :-
#     next_rank(A, B), next_rank(B, C), next_rank(C, D), next_rank(D, E).
# has_consecutive_five([_|T]) :- has_consecutive_five(T).

# has_flush_with_ranks(Cards, RequiredRanks) :-
#     member(card(Suit, _), Cards),
#     include(same_suit(Suit), Cards, SameSuitCards),
#     extract_ranks(SameSuitCards, Ranks),
#     subset(RequiredRanks, Ranks).

# same_suit(Suit, card(Suit, _)).
# extract_ranks([], []).
# extract_ranks([card(_, Rank) | Rest], [Rank | Ranks]) :- extract_ranks(Rest, Ranks).

# % AI 下注逻辑
# bet_action(Player, raise) :- win_probability(Player, 10000, WinRate), WinRate > 70.
# bet_action(Player, call) :- win_probability(Player, 10000, WinRate), WinRate > 40.
# bet_action(Player, fold) :- win_probability(Player, 10000, WinRate), WinRate =< 40.

# update_ai_decision :-
#     win_probability(player, 10000, WinRate),
#     format('当前胜率: ~2f%~n', [WinRate]),
#     bet_action(player, Action),
#     format('AI 选择的行动: ~w~n', [Action]).



:- use_module(library(scasp)).  % 载入 s(CASP)

% 牌的花色
suit(clubs).
suit(diamonds).
suit(hearts).
suit(spades).

% 牌的点数
rank(2). rank(3). rank(4). rank(5). rank(6).
rank(7). rank(8). rank(9). rank(10).
rank(jack). rank(queen). rank(king). rank(ace).

% 避免 `rank/1` 和 `suit/1` 交错
:- discontiguous rank/1.
:- discontiguous suit/1.

% 定义扑克牌结构
card(Suit, Rank) :- 
    suit(Suit), 
    rank(Rank).


hand(player, card(hearts, ace), card(spades, ace)).


has_high_card(Player) :-
    \+ has_pair(Player), \+ has_flush(Player), \+ has_straight(Player).

has_pair(Player) :-
    hand(Player, card(_, Rank1), card(_, Rank2)),
    Rank1 = Rank2.    % ✅ 使用 `=` 替换 `==`


has_straight(Player) :-
    hand(Player, card(_, Rank1), card(_, Rank2)),
    next_rank(Rank1, Rank2).

% 定义相邻点数
next_rank(2, 3). next_rank(3, 4). next_rank(4, 5).
next_rank(5, 6). next_rank(6, 7). next_rank(7, 8).
next_rank(8, 9). next_rank(9, 10). next_rank(10, jack).
next_rank(jack, queen). next_rank(queen, king).
next_rank(king, ace).


has_flush(Player) :-
    hand(Player, card(Suit1, _), card(Suit2, _)),
    Suit1 = Suit2.   % ✅ 使用 `=`


has_straight_flush(Player) :-
    has_flush(Player),
    has_straight(Player).

has_royal_flush(Player) :-
    hand(Player, card(Suit, ace), card(Suit, king)),
    Suit == Suit.

