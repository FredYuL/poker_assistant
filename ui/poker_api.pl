:- module(poker_api, [run_server/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(poker_ai_monte_carlo_kelly). % 载入你的AI模块

% 服务器端口
server_port(5000).

% 启动服务器
run_server :-
    server_port(Port),
    http_server(http_dispatch, [port(Port)]).

% API 端点
:- http_handler(root(set_hand), set_hand, []).
:- http_handler(root(set_board), set_board, []).
:- http_handler(root(calculate), calculate, []).

% =========================
% 🎯 1. 设定玩家手牌
% =========================
set_hand(Request) :-
    http_read_json_dict(Request, Data),
    _Player = hero, % 假设玩家始终是 hero
    Card1 = Data.get(card1),
    Card2 = Data.get(card2),
    atomic_list_concat([Suit1, Rank1], '-', Card1),
    atomic_list_concat([Suit2, Rank2], '-', Card2),
    set_my_hand(hero, card(Suit1, Rank1), card(Suit2, Rank2)),
    reply_json_dict(_{status: "ok"}).

% =========================
% 🎯 2. 逐步添加公共牌
% =========================
set_board(Request) :-
    http_read_json_dict(Request, Data),
    atom_string(Data.get(card), CardAtom),
    atomic_list_concat([Suit, Rank], '-', CardAtom),
    community_cards(CurrentBoard),
    append(CurrentBoard, [card(Suit, Rank)], NewBoard),
    set_community_cards(NewBoard),
    reply_json_dict(_{status: "ok"}).

% =========================
% 🎯 3. 计算胜率 & 建议行动
% =========================
calculate(Request) :-
    http_read_json_dict(Request, Data),
    NumSamples = 10000, % Monte Carlo 计算样本数
    Pot = Data.get(pot),
    OpponentBet = Data.get(opponentBet),
    Bankroll = Data.get(bankroll),

    % 计算胜率
    winning_probability_monte_carlo(hero, NumSamples, Probability),

    % 计算下注策略
    suggest_action_kelly(hero, NumSamples, Pot, OpponentBet, Bankroll, Action, RaiseAmount),

    % 返回 JSON 结果
    reply_json_dict(_{
        winRate: Probability * 100,
        aiDecision: Action,
        raiseAmount: RaiseAmount
    }).
