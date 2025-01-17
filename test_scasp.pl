:- use_module(library(scasp)).  % 载入 s(CASP)

% 规则示例
p :- q.   % p 成立当 q 成立
q.        % 事实：q 成立

% s(CASP) 查询
:- scasp(p, []).
