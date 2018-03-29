% Implementation of the strategy module.  See strategy.pl for a description of
% the exported predicates.
:- module(strategy, [play/5]).

% http://en.wikipedia.org/wiki/Negascout
%
% function negascout(node, depth, ?, ?)
%     if node is a terminal node or depth = 0
%         return the heuristic value of node
%     b := ?                                          (* initial window is (-?, -?) *)
%     foreach child of node
%         a := -negascout (child, depth-1, -b, -?)
%         if a>?
%             ? := a
%         if ???
%             return ?                                (* Beta cut-off *)
%         if ??b                                      (* check if null-window failed high*)
%            ? := -negascout(child, depth-1, -?, -?)  (* full re-search *)
%            if ???
%                return ?                             (* Beta cut-off *)
%         b := ?+1                                    (* set new null window *)
%     return ?

% vim: filetype=prolog
