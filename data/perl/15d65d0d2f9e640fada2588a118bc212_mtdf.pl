% Implementation of the strategy module.  See strategy.pl for a description of
% the exported predicates.
:- module(strategy, [play/5]).

% http://en.wikipedia.org/wiki/MTD-f
%
% function MTDF(root, f, d)
%     g := f
%     upperBound := +?
%     lowerBound := -?
%     while lowerBound < upperBound
%         if g = lowerBound then
%             ? := g+1
%         else
%             ? := g
%         g := AlphaBetaWithMemory(root, ?-1, ?, d)
%         if g < ? then
%             upperBound := g
%         else
%             lowerBound := g
%     return g

% vim: filetype=prolog
