%%%%
%% This file is part of gCoKe [ http://www.gcoke.org ]
%%
%% Copyright (C) 2010-  Sebastien Mosser
%%
%% gCoKe is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as 
%% published by the Free Software Foundation; either version 2 of 
%% the License, or (at your option) any later version.
%%
%% gCoKe is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public 
%% License along with gCoKe; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
%%%%

:- module(algorithm, []).
/** <module> Algorithm declaration and execution
  This module supports the declaration of a composition algorithms, and 
  implements the associated execution engine (in top of engine.pl).

  An algorithm declaration binds a regular Prolog predicate with an
  associated meta-description. A meta-description includes (i) the name of the 
  predicate, (ii) a description of its parameters and (iii) graph transfer  

  An algorithm can use multiple inputs and multiple outputs. An algorithm can 
  use as inputs regular terms, gCoKe graphs or gCoKe graph sets. It *always* 
  produces as output a sequence of action to be executed.  If no graph transfer
  directive is found for a given output, the generated actions are executed on
  the empty graph.

  In the following example, we declare that =|my_predicate|= is a composition 
  algorithm, using as first (input) parameter a graph named =|g|= and as 
  second parameter (output) an action sequence named =|x|=. The graph transfer 
  directive =|[g,x]|= asks to execute the action sequence retrieved in the =|x|=
  output on the graph given as =|g|= parameter.
  ==
    :- algorithm:declare(my_predicate, [in(g,graph),out(x)], [[g,x]]).
    my_predicate(A_graph, Output_actions) :- ...
  ==
*/

%%%%
%% Algorithm declaration
%%%%

:- dynamic description/3.

%% declare(+Predicate, +Signature, +Graph_transfers)
% Declare Predicate as a composition algorithm, using Signature to meta-describe
% its arguments and Graph_transfers to reify how the graphs used as inputs must
% be transfered as output (i.e., on which graph the computed action sequence
% will be executed).
declare(Predicate, Signature, Graph_transfers) :- 
	assert(description(Predicate, Signature,Graph_transfers)).

%%%%
%% Parameter Check
%%%%

% FIXME

%%%%
%% Algorithm execution
%%%%

%% execute(+Call, +Context, -Outputs)
% execute Call in a given Context (filled by composition.pl). Outputs is 
% unified with a list of algorithm outputs, i.e., a couple (G,A) where A is 
% the sequence of actions to be executed on G.
execute(Call, Context, Outputs) :- 
	Call = gcoke_call(Algo, Input_bindings, Output_bindings),
	append(Input_bindings, Output_bindings, Params),
	verify(Algo, Params),
	build_exec_term(Algo, Context, Params, Term, Output_action_bindings), 
	call(Term), !,
	propagate_graph_transfer(Context,Algo,Params, Output_action_bindings, 
	                         Outputs). 

%% build_exec_term(+Algo, +Ctx, +Call_values, -Term, -Output_bindings).
% Based on the declaration of Algo, the predicate unify with Term the term to 
% be executed in the interpreter. Parameters are described in Call_values, and 
% retrieved in Context. Output_bindings keep track of free variables used as 
% output.
build_exec_term(Algo, Ctx, Call_values, Term, Output_bindings) :- 
 	description(Algo, Signature, _),
 	build_term_members(Signature,Ctx,Call_values,Term_list,Output_bindings),
 	Term =.. [call, Algo|Term_list].

%% build_term_members(+Signature, +Ctx, +Call_values, -Term_list, -Outputs)
% iterates over Signature, retrieving expected values in Call_values and real
% data in Ctx. A Term_list is built, and Outputs keeps track of Free variables.
build_term_members([], _, _, [], []) :- !. 
build_term_members([in(Param,_)|Tail], Ctx, Call_values, Term_list, Outputs) :- 
 	member(binding(Param, Val_term), Call_values), 
	retrieve_value(Ctx, Val_term, Val),
 	build_term_members(Tail, Ctx, Call_values, Other_terms, Outputs),
 	Term_list = [Val | Other_terms].
build_term_members([out(Param) | Tail], Ctx, Call_values, Term_list, Outputs) :-
 	member(binding(Param, graph(Symbol)), Call_values),
 	build_term_members(Tail, Ctx, Call_values, Other_terms, Other_outputs),
 	Term_list = [Free | Other_terms],
	Outputs = [[out(Param,Symbol), Free] | Other_outputs].

%% retrieve_value(+Context, +Value_symbol, -Value)
% Find the Value associated to Value_symbol (i.e., =|term(Scalar_value)|=, 
% =|graph(Graph_name)|= or =|graph_set([Graph_name_1, ...])|=) in a given 
% Context.
retrieve_value(_, term(Value), Value).
retrieve_value(Context, graph(Name), Value) :- 
	composition:pull_from_context(Context, Name, Value).
retrieve_value(Context, graph_set(L), Value) :- 
	findall(G,(member(N,L), composition:pull_from_context(Context,N,G)),
	        Value).

%% propagate_graph_transfer(+Ctx, +Algo, +Vals, +Bindings, -Outputs)
% Ctx is the current execution context. Algo is the name of the executed 
% algorithms. Binding is a list of =|[out(Param,Symbol), Actions]|= pairs. Vals
% is a call_values list reifiying the parameters of Algo. Output is unified with
% algorithm output terms (i.e., =|algo_out(Graph, Actions)|=) based on the
% description associated to Algo (especially its graph_transfer part).
propagate_graph_transfer(Ctx, Algo, Vals, Output_action_bindings, Outputs) :- 
	description(Algo,_, Transfers), 
	findall(O, (member(B, Output_action_bindings), 
	            algorithm:handle_transfer(Ctx,Transfers,Vals,B,G,Acts),
		    algorithm:handle_output(O, G, Acts)),
		Outputs).

%% handle_transfer(+Ctx, +Transfers, +Values, +Binding, -Graph, -Actions)
% Properly transfer the content of Binding into Graph and Actions, based on the
% current Context and the meta-described Transfers directives.
handle_transfer(Context, Transfers, Values, Binding, Graph, Actions) :- 
	Binding = [out(Param, Symbol), Actions],
	(member([Input, Param], Transfers) -> 
	  ( member(binding(Input, graph(Name)), Values), 
	    composition:pull_from_context(Context, Name, Graph_init), 
	    graph:rename(Graph_init, Symbol, Graph))
	  ; graph:build_graph(Symbol, Graph)).

%% handle_output(?Output, ?Graph, ?Actions)
% Accessor used to explore the output of an algorithm.
handle_output(Output, Graph, Actions) :- Output = algo_out(Graph, Actions).

%%%%
%% Algorithm Verification
%%%%

%% verify(+Algo, +Call_values)
% Throw an error if Algo cannot be called on Call_values.
verify(Algorithm, Call_values) :- 
	\+ description(Algorithm,_,_),
	throw_error(Algorithm, Call_values, unknown, 'unknown algorithm').
verify(Algorithm, _) :- 
	description(Algorithm,_,_).

%% throw_error(+Algo, +Call_values, +Code, +Reason)
% Helper predicate to factorize error throwing. Throw an error 
% (=|gcoke_algo_error|=) for Algo and Call_values, using Code and Reason to
% discriminate errors.
throw_error(Algo, Call_values, Code, Reason) :- 
	Error = error(gcoke_algo_error(Call_values,Code), 
	              context(algo(Algo),Reason)),
	throw(Error).


%%%%                                 %%%%
%%                Attic                %%
%%%%                                 %%%%

%%%%
%%  Algorithm signature verification
%%%%

% verify(Algo, _) :- 
%  	\+ description(Algo, _, _), 
% 	writef('Unknown algorithm: %w.', [Algo]), fail.
% verify(Algo, Call_values) :- 
%  	description(Algo, _, Signature), 
%  	verify_parameters(Algo, Signature, Call_values).

% verify_parameters(Algo, Signature, Call_values) :- 
%  	findall(E, check_param_error(Signature, Call_values, E), Err_list),
%  	(Err_list = [] -> true ; writef('%w: %w',[Algo, Err_list]), fail).

% check_param_error(Signature, Call_values, is_extra(P)) :- 
%  	member([P,_], Call_values), 
%  	\+ (member(in(P,_), Signature) | member(out(P), Signature)).
% check_param_error(Signature, Call_values, is_missing(P)) :- 
% 	member(in(P,_), Signature), \+ member([P,_],Call_values).
% check_param_error(Signature, Call_values, has_wrong_type(P)) :- 
% 	member(in(P,Pred), Signature), member([P,Value], Call_values),
% 	\+ call(Pred, Value).
% check_param_error(Signature, Call_values, forgotten_output(P)) :- 
% 	member(out(P), Signature), \+ member([P,_], Call_values).
