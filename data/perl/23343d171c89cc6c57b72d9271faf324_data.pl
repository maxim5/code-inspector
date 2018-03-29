/**
	Data module for SWI-Prolog: Predicates to handle data in multimedia computations. The are 3 ways of data representation: prolog lists, pl blobs and blob with identifiers.
	Prolog lists are a limited way to handle data as they easily crash for really long arrays.
	PL Blobs have also size limitations and are cumbersome as datatype. These terms are still included here but deprecated after introducing the blob with identifiers
	Blobs with identifiers. Each block of data (now really arbitrarly large) is identified with an id, so we have BLOBIDs which represent data and are used in their place for computations. Each module of SWI-DSP is and must be implemented to read these identifiers and extract the data from the runtime database defined in swilib.
	This module offers predicates to check the status of the database where each BLOBID is considered as an identifier. The other bunch of predicates treat each BLOBID as a real data object and perform different operations with them.

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007-2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

:- module(data, [	reserve_id/1
		,	current_id/1
		,	next_id/1
		, 	blobs/1
		,	ids_status/3
		, 	busy_id/1
		,	is_blob/1
		,	clean_blob/1
		,	blob_size/2
		,	data_size/2
		,	blobs_mean/3
		,	equal_blobs/2
		,	blob_frame/4
		, 	concat_blobs/3	
		,	blob_list/2
		,	list_blob/2
		,	blob_in/2
		,	blob_out/2
		,	plblob_to_blob/2
		,	blob_to_plblob/2
			]).


:- style_check(-discontiguous).
:- load_foreign_library(swidata).
%:- use_module(library(pldoc)).

%% reserve_id(+BLOBID) is det
% reserves the passed id in the database so it can not be assigned to any outcoming blob unless we pass it for unification

reserve_id(Term):-
	atom(Term),
	reserve_data_id(Term).	

%% is_blob(+Term) is det
% checks if the passed term is a BLOBID (an atom stored in the ids database). The id may be reserved but not pointing to any block of data

is_blob(T):-
	atom(T),
	is_blobid(T, _).
	
%% busy_id(+Term) is det
% checks if the BLOBID is actually pointing to any block of data

busy_id(BLOBID):-
	active_id(BLOBID).

%% clean_blob(+BLOBID) is det
% deletes the data pointed by the ID from memory

clean_blob(T):-
	busy_id(T),
	clean_data(T).

%% blobs(+Number) is det
% Number of blobid in the database

blobs(L):-
	ids_in_db(L).

%% current_id(?BLOBID) is det
% Last id assigned by the system

current_id(I):-	
	current_blob_id(Id),
	I = Id.

%% next_id(?BLOBID) is det
% Next id to assign automatically by the system

next_id(I):-
	next_blob_id(ID),
	I = ID.

%% ids_status(-CurrentID, -NextID, -IDs)
% Shows the BLOBIDs status

ids_status(A, L, O):-
	current_id(A),
	next_id(L),
	blobs(O).
	
%% blob_size(+BLOBID, -Size) is det
% Returns the size of the blob

blob_size(T, L):-
	busy_id(T),
	get_blob_size(T, L).

%% blobs_mean(+BLOBID, +BLOBID2, -BLOBIDMEAN) is det
% Returns the mean of two blobs

blobs_mean(B1, B2, Mean):-
	blob_size(B1, L),
	blob_size(B2, L),
	mean_of_blobs(B1, B2, Mean).

%% equal_blobs(+BLOBID1, +BLOBID2) is det
% Tells if the blobs are equal

equal_blobs(B1, B2):-
	blob_size(B1, L),
	blob_size(B2, L),
	are_equal_blobs(B1, B2).

%% data_size(+DataObject, -Size) is det
% Returns the size of the data object (list of BLOBID)

data_size(O, L):-
	is_list(O),
	length(O, L).

data_size(O, L):-
	blob_size(O, L).

%% blob_frame(+BLOBID1, Start, Size, -BLOBIDFRAME) is det
% Returns a frame of the original blob

blob_frame(B, S, Si, F):-
	busy_id(B),
	get_frame_of_blob(B, S, Si, F).

%% concat_blobs(BLOBID1, BLOBID2, BLOBID3) is det
% Concats two blobs and returns the new one

concat_blobs(B1, B2, B3):-
	busy_id(B1),
	busy_id(B2),
	concat_of_blobs(B1, B2, B3).

%% blob_list(+BLOBID, -List) is det
% Represents the blob as a prolog list. It may crash for very large blobs

blob_list(B, L):-
	busy_id(B),
	blobid_to_list(B, L).

%% list_blob(+List, ?Blob) is det
% Registers a List as a blob. The BLOBID can be passed for unification or assigned as the next id

list_blob(L, B):-
	var(B),!,
	next_id(B),
	reserve_id(B),
	list_to_blobid(L, B).
list_blob(_L, B):-
	nonvar(B),
	busy_blob(B), !, fail.
list_blob(L, B):-
	nonvar(B),
	is_blob(B),
	list_to_blobid(L, B).
list_blob(L, B):-
	nonvar(B),
	\+is_blob(B),
	reserve_id(B),
	list_to_blobid(L, B).

%% blob_out(+BLOBID, +FilePath) is det
% Dumps the binary data held by the blob into a file given the path

blob_out(B, F):-
	busy_id(B),
	blob_to_file(B, F).

%% blob_in(+FilePath, ?BLOBID) is det
% Loads the data of the file into a blob which can be passed for unification

blob_in(F, B):-
	var(B),
	next_id(B), reserve_id(B),
	blob_from_file(F, B).
blob_in(_F, B):-
	nonvar(B),
	busy_blob(B), !, fail.
blob_in(F, B):-
	nonvar(B),
	is_blob(B),
	blob_from_file(F, B).
blob_in(F, B):-
	nonvar(B),
	\+is_blob(B),
	reserve_id(B),
	blob_from_file(F, B).

%% plblob_to_blob(+PlBlob, +BLOBID) is det
% Deprecated: registering a pl_term_t blob as blob id

%% blob_to_plblob(+BLOBID, -PlBlob) is det
% Deprecated: exporting a blob id as pl blob
