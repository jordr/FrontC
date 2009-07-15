(* TCL -- Calipso Main library interface.
**
** Project: 		Calipso
** File: 			calipso.ml
** Version:			3.0
** Date:			7.20.99
** Author:			Hugues Cassé
**
*)

exception InternalError

(** Calipso is an OCAML library useful to transform a C source code by removing
	goto, break, continue, and simplifying return and switch.
*)


(*
** Argument definition
*)
let rm_goto = ref false
let verbose_mode = ref false

(* process_defs definition list -> definition list
**		Remove "goto" and statement alike in the definition list.
*)
let rec process_defs (defs : Cabs.definition list) =
	let process_func typ name body =
		Gen.reset ();
		let body' = Reduce.reduce body typ in
		if !rm_goto then Algo.remove body' else body' in
	match defs with
	[] -> []
	| (Cabs.FUNDEF ((base, sto, (id, proto, [], exp)), body))::fol ->
		let _ = if !verbose_mode then prerr_string (id ^ "()\n") in
		(match proto with
		Cabs.PROTO (typ, pars, ell) ->
			(Cabs.FUNDEF (
				(base, sto, (id, proto, [], exp)),
				process_func typ id body))
			::(process_defs fol)
		| _ -> raise InternalError)
	| (Cabs.OLDFUNDEF ((base, sto, (id, proto, [], exp)), decs, body))::fol ->
		let _ = if !verbose_mode then prerr_string (id ^ "()\n") in
		(match proto with
		Cabs.OLD_PROTO (typ, pars, ell) ->
			(Cabs.OLDFUNDEF (
				(base, sto, (id, proto, [], exp)),
				decs,
				process_func typ id body))
		::(process_defs fol)
		| _ -> raise InternalError)
	| def::fol -> def::(process_defs fol)


(** Process operations on an abstract C source file.
	
	This function provide the full set of options.
	
	@param defs the definition list to be processed.
	@param verbose switch to the verbose mode.
	@param use_mask use bitfield masks to handle labels.
	@param remove_goto Remove the goto statements.
	@param remove_break Remove the break statements.
	@param remove_continue Remove the continue statements.
	@param remove_return Remove the return statements.
	@param remove_switch Set the reducing method for switch statements
			(Reduce.NO: don't reduce; Reduce.RAW: reduce rawly; Reduce.KEEP:
			reduce but keep the regular ones; Reduce.REDUCE: use a faster
			method for regular ones.)
	@param strategy Set the algorithme strategy to organize sequences
			(Algo.LEFT: organize sequences to the left; Algo.RIGHT: organize to
			the right; Algo.WEIGHTED: use the weight)
	@return the processed definition list.
*)
let process_remove (defs : Cabs.definition list) verbose use_mask
			remove_goto remove_break remove_continue remove_return
			(remove_switch : Reduce.switch_action)
			(strategy : Algo.strategy_kind) =
	begin
		verbose_mode := verbose;
		Algo.use_mask := use_mask;
		rm_goto := remove_goto;
		Reduce.remove_break := remove_break;
		Reduce.remove_continue := remove_continue;
		Reduce.remove_return := remove_return;
		Reduce.remove_switch := remove_switch;
		if remove_switch = Reduce.KEEP
			then Algo.regular_switch := true;
		Algo.strategy := strategy;
		process_defs defs;
	end

(** Process a standard remove, by removing goto, break, continue, return and
	reducing switch with the Reduce.REDUCE method.
	@param defs the definition list to be processed.
	@return the processed definition list.
*)
let process_standard_remove (defs : Cabs.definition list) =
	process_remove defs false false true true true true Reduce.REDUCE Algo.LEFT

(** Process a subtle remove, by removing goto, break, continue, return and
	reducing switch with the Reduce.KEEP method.
	@param defs the definition list to be processed.
	@return the processed definition list.
*)
let process_subtle_remove (defs : Cabs.definition list) =
	process_remove defs false false true true true true Reduce.KEEP Algo.LEFT

