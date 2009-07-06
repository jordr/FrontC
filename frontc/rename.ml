(** Change all occurences of a name into another in a C source file.
	
	The main entry point of this module is the rename function.
	
	@author Florian Birée <florian\@biree.name>
	@version 0.2
*)

open Cabs

(** Type to manage various kind of C elements. *)
type elt =
	| E_DEF of definition list
	| E_BASETYPE of base_type
	| E_SINGLE_NAME of single_name
	| E_NAME_GROUP of name_group
	| E_BODY of body
	| E_EXPR of expression

(** Generic renaming function.
	
	For general purpose, see the rename function.
	
	@param element a C element to look for names to rename.
	@param rn_name function that is applied on all names.
	@return the resulting element.
*)
let rec generic_rename = fun element rn_name ->
	
	(* recursive calls to generic rename, with type convertion *)
	let generic_rn_def = fun deflist ->
		match generic_rename (E_DEF(deflist)) rn_name with
			E_DEF(result) -> result
			| _ -> failwith("Bad answer") in
	let generic_rn_basetype = fun basetype ->
		match generic_rename (E_BASETYPE(basetype)) rn_name with
			E_BASETYPE(result) -> result
			| _ -> failwith("Bad answer") in
	let generic_rn_single_name = fun sn ->
		match generic_rename (E_SINGLE_NAME(sn)) rn_name with
			E_SINGLE_NAME(result) -> result
			| _ -> failwith("Bad answer") in
	let generic_rn_name_group = fun ng ->
		match generic_rename (E_NAME_GROUP(ng)) rn_name with
			E_NAME_GROUP(result) -> result
			| _ -> failwith("Bad answer") in
	let generic_rn_body = fun body ->
		match generic_rename (E_BODY(body)) rn_name with
			E_BODY(result) -> result
			| _ -> failwith("Bad answer") in
	let generic_rn_expr = fun expr ->
		match generic_rename (E_EXPR(expr)) rn_name with
			E_EXPR(result) -> result
			| _ -> failwith("Bad answer") in
	
	(* renaming in a constant *)
	let rn_constant = fun constant ->
		match constant with
			CONST_INT(name) -> CONST_INT(rn_name name)
			| CONST_FLOAT(name) -> CONST_FLOAT(rn_name name)
			| CONST_CHAR(name) -> CONST_CHAR(rn_name name)
			| CONST_STRING(name) -> CONST_STRING(rn_name name)
			| CONST_COMPOUND(expr_list) ->
				CONST_COMPOUND(List.map generic_rn_expr expr_list) in
	
	(* renaming in an expression *)
	let rec rn_expr = fun expr ->
		match expr with
			UNARY(op, u_expr) ->
				UNARY(op, (rn_expr u_expr))
			| BINARY(op, expr1, expr2) ->
				BINARY(op, (rn_expr expr1), (rn_expr expr2))
			| QUESTION(expr1, expr2, expr3) ->
				QUESTION((rn_expr expr1), (rn_expr expr2), (rn_expr expr3))
			| CAST(base_type, c_expr) ->
				CAST((generic_rn_basetype base_type), (rn_expr c_expr))
			| CALL(c_expr, expr_list) ->
				CALL((rn_expr c_expr), List.map rn_expr expr_list)
			| COMMA(expr_list) ->
				COMMA(List.map rn_expr expr_list)
			| CONSTANT(cst) ->
				CONSTANT(rn_constant cst)
			| VARIABLE(name) ->
				VARIABLE(rn_name name)
			| EXPR_SIZEOF(s_expr) ->
				EXPR_SIZEOF(rn_expr s_expr)
			| TYPE_SIZEOF(base_type) ->
				TYPE_SIZEOF(generic_rn_basetype base_type)
			| INDEX(expr1, expr2) ->
				INDEX((rn_expr expr1), (rn_expr expr2))
			| MEMBEROF(m_expr, name) ->
				MEMBEROF((rn_expr m_expr), (rn_name name))
			| MEMBEROFPTR(m_expr, name) ->
				MEMBEROFPTR((rn_expr m_expr), (rn_name name))
			| GNU_BODY(body) ->
				GNU_BODY(generic_rn_body body)
			| EXPR_LINE(exprl, str, num) ->
				EXPR_LINE((rn_expr exprl), str, num)
			| other -> other in
	
	(* renaming in a proto *)
	let rn_proto = fun proto ->
		let (base_type, single_name_list, other_param) = proto in
		let new_sn_list = (List.map generic_rn_single_name single_name_list)
		in ((generic_rn_basetype base_type), new_sn_list, other_param)
		in
	
	(* renaming in an old K&R proto *)
	let rn_old_proto = fun old_proto ->
		let (base_type, str_list, other_param) = old_proto
		in (generic_rn_basetype base_type, List.map rn_name str_list,
			other_param) in
	
	(* renaming in an enum item *)
	let rn_enum_item = fun enum_item ->
		let (str, expr) = enum_item
		in ((rn_name str), (rn_expr expr)) in
	
	(* renaming in gnu_attr *)
	let rec rn_gnu_attr = fun gnu_attr ->
		match gnu_attr with
			GNU_CALL(str,call_gnu_attrs) ->
				GNU_CALL(
					(rn_name str),
					(List.map rn_gnu_attr call_gnu_attrs)
					)
			| GNU_ID(str) -> GNU_ID(rn_name str)
			| other -> other in
	
	(* renaming in gnu_attrs *)
	let rn_gnu_attrs = fun gnu_attrs ->
		List.map rn_gnu_attr gnu_attrs in
	
	(* renaming in a base type *)
	let rec rn_base_type = fun base_type ->
		match base_type with
			PTR(ptr_base_type) ->
				PTR(rn_base_type ptr_base_type)
			| RESTRICT_PTR(ptr_base_type) ->
				RESTRICT_PTR(rn_base_type ptr_base_type)
			| ARRAY(a_base_type, expr) ->
				ARRAY((rn_base_type a_base_type), (rn_expr expr))
			| STRUCT(name, name_group_list) ->
				STRUCT(
					(rn_name name),
					(List.map generic_rn_name_group name_group_list)
				)
			| UNION(name, name_group_list) ->
				UNION(
					(rn_name name),
					(List.map generic_rn_name_group name_group_list)
				)
			| PROTO(proto) ->
				PROTO(rn_proto proto)
			| OLD_PROTO(old_proto) ->
				OLD_PROTO(rn_old_proto old_proto)
			| NAMED_TYPE(name) ->
				NAMED_TYPE(rn_name name)
			| ENUM(name, enum_item_list) ->
				ENUM((rn_name name), (List.map rn_enum_item enum_item_list))
			| CONST(c_base_type) ->
				CONST(rn_base_type c_base_type)
			| VOLATILE(v_base_type) ->
				VOLATILE(rn_base_type v_base_type)
			| GNU_TYPE(gnu_attrs, g_base_type) ->
				GNU_TYPE(
					(rn_gnu_attrs gnu_attrs),
					(rn_base_type g_base_type)
				)
			| other -> other in
	
	(* renaming a cabs_name (type name in Cabs) *)
	let rn_cabs_name = fun cabs_name ->
		let (name, base_type, gnu_attrs, expr) = cabs_name
		in ((rn_name name),
			(rn_base_type base_type),
			(rn_gnu_attrs gnu_attrs),
			(rn_expr expr)) in
	
	(* renaming in a single_name *)
	let rn_single_name = fun single_name ->
		let (base_type, storage, cabs_name) = single_name
		in ((rn_base_type base_type), storage, (rn_cabs_name cabs_name)) in
	
	(* renaming in a name_group *)
	let rn_name_group = fun name_group ->
		let (base_type, storage, cabs_name_list) = name_group
		in let res_name_list = List.map rn_cabs_name cabs_name_list
		in ((rn_base_type base_type), storage, res_name_list) in
	
	(* renaming in statement *)
	let rec rn_statement = fun statement ->
		match statement with
			COMPUTATION(expr) ->
				COMPUTATION(rn_expr expr)
			| BLOCK(body) ->
				BLOCK(generic_rn_body body)
			| SEQUENCE(stat1, stat2) ->
				SEQUENCE(rn_statement stat1, rn_statement stat2)
			| IF(expr, stat1, stat2) ->
				IF(rn_expr expr, rn_statement stat1, rn_statement stat2)
			| WHILE(expr, stat) ->
				WHILE(rn_expr expr, rn_statement stat)
			| DOWHILE(expr, stat) ->
				DOWHILE(rn_expr expr, rn_statement stat)
			| FOR(expr1, expr2, expr3, stat) ->
				FOR(
					(rn_expr expr1),
					(rn_expr expr2),
					(rn_expr expr3),
					(rn_statement stat)
					)
			| RETURN(expr) ->
				RETURN(rn_expr expr)
			| SWITCH(expr, stat) ->
				SWITCH((rn_expr expr), (rn_statement stat))
			| CASE(expr, stat) ->
				CASE((rn_expr expr), (rn_statement stat))
			| DEFAULT(stat) ->
				DEFAULT(rn_statement stat)
			| LABEL(str, stat) ->
				LABEL(str, (rn_statement stat))
			| STAT_LINE(stat, str, num) ->
				STAT_LINE((rn_statement stat), str, num)
			| other -> other in
	
	(* renaming in a body *)
	let rec rn_body = fun body ->
		let (def_list, statement) = body
		in ((generic_rn_def def_list), (rn_statement statement)) in
	
	(* renaming in a definition list *)
	let rec rn_def_list = fun def_list ->
		match def_list with
			FUNDEF(single_name,body) :: t ->
				FUNDEF(
					(rn_single_name single_name),
					(rn_body body)
				)::(rn_def_list t)
			| OLDFUNDEF(single_name,name_group_list,body) :: t ->
				OLDFUNDEF(
					(rn_single_name single_name),
					(List.map rn_name_group name_group_list),
					(rn_body body)
				) :: (rn_def_list t)
			| DECDEF(name_group) :: t ->
				DECDEF(rn_name_group name_group) :: (rn_def_list t)
			| TYPEDEF(name_group,gnu_attrs) :: t ->
				TYPEDEF(
					(rn_name_group name_group),
					(rn_gnu_attrs gnu_attrs)
				) :: (rn_def_list t)
			| ONLYTYPEDEF(name_group) :: t ->
				ONLYTYPEDEF(rn_name_group name_group) :: (rn_def_list t)
			| [] -> []
	
	(* dispatch generic elements -- generic_remane *)
	in match element with
		E_DEF(def_list) -> E_DEF(rn_def_list def_list)
		| E_BASETYPE(base_type) -> E_BASETYPE(rn_base_type base_type)
		| E_SINGLE_NAME(single_name) ->
			E_SINGLE_NAME(rn_single_name single_name)
		| E_NAME_GROUP(group_name) -> E_NAME_GROUP(rn_name_group group_name)
		| E_BODY(body) -> E_BODY(rn_body body)
		| E_EXPR(expr) -> E_EXPR(rn_expr expr)

(** Change all occurences of a name to another in a C source file.
	This will replace names without checking the type of the elements
	(the file will keep the same naming scheme).
	
	This function hopes that the new name doesn't currently exists.
	
	@return the resulting file.
*)
let rename = fun old_name new_name tree ->
	(* return the named, changed if needed *)
	let rn_name = fun name ->
		if old_name = name
			then new_name
			else name
	(* convert the tree into a generic element *)
	in match (generic_rename (E_DEF(tree)) rn_name) with
		E_DEF(new_tree) -> new_tree
		| _ -> failwith("Not implemented")

