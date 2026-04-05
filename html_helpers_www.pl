%:- use_module(library(lists)).
%:- use_module(library(dom)).
%:- use_module(library(js)).
:- discontiguous facets/2.

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

facets(element, [
	   template(genericWithContent)]).

facets(contentFormControl, [
	  base(element),
	  add_class('form-control')]).

facets(selfClosingFormControl, [
		add_class('form-control'),
		template(genericSelfClosing)]).

facets(input, [
	   base(selfClosingFormControl),
	   tag(input)]).

facets(textFormFieldElement, [
	   base(input),
	   type(text)]).

facets(hiddenFormFieldElement, [
       base(input),
	   type(hidden)]).

facets(selectFormFieldElement, [
	   template(genericWithContent),
	   tag(select),
	   add_class('form-control')]).

facets(labelElement, [base(element), tag(label)]).
facets(formGroup, [
	   base(div),
	   add_class('form-group')]).
facets(formGroupRow, [
	   base(row),
	   add_class('form-group')]).

facets(row, [base(div), add_class(row)]).
facets(col, [base(div), add_class(col)]).
facets(div, [base(element), tag(div)]).
facets(form, [base(element), tag(form)]).
facets(radioButtonElement, [
	   base(input),
	   add_class('custom-control-input'),
	   type('radio')]).


facets(emailInput, [
	base(textFormFieldElement),
	id('email-input'),
	name('email-input')
]).

isBase(base(_)) :- true.

getBase(Facets, Base) :- member(base(Base), Facets), !.
getBase(_, root).


getFacet(Element, Facet) :- facets(Element, Facets), member(Facet, Facets).
getFacet(Element, Facet) :- facets(Element, Facets), getBase(Facets, Base), getFacet(Base, Facet).

getFacet(element(Facets), Facet) :- member(Facet, Facets).
getFacet(element(Facets), Facet) :- getBase(Facets, Base), getFacet(Base, Facet).

getFacet(Facets, Facet) :- is_list(Facets), member(Facet, Facets).
getFacet(Facets, Facet) :- is_list(Facets), getBase(Facets, Base), getFacet(Base, Facet).

allFacets(Element, AllList) :- findall(Facet, getFacet(Element, Facet), AllList).


classStringFromFacets([], Acc, Acc).
classStringFromFacets([add_class(C)|Tail], Acc, ClassAttribute) :- atomic_list_concat([Acc, C, ' '], NewAcc), classStringFromFacets(Tail, NewAcc, ClassAttribute).
classStringFromFacets([F|Tail], Acc, ClassAttribute) :- F \= add_class(_),  classStringFromFacets(Tail, Acc, ClassAttribute).

addClassAttribute(Facets, NewFacetList) :-
	classStringFromFacets(Facets, '', '') -> NewFacetList = Facets ; classStringFromFacets(Facets, '', ClassString), NewFacetList = [class(ClassString)|Facets].

allFacetsWithClass(Element, NewFacetList) :- allFacets(Element, Facets), addClassAttribute(Facets, NewFacetList).


logger(Logger) :- prop(console, C), prop(C, log, Logger).

log(List) :-
	is_list(List),
	logger(L),
	apply(L, ['log: ' | List], _).

log(S) :-
	atom(S),
	log([S]).

log(S1, S2) :- log([S1, S2]).
log(S1, S2, S3) :- log([S1, S2, S3]).
log(S1, S2, S3, S4) :- log([S1, S2, S3, S4]).
log(S1, S2, S3, S4, S5) :- log([S1, S2, S3, S4, S5]).



textFormGroup([
	base(formGroupRow),

	children([
		[
			base(labelElement),
			htmlContent('Enter Email:'),
			for('email-input')
		],
		[
			base(textFormFieldElement),
			name('email-input'),
			id('email-input'),
			value('')
		]
	])
]).



configAttrFromFacet(DE, name(V)) :- attr(DE, name, V).
configAttrFromFacet(DE, value(V)) :- attr(DE, value, V).
configAttrFromFacet(DE, type(V)) :- attr(DE, type, V).
configAttrFromFacet(DE, id(V)) :- attr(DE, id, V).
configAttrFromFacet(DE, style(V)) :- attr(DE, style, V).
configAttrFromFacet(DE, for(V)) :- attr(DE, for, V).
configAttrFromFacet(DE, checked(V)) :- attr(DE, checked, V).
configAttrFromFacet(DE, placeholder(V)) :- attr(DE, placeholder, V).
configAttrFromFacet(DE, class(V)) :- attr(DE, class, V).
configAttrFromFacet(_, _) :- true.

append_by_id(Id, Child) :-
	get_by_id(Id, Parent),
	append_child(Parent, Child).


facets(userFormContainer, [
	base(form),
	id('user-form-container'),
	children([
		[
			tag(button),
			type(button),
			htmlContent('Cancel'),
			add_class('btn btn-success')
		],
		[
			tag(button),
			type(button),
			htmlContent('Submit'),
			add_class('btn btn-danger')
		],
		[
			base(formGroup),
			children([
				[
					base(labelElement),
					htmlContent('Email: '),
					for('email-input')
				],
				[
					base(textFormFieldElement),
					name('email-input'),
				 	id('email-input'),
				 	value('some value')
				]

			])
		]

	])
]).

% facets(contactEmailFormGroup, [
%     base(formGroup),
%     children([
%         [
% 			base(labelElement),
% 			htmlContent('Email: '),
% 			for('email-input')
% 		],
%         [
%             base(textFormFieldElement),
%             name('email-input'),
%             id('email-input'),
%             value('some value')
%         ]
%     ])
% ]).

facets(contactForm, [
	tag(form),
	children([
		contactEmailFormGroup
	])
]).

% create_logged/2

create_logged(Name, E) :-
	log(['create_element: ', Name]),
	create(Name, E).

% prop_global/3

prop_chain([], Value, Value).

prop_chain([Property|Tail], Acc, Value) :-
	prop(Acc, Property, Value1),
	prop_chain(Tail, Value1, Value).

prop_chain_global(List, Value) :-
	global(Global),
	prop_chain(List, Global, Value).



% append_tpl_to/2 -- clones template contents from TplID and appends to DestID

append_tpl_to(DestID, TplID) :-
	atom(DestID),
	get_by_id(DestID, Dest),
	append_tpl_to(Dest, TplID).

append_tpl_to(Dest, TplID) :-
	get_by_id(TplID, Tpl),
	prop(Tpl, content, TplContent),
	prop_chain_global([document], Value),
	apply(Value, importNode, [TplContent, 1], Clone),
	apply(Dest, appendChild, [Clone], _).


%append_tpl_to(ParentId, TplId) :-
%	get_by_id(TplId, Tpl),
%	prop(document, Document),


% append_to/2

append_to(ParentId, E) :-
	atom(ParentId),
	get_by_id(ParentId, Parent),
	append_to(Parent, E).

append_to(Parent, E) :-
	getFacet(E, tag(Tag)),
	allFacetsWithClass(E, AllFacets),
	create_logged(Tag, DE),
	maplist(configAttrFromFacet(DE), AllFacets),
	append_child(Parent, DE),
	(getFacet(E, htmlContent(HTML)), set_html(DE, HTML)
	; true),
	! ,
	(getFacet(AllFacets, children(Children)),
		member(Child, Children),
		append_to(DE, Child)).



print_raw_html(Term) :-
 	get_by_id('pl-output', Element),
 	create(div, Div),
 	set_style(Div, 'margin-bottom', '3px'),
 	set_html(Div, Term),
 	append_child(Element, Div).

print(Term) :-
	prop(print_query_list, Func),
	apply(Func, [Term], _).

print(A,B) :- print([A,B]).
print(A,B,C) :- print([A,B,C]).
print(A,B,C,D) :- print([A,B,C,D]).
print(A,B,C,D,E) :- print([A,B,C,D,E]).


alc(A, Result) :- atomic_list_concat([A], Result).
alc(A,B, Result) :- atomic_list_concat([A,B], Result).
alc(A,B,C, Result) :- atomic_list_concat([A,B,C], Result).
alc(A,B,C,D, Result) :- atomic_list_concat([A,B,C,D], Result).
alc(A,B,C,D,E, Result) :- atomic_list_concat([A,B,C,D,E], Result).

print_query_list(Line) :-
		get_by_id('query-list', QL),
		create(div, Code),
		%alc('?- ', Query, Line),
		set_html(Code, Line),
		append_child(QL, Code).

print_query_list(A,B) :- alc(A,B,R), print_query_list(R).
print_query_list(A,B,C) :- alc(A,B,C,R), print_query_list(R).



init :-

	get_by_id('query-form', Qform),
	bind(Qform, submit, Event, (
		prevent_default(Event),

		get_by_id('enter-query', EQ),
		attr(EQ, value, Query),

		print_query_list('?- ', Query),

		prop(run_query, Func),
		apply(Func, [Query], _),

		attr(EQ, value, '')
	)).

