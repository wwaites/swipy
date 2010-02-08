/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdf_debug,
	  [ count/1
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).


		 /*******************************
		 *	       MEASURE		*
		 *******************************/

:- meta_predicate
	count(:).

count(G) :-
	get_time(T0),
	statistics(cputime, CPU0),
	C = c(0),
	(   G,
	    arg(1, C, C0),
	    C1 is C0+1,
	    nb_setarg(1, C, C1),
	    fail
	;   arg(1, C, Count)
	),
	statistics(cputime, CPU1),
	get_time(T1),
	CPU is CPU1 - CPU0,
	Wall is T1 - T0,
	format('~D solutions, ~2f CPU in ~2f seconds~n',
	       [ Count, CPU, Wall ]).


		 /*******************************
		 *	     PORTRAY		*
		 *******************************/

:- multifile
	user:portray/1.

user:portray(X) :-
	atom(X),
	rdf_global_id(NS:Local, X),
	(   rdfs_label(X, Label)
	->  format('~q (~w)', [NS:Local, Label])
	;   writeq(NS:Local)
	).
