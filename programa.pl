% Parcial Kiosco.

% atiende(Nombre,Dia,HorarioEntrada,HorarioSalida)
atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).
atiende(lucas,martes,10,20).
atiende(juanC,sabado,18,22).
atiende(juanC,domingo,18,22).
atiende(juanFds,jueves,10,20).
atiende(juanFds,viernes,12,20).
atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).
atiende(martu,miercoles,23,24).


%1 

atiende(vale,Dia,HorarioEntrada,HorarioSalida):-
    atiende(dodain,Dia,HorarioEntrada,HorarioSalida).

atiende(vale,Dia,HorarioEntrada,HorarioSalida):-
    atiende(juanC,Dia,HorarioEntrada,HorarioSalida).

%2  quienAtiende(Persona,Dia,Hora)

quienAtiende(Persona,Dia,Hora):-
    atiende(Persona,Dia,HorarioEntrada,HorarioSalida),
    between(HorarioEntrada, HorarioSalida, Hora).
    
%3 foreverAlone(Persona,Dia,Hora)

foreverAlone(Persona,Dia,Hora):-
    atiende(Persona,Dia,_,_),
    not(atiendeOtraPersona(Persona,Dia,Hora)).

atiendeOtraPersona(Persona,Dia,Hora):-
    quienAtiende(OtraPersona, Dia, Hora), 
    Persona \= OtraPersona.

%4  posibilidadesDeAtencion(Persona,Dia) --------- NO ENTENDI

posibilidadesDeAtencion(Persona,Dia):-
    findall(Nombres,quienesAtiendenEseDia(Nombres,Dia,_) , ListaDeNombres),
    list_to_set(ListaDeNombres,Persona).


quienesAtiendenEseDia(Nombres,Dia,Hora):-
    between(0, 24, Hora),
    findall(Nombre,quienAtiende(Nombre,Dia,Hora), Nombres).

%5 

venta(dodain,lunes,10,8,[golosinas(1200),cigarrillos([jockey]),golosinas(50)]).
venta(dodain,miercoles,12,8,[bebida(true,8),bebida(false,1),golosina(10)]).
venta(martu,miercoles,12,8,[golosinas(1000),cigarrillos([chesterfield,colorado,parisiennes])]).
venta(lucas,martes,11,8,[golosinas(600)]).
venta(lucas,martes,18,8,[bebida(false,2),cigarrillos([derby])]).

personaSuertuda(Persona):-
    venta(Persona,_,_,_,_),
    forall(venta(Persona,_,_,_,[Venta|_]), esVentaImportante(Venta)).


esVentaImportante(golosinas(Precio)):-
    Precio > 100.

esVentaImportante(cigarrillos([MarcasCigarrillos])):-
    length(MarcasCigarrillos, CantidadDeMarcas),
    CantidadDeMarcas > 2 .

esVentaImportante(bebida(true,_)).

esVentaImportante(bebida(false,Cantidad)):-
    Cantidad > 5.
    

    