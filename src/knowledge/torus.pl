% Imagine a donut torus - poloidal direction is the the vertical 'through the hole' direction
% toroidal direction is then 'around the donut' or moving horizontally.
:- dynamic
	poloidalCircumference/1,
	toroidalCircumference/1.
	
torus :-
	poloidalCircumference(_), toroidalCircumference(_).

% The cylinder is "lying on its' side"
horizontalCylinder :-
	poloidalCircumference(_), not(toroidalCircumference(_)).
	
% The cylinder is "standing upright"
verticalCylinder :-
	not(poloidalCircumference(_)), toroidalCircumference(_).
	
rectangle :-
	not(poloidalCircumference(_)), not(toroidalCircumference(_)).
	


relativePositionOfCoordinatesFromMe(X, Y, XFromMe, YFromMe) :-
	myPosition(MyX, MyY),
	relativePositionOfCoordinatesFromPoint(X, Y, MyX, MyY, XFromMe, YFromMe).
		
relativeToAbsolutePositionOfPoints((Xr, Yr), (XFromMyOrigin, YFromMyOrigin)) :-
	relativeToAbsolutePositionOfCoordinates(Xr, Yr, XFromMyOrigin, YFromMyOrigin).
	
relativeToAbsolutePositionOfCoordinates(Xr, Yr, XFromMyOrigin, YFromMyOrigin) :-
	myPosition(MyX, MyY),
	X is Xr + MyX,
	Y is Yr + MyY,	
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin).
	
translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin) :-
	relativePositionOfCoordinatesFromPoint(X, Y, 0, 0, XFromMyOrigin, YFromMyOrigin).
	
relativePositionOfCoordinatesFromPoint(X, Y, PointX, PointY, XFromPoint, YFromPoint) :-
	distanceFromSourceToDestinationByDimension(PointX, PointY, X, Y, XFromPoint, YFromPoint).
	
distanceBetweenPoints_Euclidian(Point1X, Point1Y, Point2X, Point2Y, Distance) :-
	distanceFromSourceToDestinationByDimension(Point1X, Point1Y, Point2X, Point2Y, Dx, Dy),
	Distance is sqrt((Dx^2) + (Dy^2)).

distanceBetweenPoints_Manhattan(Point1X, Point1Y, Point2X, Point2Y, Distance) :-
	distanceFromSourceToDestinationByDimension(Point1X, Point1Y, Point2X, Point2Y, Dx, Dy),
	Distance is abs(Dx) + abs(Dy).
	
distanceFromSourceToDestinationByDimension(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	torus ->
		distanceFromSourceToDestinationByDimension_torus(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	horizontalCylinder ->
		distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	verticalCylinder ->
		distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	rectangle ->
		distanceFromSourceToDestinationByDimension_rectangle(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
		fail.	
	
distanceFromSourceToDestinationByDimension_torus(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, _, Dy),
	distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, _).
	
distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	poloidalCircumference(PC),
	Dx is DestinationX - SourceX,
	NaiveDy is (DestinationY - SourceY) mod PC,
	(
		(NaiveDy > PC/2, Dy is NaiveDy - PC) 
		;
		(NaiveDy < -(PC/2), Dy is PC + NaiveDy)
		;
		(NaiveDy =< PC/2, NaiveDy >= -(PC/2), Dy = NaiveDy)
	).
	
distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	toroidalCircumference(TC),
	NaiveDx is (DestinationX - SourceX) mod	TC,
	(
		(NaiveDx > TC/2, Dx is NaiveDx - TC) 
		;
		(NaiveDx < -(TC/2), Dx is TC + NaiveDx)
		;
		(NaiveDx =< TC/2, NaiveDx >= -(TC/2), Dx = NaiveDx)
	),
	Dy is DestinationY - SourceY.
	
distanceFromSourceToDestinationByDimension_rectangle(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	Dx is DestinationX - SourceX,
	Dy is DestinationY - SourceY.