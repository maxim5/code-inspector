% Implementation de la methode de Runge-Kutta, pour voir

function Resultat=RK4(P0,N,XFin)

% Où :
% - F est la fonction à intégrer (tq dy/dx = F(x,y) )
% - P0 est la matrice (1,2) définissant le point de départ de la méthode
% - N est le nombre d'itérations
% - XFin est l'abscisse où s'arrête la méthode
%
% le pas utilisé est obtenu via (XFin - X0)/N
%
% Resultat est retourné sous la forme d'une matrice à n+1 lignes et deux
% colonnes :
%
% x0 | y0
% x1 | y1
%   ...
% xn | yn

Pas = ( XFin - P0( 1, 1 ) ) / N;

% Initialisation du vecteur-résultat.
Resultat = zeros( N + 1, 2 );

Resultat( 1, : ) = P0;

% Initialisation de la matrice stockant les coef k1, k2, k3, k4
k = zeros( 1, 4 );

% Le point courant
Xprev=zeros( 1, 1 );
Yprev=zeros( 1, 1 );

for i = 2:1:( N + 1 )

    % Calcul de l'abscisse du point suivant
    Resultat( i , 1 ) = Resultat( 1, 1 ) + ( ( i - 1 ) * Pas );

    Xprev = Resultat( ( i - 1 ), 1 );
    Yprev = Resultat( ( i - 1 ), 2 );

    % Calcul des coefficients k1, k2, k3, k4
    k( 1 ) = Pas * F( Xprev, Yprev );

    k( 2 ) = Pas * F( Xprev + ( Pas / 2 ), Yprev + ( k( 1 ) * ( Pas / 2 ) ) );

    k( 3 ) = Pas * F( Xprev + ( Pas / 2 ), Yprev + ( k( 2 ) * ( Pas / 2 ) ) );

    k( 4 ) = Pas * F( Xprev + Pas, Yprev + ( k( 3 ) * Pas ) );

    % Calcul de l'abscisse finale
    Resultat( i, 2 ) = Resultat( ( i - 1 ), 2 ) + ( 1 / 6 ) * ( k( 1 ) + 2 * k( 2 ) + 2 * k( 3 ) + k( 4 ) );

end
