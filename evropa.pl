eu(belehrad, bukurest, 447).
eu(belehrad, budapest, 316).
eu(berlin, kodan, 354).
eu(hamburg, kodan, 287).
eu(berlin, hamburg, 254).
eu(brusel, hamburg, 489).
eu(bukurest, istanbul, 445).
eu(brusel, londyn, 318).
eu(dublin, londyn, 462).
eu(barcelona, madrid, 504).
eu(berlin, mnichov, 501).
eu(budapest, mnichov, 563).
eu(milano, mnichov, 348).
eu(brusel, pariz, 261).
eu(londyn, pariz, 340).
eu(berlin, praha, 280).
eu(budapest, praha, 443).
eu(hamburg, praha, 492).
eu(mnichov, praha, 300).
eu(milano, rim, 476).
eu(belehrad, sofia, 329).
eu(bukurest, sofia, 296).
eu(istanbul, sofia, 502).
eu(kodan, stockholm, 521).
eu(belehrad, viden, 489).
eu(berlin, viden, 523).
eu(budapest, viden, 216).
eu(mnichov, viden, 354).
eu(praha, viden, 250).
eu(varsava, viden, 557).
eu(berlin, varsava, 516).
eu(budapest, varsava, 545).
eu(praha, varsava, 514).

%findall(X, eu(Brusel, X, _), L).

way(X, Y):- eu(X, Y, _).
way(X, Y):- eu(Y, X, _).

%appendSuccessors(City, Open, New):- findall(Dest, way(City, Dest), List), append(Open, List, New).

appendSuccessors(state(City, Closed), Open, New):- findall(state(Dest, [City|Closed]), way(City, Dest), List), append(Open, List, New).

%bfs([state(berlin, [])], mnichov).
%takto se programuje akumulátor
bfs([state(Dest, Path)|_], Dest, Path).
bfs([state(Goal, Closed)|Tail, Goal, Path):-member(City, Closed), !, bfs(Tail, Goal, Path). %opravit podle pastebinu!
bfs([State|Open], Dest, Path):- appendSuccessors(State, Open, New), bfs(New, Dest, Path).

