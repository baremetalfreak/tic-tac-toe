type playerT =
  | X
  | O;

type gridCellT = option(playerT);

type participantT =
  | Player(playerT)
  | Observer;

type t('a) =
  | PlayMove: t(int)
  | Board: t((list(gridCellT), playerT, participantT))
  | Restart
  | Disconnect;

let stringify = (type a, t: t(a)) =>
  switch (t) {
  | PlayMove => "PlayMove"
  | Board => "Board"
  | Restart => "Restart"
  | Disconnect => "disconnect"
  };