type playerT =
  | X
  | O;

type gridCellT = option(playerT);

type t('a) =
  | PlayMove: t(int)
  | Board: t((list(gridCellT), bool))
  | Restart
  | Disconnect;

let stringify = (type a, t: t(a)) =>
  switch (t) {
  | PlayMove => "PlayMove"
  | Board => "Board"
  | Restart => "Restart"
  | Disconnect => "disconnect"
  };