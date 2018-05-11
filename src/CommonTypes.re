type gridCellT =
  | X
  | O
  | Empty;

type action =
  | PlayMove(int)
  | Board(list(gridCellT));

type t('a) =
  | Message: t(action)
  | MessageOnEnter: t(int)
  | UnusedMessageType: t('a);

let stringify = (type a, t: t(a)) =>
  switch (t) {
  | Message => "Message"
  | MessageOnEnter => "MessageOnEnter"
  | UnusedMessageType => "UnusedMessageType"
  };