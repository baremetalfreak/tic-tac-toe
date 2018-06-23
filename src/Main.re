module CustomClient = BsSocket.Client.Make(CommonTypes);

open CommonTypes;

let socket = CustomClient.create();

type gridIndex = int;

type winningCombinationT = (gridIndex, gridIndex, gridIndex);

/* State declaration.
   The grid is a simple linear list
   The turn tells us if it's X or O's turn
   The participant tells us if we are X or O or an observer
   The winningCells will be a list of indices which we'll use to highlight the grid when someone won */
type state = {
  grid: list(CommonTypes.gridCellT),
  turn: CommonTypes.playerT,
  participant: CommonTypes.participantT,
  winningCells: option((gridIndex, gridIndex, gridIndex)),
};

/* Action declaration */
type action =
  | Restart
  | SetParticipant(CommonTypes.participantT)
  | UpdateBoard(list(CommonTypes.gridCellT))
  | Turn(CommonTypes.playerT)
  | Click(int);

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Game");

/* Helper functions for CSS properties. */
let px = x => string_of_int(x) ++ "px";

/* Main function that creates a component, which is a simple record
   `component` is the default record, of which we overwrite initialState, reducer and render
   */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,
  initialState: () => {
    grid: [None, None, None, None, None, None, None, None, None],
    turn: X,
    participant: Observer,
    winningCells: None,
  },
  didMount: self =>
    CustomClient.on(
      socket,
      CommonTypes.Board,
      ((board, turn, participant)) => {
        self.send(SetParticipant(participant));
        self.send(UpdateBoard(board));
        self.send(Turn(turn));
      },
    ),
  /* State transitions */
  reducer: (action, state) =>
    switch (state, action) {
    | (_, SetParticipant(participant)) =>
      ReasonReact.Update({...state, participant})
    | (_, Click(cell)) =>
      CustomClient.emit(socket, CommonTypes.PlayMove, cell);
      /* Return new winningCells, new turn and new grid. */
      ReasonReact.NoUpdate;
    | (_, Restart) =>
      CustomClient.emit(socket, Restart, ());
      ReasonReact.NoUpdate;
    | (_, UpdateBoard(grid)) =>
      let arrGrid = Array.of_list(grid);
      /* Military grade, Machine Learning based, winning-condition checking algorithm:
         just list all the possible options one by one.
         */
      let winningCombinations = [
        (0, 1, 2),
        (3, 4, 5),
        (6, 7, 8),
        (0, 3, 6),
        (1, 4, 7),
        (2, 5, 8),
        (0, 4, 8),
        (2, 4, 6),
      ];
      let isWinning = ((a, b, c)) =>
        arrGrid[a] != None
        && arrGrid[a] == arrGrid[b]
        && arrGrid[b] == arrGrid[c];
      let safeFind = (p, l) =>
        try (Some(List.find(p, l))) {
        | Not_found => None
        };
      let winningCells = safeFind(isWinning, winningCombinations);
      ReasonReact.Update({...state, winningCells, grid});
    | (_, Turn(turn)) => ReasonReact.Update({...state, turn})
    },
  render: self => {
    let winner =
      switch (self.state.winningCells) {
      | None => None
      | Some((i, _, _)) => Array.of_list(self.state.grid)[i]
      };
    let gameMessage =
      switch (winner, self.state.participant, self.state.turn) {
      | (Some(_), _, _) => "Game over, "
      | (None, Observer, _) => "You are an observer"
      | (None, Player(X), X)
      | (None, Player(O), O) => "Your turn"
      | (None, Player(X), O)
      | (None, Player(O), X) => "Their turn"
      };
    let winningMessage =
      switch (winner, self.state.participant) {
      | (Some(_ as winner), Observer) =>
        switch (winner) {
        | X => "X Wins"
        | O => "O Wins"
        }
      | (Some(X), Player(X))
      | (Some(O), Player(O)) => "You win"
      | (Some(X), Player(O))
      | (Some(O), Player(X)) => "You loose"
      | (None, _) => ""
      };
    let myTurn = self.state.participant == Player(self.state.turn);
    ReasonReact.(
      <div
        style=(
          ReactDOMRe.Style.make(
            ~display="flex",
            ~width=px(439),
            ~alignItems="center",
            ~flexDirection="column",
            (),
          )
        )>
        <div style=(ReactDOMRe.Style.make(~fontSize=px(45), ()))>
          (string(gameMessage ++ winningMessage))
        </div>
        <button
          style=(
            ReactDOMRe.Style.make(
              ~fontSize=px(20),
              ~marginTop=px(8),
              ~marginBottom=px(16),
              ~border="1px solid #AAAAAA",
              ~backgroundColor="#EEEEEE",
              ~cursor="pointer",
              (),
            )
          )
          onClick=(_event => self.send(Restart))>
          (string("Restart"))
        </button>
        <div
          style=(
            ReactDOMRe.Style.make(
              ~display="flex",
              ~width=px(443),
              ~height=px(443),
              ~flexWrap="wrap",
              ~justifyContent="left",
              /*~alignItems="center",*/
              /*~backgroundColor="black",*/
              (),
            )
          )>
          (
            /* Iterate over our grid and create the cells, with their contents and background color
               if there's a winningCells.*/
            array(
              Array.of_list(
                List.mapi(
                  (i, piece) => {
                    let (txt, canClick) =
                      switch (piece) {
                      | None => (" ", true)
                      | Some(X) => ("X", false)
                      | Some(O) => ("O", false)
                      };
                    let backgroundColor =
                      switch (self.state.winningCells) {
                      | None => "white"
                      | Some((a, b, c)) =>
                        let isCurrentCellWinner = i == a || i == b || i == c;
                        switch (
                          isCurrentCellWinner,
                          self.state.participant,
                          winner,
                        ) {
                        | (true, Player(X), Some(X))
                        | (true, Player(O), Some(O)) => "green"
                        | (true, Player(X), Some(O))
                        | (true, Player(O), Some(X)) => "red"
                        | (true, Observer, _) => "blue"
                        | (_, _, _) => "white"
                        };
                      };
                    /* We check if the user can click here so we can hide the cursor: pointer. */
                    let canClick =
                      canClick && myTurn && self.state.winningCells == None;
                    <div
                      key=(string_of_int(i))
                      onClick=(_event => canClick ? self.send(Click(i)) : ())
                      style=(
                        ReactDOMRe.Style.make(
                          ~display="flex",
                          ~width=px(145),
                          ~height=px(145),
                          ~fontSize=px(45),
                          ~marginLeft=px(-1),
                          ~paddingTop=px(2),
                          ~marginBottom=px(-1),
                          ~justifyContent="center",
                          ~alignItems="center",
                          ~backgroundColor,
                          ~border="1px solid black",
                          ~cursor=canClick ? "pointer" : "",
                          (),
                        )
                      )>
                      <span> (string(txt)) </span>
                    </div>;
                  },
                  self.state.grid,
                ),
              ),
            )
          )
        </div>
      </div>
    );
  },
};