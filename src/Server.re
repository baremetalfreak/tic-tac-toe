module InnerServer = BsSocket.Server.Make(CommonTypes);

let player = ref(0);

let board =
  CommonTypes.(
    [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|]
  );

let startSocketIOServer = http => {
  print_endline("starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    socket => {
      open InnerServer;
      let updateClients = board => {
        Socket.broadcast(socket, Board, (Array.to_list(board), true));
        Socket.emit(socket, Board, (Array.to_list(board), false));
      };
      let onRestart = () => {
        Array.fill(board, 0, 9, Empty);
        updateClients(board);
      };
      let onPlayMove = (player, cell) => {
        board[cell] = player === 0 ? X : O;
        updateClients(board);
      };
      Js.log2("Got a connection:", player);
      Socket.on(socket, CommonTypes.Restart, onRestart);
      Socket.on(socket, CommonTypes.PlayMove, onPlayMove(player^));
      Socket.emit(
        socket,
        CommonTypes.Board,
        (Array.to_list(board), player^ === 0),
      );
      player := (player^ + 1) mod 2;
    },
  );
};