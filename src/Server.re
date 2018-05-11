module InnerServer = BsSocket.Server.Make(CommonTypes);

let player = ref(0);

open CommonTypes;

let board = [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|];

let startSocketIOServer = http => {
  print_endline("starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    socket => {
      open InnerServer;
      print_endline("Got a connection!");
      Js.log(player);
      let onRestart = () => {
        Array.fill(board, 0, 9, Empty);
        Socket.broadcast(socket, Board, (Array.to_list(board), true));
        Socket.emit(socket, Board, (Array.to_list(board), false));
      };
      let onBoard = (player, cell) => {
        let canPlay = player === 0;
        board[cell] = canPlay ? X : O;
        Socket.broadcast(socket, Board, (Array.to_list(board), true));
        Socket.emit(socket, Board, (Array.to_list(board), false));
      };
      let onPlayMove = (player, cell) => {
        let canPlay = player === 0;
        board[cell] = canPlay ? X : O;
        Socket.broadcast(socket, Board, (Array.to_list(board), true));
        Socket.emit(socket, Board, (Array.to_list(board), false));
      };
      /* Polymorphic pipe which actually knows about CommonTypes.t from InnerServer */
      Socket.on(socket, CommonTypes.Restart, onRestart);
      Socket.on(socket, CommonTypes.Board, onBoard(player^));
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